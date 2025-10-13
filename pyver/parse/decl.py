from lex import Tok, Loc, LocPtr
from lex.src_loc import LOC_INVALID
from ns_ast.nodes.decl import (
    Decl,
    EnumDecl,
    StructDecl,
    TypeDecl,
    FieldDecl,
    EnumVariantDecl,
    ParamDecl,
    VarDecl,
    FnDecl,
    TranslationUnitDecl,
)
from ns_ast.nodes.types import Type, EnumType, StructType, AliasType
from semantic_analysis import actions, ScopeFlags
from utils.diagnostic import diag, Diag
from .parser import parser

from typing import Tuple


def parse_translation_unit() -> TranslationUnitDecl:
    decls = []
    actions.act_on_start_of_translation_unit()
    while parser().tok.ty != Tok.EOF:
        d = parse_top_level_decl()
        if d is not None:
            decls.append(d)
    actions.act_on_end_of_translation_unit()
    return TranslationUnitDecl(decls)


def parse_top_level_decl() -> Decl | None:
    return parse_decl(False)


def parse_decl(in_func: bool, decl_end: LocPtr = None) -> Decl | None:
    filectx = not in_func
    decl = None
    if parser().tok.ty == Tok.KW_FN:
        if not filectx:
            diag(
                parser().tok.loc,
                "Can't declare a function in a function body (FATAL)",
                Diag.ERROR,
            )
            assert False
        decl = parse_fn_decl()
    elif parser().tok.ty == Tok.KW_LIB:
        if not filectx:
            diag(
                parser().tok.loc,
                "Can't declare a lib decl in a function body",
                Diag.ERROR,
            )
            parser().consume_token()
            return parse_decl(in_func, decl_end)
        start_loc = parser().consume_token()
        decl = parse_decl(in_func, decl_end)
        if decl is not None:
            decl.is_lib = True
            decl.src_range = (start_loc, decl.src_range[1])
        else:
            pass  # assert decl is not None, "TODO: diag here"
    elif parser().tok.ty == Tok.KW_LET:
        decl = parse_var_decl()
    elif parser().tok.ty == Tok.KW_TYPE:
        decl = parse_type_alias_decl()
    elif parser().tok.ty == Tok.KW_STRUCT:
        decl = parse_struct_decl()
    elif parser().tok.ty == Tok.KW_ENUM:
        decl = parse_enum_decl()
    else:
        diag(parser().tok.loc, f"Unexpected token {parser().tok.ty}", Diag.ERROR)
        parser().consume_any_token()
    if decl is not None and decl_end is not None:
        decl_end.value = decl.get_range()[1]
    return decl


ENUM_DECL_CTR = 0


def parse_enum_decl() -> EnumDecl:
    from .ty import parse_type

    assert parser().tok.ty == Tok.KW_ENUM, "not enum decl"
    start_loc = parser().consume_token()
    # TODO: check previous definition
    # TODO: partial definition
    name = None
    if parser().tok.ty == Tok.IDENT:
        name = parser().tok.ident().val
        parser().consume_token()
    aliased_type = None
    if parser().tok.ty == Tok.COLON:
        parser().consume_token()
        aliased_type = parse_type()
    parser().expect_and_consume(Tok.LBRACE)
    global ENUM_DECL_CTR
    decl = EnumDecl(
        (start_loc, LOC_INVALID),
        name or f"__annonymous_enum_decl{ENUM_DECL_CTR}",
        EnumType(name, aliased_type),
    )
    ENUM_DECL_CTR += 1
    last_val = 0
    allow_more = True
    while allow_more and parser().tok.ty != Tok.RBRACE:
        last_val, allow_more = parse_enum_variant(decl, last_val)
    parser().expect_and_consume(Tok.RBRACE)
    end_loc = parser().tok.loc
    decl.src_range = (decl.src_range[0], end_loc)
    parser().expect_and_consume_semi("Expected ';' after 'enum'")
    if name is not None:
        parser().cur_scope.add_decl(decl)
    return decl


def parse_enum_variant(decl: EnumDecl, last_val: int) -> Tuple[int, bool]:
    from .expr import parse_integer_constexpr

    if parser().tok.ty != Tok.IDENT:
        diag(parser().tok.loc, "Expected identifier as declaration name", Diag.ERROR)
        parser().skip_until(Tok.COMMA, Tok.RBRACE, stop_before_match=True)
        allow_more = False
        if parser().tok == Tok.COMMA:
            parser().consume_token()
            allow_more = True
        return last_val, allow_more
    name = parser().tok.ident().val
    start_loc = parser().consume_token()
    end_loc = start_loc
    if parser().tok.ty == Tok.EQUAL:
        parser().consume_token()
        last_val, end_loc = parse_integer_constexpr()
    allow_more = parser().tok.ty == Tok.COMMA
    if allow_more:
        parser().consume_token()
    variant_decl = EnumVariantDecl((start_loc, end_loc), name, decl.ty, last_val)
    parser().cur_scope.add_decl(variant_decl)
    decl.variants.append(variant_decl)
    return last_val + 1, allow_more


def parse_end_var_decl_common() -> Tuple[str, Type, Tuple[Loc, Loc]] | None:
    from .ty import parse_type

    if parser().tok.ty != Tok.IDENT:
        diag(
            parser().tok.loc,
            f"Expected identifier as declaration name (got {parser().tok.ty})",
            Diag.ERROR,
        )
        parser().skip_until(Tok.COMMA, Tok.SEMI, stop_before_match=True)
        return None
    ident = parser().tok.ident().val
    ident_loc = parser().consume_token()
    if parser().tok.ty != Tok.COLON:
        diag(parser().tok.loc, "Expected ':' after ident in var decl", Diag.ERROR)
        parser().skip_until(Tok.COMMA, Tok.SEMI)
        return None
    colon_loc = parser().consume_token()
    ty = parse_type()
    if ty is None:
        diag(colon_loc, "Expected Type after ':'", Diag.ERROR)
        return None
    return (ident, ty, (ident_loc, parser().prev_tok_location))


def parse_param_decl() -> ParamDecl | None:
    name, ty, rge = parse_end_var_decl_common() or (None, None, None)
    if name is None or ty is None:
        return None
    out_decl = ParamDecl(rge, name, ty)
    parser().cur_scope.decls_in_scope.append(out_decl)
    return out_decl


def parse_var_decl() -> VarDecl | None:
    assert parser().tok.ty == Tok.KW_LET, "Not a var decl"
    start_loc = parser().consume_token()
    name, ty, (_, end_loc) = parse_end_var_decl_common() or (None, None, (0, 0))
    if name is None or ty is None:
        return None
    # TODO: initializer
    parser().expect_and_consume_semi("Expected ';' at end of var decl")
    out_decl = VarDecl((start_loc, end_loc), name, ty)
    parser().cur_scope.decls_in_scope.append(out_decl)
    return out_decl


def parse_fn_decl() -> FnDecl | None:
    from .stmt import parse_compound_stmt_body
    from .ty import parse_type

    assert parser().tok.ty == Tok.KW_FN, "Not a function decl"
    start_loc = parser().consume_token()
    if parser().tok.ty != Tok.IDENT:
        diag(parser().tok.loc, "Expected identifier as declaration name", Diag.ERROR)
        # TODO: skip the fn decl
        parser().skip_until(Tok.COMMA, Tok.SEMI, stop_before_match=True)
        return None

    parser().enter_scope(ScopeFlags.FN | ScopeFlags.DECL | ScopeFlags.COMPOUND_STMT)

    fn_name = parser().tok.value_str()
    fn_scope = None
    method_name = ""
    name_loc = parser().consume_token()
    if parser().tok.ty == Tok.COLONCOLON:
        parser().consume_token()
        if parser().tok.ty != Tok.IDENT:
            diag(
                parser().tok.loc, "Expected identifier as declaration name", Diag.ERROR
            )
            # TODO: skip the fn decl
            parser().skip_until(Tok.COMMA, Tok.SEMI, stop_before_match=True)
            return None
        method_name = parser().tok.value_str()
        fn_name, fn_scope = actions.act_on_method_decl_name(
            parser().cur_scope, fn_name, name_loc, method_name, parser().tok.loc
        )
        parser().consume_token()

    parser().expect_and_consume(Tok.LPAREN)
    params = []
    is_vararg = False
    if parser().tok.ty != Tok.RPAREN:
        while True:
            if parser().tok.ty == Tok.ELLIPSIS:
                is_vararg = True
                parser().consume_token()
            else:
                param = parse_param_decl()
                if param is not None:
                    params.append(param)
                else:
                    parser().skip_until(Tok.COMMA, Tok.RPAREN, stop_before_match=True)
            if parser().tok.ty != Tok.COMMA:
                break
            if is_vararg:
                diag(parser().tok.loc, "Additional param decl after '...'", Diag.ERROR)
                parser().skip_until(Tok.RPAREN, stop_before_match=True)
                break
            parser().consume_token()
    parser().expect_and_consume(Tok.RPAREN)
    return_type = None
    if parser().tok.ty == Tok.ARROW:
        parser().consume_token()
        return_type = parse_type()

    if parser().tok.ty == Tok.SEMI:
        parser().exit_scope()
        semi_loc = parser().consume_token()
        decl = actions.act_on_fn_decl(
            parser().cur_scope,
            fn_name,
            params,
            return_type,
            start_loc,
            semi_loc,
            is_vararg,
        )
        if fn_scope is not None:
            fn_scope.add_method(method_name, decl)
        return decl

    decl = actions.act_on_start_fn_definition(
        parser().cur_scope.parent_scope,
        fn_name,
        params,
        return_type,
        start_loc,
        is_vararg,
    )
    if decl is None:
        parser().consume_brace()
        parser().skip_until(Tok.RBRACE)
        return None

    if fn_scope is not None:
        fn_scope.add_method(method_name, decl)

    fn_body = parse_compound_stmt_body()

    parser().exit_scope()

    return actions.act_on_end_fn_definition(decl, fn_body)


def parse_type_alias_decl() -> TypeDecl | None:
    from .ty import parse_type

    assert parser().tok.ty == Tok.KW_TYPE, "Not a type alias decl"
    start_loc = parser().consume_token()
    if parser().tok.ty != Tok.IDENT:
        diag(parser().tok.loc, "Expected identifier as declaration name", Diag.ERROR)
        parser().skip_until(Tok.SEMI)
        return None
    type_name = parser().tok.ident().val
    # lookup name availability
    parser().consume_token()
    parser().expect_and_consume(Tok.EQUAL)
    aliased_type = parse_type()
    assert aliased_type is not None
    end_loc = parser().tok.loc
    parser().expect_and_consume_semi("Expected ';' after 'type'")
    decl = TypeDecl((start_loc, end_loc), type_name, AliasType(aliased_type))
    parser().cur_scope.add_decl(decl)
    return decl


def parse_struct_decl() -> StructDecl | None:
    assert parser().tok.ty == Tok.KW_STRUCT, "Not a struct decl"
    start_loc = parser().consume_token()
    if parser().tok.ty != Tok.IDENT:
        diag(parser().tok.loc, "Expected identifier as declaration name", Diag.ERROR)
        parser().skip_until(Tok.SEMI)
        return None
    type_name = parser().tok.ident().val
    cur_decl = parser().cur_scope.lookup_named_decl(type_name)
    if cur_decl is not None and not isinstance(cur_decl, StructDecl):
        diag(parser().tok.loc, f"name '{type_name}' is already in use", Diag.ERROR)
        diag(cur_decl.get_range()[0], "defined here", Diag.NOTE, [cur_decl.get_range()])
        parser().skip_until(Tok.SEMI)
        return None
    cur_type = None
    if cur_decl is not None:
        cur_type = cur_decl.ty
        assert isinstance(cur_type, StructType)
        if not cur_type.is_incomplete:
            loc = parser().tok.loc
            parser().consume_token()
            if parser().tok.ty == Tok.SEMI:
                parser().cur_scope.add_decl(cur_decl)
                end_loc = parser().consume_token()
                cur_decl.src_range = (cur_decl.src_range[0], end_loc)
                return cur_decl
            diag(
                loc,
                f"struct '{type_name}' was already defined",
                Diag.ERROR,
            )
            parser().skip_until(Tok.SEMI)
            return None
        if cur_decl is not None:
            # TODO: get correct scope
            parser().cur_scope.remove_decl(cur_decl)
    else:
        cur_type = StructType(type_name)
        cur_decl = StructDecl((start_loc, 0), type_name, cur_type)
    parser().cur_scope.add_decl(cur_decl)
    parser().consume_token()
    # lookup name availability
    if parser().tok.ty == Tok.SEMI:
        end_loc = parser().consume_token()
        cur_decl.src_range = (cur_decl.src_range[0], end_loc)
        return cur_decl
    return parse_struct_decl_inner(cur_type, cur_decl)


def parse_struct_decl_inner(cur_type: StructType, cur_decl: StructDecl) -> StructDecl:
    assert parser().tok.ty == Tok.LBRACE
    parser().consume_brace()
    field_decls = []
    field_types = []
    has_super = False
    if parser().tok.ty == Tok.IDENT:
        tok = parser().tok.value_str()
        if tok == "super":
            has_super = True
            parser().consume_token()
    while parser().tok.ty != Tok.RBRACE:
        field = parse_field_decl()
        if field is not None:
            field_decls.append(field)
            field_types.append((field.name, field.ty))
    if has_super:
        if len(field_decls) == 0:
            diag(
                parser().tok.loc,
                "'super' identifier not used on first field of struct.",
                Diag.ERROR,
            )
            # TODO: handle error properly
            assert False
        cur_type.first_field_is_super = True
    parser().consume_brace()
    cur_type.fields = field_types
    cur_type.is_incomplete = False
    cur_decl.fields = field_decls
    cur_decl.src_range = (cur_decl.src_range[0], parser().tok.loc)
    parser().expect_and_consume_semi("Expected ';' after 'struct'")
    return cur_decl


def parse_field_decl() -> FieldDecl | None:
    name, ty, (start_loc, _) = parse_end_var_decl_common() or (None, None, (0, 0))
    if name is None or ty is None:
        return None
    end_loc = parser().tok.loc
    parser().expect_and_consume_semi("Expected ';' after field decl")
    decl = FieldDecl((start_loc, end_loc), name, ty)
    return decl
