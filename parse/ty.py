from lex import Tok
from ns_ast.nodes.types import Type, BuiltinType, AliasType, EnumType, PointerType, ArrayType
from ns_ast.nodes.decl import TypeDecl
from utils.diagnostic import diag, Diag
from .parser import parser

def parse_type() -> Type | None:
    cur_type = None
    while True:
        if cur_type is None:
            if parser().tok.ty.is_builtin_type():
                cur_type = BuiltinType.get_from_tok(parser().tok)
                parser().consume_token()
                continue
            if parser().tok.ty == Tok.KW_VOID:
                cur_type = Type()
                parser().consume_token()
                continue
            if parser().tok.ty == Tok.IDENT:
                type_name = parser().tok.value.val
                type_loc = parser().consume_token()
                lookup_decl = parser().cur_scope.lookup_named_decl(type_name)
                if lookup_decl is None or not isinstance(lookup_decl, TypeDecl):
                    diag(type_loc, f"'{type_name}' is not a type name", Diag.ERROR)
                    return None
                cur_type = lookup_decl.ty
                if isinstance(cur_type, AliasType) or isinstance(cur_type, EnumType) and cur_type.aliased_type is not None:
                    cur_type = cur_type.get_aliased_type()
                continue
            return cur_type
        if parser().tok.ty == Tok.STAR:
            cur_type = PointerType(cur_type)
            parser().consume_token()
            continue
        if parser().tok.ty == Tok.LSQUARE:
            from .expr import parse_integer_constexpr
            parser().consume_bracket()
            count, _ = parse_integer_constexpr()
            parser().expect_and_consume(Tok.RSQUARE)
            cur_type = ArrayType(cur_type, count)
            continue
        break
    # ActOnTypeName -> TODO:
    return cur_type

def is_start_of_type() -> bool:
    if parser().tok.ty.is_builtin_type():
        return True
    if parser().tok.ty == Tok.KW_VOID:
        return True
    if parser().tok.ty == Tok.IDENT:
        type_name = parser().tok.value.val
        lookup_decl = parser().cur_scope.lookup_named_decl(type_name)
        return lookup_decl is not None and isinstance(lookup_decl, TypeDecl)
    return False

def following_is_type(ctx: int) -> bool:
    # TODO:
    return False
