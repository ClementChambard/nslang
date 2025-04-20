from lex import Tok, Token, Loc, LocRef
from ns_ast.nodes.expr import Expr
from ns_ast.nodes.types import Type
from ns_ast.nodes.unqualified_id import UnqualifiedId
from semantic_analysis import actions, eval_const_expr
from utils.diagnostic import diag, Diag
from .prec import Prec
from .parser import parser

from typing import List, Tuple

# ExpressionList ::= SepList<Expr, ','>
#
# PostfixExprSuffix ::= '++'
#                     | '--'
#                     | '[' ExprList ']'
#                     | '(' ExprList ')'
#                     | '->' id
#                     | '.' id
#                     ;
#
# UnitExpr ::= UnitExpr PostfixExprSuffix
#            | '(' Expr ')'
#            | num
#            | chr
#            | str
#            | 'true'
#            | 'false'
#            | 'nullptr'
#            | 'vaarg' '<' Type '>'
#            | id
#            | '&' UnitExpr
#            | '++' UnitExpr
#            | '--' UnitExpr
#            | '*' UnitExpr
#            | '+' UnitExpr
#            | '-' UnitExpr
#            | '~' UnitExpr
#            | '!' UnitExpr
#            | 'cast' '<' Type '>' '(' Expr ')'
#            | 'sizeof' '(' Type ')'
#            | 'sizeof' '(' Expr ')'
#            | BuiltinExpr
#            ;
#
# BuiltinExpr ::= '__builtin_syscall' '(' ExprList ')' ;
#
# Expr ::= UnitExpr
#        | Expr '?' Expr ':' Expr
#        | Expr '*' Expr
#        | Expr '/' Expr
#        | Expr '%' Expr
#        | Expr '+' Expr
#        | Expr '-' Expr
#        | Expr '<<' Expr
#        | Expr '>>' Expr
#        | Expr '<' Expr
#        | Expr '>' Expr
#        | Expr '<=' Expr
#        | Expr '>=' Expr
#        | Expr '==' Expr
#        | Expr '!=' Expr
#        | Expr '&' Expr
#        | Expr '^' Expr
#        | Expr '|' Expr
#        | Expr '&&' Expr
#        | Expr '||' Expr
#        | Expr '=' Expr
#        | Expr '*=' Expr
#        | Expr '/=' Expr
#        | Expr '%=' Expr
#        | Expr '+=' Expr
#        | Expr '-=' Expr
#        | Expr '<<=' Expr
#        | Expr '>>=' Expr
#        | Expr '&=' Expr
#        | Expr '^=' Expr
#        | Expr '|=' Expr
#        ;


def parse_expression_list(exprs: List[Expr]) -> bool:
    saw_error = False
    while True:
        expr = None
        if parser().tok.ty == Tok.LBRACE:
            # diag::warn_cxx98_compat_generalized_initializer_lists
            expr = parse_brace_initializer()
        else:
            expr = parse_assignment_expr()

        if parser().tok.ty == Tok.ELLIPSIS:
            expr = actions.act_on_pack_expansion(expr, parser().consume_token())

        if expr is None:
            saw_error = True
            parser().skip_until(Tok.COMMA, Tok.RPAREN, stop_before_match=True)
        else:
            exprs.append(expr)

        if parser().tok.ty != Tok.COMMA:
            break

        parser().consume_token()
    if saw_error:
        for e in exprs:
            expr = actions.correct_delayed_typos_in_expr(e)
            if expr is not None:
                e = expr
    return saw_error


def parse_postfix_expression_suffix(lhs: Expr | None) -> Expr | None:
    loc = 0
    while True:
        match parser().tok.ty:
            case Tok.PLUSPLUS | Tok.MINUSMINUS:
                if lhs is not None:
                    arg = lhs
                    lhs = actions.act_on_postfix_unary_op(
                        parser().cur_scope, parser().tok.loc, parser().tok.ty, arg
                    )
                    if lhs is None:
                        lhs = actions.create_recovery_expr(
                            arg.get_begin_loc(), parser().tok.loc, arg
                        )
                parser().consume_token()
            case Tok.LSQUARE:
                loc = parser().consume_bracket()
                arg_exprs = []
                has_error = False

                if parse_expression_list(arg_exprs):
                    lhs = actions.correct_delayed_typos_in_expr(lhs)
                    has_error = True

                rloc = parser().tok.loc
                lhs = actions.correct_delayed_typos_in_expr(lhs)

                if lhs is not None and not has_error and parser().tok.ty == Tok.RSQUARE:
                    lhs = actions.act_on_array_subscript_expr(
                        parser().cur_scope, lhs, loc, arg_exprs, rloc
                    )
                else:
                    lhs = None
                parser().consume_bracket()
            case Tok.LPAREN:
                loc = parser().consume_paren()
                arg_exprs = []

                if parser().tok.ty != Tok.RPAREN:
                    if parse_expression_list(arg_exprs):
                        actions.correct_delayed_typos_in_expr(lhs)
                        lhs = None
                    elif lhs is None:
                        for e in arg_exprs:
                            actions.correct_delayed_typos_in_expr(e)

                if lhs is None:
                    parser().skip_until(Tok.RPAREN, stop_at_semi=True)
                elif parser().tok.ty != Tok.RPAREN:
                    had_delayed_typo = False
                    if actions.correct_delayed_typos_in_expr(lhs) != lhs:
                        had_delayed_typo = True
                    for e in arg_exprs:
                        if actions.correct_delayed_typos_in_expr(e) != e:
                            had_delayed_typo = True
                    if had_delayed_typo:
                        parser().skip_until(Tok.RPAREN, stop_at_semi=True)
                    else:
                        parser().consume_paren()
                    lhs = None
                else:
                    fn = lhs
                    rpar_loc = parser().tok.loc
                    lhs = actions.act_on_call_expr(
                        parser().cur_scope, fn, loc, arg_exprs, rpar_loc
                    )
                    if lhs is None:
                        arg_exprs = [fn] + arg_exprs
                        lhs = actions.create_recovery_expr(
                            fn.get_range()[0], rpar_loc, arg_exprs
                        )
                    parser().consume_paren()
            case Tok.ARROW | Tok.PERIOD:
                opkind = parser().tok.ty
                oploc = parser().consume_token()

                ss = None  # CXXScopeSpec
                # object_type = None  # ParsedType

                orig_lhs = lhs

                # if lhs is not None:
                #    base = orig_lhs
                #    base_type = base.ty # .get_type_ptr_or_null()
                #
                #    lhs = actions.act_on_start_cxx_member_reference(parser().cur_scope, base, oploc, opkind, None, False);
                #
                #    if lhs is None:
                #        break
                #    parse_optional_cxx_scope_specifier(ss, object_type, lhs and lhs.contains_errors(), False, &False)
                #    if ss.is_not_empty() object_type = None

                name = UnqualifiedId()

                if parse_unqualified_id(name):
                    actions.correct_delayed_typos_in_expr(lhs)
                    lhs = None

                if lhs is not None:
                    lhs = actions.act_on_member_access_expr(
                        parser().cur_scope, lhs, oploc, opkind, ss, name
                    )
                if lhs is None and orig_lhs is not None and name is not None:
                    lhs = actions.create_recovery_expr(
                        orig_lhs.get_range()[0], name.get_range()[1], [orig_lhs]
                    )
            case _:
                return lhs

    return lhs


def parse_unqualified_id(out: UnqualifiedId) -> bool:
    if parser().tok.ty != Tok.IDENT:
        # kw_operator
        # tilde (for destructor name)
        # transform_type_trait => parse ident
        diag(parser().tok.loc, "Expected unqualified identifier", Diag.ERROR)
        return True
    idinf = parser().tok.value
    idloc = parser().consume_token()
    out.set_identifier(idinf, idloc)
    return False


def parse_unit_expr(is_address_of_operand: bool = False) -> Expr | None:
    from .ty import is_start_of_type, parse_type

    res = None
    saved_kind = parser().tok.ty

    if saved_kind == Tok.LPAREN:
        # ColonProtectionRAIIObject ColonProtection(*this, false);
        open_loc = parser().consume_paren()
        res = parse_expr()
        if res is not None and parser().tok.ty == Tok.RPAREN:
            res = actions.act_on_paren_expr(open_loc, parser().tok.loc, res)
        if res is None:
            parser().skip_until(Tok.RPAREN, stop_at_semi=True)
        else:
            parser().expect_and_consume(Tok.RPAREN)
    elif saved_kind == Tok.NUM:
        res = actions.act_on_numeric_constant(parser().tok)
        parser().consume_token()
    elif saved_kind == Tok.KW_TRUE or saved_kind == Tok.KW_FALSE:
        kind = parser().tok.ty
        res = actions.act_on_bool_literal(parser().consume_token(), kind)
    elif saved_kind == Tok.KW_NULLPTR:
        res = actions.act_on_nullptr_literal(parser().consume_token())
    elif saved_kind == Tok.KW_VAARG:
        start_loc = parser().consume_token()
        parser().expect_and_consume(Tok.LESS)
        ty = parse_type()
        end_loc = parser().tok.loc
        parser().expect_and_consume(Tok.GREATER)
        res = actions.act_on_vaarg_expr(ty or Type(), start_loc, end_loc)
    elif saved_kind == Tok.IDENT:
        ii = parser().tok.ident()
        iloc = parser().consume_token()

        ss = None
        if parser().tok.ty == Tok.COLONCOLON:
            parser().consume_token()
            assert parser().tok.ty == Tok.IDENT, "TODO: error handling"
            scope_ii = ii
            scope_loc = iloc
            ii = parser().tok.ident()
            iloc = parser().consume_token()
            ss = actions.act_on_scoped_identifier(
                parser().cur_scope, scope_ii, scope_loc
            )

        if is_address_of_operand and parser().tok.ty in [
            Tok.LSQUARE,
            Tok.LPAREN,
            Tok.PERIOD,
            Tok.ARROW,
            Tok.PLUSPLUS,
            Tok.MINUSMINUS,
        ]:
            is_address_of_operand = False

        name = UnqualifiedId()
        replacement = Token()

        name.set_identifier(ii, iloc)
        res = actions.act_on_id_expression(
            parser().cur_scope,
            ss,
            name,
            parser().tok.ty == Tok.LPAREN,
            is_address_of_operand,
            False,
            replacement if parser().tok.ty == Tok.RPAREN else None,
        )
        if res is None:
            parser().unconsume_token(replacement)
            return parse_unit_expr(is_address_of_operand)
    elif saved_kind == Tok.CHR:
        res = actions.act_on_character_constant(parser().tok)
        parser().consume_token()
    elif saved_kind == Tok.STR:
        string_toks = []
        while parser().tok.ty == Tok.STR:
            string_toks.append(parser().tok)
            parser().consume_any_token()
        res = actions.act_on_string_literal(string_toks)
    elif saved_kind.is_builtin():
        return parse_builtin_expression()
    elif saved_kind == Tok.PLUSPLUS or saved_kind == Tok.MINUSMINUS:
        saved_tok2 = parser().tok
        parser().consume_token()
        res, _ = parse_unit_expr()
        if res is not None:
            arg = res
            res = actions.act_on_unary_op(
                parser().cur_scope, saved_tok2.loc, saved_kind, arg
            )
            if res is None:
                res = actions.create_recovery_expr(
                    saved_tok2.loc, arg.src_range[1], arg
                )
    elif saved_kind == Tok.AMP:
        saved_loc = parser().consume_token()
        res = parse_unit_expr(True)
        if res is not None:
            arg = res
            res = actions.act_on_unary_op(
                parser().cur_scope, saved_loc, saved_kind, arg
            )
            if res is None:
                res = actions.create_recovery_expr(
                    parser().tok.loc, arg.src_range[1], arg
                )
    elif saved_kind in [Tok.STAR, Tok.PLUS, Tok.MINUS, Tok.TILDE, Tok.EXCLAIM]:
        saved_loc = parser().consume_token()
        res = parse_unit_expr()
        if res is not None:
            arg = res
            res = actions.act_on_unary_op(
                parser().cur_scope, saved_loc, saved_kind, arg, is_address_of_operand
            )
            if res is None:
                res = actions.create_recovery_expr(
                    parser().tok.loc, arg.src_range[1], arg
                )
    elif saved_kind == Tok.KW_CAST:
        start_loc = parser().consume_token()
        parser().expect_and_consume(Tok.LESS)
        ty = parse_type()
        parser().expect_and_consume(Tok.GREATER)
        parser().expect_and_consume(Tok.LPAREN)
        expr = parse_expr()
        end_loc = parser().tok.loc
        parser().expect_and_consume(Tok.RPAREN)
        res = actions.act_on_explicit_cast(ty, expr, start_loc, end_loc)
    elif saved_kind == Tok.KW_SIZEOF:
        start_loc = parser().consume_token()
        parser().expect_and_consume(Tok.LPAREN)
        ty, expr = None, None
        if is_start_of_type():
            ty = parse_type()
        else:
            expr = parse_expr()
            ty = expr.ty
        if ty is None:
            diag(start_loc, "sizeof on invalid type", Diag.ERROR)
            ty = TYPES["i64"]
        end_loc = parser().tok.loc
        parser().expect_and_consume(Tok.RPAREN)
        res = actions.act_on_sizeof_expr(ty, expr, start_loc, end_loc)
    # CPP co_await, __extension__, alignof, ...
    # CPP casts
    # CPP typeid
    # CPP this
    # CPP type-construct-expr
    # CPP kw_operator -> idexpr
    # CPP new/delete expr
    # CPP decltype
    # ...

    return parse_postfix_expression_suffix(res)


def parse_builtin_expression() -> Expr | None:
    assert parser().tok.ty.is_builtin(), "not a builtin expression"
    builtin_tok = parser().tok
    parser().consume_token()
    if parser().tok.ty != Tok.LPAREN:
        return None
    parser().consume_paren()
    if parser().tok.ty == Tok.RPAREN:
        rparen_loc = parser().consume_paren()
        return actions.act_on_builtin_expr(builtin_tok, rparen_loc, [])
    args = []
    while True:
        arg = parse_assignment_expr()
        if arg is None:
            diag(parser().tok.loc, "Expected an expression", Diag.ERROR)
            parser().skip_until(Tok.RPAREN)
            break
        args.append(arg)
        if parser().tok.ty != Tok.COMMA:
            break
        parser().consume_token()
    rparen_loc = parser().tok.loc
    parser().expect_and_consume(Tok.RPAREN)
    return actions.act_on_builtin_expr(builtin_tok, rparen_loc, args)


def parse_assignment_expr() -> Expr | None:
    # CPP: 'throw' -> return parse_throw_expression()
    # CPP: 'co_yield' -> return parse_coyield_expression()

    lhs = parse_unit_expr()
    return parse_rhs_of_binary_expr(lhs, Prec.ASSIGN)


def parse_rhs_of_binary_expr(lhs: Expr | None, prec: Prec) -> Expr | None:
    next_tok_prec = Prec.from_bin_op(parser().tok.ty)
    colon_loc = 0
    while True:
        if next_tok_prec.value < prec.value:
            return lhs
        op_token = parser().tok
        parser().consume_token()
        if op_token.ty == Tok.COMMA and True:  # is_not_expression_start():
            parser().lexer.enter_token(parser().tok, True)
            parser().tok = op_token
            return lhs
        ternary_middle = None
        if next_tok_prec == Prec.COND:
            if parser().tok.ty == Tok.LBRACE:
                # brace_loc = parser().tok.loc
                ternary_middle = parse_brace_initializer()
                if ternary_middle is not None:
                    diag(
                        op_token.loc,
                        "initializer list cannot be used on the left hand side of operator ':'",
                        Diag.ERROR,
                    )  # actions.get_expr_range(ternary_middle)
                    ternary_middle = None
            elif parser().tok.ty != Tok.COLON:
                # x = ColonProtectionRAIIObject()
                ternary_middle = parse_expr()
            else:
                ternary_middle = None
                diag(op_token.loc, "ext_gnu_conditional_expr", Diag.WARNING)
            if ternary_middle is None:
                actions.correct_delayed_typos_in_expr(lhs)
                lhs = None
                ternary_middle = None
            colon_loc = LocRef(colon_loc)
            if not parser().try_consume_token(Tok.COLON, colon_loc):
                diag(op_token.loc, "expected ':'", Diag.ERROR)
                diag(op_token.loc, "to match this '?'", Diag.NOTE)
            colon_loc = colon_loc.value
        rhs: Expr | None = None
        rhs_is_init_list = False
        if parser().tok.ty == Tok.LBRACE:
            parse_brace_initializer()
            rhs_is_init_list = True
        elif next_tok_prec.value <= Prec.COND.value:
            rhs = parse_assignment_expr()
        else:
            rhs = parse_unit_expr()
        if rhs is None:
            actions.correct_delayed_typos_in_expr(lhs)
            if ternary_middle is not None:
                ternary_middle = actions.correct_delayed_typos_in_expr(ternary_middle)
            lhs = None
        this_prec = next_tok_prec
        next_tok_prec = Prec.from_bin_op(parser().tok.ty)
        is_right_assoc = this_prec == Prec.COND or this_prec == Prec.ASSIGN
        if this_prec.value < next_tok_prec.value or (
            this_prec == next_tok_prec and is_right_assoc
        ):
            if rhs is not None and rhs_is_init_list:
                diag(
                    parser().tok.loc,
                    f"initializer list cannot be used on the left hand side of operator '{op_token.ty}'",
                    Diag.ERROR,
                )  # actions.get_expr_range(rhs)
                rhs = None
            rhs = parse_rhs_of_binary_expr(
                rhs, Prec(this_prec.value + int(not is_right_assoc))
            )
            rhs_is_init_list = False
            if rhs is None:
                actions.correct_delayed_typos_in_expr(lhs)
                if ternary_middle is not None:
                    ternary_middle = actions.correct_delayed_typos_in_expr(
                        ternary_middle
                    )
                lhs = None
            next_tok_prec = Prec.from_bin_op(parser().tok.ty)
        if rhs is not None and rhs_is_init_list:
            if this_prec == Prec.ASSIGN:
                pass
            elif colon_loc != 0:
                diag(
                    colon_loc,
                    "initializer list cannot be used on the right hand side of operator ':'",
                    Diag.ERROR,
                )  # actions.get_expr_range(rhs)
                lhs = None
            else:
                diag(
                    parser().tok.loc,
                    f"initializer list cannot be used on the right hand side of operator '{op_token.ty}'",
                    Diag.ERROR,
                )  # get_spelling # actions.get_expr_range(rhs)
                lhs = None
        orig_lhs = lhs
        if lhs is not None:
            if ternary_middle is None:
                bin_op = actions.act_on_bin_op(
                    parser().cur_scope, op_token.loc, op_token.ty, lhs, rhs
                )
                if bin_op is None:
                    bin_op = actions.create_recovery_expr(
                        lhs.get_range()[0], rhs.get_range()[1], [lhs, rhs]
                    )
                lhs = bin_op
            else:
                cond_op = actions.act_on_conditional_op(
                    op_token.loc, colon_loc, lhs, ternary_middle, rhs
                )
                if cond_op is None:
                    args = []
                    if ternary_middle is not None:
                        args = [lhs, ternary_middle, rhs]
                    else:
                        args = [lhs, rhs]
                    cond_op = actions.create_recovery_expr(
                        lhs.get_range()[0], rhs.get_range()[1], args
                    )
                lhs = cond_op
        if lhs is None:
            actions.correct_delayed_typos_in_expr(orig_lhs)
            actions.correct_delayed_typos_in_expr(ternary_middle)
            actions.correct_delayed_typos_in_expr(rhs)


def parse_expr() -> Expr | None:
    lhs = parse_assignment_expr()
    return parse_rhs_of_binary_expr(lhs, Prec.COMMA)


def parse_integer_constexpr() -> Tuple[int, Loc]:
    e = parse_expr()
    end_loc = e.get_range()[1]
    if not e.ty.is_integer_type():
        diag(
            e.get_range()[0], "Expected integer constexpr", Diag.ERROR, [e.get_range()]
        )
        return 0, end_loc
    return eval_const_expr(e), end_loc
