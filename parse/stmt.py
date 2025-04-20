from lex import Tok, LocPtr, LocRef
from ns_ast.nodes.expr import Expr
from ns_ast.nodes.stmt import (
    Stmt,
    IfStmt,
    DefaultStmt,
    SwitchStmt,
    WhileStmt,
    DoStmt,
    ContinueStmt,
    BreakStmt,
    ReturnStmt,
    CompoundStmt,
)
from semantic_analysis import actions, ScopeFlags, TYPES
from utils.diagnostic import diag, Diag
import enum
from .parser import parser
from .expr import parse_expr


class ParsedStmtContext(enum.Flag):
    ALLOW_DECLARATION_IN_C = 1
    IN_STMT_EXPR = 4
    SUB_STMT = 0
    COMPOUND = 1

    def in_stmt_expr(self) -> bool:
        return (self.value & 4) != 0


def parse_if_stmt(trailing_else_loc: LocPtr) -> IfStmt | None:
    assert parser().tok.ty == Tok.KW_IF, "Not an if stmt!"
    if_loc = parser().consume_token()

    if parser().tok.ty != Tok.LPAREN:
        diag(parser().tok.loc, "expected '(' after 'if'", Diag.ERROR)
        parser().skip_until(Tok.SEMI)
        return None

    parser().enter_scope(ScopeFlags.DECL | ScopeFlags.CONTROL)

    lparen_loc = parser().consume_paren()
    start = parser().tok.loc
    cond = parse_expr()

    if cond is None and parser().tok.ty != Tok.RPAREN:
        parser().skip_until(Tok.SEMI)
        if parser().tok.ty != Tok.RPAREN:
            parser().exit_scope()
            return None

    if cond is None:
        cond = actions.create_recovery_expr(
            start,
            start if parser().tok.loc == start else parser().prev_tok_location,
            [],
        )

    rparen_loc = parser().tok.loc
    parser().expect_and_consume(Tok.RPAREN)

    while parser().tok.ty == Tok.RPAREN:
        diag(
            parser().tok.loc,
            "extraneous ')' after condition, expected a statement",
            Diag.ERROR,
        )  # FixItHint::CreateRemoval(Tok.getLocation());
        parser().consume_paren()

    is_braced = parser().tok.ty == Tok.LBRACE
    if is_braced:
        parser().enter_scope(ScopeFlags.DECL)

    then_stmt_loc = parser().tok.loc
    inner_statement_trailing_else_loc = LocRef(0)
    then_stmt = parse_stmt(
        ParsedStmtContext.SUB_STMT, inner_statement_trailing_else_loc
    )
    if is_braced:
        parser().exit_scope()

    else_loc = 0
    else_stmt_loc = 0
    else_stmt = None

    if parser().tok.ty == Tok.KW_ELSE:
        if trailing_else_loc is not None:
            trailing_else_loc.value = parser().tok.loc

        else_loc = parser().consume_token()

        is_braced = parser().tok.ty == Tok.LBRACE
        if is_braced:
            parser().enter_scope(ScopeFlags.DECL)

        else_stmt_loc = parser().tok.loc
        else_stmt = parse_stmt(ParsedStmtContext.SUB_STMT, None)
        if is_braced:
            parser().exit_scope()
    elif inner_statement_trailing_else_loc.value != 0:
        diag(
            inner_statement_trailing_else_loc.value,
            "add explicit braces to avoid dangling else",
            Diag.WARNING,
        )

    parser().exit_scope()

    if then_stmt is None and else_stmt is None:
        return None

    if then_stmt is None:
        then_stmt = actions.act_on_null_stmt(then_stmt_loc)
    if else_stmt is None:
        else_stmt = actions.act_on_null_stmt(else_stmt_loc)

    return actions.act_on_if_stmt(
        if_loc, lparen_loc, cond, rparen_loc, then_stmt, else_loc, else_stmt
    )


def parse_case_stmt(
    stmt_ctx: ParsedStmtContext, missing_case: bool = False, expr: Expr | None = None
) -> Stmt | None:
    assert missing_case or parser().tok.ty == Tok.KW_CASE, "Not a case stmt!"
    top_level_case = None
    deepest_parsed_case_stmt = None
    colon_loc = LocRef(0)
    while missing_case or parser().tok.ty == Tok.KW_CASE:
        case_loc = 0
        if not missing_case:
            case_loc = parser().consume_token()
        else:
            assert expr is not None
            case_loc = expr.get_range()[0]
        colon_loc.value = 0

        lhs = None
        if not missing_case:
            lhs = parse_expr()  # parse_case_expression(case_loc);
            if lhs is None:
                if not parser().skip_until(
                    Tok.COLON, Tok.RBRACE, stop_at_semi=True, stop_before_match=True
                ):
                    return None
        else:
            lhs = expr
            missing_case = False
        assert lhs is not None

        if parser().try_consume_token(Tok.COLON, colon_loc):
            pass
        # check common typo: '::'
        else:
            expected_loc = parser().prev_tok_location  # get_loc_for_end_of_token
            diag(
                expected_loc, "expected ':' after 'case'", Diag.ERROR
            )  # FixItHint::CreateInsertion(ExpectedLoc, ":");
            colon_loc.value = expected_loc

        case_stmt = actions.act_on_case_stmt(case_loc, lhs, colon_loc.value)
        if case_stmt is None:
            if top_level_case is None:
                return parse_stmt(stmt_ctx, None)
        else:
            next_deepest = case_stmt
            if top_level_case is None:
                top_level_case = case_stmt
            else:
                assert deepest_parsed_case_stmt is not None
                actions.act_on_case_stmt_body(deepest_parsed_case_stmt, case_stmt)
            deepest_parsed_case_stmt = next_deepest

    sub_stmt = None

    if parser().tok.ty == Tok.RBRACE:
        sub_stmt = actions.act_on_null_stmt(colon_loc.value)
    else:
        sub_stmt = parse_stmt(stmt_ctx, None)

    if deepest_parsed_case_stmt is not None:
        if sub_stmt is None:
            sub_stmt = actions.act_on_null_stmt(0)
        # diagnose_label_followed_by_decl(sub_stmt)
        actions.act_on_case_stmt_body(deepest_parsed_case_stmt, sub_stmt)

    return top_level_case


def parse_default_stmt(stmt_ctx: ParsedStmtContext) -> DefaultStmt | None:
    assert parser().tok.ty == Tok.KW_DEFAULT, "Not a default stmt!"
    default_loc = parser().consume_token()
    colon_loc = LocRef(0)
    if parser().try_consume_token(Tok.COLON, colon_loc):
        pass
    elif parser().try_consume_token(Tok.SEMI, colon_loc):
        diag(
            colon_loc.value, "expected ':' after 'default'", Diag.ERROR
        )  # FixItHint::CreateReplacement(ColonLoc, ":");
    else:
        expected_loc = parser().prev_tok_location  # get_loc_for_end_of_token
        diag(
            expected_loc, "expected ':' after 'default'", Diag.ERROR
        )  # FixItHint::CreateInsertion(ExpectedLoc, ":");
        colon_loc.value = expected_loc
    sub_stmt = None
    if parser().tok.ty == Tok.RBRACE:
        sub_stmt = actions.act_on_null_stmt(colon_loc.value)
    else:
        sub_stmt = parse_stmt(stmt_ctx, None)

    if sub_stmt is None:
        sub_stmt = actions.act_on_null_stmt(colon_loc.value)

    # diagnose_label_followed_by_decl(sub_stmt)
    return actions.act_on_default_stmt(
        default_loc, colon_loc.value, sub_stmt, parser().cur_scope
    )


def parse_switch_stmt(trailing_else_loc: LocPtr) -> SwitchStmt | None:
    assert parser().tok.ty == Tok.KW_SWITCH, "Not a switch stmt!"
    switch_loc = parser().consume_token()

    if parser().tok.ty != Tok.LPAREN:
        diag(parser().tok.loc, "expected '(' after 'switch'", Diag.ERROR)
        parser().skip_until(Tok.SEMI)
        return None

    parser().enter_scope(ScopeFlags.SWITCH | ScopeFlags.DECL | ScopeFlags.CONTROL)

    lparen_loc = parser().consume_paren()
    start = parser().tok.loc
    cond = parse_expr()

    if cond is None and parser().tok.ty != Tok.RPAREN:
        parser().skip_until(Tok.SEMI)
        if parser().tok.ty != Tok.RPAREN:
            parser().exit_scope()
            return None

    if cond is None:
        cond = actions.create_recovery_expr(
            start,
            start if parser().tok.loc == start else parser().prev_tok_location,
            [],
        )

    rparen_loc = parser().tok.loc
    parser().expect_and_consume(Tok.RPAREN)

    while parser().tok.ty == Tok.RPAREN:
        diag(
            parser().tok.loc,
            "extraneous ')' after condition, expected a statement",
            Diag.ERROR,
        )  # FixItHint::CreateRemoval(Tok.getLocation());
        parser().consume_paren()

    switch = actions.act_on_start_of_switch_stmt(
        switch_loc, lparen_loc, cond, rparen_loc
    )
    if switch is None:
        if parser().tok.ty == Tok.LBRACE:
            parser().consume_brace()
            parser().skip_until(Tok.RBRACE)
        else:
            parser().skip_until(Tok.SEMI)
        return switch

    parser().cur_scope.flags |= ScopeFlags.BREAK

    has_lbrace = parser().tok.ty == Tok.LBRACE
    if has_lbrace:
        parser().enter_scope(ScopeFlags.DECL)

    body = parse_stmt(ParsedStmtContext.SUB_STMT, trailing_else_loc)
    assert body is not None

    if has_lbrace:
        parser().exit_scope()

    parser().exit_scope()

    return actions.act_on_finish_switch_stmt(switch_loc, switch, body)


def parse_while_stmt(trailing_else_loc: LocPtr) -> WhileStmt | None:
    assert parser().tok.ty == Tok.KW_WHILE, "Not a while stmt!"
    while_loc = parser().consume_token()
    if parser().tok.ty != Tok.LPAREN:
        diag(parser().tok.loc, "expected '(' after 'while'", Diag.ERROR)
        parser().skip_until(Tok.SEMI)
        return None

    parser().enter_scope(
        ScopeFlags.BREAK | ScopeFlags.CONTINUE | ScopeFlags.DECL | ScopeFlags.CONTROL
    )

    lparen_loc = parser().consume_paren()
    start = parser().tok.loc
    cond = parse_expr()

    if cond is None and parser().tok.ty != Tok.RPAREN:
        parser().skip_until(Tok.SEMI)
        if parser().tok.ty != Tok.RPAREN:
            parser().exit_scope()
            return None

    if cond is None:
        cond = actions.create_recovery_expr(
            start,
            start if parser().tok.loc == start else parser().prev_tok_location,
            [],
        )

    rparen_loc = parser().tok.loc
    parser().expect_and_consume(Tok.RPAREN)

    has_brace = parser().tok.ty == Tok.LBRACE
    if has_brace:
        parser().enter_scope(ScopeFlags.DECL)

    body = parse_stmt(ParsedStmtContext.SUB_STMT, trailing_else_loc)

    if has_brace:
        parser().exit_scope()
    parser().exit_scope()

    if cond is None or body is None:
        return None

    return actions.act_on_while_stmt(while_loc, lparen_loc, cond, rparen_loc, body)


def parse_do_stmt() -> DoStmt | None:
    assert parser().tok.ty == Tok.KW_DO, "Not a do stmt!"
    do_loc = parser().consume_token()

    parser().enter_scope(ScopeFlags.BREAK | ScopeFlags.CONTINUE | ScopeFlags.DECL)

    has_lbrace = parser().tok.ty == Tok.LBRACE
    if has_lbrace:
        parser().enter_scope(ScopeFlags.DECL)

    body = parse_stmt(ParsedStmtContext.SUB_STMT, None)

    if has_lbrace:
        parser().exit_scope()

    if parser().tok.ty != Tok.KW_WHILE:
        if body is not None:
            diag(parser().tok.loc, "expected 'while' in do/while loop", Diag.ERROR)
            diag(do_loc, "to match this 'do'", Diag.NOTE)
            parser().skip_until(Tok.SEMI, stop_before_match=True)
        return None

    while_loc = parser().consume_token()

    if parser().tok.ty != Tok.LPAREN:
        diag(parser().tok.loc, "expected '(' after do/while", Diag.ERROR)
        parser().skip_until(Tok.SEMI, stop_before_match=True)
        return None

    lparen_loc = parser().consume_paren()

    start = parser().tok.loc

    cond = parse_expr()

    if cond is not None:
        cond = actions.correct_delayed_typos_in_expr(cond, None, True)
    else:
        if parser().tok.ty not in [Tok.RPAREN, Tok.RSQUARE, Tok.RBRACE]:
            parser().skip_until(Tok.SEMI)
        cond = actions.create_recovery_expr(
            start,
            start if start == parser().tok.loc else parser().prev_tok_location,
            [],
            TYPES["bool"],
        )
    rparen_loc = parser().consume_paren()

    parser().exit_scope()

    if cond is None or body is None:
        return None

    return actions.act_on_do_stmt(do_loc, body, while_loc, lparen_loc, cond, rparen_loc)


def parse_continue_stmt() -> ContinueStmt | None:
    continue_loc = parser().consume_token()
    return actions.act_on_continue_stmt(continue_loc, parser().cur_scope)


def parse_break_stmt() -> BreakStmt | None:
    break_loc = parser().consume_token()
    return actions.act_on_break_stmt(break_loc, parser().cur_scope)


def parse_initializer():
    assert False, "Unimplemented"


def parse_return_stmt() -> ReturnStmt | None:
    assert parser().tok.ty == Tok.KW_RETURN, "not a return stmt!"
    return_loc = parser().consume_token()
    r = None
    if parser().tok.ty != Tok.SEMI:
        if parser().tok.ty == Tok.LBRACE:
            r = parse_initializer()
        else:
            r = parse_expr()
        if r is None:
            parser().skip_until(Tok.RBRACE, stop_at_semi=True, stop_before_match=True)
            return None
    return actions.act_on_return_stmt(return_loc, r, parser().cur_scope)


def parse_expr_stmt(stmt_ctx: ParsedStmtContext) -> Stmt | None:
    old_token = parser().tok

    expr = parse_expr()
    if expr is None:
        parser().skip_until(Tok.RBRACE, stop_at_semi=True, stop_before_match=True)
        if parser().tok.ty == Tok.SEMI:
            parser().consume_token()
        return actions.act_on_expr_stmt_error()

    if (
        parser().tok.ty == Tok.COLON
        and parser().cur_scope.is_switch_scope()
        and actions.check_case_expression(expr)
    ):
        diag(
            old_token.loc, "expected 'case' keyword before expression", Diag.ERROR
        )  # FixItHint::CreateInsertion(OldToken.getLocation(), "case ");
        return parse_case_stmt(stmt_ctx, True, expr)

    parser().expect_and_consume_semi("expected ';' after expression")

    return actions.act_on_expr_stmt(expr, True)


def parse_compound_stmt_body() -> CompoundStmt | None:
    if parser().tok.ty != Tok.LBRACE:
        return None
    open_loc = parser().consume_brace()
    # actions.push_compound_scope(False)
    stmts = []
    sub_stmt_ctx = ParsedStmtContext.COMPOUND

    while parser().tok.ty not in [Tok.RBRACE, Tok.EOF]:
        r = parse_stmt(sub_stmt_ctx)
        if r is not None:
            stmts.append(r)

    close_loc = parser().consume_brace()
    out = actions.act_on_compound_stmt(open_loc, close_loc, stmts)
    # actions.pop_compound_scope()
    return out


def parse_compound_stmt(
    scope_flags: ScopeFlags = ScopeFlags.DECL | ScopeFlags.COMPOUND_STMT,
) -> CompoundStmt | None:
    assert parser().tok.ty == Tok.LBRACE, "not a compound stmt!"
    parser().enter_scope(scope_flags)
    out = parse_compound_stmt_body()
    parser().exit_scope()
    return out


def parse_stmt(
    stmt_ctx: ParsedStmtContext, trailing_else_loc: LocPtr = None
) -> Stmt | None:
    semi_error = ""
    res = None
    # if parser().tok.ty == Tok.IDENT:
    #     if colon after, parse label statement
    #     next_tok = parser().next_token()
    #     if next_tok.ty != Tok.COLONCOLON:
    #         typo correct ident
    #         if it is now a keyword, retry
    match parser().tok.ty:
        case Tok.KW_CASE:
            return parse_case_stmt(stmt_ctx)
        case Tok.KW_DEFAULT:
            return parse_default_stmt(stmt_ctx)
        case Tok.LBRACE:
            return parse_compound_stmt()
        case Tok.SEMI:
            return actions.act_on_null_stmt(
                parser().consume_token()
            )  # Tok.has_leading_empty_macro()
        case Tok.KW_IF:
            return parse_if_stmt(trailing_else_loc)
        case Tok.KW_SWITCH:
            return parse_switch_stmt(trailing_else_loc)
        case Tok.KW_WHILE:
            return parse_while_stmt(trailing_else_loc)
        case Tok.KW_DO:
            res = parse_do_stmt()
            semi_error = "do/while"
        case Tok.KW_FOR:
            assert False, "'for' is not implemented"
        # goto
        case Tok.KW_CONTINUE:
            res = parse_continue_stmt()
            semi_error = "continue"
        case Tok.KW_BREAK:
            res = parse_break_stmt()
            semi_error = "break"
        case Tok.KW_RETURN:
            res = parse_return_stmt()
            semi_error = "return"
        case _:
            if is_declaration_statement():
                from .decl import parse_decl, DeclaratorContext

                decl_start = parser().tok.loc
                decl_end = LocRef(0)
                decl = parse_decl(DeclaratorContext.BLOCK, decl_end)
                return actions.act_on_decl_stmt(decl, decl_start, decl_end.value)
            if parser().tok.ty == Tok.RBRACE:
                diag(parser().tok.loc, "expected statement", Diag.ERROR)
                return None
            return parse_expr_stmt(stmt_ctx)
    if not parser().try_consume_token(Tok.SEMI) and res is not None:
        parser().expect_and_consume(
            Tok.SEMI, "expected ';' after {} statement", semi_error
        )
        parser().skip_until(Tok.RBRACE, stop_at_semi=True, stop_before_match=True)
    return res


def is_declaration_statement() -> bool:
    match parser().tok.ty:
        case (
            Tok.KW_LET
            | Tok.KW_FN
            | Tok.KW_LIB
            | Tok.KW_TYPE
            | Tok.KW_STRUCT
            | Tok.KW_ENUM
        ):  # TODO: other kw
            return True
        case _:
            return False
