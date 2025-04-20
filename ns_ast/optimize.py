from .nodes import *


@dataclass
class OptimizeExprResult:
    result: Expr
    is_pure: bool


def _optimize_binary_expr(expr: BinaryExpr) -> OptimizeExprResult:
    lhs_res = _optimize_expr(expr.lhs)
    rhs_res = _optimize_expr(expr.rhs)
    if not isinstance(lhs_res.result, IntegerLiteral) or not isinstance(
        rhs_res.result, IntegerLiteral
    ):
        expr.lhs = lhs_res.result
        expr.rhs = rhs_res.result
        return OptimizeExprResult(expr, lhs_res.is_pure and rhs_res.is_pure)
    assert lhs_res.is_pure and rhs_res.is_pure
    lhs_lit = lhs_res.result.value
    rhs_lit = rhs_res.result.value
    result = 0
    match expr.opc:
        case BinaryOperatorKind.ADD:
            result = lhs_lit + rhs_lit
        case BinaryOperatorKind.SUB:
            result = lhs_lit - rhs_lit
        case BinaryOperatorKind.MUL:
            result = lhs_lit * rhs_lit
        case BinaryOperatorKind.DIV:
            result = lhs_lit // rhs_lit
        case BinaryOperatorKind.REM:
            result = lhs_lit % rhs_lit
        case BinaryOperatorKind.SHL:
            result = lhs_lit << rhs_lit
        case BinaryOperatorKind.SHR:
            result = lhs_lit >> rhs_lit
        case BinaryOperatorKind.LT:
            result = int(lhs_lit < rhs_lit)
        case BinaryOperatorKind.GT:
            result = int(lhs_lit > rhs_lit)
        case BinaryOperatorKind.LE:
            result = int(lhs_lit <= rhs_lit)
        case BinaryOperatorKind.GE:
            result = int(lhs_lit >= rhs_lit)
        case BinaryOperatorKind.EQ:
            result = int(lhs_lit == rhs_lit)
        case BinaryOperatorKind.NE:
            result = int(lhs_lit != rhs_lit)
        case BinaryOperatorKind.AND:
            result = lhs_lit & rhs_lit
        case BinaryOperatorKind.XOR:
            result = lhs_lit ^ rhs_lit
        case BinaryOperatorKind.OR:
            result = lhs_lit | rhs_lit
        case BinaryOperatorKind.LAND:
            result = int(bool(lhs_lit) and bool(rhs_lit))
        case BinaryOperatorKind.LOR:
            result = int(bool(lhs_lit) or bool(rhs_lit))
        case _:
            assert (False), f"Unimplemented binary expr optimization: {expr.opc.name}"
    return OptimizeExprResult(IntegerLiteral(result, expr.ty, expr.op_loc), True)

def _optimize_unary_expr(expr: UnaryExpr) -> OptimizeExprResult:
    arg_res = _optimize_expr(expr.arg)
    if not isinstance(arg_res.result, IntegerLiteral):
        expr.arg = arg_res.result
        return OptimizeExprResult(expr, arg_res.is_pure)
    assert arg_res.is_pure
    arg_lit = arg_res.result.value
    result = 0
    match expr.opc:
        case UnaryOperatorKind.MINUS:
            result = -arg_lit
        case UnaryOperatorKind.PLUS:
            result = arg_lit
        case UnaryOperatorKind.LNOT:
            result = int(arg_lit == 0)
        case UnaryOperatorKind.NOT:
            result = ~arg_lit
        case _:
            # should not happen
            pass
            # assert (False), f"Unimplemented unary expr optimization: {expr.opc.name}"
    return OptimizeExprResult(IntegerLiteral(result, expr.ty, expr.loc), True)

def _optimize_conditional_expr(expr: ConditionalExpr) -> OptimizeExprResult:
    cond_res = _optimize_expr(expr.cond)
    if not isinstance(cond_res.result, IntegerLiteral):
        expr.cond = cond_res.result
        lhs_res = _optimize_expr(expr.lhs)
        rhs_res = _optimize_expr(expr.rhs)
        expr.lhs = lhs_res.result
        expr.rhs = rhs_res.result
        return OptimizeExprResult(expr, cond_res.is_pure and lhs_res.is_pure and rhs_res.is_pure)
    cond_lit = cond_res.result.value
    if cond_lit != 0:
        return _optimize_expr(expr.lhs)
    else:
        return _optimize_expr(expr.rhs)

def _optimize_builtin_expr(expr: BuiltinExpr) -> OptimizeExprResult:
    expr.args = [_optimize_expr(e).result for e in expr.args]
    return OptimizeExprResult(expr, False)

def _optimize_array_subscript_expr(expr: ArraySubscriptExpr) -> OptimizeExprResult:
    # TODO:
    return OptimizeExprResult(expr, False)

def _optimize_member_expr(expr: MemberExpr) -> OptimizeExprResult:
    # TODO:
    return OptimizeExprResult(expr, False)

def _optimize_call_expr(expr: CallExpr) -> OptimizeExprResult:
    expr.fn = _optimize_expr(expr.fn).result
    expr.args = [_optimize_expr(a).result for a in expr.args]
    return OptimizeExprResult(expr, False)

def _optimize_method_expr(expr: MethodExpr) -> OptimizeExprResult:
    # TODO: 
    return OptimizeExprResult(expr, False)

def _optimize_expr(expr: Expr) -> OptimizeExprResult:
    if isinstance(expr, IntegerLiteral):
        return OptimizeExprResult(expr, True)
    elif isinstance(expr, StringLiteral):
        return OptimizeExprResult(expr, True)
    elif isinstance(expr, DeclRefExpr):
        return OptimizeExprResult(expr, False)
    elif isinstance(expr, VAArgExpr):
        return OptimizeExprResult(expr, False)
    elif isinstance(expr, BoolLiteral):
        return OptimizeExprResult(IntegerLiteral(int(expr.value), expr.ty, expr.loc), True)
    elif isinstance(expr, ParenExpr):
        return _optimize_expr(expr.val)
    elif isinstance(expr, BinaryExpr):
        return _optimize_binary_expr(expr)
    elif isinstance(expr, CompoundAssignExpr):
        expr.rhs = _optimize_expr(expr.rhs).result
        expr.lhs = _optimize_expr(expr.lhs).result
        return OptimizeExprResult(expr, False)
    elif isinstance(expr, BuiltinExpr):
        return _optimize_builtin_expr(expr)
    elif isinstance(expr, UnaryExpr):
        return _optimize_unary_expr(expr)
    elif isinstance(expr, ConditionalExpr):
        return _optimize_conditional_expr(expr)
    elif isinstance(expr, ArraySubscriptExpr):
        return _optimize_array_subscript_expr(expr)
    elif isinstance(expr, MemberExpr):
        return _optimize_member_expr(expr)
    elif isinstance(expr, MethodExpr):
        return _optimize_method_expr(expr)
    elif isinstance(expr, CallExpr):
        return _optimize_call_expr(expr)
    elif isinstance(expr, SizeofExpr):
        expr.expr = None
        return OptimizeExprResult(expr, True)
    elif isinstance(expr, CastExpr):
        res = _optimize_expr(expr.op)
        expr.op = res.result
        return OptimizeExprResult(expr, res.is_pure)
    else:
        print(f"Unimplemented expr optimization: {expr.__class__}")
        return OptimizeExprResult(expr, False)


@dataclass
class OptimizeStmtResult:
    result: Stmt


def _optimize_compound_stmt(stmt: CompoundStmt) -> OptimizeStmtResult:
    res = []
    for s in stmt.inner:
        r = _optimize_stmt(s)
        if not isinstance(r.result, NullStmt):
            res.append(r.result)
    if len(res) == 0:
        return OptimizeStmtResult(NullStmt(0))
    if len(res) == 1:
        return OptimizeStmtResult(res[0])
    stmt.inner = res
    return OptimizeStmtResult(stmt)


def _optimize_expr_stmt(expr: Expr) -> OptimizeStmtResult:
    res = _optimize_expr(expr)
    if res.is_pure:
        return OptimizeStmtResult(NullStmt(0))
    else:
        return OptimizeStmtResult(res.result)


def _optimize_if_stmt(stmt: IfStmt) -> OptimizeStmtResult:
    then_stmt = _optimize_stmt(stmt.then_stmt)
    else_stmt = (
        _optimize_stmt(stmt.else_stmt)
        if stmt.else_stmt is not None
        else OptimizeStmtResult(NullStmt(0))
    )
    if isinstance(then_stmt.result, NullStmt) and isinstance(else_stmt.result, NullStmt):
        expr = _optimize_expr(stmt.cond)
        if expr.is_pure:
            return OptimizeStmtResult(NullStmt(0))
        else:
            return OptimizeStmtResult(expr.result)
    if isinstance(then_stmt.result, NullStmt):
        # if then is none, instead, invert the condition
        # right now it's only a null stmt
        then_stmt.result = NullStmt(stmt.then_stmt.get_range()[0])
    expr = _optimize_expr(stmt.cond).result
    if not isinstance(expr, IntegerLiteral):
        stmt.then_stmt = then_stmt.result
        stmt.else_stmt = else_stmt.result
        stmt.cond = expr
        return OptimizeStmtResult(stmt)
    expr_val = expr.value
    if expr_val != 0:
        return then_stmt
    else:
        return else_stmt

def _optimize_return_stmt(stmt: ReturnStmt) -> OptimizeStmtResult:
    if stmt.ret_expr is not None:
        stmt.ret_expr = _optimize_expr(stmt.ret_expr).result
    return OptimizeStmtResult(stmt)

def _optimize_while_stmt(stmt: WhileStmt) -> OptimizeStmtResult:
    body = _optimize_stmt(stmt.while_stmt)
    expr = _optimize_expr(stmt.cond).result
    if not isinstance(expr, IntegerLiteral):
        stmt.cond = expr
        stmt.while_stmt = body.result
        return OptimizeStmtResult(stmt)
    expr_val = expr.value
    if expr_val != 0:
        # TODO: loop stmt, w expr if not pure
        stmt.cond = expr
        stmt.while_stmt = body.result
        return OptimizeStmtResult(stmt)
    return OptimizeStmtResult(NullStmt(0))


def _optimize_do_stmt(stmt: DoStmt) -> OptimizeStmtResult:
    # TODO:
    stmt.expr = _optimize_expr(stmt.expr).result
    stmt.body = _optimize_stmt(stmt.body).result
    return OptimizeStmtResult(stmt)

def _optimize_stmt(stmt: Stmt) -> OptimizeStmtResult:
    if isinstance(stmt, CompoundStmt):
        return _optimize_compound_stmt(stmt)
    if isinstance(stmt, NullStmt):
        return OptimizeStmtResult(stmt)
    if isinstance(stmt, IfStmt):
        return _optimize_if_stmt(stmt)
    if isinstance(stmt, WhileStmt):
        return _optimize_while_stmt(stmt)
    if isinstance(stmt, ContinueStmt):
        return OptimizeStmtResult(stmt)
    if isinstance(stmt, BreakStmt):
        return OptimizeStmtResult(stmt)
    if isinstance(stmt, DoStmt):
        return _optimize_do_stmt(stmt)
    if isinstance(stmt, Expr):
        return _optimize_expr_stmt(stmt)
    if isinstance(stmt, DeclStmt):
        return OptimizeStmtResult(stmt)
    if isinstance(stmt, ReturnStmt):
        return _optimize_return_stmt(stmt)
    print(f"Unhandled statement type {stmt.__class__}")
    return OptimizeStmtResult(stmt)


def optimize_ast(translation_unit: TranslationUnitDecl):
    for d in translation_unit.decls:
        if isinstance(d, FnDecl) and d.body is not None:
            out_body = []
            for stmt in d.body.inner:
                stmt = _optimize_stmt(stmt).result
                if not isinstance(stmt, NullStmt):
                    out_body.append(stmt)
            d.body.inner = out_body
