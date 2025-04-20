from ns_ast.nodes import (
    Expr,
    ParenExpr,
    StringLiteral,
    IntegerLiteral,
    BoolLiteral,
    SizeofExpr,
    CastExpr,
    CastKind,
    DeclRefExpr,
    EnumVariantDecl,
    BinaryExpr,
    BinaryOperatorKind,
    UnaryExpr,
    UnaryOperatorKind,
    ConditionalExpr,
    CallExpr,
    BuiltinExpr,
    ArraySubscriptExpr,
    MemberExpr,
    RecoveryExpr,
)
from utils.diagnostic import diag, Diag


def eval_const_expr(e: Expr) -> int:
    if isinstance(e, ParenExpr):
        return eval_const_expr(e.val)
    if isinstance(e, StringLiteral):
        assert False, "String literal in constexpr context"
        # return e.value
    if isinstance(e, IntegerLiteral):
        return e.value
    if isinstance(e, BoolLiteral):
        return e.value
    if isinstance(e, SizeofExpr):
        return e.ty_of_sizeof.get_size()
    if isinstance(e, CastExpr):
        match e.kind:
            case (
                CastKind.NOOP
                | CastKind.TO_VOID
                | CastKind.POINTER_TO_BOOLEAN
                | CastKind.INTEGRAL_TO_BOOLEAN
                | CastKind.INTEGRAL_CAST
            ):
                return eval_const_expr(e.op)
            case (
                CastKind.ARRAY_TO_POINTER_DECAY
                | CastKind.FUNCTION_TO_POINTER_DECAY
                | CastKind.LVALUE_TO_RVALUE
            ):
                diag(
                    e.get_range()[0],
                    "unsupported cast in constexpr",
                    Diag.ERROR,
                    [e.get_range()],
                )
    if isinstance(e, DeclRefExpr):
        if not isinstance(e.decl, EnumVariantDecl):
            diag(
                e.get_range()[0],
                "non const variable used in constexpr",
                Diag.ERROR,
                [e.get_range()],
            )
    if isinstance(e, BinaryExpr):
        match e.opc:
            case BinaryOperatorKind.MUL:
                return eval_const_expr(e.lhs) * eval_const_expr(e.rhs)
            case BinaryOperatorKind.DIV:
                return eval_const_expr(e.lhs) // eval_const_expr(e.rhs)
            case BinaryOperatorKind.REM:
                return eval_const_expr(e.lhs) % eval_const_expr(e.rhs)
            case BinaryOperatorKind.ADD:
                return eval_const_expr(e.lhs) + eval_const_expr(e.rhs)
            case BinaryOperatorKind.SUB:
                return eval_const_expr(e.lhs) - eval_const_expr(e.rhs)
            case BinaryOperatorKind.SHL:
                return eval_const_expr(e.lhs) << eval_const_expr(e.rhs)
            case BinaryOperatorKind.SHR:
                return eval_const_expr(e.lhs) >> eval_const_expr(e.rhs)
            case BinaryOperatorKind.LT:
                return int(eval_const_expr(e.lhs) < eval_const_expr(e.rhs))
            case BinaryOperatorKind.GT:
                return int(eval_const_expr(e.lhs) > eval_const_expr(e.rhs))
            case BinaryOperatorKind.LE:
                return int(eval_const_expr(e.lhs) <= eval_const_expr(e.rhs))
            case BinaryOperatorKind.GE:
                return int(eval_const_expr(e.lhs) >= eval_const_expr(e.rhs))
            case BinaryOperatorKind.EQ:
                return int(eval_const_expr(e.lhs) == eval_const_expr(e.rhs))
            case BinaryOperatorKind.NE:
                return int(eval_const_expr(e.lhs) != eval_const_expr(e.rhs))
            case BinaryOperatorKind.AND:
                return eval_const_expr(e.lhs) & eval_const_expr(e.rhs)
            case BinaryOperatorKind.XOR:
                return eval_const_expr(e.lhs) ^ eval_const_expr(e.rhs)
            case BinaryOperatorKind.OR:
                return eval_const_expr(e.lhs) | eval_const_expr(e.rhs)
            case BinaryOperatorKind.LAND:
                if eval_const_expr(e.lhs) == 0:
                    return 0
                return eval_const_expr(e.rhs)
            case BinaryOperatorKind.LOR:
                if eval_const_expr(e.lhs) == 1:
                    return 1
                return eval_const_expr(e.rhs)
            case _:
                diag(
                    e.op_loc,
                    "assignments are not constexpr",
                    Diag.ERROR,
                    [e.get_range()],
                )
                return 0
    if isinstance(e, UnaryExpr):
        match e.opc:
            case (
                UnaryOperatorKind.POSTINC
                | UnaryOperatorKind.POSTDEC
                | UnaryOperatorKind.PREINC
                | UnaryOperatorKind.PREDEC
            ):
                diag(
                    e.loc,
                    "increment/decrement operation is not constexpr",
                    Diag.ERROR,
                    [e.get_range()],
                )
                return 0
            case UnaryOperatorKind.ADDROF:
                diag(
                    e.loc,
                    "addrof operation is not constexpr (TODO: what if get addr of global var ?)",
                    Diag.ERROR,
                    [e.get_range()],
                )
                return 0
            case UnaryOperatorKind.DEREF:
                diag(
                    e.loc,
                    "deref operation is not constexpr (TODO: what if deref of global const ptr var ?)",
                    Diag.ERROR,
                    [e.get_range()],
                )
                return 0
            case UnaryOperatorKind.PLUS:
                return eval_const_expr(e.arg)
            case UnaryOperatorKind.MINUS:
                return -eval_const_expr(e.arg)
            case UnaryOperatorKind.NOT:
                return ~eval_const_expr(e.arg)
            case UnaryOperatorKind.LNOT:
                return int(not eval_const_expr(e.arg))
    if isinstance(e, ConditionalExpr):
        cond_val = eval_const_expr(e.cond)
        if cond_val != 0:
            return eval_const_expr(e.lhs)
        return eval_const_expr(e.rhs)
    if isinstance(e, CallExpr):
        diag(
            e.get_range()[0],
            "function calls are not constexpr",
            Diag.ERROR,
            [e.get_range()],
        )
        return 0
    if isinstance(e, BuiltinExpr):
        diag(
            e.get_range()[0],
            "builtin calls are not constexpr",
            Diag.ERROR,
            [e.get_range()],
        )
        return 0
    if isinstance(e, ArraySubscriptExpr):
        diag(
            e.get_range()[0],
            "array subscripts are not constexpr (TODO: what if subscript in global const array ?)",
            Diag.ERROR,
            [e.get_range()],
        )
        return 0
    if isinstance(e, MemberExpr):
        diag(
            e.get_range()[0],
            "member access is not constexpr (TODO: what if access in global const struct ?)",
            Diag.ERROR,
            [e.get_range()],
        )
        return 0
    if isinstance(e, RecoveryExpr):
        # TODO: should this emit a diagnostic ?
        return 0
    assert False, "not a const expression"
