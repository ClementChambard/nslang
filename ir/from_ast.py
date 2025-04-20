from dataclasses import dataclass
from typing import List
from . import (
    IrInstrKind,
    IrInstr,
    FunctionIr,
    IrGlobal,
    FullIr,
    StackFrameEntry,
    ParamEntry,
)
from utils.diagnostic import diag, Diag
from ns_ast.nodes import *


LABEL_ID: int = 0


@dataclass
class LabelUsage:
    used: bool
    name: str

    def __init__(self, name: str):
        self.used = False
        self.name = name


@dataclass
class StmtGenInfo:
    stack_frame: List[StackFrameEntry]
    break_stack: List[LabelUsage]
    continue_stack: List[LabelUsage]

    def __init__(self):
        self.stack_frame = []
        self.break_stack = []
        self.continue_stack = []


def new_label_name(name: str) -> str:
    global LABEL_ID
    name = f"{name}_{LABEL_ID}"
    LABEL_ID += 1
    return name


def scope_lookup(cur_scope: dict, scope_stack: list, name: str) -> int:
    if name in cur_scope.keys():
        return cur_scope[name]
    assert len(scope_stack) > 0
    return scope_lookup(scope_stack[-1], scope_stack[:-1], name)


def generate_declref_lvalue_addr(
    e: DeclRefExpr, ir: FullIr, cur_scope, scope_stack
) -> List[IrInstr]:
    if (g := ir.has_global(e.decl.name)) >= 0:
        return [IrInstr(IrInstrKind.PSH, 3, g)]
    else:
        var_id = scope_lookup(cur_scope, scope_stack, e.decl.name)
    return [IrInstr(IrInstrKind.PSH, 2, var_id)]


def generate_lvalue_addr(e: Expr, ir: FullIr, cur_scope, scope_stack) -> List[IrInstr]:
    assert e.value_kind == ValueKind.LVALUE
    # if isinstance(e, DeclRefExpr) and isinstance(e.ty, PointerType):
    #     return generate_decl_ref_lvalue_addr(e, ir, cur_scope, scope_stack)
    #     if (g := ir.has_global(e.decl.name)) >= 0:
    #         return [IrInstr(IrInstrKind.PSH, 4, g)]
    #     else:
    #         var_id = scope_lookup(cur_scope, scope_stack, e.decl.name)
    #         return [IrInstr(IrInstrKind.PSH, 1, var_id)]
    # elif isinstance(e, MemberExpr) and isinstance(e.ty, PointerType):
    #     out = generate_member_expr_addr(e, ir, cur_scope, scope_stack)
    #     out += [IrInstr(IrInstrKind.LDA, e.ty.subtype.get_size(), None)]
    #     return out
    if isinstance(e, ParenExpr):
        return generate_lvalue_addr(e.val, ir, cur_scope, scope_stack)
    elif isinstance(e, CastExpr):
        # if e.kind == CastKind.ARRAY_TO_POINTER_DECAY:
        #     e = e.op
        #     if not isinstance(e, DeclRefExpr) or not isinstance(e.ty, ArrayType):
        #         print(e)
        #         diag(e.get_range()[0], "ASSERT FALSE", Diag.ERROR, [e.get_range()])
        #         assert False
        #     var_id = scope_lookup(cur_scope, scope_stack, e.decl.name)
        #     return [IrInstr(IrInstrKind.PSH, 2, var_id)]
        if e.kind == CastKind.NOOP:
            return generate_lvalue_addr(e.op, ir, cur_scope, scope_stack)
    elif isinstance(e, ArraySubscriptExpr):
        return generate_array_subscript_addr(e, ir, cur_scope, scope_stack)
    elif isinstance(e, DeclRefExpr):
        return generate_declref_lvalue_addr(e, ir, cur_scope, scope_stack)
    elif isinstance(e, MemberExpr):
        return generate_member_expr_addr(e, ir, cur_scope, scope_stack)
    print(e)
    diag(
        e.get_range()[0],
        "Unimplemented lvalue_addr case",
        Diag.UNIMPLEMENTED,
        [e.get_range()],
    )
    assert False


def generate_array_subscript_addr(
    e: ArraySubscriptExpr, ir: FullIr, cur_scope, scope_stack
) -> List[IrInstr]:
    # Generates the code for &(e.lhs)[(e.rhs)]
    # equals e.lhs + e.rhs * sizeof(*e.lhs)
    out = []
    assert isinstance(e.lhs.ty, PointerType)
    out += generate_expr_ir(e.lhs, ir, cur_scope, scope_stack)
    out += generate_expr_ir(e.rhs, ir, cur_scope, scope_stack)
    off_size = e.lhs.ty.subtype.get_size()
    out += [IrInstr(IrInstrKind.MUL, 0, off_size)]
    out += [IrInstr(IrInstrKind.ADD, None, None)]
    return out


def generate_member_expr_addr(
    e: MemberExpr, ir: FullIr, cur_scope, scope_stack
) -> List[IrInstr]:
    # Generates the code for &((e.lhs).(e.field))
    # equals &(e.lhs) + offsetof(e.field)
    out = []
    if e.base.value_kind == ValueKind.LVALUE and not e.is_arrow:
        out += generate_lvalue_addr(e.base, ir, cur_scope, scope_stack)
    elif e.base.value_kind == ValueKind.PRVALUE and e.is_arrow:
        assert isinstance(e.base.ty, PointerType), "-> on non pointer"
        out += generate_expr_ir(e.base, ir, cur_scope, scope_stack)
    else:
        assert False, "Member access on non lvalue"
    out += [IrInstr(IrInstrKind.ADD, 0, e.field_offset)]
    return out


def generate_addrof_ir(e: Expr, ir: FullIr, cur_scope, scope_stack) -> List[IrInstr]:
    # Generates the code for &e
    if isinstance(e, ArraySubscriptExpr):
        return generate_array_subscript_addr(e, ir, cur_scope, scope_stack)
    elif isinstance(e, CastExpr) and e.kind == CastKind.NOOP:
        return generate_addrof_ir(e.op, ir, cur_scope, scope_stack)
    elif isinstance(e, ParenExpr):
        return generate_addrof_ir(e.val, ir, cur_scope, scope_stack)
    elif isinstance(e, MemberExpr):
        return generate_member_expr_addr(e, ir, cur_scope, scope_stack)
    elif isinstance(e, DeclRefExpr):
        return generate_declref_lvalue_addr(e, ir, cur_scope, scope_stack)
    else:
        assert False, "unimplemented addrof_ir case"


def generate_assignment(e, ir: FullIr, cur_scope, scope_stack) -> List[IrInstr]:
    out = []
    if isinstance(e, DeclRefExpr):
        if (g := ir.has_global(e.decl.name)) >= 0:
            out += [IrInstr(IrInstrKind.DUP, None, None)]
            out += [IrInstr(IrInstrKind.PSH, 3, g)]
            out += [IrInstr(IrInstrKind.STA, 8, None)]
        else:
            var_id = scope_lookup(cur_scope, scope_stack, e.decl.name)
            out += [IrInstr(IrInstrKind.STV, var_id, None)]
            out += [IrInstr(IrInstrKind.PSH, 1, var_id)]
    elif isinstance(e, MemberExpr):
        out += [IrInstr(IrInstrKind.DUP, None, None)]
        out += generate_member_expr_addr(e, ir, cur_scope, scope_stack)
        store_size = e.ty.get_size()
        out += [IrInstr(IrInstrKind.STA, store_size, None)]
    elif isinstance(e, ArraySubscriptExpr):
        out += [IrInstr(IrInstrKind.DUP, None, None)]
        out += generate_array_subscript_addr(e, ir, cur_scope, scope_stack)
        store_size = e.ty.get_size()
        out += [IrInstr(IrInstrKind.STA, store_size, None)]
    elif isinstance(e, UnaryExpr) and e.opc == UnaryOperatorKind.DEREF:
        out += [IrInstr(IrInstrKind.DUP, None, None)]
        out += generate_expr_ir(e.arg, ir, cur_scope, scope_stack)
        store_size = e.ty.get_size()
        out += [IrInstr(IrInstrKind.STA, store_size, None)]
    else:
        assert False, f"assignment case unimplemented: {e.__class__}"
    return out


def generate_unary_expr_ir(
    e: UnaryExpr, ir: FullIr, cur_scope, scope_stack
) -> List[IrInstr]:
    # TODO: decr / incr / deref
    if e.opc == UnaryOperatorKind.ADDROF:
        return generate_addrof_ir(e.arg, ir, cur_scope, scope_stack)
    out = []
    out += generate_expr_ir(e.arg, ir, cur_scope, scope_stack)
    match e.opc:
        case UnaryOperatorKind.DEREF:
            out += [IrInstr(IrInstrKind.LDA, e.ty.get_size(), None)]
        case UnaryOperatorKind.PLUS:
            pass
        case UnaryOperatorKind.MINUS:
            out += [IrInstr(IrInstrKind.NEG, None, None)]
        case UnaryOperatorKind.NOT:
            out += [IrInstr(IrInstrKind.INV, None, None)]
        case UnaryOperatorKind.LNOT:
            out += [IrInstr(IrInstrKind.NOT, None, None)]
        case _:
            assert False, f"unhandled unary op: {e.opc}"
    return out


def generate_binary_expr_ir(
    e: BinaryExpr, ir: FullIr, cur_scope, scope_stack
) -> List[IrInstr]:
    out = []
    if e.opc == BinaryOperatorKind.ASSIGN:
        out += generate_expr_ir(e.rhs, ir, cur_scope, scope_stack)
        out += generate_assignment(e.lhs, ir, cur_scope, scope_stack)
        return out
    out += generate_expr_ir(e.lhs, ir, cur_scope, scope_stack)
    out += generate_expr_ir(e.rhs, ir, cur_scope, scope_stack)
    match e.opc:
        case BinaryOperatorKind.ADD:
            out.append(IrInstr(IrInstrKind.ADD, None, None))
        case BinaryOperatorKind.SUB:
            out.append(IrInstr(IrInstrKind.SUB, None, None))
        case BinaryOperatorKind.MUL:
            out.append(IrInstr(IrInstrKind.MUL, None, None))
        case BinaryOperatorKind.DIV:
            out.append(IrInstr(IrInstrKind.DIV, None, None))
        case BinaryOperatorKind.REM:
            out.append(IrInstr(IrInstrKind.REM, None, None))
        case BinaryOperatorKind.SHL:
            out.append(IrInstr(IrInstrKind.SHL, None, None))
        case BinaryOperatorKind.SHR:
            out.append(IrInstr(IrInstrKind.SHR, None, None))
        case BinaryOperatorKind.LT:
            out.append(IrInstr(IrInstrKind.LTH, None, None))
        case BinaryOperatorKind.GT:
            out.append(IrInstr(IrInstrKind.GTH, None, None))
        case BinaryOperatorKind.LE:
            out.append(IrInstr(IrInstrKind.LEQ, None, None))
        case BinaryOperatorKind.GE:
            out.append(IrInstr(IrInstrKind.GEQ, None, None))
        case BinaryOperatorKind.EQ:
            out.append(IrInstr(IrInstrKind.EQU, None, None))
        case BinaryOperatorKind.NE:
            out.append(IrInstr(IrInstrKind.NEQ, None, None))
        case BinaryOperatorKind.AND:
            out.append(IrInstr(IrInstrKind.AND, None, None))
        case BinaryOperatorKind.XOR:
            out.append(IrInstr(IrInstrKind.XOR, None, None))
        case BinaryOperatorKind.OR:
            out.append(IrInstr(IrInstrKind.IOR, None, None))
        # Logical &&, || => branch for lazy evaluation
        case _:
            assert False, f"unimplemented binary op: {e.opc}"
    return out


def generate_compound_assign_expr_ir(
    e: CompoundAssignExpr, ir: FullIr, cur_scope, scope_stack
) -> List[IrInstr]:
    out = []
    # TODO: different kind of lhs
    assert isinstance(e.lhs, DeclRefExpr), "compound assign only on var for now"
    var_id = scope_lookup(cur_scope, scope_stack, e.lhs.decl.name)
    out += [IrInstr(IrInstrKind.PSH, 1, var_id)]

    out += generate_expr_ir(e.rhs, ir, cur_scope, scope_stack)
    match e.opc:
        case BinaryOperatorKind.ADDASSIGN:
            out.append(IrInstr(IrInstrKind.ADD, None, None))
        case BinaryOperatorKind.SUBASSIGN:
            out.append(IrInstr(IrInstrKind.SUB, None, None))
        case BinaryOperatorKind.MULASSIGN:
            out.append(IrInstr(IrInstrKind.MUL, None, None))
        case BinaryOperatorKind.DIVASSIGN:
            out.append(IrInstr(IrInstrKind.DIV, None, None))
        case BinaryOperatorKind.REMASSIGN:
            out.append(IrInstr(IrInstrKind.REM, None, None))
        case BinaryOperatorKind.SHLASSIGN:
            out.append(IrInstr(IrInstrKind.SHL, None, None))
        case BinaryOperatorKind.SHRASSIGN:
            out.append(IrInstr(IrInstrKind.SHR, None, None))
        case BinaryOperatorKind.ANDASSIGN:
            out.append(IrInstr(IrInstrKind.AND, None, None))
        case BinaryOperatorKind.XORASSIGN:
            out.append(IrInstr(IrInstrKind.XOR, None, None))
        case BinaryOperatorKind.ORASSIGN:
            out.append(IrInstr(IrInstrKind.IOR, None, None))
        case _:
            assert False, f"unimplemented compassign op: {e.opc}"

    # TODO: XXX: => lhs might be calculated twice => error if lhs or rhs changes value of lhs
    out += generate_assignment(e.lhs, ir, cur_scope, scope_stack)
    return out


def generate_cast_expr_ir(
    e: CastExpr, ir: FullIr, cur_scope, scope_stack
) -> List[IrInstr]:
    if e.kind == CastKind.NOOP:
        return generate_expr_ir(e.op, ir, cur_scope, scope_stack)
    elif (
        e.kind == CastKind.INTEGRAL_TO_BOOLEAN or e.kind == CastKind.POINTER_TO_BOOLEAN
    ):
        # DO NOTHING TO CONVERT TO BOOLEAN
        return generate_expr_ir(e.op, ir, cur_scope, scope_stack)
    elif e.kind == CastKind.ARRAY_TO_POINTER_DECAY:
        if isinstance(e.op, DeclRefExpr):
            decl = e.op.decl
            assert isinstance(e.op.decl.ty, ArrayType)
            # TODO:
            var_id = scope_lookup(cur_scope, scope_stack, decl.name)
            return [IrInstr(IrInstrKind.PSH, 2, var_id)]
        elif isinstance(e.op, StringLiteral):
            global_id = len(ir.globs)
            ir.globs.append(IrGlobal(e.op.value, False, True))
            return [IrInstr(IrInstrKind.PSH, 3, global_id)]
    elif e.kind == CastKind.LVALUE_TO_RVALUE:
        if isinstance(e.op, DeclRefExpr):
            if (g := ir.has_global(e.op.decl.name)) >= 0:
                return [IrInstr(IrInstrKind.PSH, 4, g)]
            else:
                var_id = scope_lookup(cur_scope, scope_stack, e.op.decl.name)
                return [IrInstr(IrInstrKind.PSH, 1, var_id)]
        elif isinstance(e.op, ArraySubscriptExpr):
            out = []
            out += generate_array_subscript_addr(e.op, ir, cur_scope, scope_stack)
            size = e.op.ty.get_size()
            out += [IrInstr(IrInstrKind.LDA, size, None)]
            return out
        elif isinstance(e.op, MemberExpr):
            out = []
            out += generate_member_expr_addr(e.op, ir, cur_scope, scope_stack)
            size = e.op.ty.get_size()
            out += [IrInstr(IrInstrKind.LDA, size, None)]
            return out
        elif isinstance(e.op, BinaryExpr) and e.op.opc == BinaryOperatorKind.ASSIGN:
            return generate_expr_ir(e.op, ir, cur_scope, scope_stack)
        elif isinstance(e.op, UnaryExpr) and e.op.opc == UnaryOperatorKind.DEREF:
            out = []
            out += generate_expr_ir(e.op.arg, ir, cur_scope, scope_stack)
            size = e.op.ty.get_size()
            out += [IrInstr(IrInstrKind.LDA, size, None)]
            return out
    elif e.kind == CastKind.INTEGRAL_CAST:
        # TODO: what to do ? for now nothing in IR
        return generate_expr_ir(e.op, ir, cur_scope, scope_stack)
    assert False, f"unhandled op kind {e.op.__class__} for cast kind: {e.kind}"


def generate_method_call_expr_ir(
    e: MethodCallExpr, ir: FullIr, cur_scope, scope_stack
) -> List[IrInstr]:
    out = []
    for a in e.args[::-1]:
        out += generate_expr_ir(a, ir, cur_scope, scope_stack)
    assert isinstance(e.fn, MethodExpr)
    # Generate self arg addr
    if e.fn.self_object.value_kind == ValueKind.LVALUE and not e.fn.is_arrow:
        out += generate_lvalue_addr(e.fn.self_object, ir, cur_scope, scope_stack)
    elif e.fn.self_object.value_kind == ValueKind.PRVALUE and e.fn.is_arrow:
        assert isinstance(e.fn.self_object.ty, PointerType), "-> on non pointer"
        out += generate_expr_ir(e.fn.self_object, ir, cur_scope, scope_stack)
    else:
        assert False, "Member access on non lvalue"
    name = e.fn.method_func.name.replace(":", "_")
    return out + [IrInstr(IrInstrKind.CAL, name, len(e.args) + 1)]


def generate_call_expr_ir(
    e: CallExpr, ir: FullIr, cur_scope, scope_stack
) -> List[IrInstr]:
    out = []
    for a in e.args[::-1]:
        out += generate_expr_ir(a, ir, cur_scope, scope_stack)
    assert isinstance(e.fn, DeclRefExpr) and isinstance(e.fn.decl, FnDecl), (
        "fn of callexpr is not a function"
    )
    name = e.fn.decl.name.replace(":", "_")
    return out + [IrInstr(IrInstrKind.CAL, name, len(e.args))]


def generate_conditional_expr_ir(
    e: ConditionalExpr, ir: FullIr, cur_scope, scope_stack
) -> List[IrInstr]:
    _, _, _, _ = e, ir, cur_scope, scope_stack  # UNUSED
    assert False, "ConditionalExpr to ir not implemented"


def generate_expr_ir(e, ir: FullIr, cur_scope, scope_stack) -> List[IrInstr]:
    if isinstance(e, DeclRefExpr) and isinstance(e.decl, EnumVariantDecl):
        return [IrInstr(IrInstrKind.PSH, 0, e.decl.val)]
    if isinstance(e, ParenExpr):
        return generate_expr_ir(e.val, ir, cur_scope, scope_stack)
    if isinstance(e, BuiltinExpr):
        out = []
        for a in e.args[::-1]:
            out += generate_expr_ir(a, ir, cur_scope, scope_stack)
        return out + [IrInstr(IrInstrKind.BUI, e.builtin_name, len(e.args))]
    if isinstance(e, IntegerLiteral) or isinstance(e, BoolLiteral):
        try:
            return [IrInstr(IrInstrKind.PSH, 0, int(e.value))]
        except:
            assert False, "int conversion failed"
    if isinstance(e, UnaryExpr):
        return generate_unary_expr_ir(e, ir, cur_scope, scope_stack)
    if isinstance(e, ConditionalExpr):
        return generate_conditional_expr_ir(e, ir, cur_scope, scope_stack)
    if isinstance(e, CompoundAssignExpr):
        return generate_compound_assign_expr_ir(e, ir, cur_scope, scope_stack)
    if isinstance(e, BinaryExpr):
        return generate_binary_expr_ir(e, ir, cur_scope, scope_stack)
    if isinstance(e, SizeofExpr):
        return [IrInstr(IrInstrKind.PSH, 0, e.ty_of_sizeof.get_size())]
    # if isinstance(e, AlignofExpr):
    #     return [IrInstr(IrInstrKind.PSH, 0, e.ty_of_sizeof.get_align())]
    if isinstance(e, CastExpr):
        return generate_cast_expr_ir(e, ir, cur_scope, scope_stack)
    if isinstance(e, MethodCallExpr):
        return generate_method_call_expr_ir(e, ir, cur_scope, scope_stack)
    if isinstance(e, CallExpr):
        return generate_call_expr_ir(e, ir, cur_scope, scope_stack)
    if isinstance(e, VAArgExpr):
        return [IrInstr(IrInstrKind.VAA, False, None)]  # TODO: handle is_float = True
    diag(e.get_range()[0], "ASSERT FALSE", Diag.ERROR, [e.get_range()])
    assert False, f"Raw {e.__class__} encountered in ast"


def generate_stmt_ir(
    s: Stmt, ir: FullIr, cur_scope, scope_stack, stmt_gen_info
) -> List[IrInstr]:
    out = []
    if isinstance(s, NullStmt):
        return out
    elif isinstance(s, CompoundStmt):
        for i in s.inner:
            g = generate_stmt_ir(i, ir, cur_scope, scope_stack, stmt_gen_info)
            out += g
    elif isinstance(s, IfStmt):
        out += generate_expr_ir(s.cond, ir, cur_scope, scope_stack)
        lbl_false = new_label_name("cond_false")
        out += [IrInstr(IrInstrKind.JZO, lbl_false, None)]
        out += generate_stmt_ir(s.then_stmt, ir, cur_scope, scope_stack, stmt_gen_info)
        if s.else_stmt is not None:
            lbl_end = new_label_name("cond_end")
            out += [IrInstr(IrInstrKind.JMP, lbl_end, None)]
            out += [IrInstr(IrInstrKind.LBL, lbl_false, None)]
            out += generate_stmt_ir(
                s.else_stmt, ir, cur_scope, scope_stack, stmt_gen_info
            )
            lbl_false = lbl_end
        out += [IrInstr(IrInstrKind.LBL, lbl_false, None)]
    elif isinstance(s, WhileStmt):
        while_cond_lbl = new_label_name("while_cond")
        while_loop_lbl = new_label_name("while_loop")
        while_break_usage = LabelUsage(new_label_name("while_break"))
        while_continue_usage = LabelUsage(while_cond_lbl)
        stmt_gen_info.break_stack.append(while_break_usage)
        stmt_gen_info.continue_stack.append(while_continue_usage)
        out += [IrInstr(IrInstrKind.JMP, while_cond_lbl, None)]
        out += [IrInstr(IrInstrKind.LBL, while_loop_lbl, None)]
        out += generate_stmt_ir(s.while_stmt, ir, cur_scope, scope_stack, stmt_gen_info)
        out += [IrInstr(IrInstrKind.LBL, while_cond_lbl, None)]
        out += generate_expr_ir(s.cond, ir, cur_scope, scope_stack)
        out += [IrInstr(IrInstrKind.JNZ, while_loop_lbl, None)]
        if while_break_usage.used:
            out += [IrInstr(IrInstrKind.LBL, while_break_usage.name, None)]
        stmt_gen_info.break_stack.pop()
        stmt_gen_info.continue_stack.pop()
    elif isinstance(s, DoStmt):
        do_loop_lbl = new_label_name("do_loop")
        do_break_usage = LabelUsage(new_label_name("do_break"))
        do_continue_usage = LabelUsage(new_label_name("do_continue"))
        stmt_gen_info.break_stack.append(do_break_usage)
        stmt_gen_info.continue_stack.append(do_continue_usage)
        out += [IrInstr(IrInstrKind.LBL, do_loop_lbl, None)]
        out += generate_stmt_ir(s.body, ir, cur_scope, scope_stack, stmt_gen_info)
        if do_continue_usage.used:
            out += [IrInstr(IrInstrKind.LBL, do_continue_usage.name, None)]
        out += generate_expr_ir(s.expr, ir, cur_scope, scope_stack)
        out += [IrInstr(IrInstrKind.JNZ, do_loop_lbl, None)]
        if do_break_usage.used:
            out += [IrInstr(IrInstrKind.LBL, do_break_usage.name, None)]
        stmt_gen_info.break_stack.pop()
        stmt_gen_info.continue_stack.pop()
    elif isinstance(s, BreakStmt):
        brk_usage = stmt_gen_info.break_stack[-1]
        brk_usage.used = True
        out += [IrInstr(IrInstrKind.JMP, brk_usage.name, None)]
    elif isinstance(s, ContinueStmt):
        cnt_usage = stmt_gen_info.continue_stack[-1]
        cnt_usage.used = True
        out += [IrInstr(IrInstrKind.JMP, cnt_usage.name, None)]
    elif isinstance(s, Expr):
        out = generate_expr_ir(s, ir, cur_scope, scope_stack)
        if s.ty is not None and s.ty != Type():
            out.append(IrInstr(IrInstrKind.DRP, None, None))
    elif isinstance(s, ReturnStmt):
        if s.ret_expr is None:
            out.append(IrInstr(IrInstrKind.RET, None, None))
        else:
            out += generate_expr_ir(s.ret_expr, ir, cur_scope, scope_stack)
            out.append(IrInstr(IrInstrKind.RTV, None, None))
    elif isinstance(s, DeclStmt):
        d = s.decl
        if isinstance(d, TypeDecl):
            return []
        if not isinstance(d, VarDecl):
            print(s)
            diag(
                s.get_range()[0],
                "Only var decl are supported for now",
                Diag.UNIMPLEMENTED,
                [s.get_range()],
            )
            assert False
        cur_scope[d.name] = len(stmt_gen_info.stack_frame)
        stmt_gen_info.stack_frame.append(
            StackFrameEntry(d.ty.get_size(), d.ty.get_align(), d.name)
        )
    else:
        print(s)
        diag(
            s.get_range()[0],
            f"Unimplemented: {s.__class__}",
            Diag.UNIMPLEMENTED,
            [s.get_range()],
        )
        assert False
    return out


def generate_function_ir(f: FnDecl, ir: FullIr):
    fn_scope = {}
    out_instrs = []
    stmt_gen_info = StmtGenInfo()
    params = []
    assert isinstance(f.ty, FunctionType)
    if len(f.param_decls) > 0:
        for i, p in enumerate(f.param_decls):
            fn_scope[p.name] = i
            stmt_gen_info.stack_frame.append(
                StackFrameEntry(p.ty.get_size(), p.ty.get_align(), p.name)
            )
            params.append(ParamEntry(p.ty.get_size(), False))  # is float
    scope_stack = [fn_scope]
    body_instrs = generate_stmt_ir(f.body, ir, {}, scope_stack, stmt_gen_info)
    out_instrs += body_instrs
    if len(out_instrs) == 0 or out_instrs[-1].opcode not in [
        IrInstrKind.RET,
        IrInstrKind.RTV,
    ]:
        if f.ty.return_type is not None and f.ty.return_type != Type():
            out_instrs.append(IrInstr(IrInstrKind.PSH, 0, 0))
            out_instrs.append(IrInstr(IrInstrKind.RTV, None, None))
        else:
            out_instrs.append(IrInstr(IrInstrKind.RET, None, None))
    returns_value = out_instrs[-1].opcode == IrInstrKind.RTV

    return FunctionIr(
        returns_value,
        out_instrs,
        stmt_gen_info.stack_frame,
        params,
        f.is_vararg,
        f.is_lib,
    )


def generate_lib_function_proto(f: FnDecl, ir: FullIr):
    _ = ir  # UNUSED
    assert f.is_lib, "lib function proto not marked as lib"
    assert isinstance(f.ty, FunctionType)
    returns_value = f.ty.return_type is not None and f.ty.return_type != Type()
    return FunctionIr(returns_value, [], [], [], f.is_vararg, f.is_lib)


def generate_ir(translation_unit: TranslationUnitDecl) -> FullIr:
    ir = FullIr()
    for d in translation_unit.decls:
        if isinstance(d, VarDecl):
            siz = d.ty.get_size()
            if siz % 8 != 0:
                siz += 8 - (siz % 8)
            ir.globs.append(IrGlobal(bytes([0] * siz), d.is_lib, False, d.name))
        if not isinstance(d, FnDecl):
            continue
        ir_name = d.name.replace(":", "_")
        if d.body is None:
            if d.is_lib:
                ir.functions[ir_name] = generate_lib_function_proto(d, ir)
            continue
        ir.functions[ir_name] = generate_function_ir(d, ir)
    return ir
