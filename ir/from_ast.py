from typing import List
from . import IrInstrKind, IrInstr, FunctionIr, IrGlobal, FullIr
from utils.diagnostic import diag, Diag

from ns_ast.nodes import *


LABEL_ID: int = 0

class LabelUsage:
    used: bool
    name: str

    def __init__(self, name: str):
        self.used = False
        self.name = name

class StmtGenInfo:
    stack_frame: []
    break_stack: []
    continue_stack: []

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

def generate_lvalue_addr(e: Expr, ir: FullIr, cur_scope, scope_stack) -> List[IrInstr]:
    if isinstance(e, ImplicitCastExpr):
        assert e.kind == CastKind.ARRAY_TO_POINTER_DECAY
        e = e.op
        assert isinstance(e, DeclRefExpr) and isinstance(e.ty, ArrayType)
        var_id = scope_lookup(cur_scope, scope_stack, e.decl.name)
        return [IrInstr(IrInstrKind.PSH, 2, var_id)]
    if e.value_kind == ValueKind.PRVALUE and isinstance(e.ty, PointerType):
        return generate_expr_ir(e, ir, cur_scope, scope_stack)
    # TEMP:
    if isinstance(e, DeclRefExpr) and isinstance(e.ty, PointerType):
        var_id = scope_lookup(cur_scope, scope_stack, e.decl.name)
        return [IrInstr(IrInstrKind.PSH, 1, var_id)]
    elif isinstance(e, DeclRefExpr):
        var_id = scope_lookup(cur_scope, scope_stack, e.decl.name)
        return [IrInstr(IrInstrKind.PSH, 2, var_id)]
    elif isinstance(e, MemberExpr):
        return generate_member_expr_addr(e, ir, cur_scope, scope_stack)
    print(e)
    assert False

def generate_array_subscript_addr(e: ArraySubscriptExpr, ir: FullIr, cur_scope, scope_stack) -> List[IrInstr]:
    out = []
    out += generate_lvalue_addr(e.lhs, ir, cur_scope, scope_stack)
    out += generate_expr_ir(e.rhs, ir, cur_scope, scope_stack)
    off_size = e.lhs.ty.subtype.get_size()
    out += [IrInstr(IrInstrKind.MUL, 0, off_size)]
    out += [IrInstr(IrInstrKind.ADD, None, None)]
    return out

def generate_member_expr_addr(e: MemberExpr, ir: FullIr, cur_scope, scope_stack) -> List[IrInstr]:
    out = []
    out += generate_lvalue_addr(e.base, ir, cur_scope, scope_stack)
    out += [IrInstr(IrInstrKind.ADD, 0, e.field_offset)]
    return out

def generate_addrof_ir(e: Expr, ir: FullIr, cur_scope, scope_stack) -> List[IrInstr]:
    if isinstance(e, ArraySubscriptExpr):
        return generate_array_subscript_addr(e, ir, cur_scope, scope_stack)
    elif isinstance(e, MemberExpr):
        return generate_member_expr_addr(e, ir, cur_scope, scope_stack)
    elif isinstance(e, DeclRefExpr):
        var_id = scope_lookup(cur_scope, scope_stack, e.decl.name)
        return [IrInstr(IrInstrKind.PSH, 2, var_id)]
    else:
        assert False

def generate_assignment(e, ir: FullIr, cur_scope, scope_stack) -> List[IrInstr]:
    out = []
    if isinstance(e.lhs, DeclRefExpr):

        if (g := ir.has_global(e.lhs.decl.name)) >= 0:
            out += generate_expr_ir(e.rhs, ir, cur_scope, scope_stack)
            out += [IrInstr(IrInstrKind.PSH, 3, g)]
            out += [IrInstr(IrInstrKind.STA, 8, None)]
            out += [IrInstr(IrInstrKind.PSH, 0, 0)]
        else:
            var_id = scope_lookup(cur_scope, scope_stack, e.lhs.decl.name)
            out += generate_expr_ir(e.rhs, ir, cur_scope, scope_stack)
            out += [IrInstr(IrInstrKind.STV, var_id, None)]
            out += [IrInstr(IrInstrKind.PSH, 1, var_id)]
    elif isinstance(e.lhs, MemberExpr) or isinstance(e.lhs, ArraySubscriptExpr):
        out += generate_expr_ir(e.rhs, ir, cur_scope, scope_stack)
        out += generate_addrof_ir(e.lhs, ir, cur_scope, scope_stack)
        store_size = e.lhs.ty.get_size()
        out += [IrInstr(IrInstrKind.STA, store_size, None)]
        out += [IrInstr(IrInstrKind.PSH, 0, 0)] # TODO: push actual result
    elif isinstance(e.lhs, UnaryExpr) and e.lhs.opc == UnaryOperatorKind.DEREF:
        out += generate_expr_ir(e.rhs, ir, cur_scope, scope_stack)
        out += generate_expr_ir(e.lhs.arg, ir, cur_scope, scope_stack)
        store_size = e.lhs.ty.get_size()
        out += [IrInstr(IrInstrKind.STA, store_size, None)]
        out += [IrInstr(IrInstrKind.PSH, 0, 0)] # TODO: push actual result
    else:
        assert False, "assign only on var for now"
    return out

def generate_expr_ir(e, ir: FullIr, cur_scope, scope_stack) -> List[IrInstr]:
    if isinstance(e, DeclRefExpr):
        if isinstance(e.decl, EnumVariantDecl):
            return [IrInstr(IrInstrKind.PSH, 0, e.decl.val)]
        print(e)
        diag(e.get_range()[0], "ASSERT FALSE", Diag.ERROR)
        assert False, "Raw DeclRefExpr encountered in ast"
    if isinstance(e, MemberExpr):
        print(e)
        diag(e.get_range()[0], "ASSERT FALSE", Diag.ERROR)
        assert False, "Raw MemberExpr encountered in ast"
    elif isinstance(e, BuiltinExpr):
        out = []
        for a in e.args[::-1]:
            out += generate_expr_ir(a, ir, cur_scope, scope_stack)
        return out + [IrInstr(IrInstrKind.BUI, e.builtin_name, len(e.args))]
    elif isinstance(e, IntegerLiteral):
        return [IrInstr(IrInstrKind.PSH, 0, e.value)]
    elif isinstance(e, UnaryExpr):
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
    elif isinstance(e, BinaryExpr):
        if e.opc == BinaryOperatorKind.ASSIGN:
            return generate_assignment(e, ir, cur_scope, scope_stack)
        out = []
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
                assert False, e.opc
        return out
    elif isinstance(e, CompoundAssignExpr):
        assert isinstance(e.lhs, DeclRefExpr), "compound assign only on var for now"
        var_id = scope_lookup(cur_scope, scope_stack, e.lhs.decl.name)
        out = []
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
                assert False, e.opc
        out += [IrInstr(IrInstrKind.STV, var_id, None)]
        out += [IrInstr(IrInstrKind.PSH, 1, var_id)]
        return out
    elif isinstance(e, SizeofExpr):
        val = e.ty_of_sizeof.get_size()
        return [IrInstr(IrInstrKind.PSH, 0, val)]
    # elif isinstance(e, AlignofExpr):
    #     val = e.ty_of_sizeof.get_align()
    #     return [IrInstr(IrInstrKind.PSH, 0, val)]
    elif isinstance(e, CastExpr):
        if e.kind == CastKind.NOOP:
            return generate_expr_ir(e.op, ir, cur_scope, scope_stack)
        if e.kind == CastKind.INTEGRAL_TO_BOOLEAN or e.kind == CastKind.POINTER_TO_BOOLEAN:
            # DO NOTHING TO CONVERT TO BOOLEAN
            return generate_expr_ir(e.op, ir, cur_scope, scope_stack)
        if e.kind == CastKind.ARRAY_TO_POINTER_DECAY:
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
            assert False, "TODO: ARRAY_TO_POINTER_DECAY"
        if e.kind == CastKind.LVALUE_TO_RVALUE:
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
            else:
                diag(e.get_range()[0], "ASSERT FALSE", Diag.ERROR)
                assert False, "TODO: LVALUE_TO_RVALUE"
        if e.kind == CastKind.INTEGRAL_CAST:
            # TODO: what to do ? for now nothing in IR
            return generate_expr_ir(e.op, ir, cur_scope, scope_stack)
        assert False, "TODO: other cast not implemented"
    elif isinstance(e, ArraySubscriptExpr):
        assert False, "TODO: implement array subscript"
    elif isinstance(e, CallExpr):
        out = []
        for a in e.args[::-1]:
            out += generate_expr_ir(a, ir, cur_scope, scope_stack)
        assert isinstance(e.fn, DeclRefExpr) and isinstance(e.fn.decl, FnDecl)
        name = e.fn.decl.name
        return out + [IrInstr(IrInstrKind.CAL, name, len(e.args))]
    else:
        assert False, e.__class__


def generate_stmt_ir(s: Stmt, ir: FullIr, cur_scope, scope_stack, stmt_gen_info) -> List[IrInstr]:
    out = []
    if isinstance(s, CompoundStmt):
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
            out += generate_stmt_ir(s.else_stmt, ir, cur_scope, scope_stack, stmt_gen_info)
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
        assert isinstance(d, VarDecl), "Only var decl are supported for now"
        cur_scope[d.name] = len(stmt_gen_info.stack_frame)
        stmt_gen_info.stack_frame.append(d.ty.get_size())
    else:
        assert False, s.__class__
    return out


def generate_function_ir(f: FnDecl, ir: FullIr):
    fn_scope = {}
    out_instrs = []
    stmt_gen_info = StmtGenInfo()
    stack_frame = []
    if len(f.param_decls) > 0:
        out_instrs.append(IrInstr(IrInstrKind.PRM, len(f.param_decls), None))
        for i, p in enumerate(f.param_decls):
            fn_scope[p.name] = i
            stmt_gen_info.stack_frame.append(p.ty.get_size())
    scope_stack = [fn_scope]
    body_instrs = generate_stmt_ir(f.body, ir, {}, scope_stack, stmt_gen_info)
    out_instrs += body_instrs
    if len(out_instrs) == 0 or out_instrs[-1].opcode not in [IrInstrKind.RET, IrInstrKind.RTV]:
        if f.ty.return_type is not None and f.ty.return_type != Type():
            out_instrs.append(IrInstr(IrInstrKind.PSH, 0, 0))
            out_instrs.append(IrInstr(IrInstrKind.RTV, None, None))
        else:
            out_instrs.append(IrInstr(IrInstrKind.RET, None, None))
    returns_value = out_instrs[-1].opcode == IrInstrKind.RTV

    return FunctionIr(returns_value, out_instrs, stmt_gen_info.stack_frame, f.is_lib)


def generate_lib_function_proto(f: FnDecl, ir: FullIr):
    assert f.is_lib
    returns_value = f.ty.return_type is not None and f.ty.return_type != Type()
    return FunctionIr(returns_value, [], [], f.is_lib)


def generate_ir(ast) -> FullIr:
    ir = FullIr()
    for d in ast:
        if isinstance(d, VarDecl):
            ir.globs.append(IrGlobal(bytes([0] * d.ty.get_size()), d.is_lib, False, d.name))
        if not isinstance(d, FnDecl):
            continue
        if d.body is None:
            if d.is_lib:
                ir.functions[d.name] = generate_lib_function_proto(d, ir)
            continue
        ir.functions[d.name] = generate_function_ir(d, ir)
    return ir
