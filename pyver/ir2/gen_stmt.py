from ir2.gen_state import (
    ir2_insert,
    ir2_insert_b,
    ir2_set_value_for_decl,
    tmp_value_name,
)
from ir2.data.instrs.term import BranchInstr, RetInstr
from ir2.data.instrs.memory import LocalVarInstr
from ir2.data.prog import Block
from ir2.data.types import ir2_get_type
from ns_ast.nodes.decl import VarDecl
from ns_ast.nodes.expr import Expr
from ns_ast.nodes.stmt import (
    BreakStmt,
    CompoundStmt,
    ContinueStmt,
    DeclStmt,
    DoStmt,
    IfStmt,
    NullStmt,
    ReturnStmt,
    Stmt,
    WhileStmt,
)

break_stack = []
continue_stack = []


def ir2_compound_stmt(s: CompoundStmt):
    for sub in s.inner:
        ir2_stmt(sub)


def ir2_decl_stmt(s: DeclStmt):
    d = s.decl
    assert isinstance(d, VarDecl), "unimplemented"
    v = LocalVarInstr(d.name, ir2_get_type(d.ty))
    ir2_set_value_for_decl(d, v)
    ir2_insert(v)


def ir2_return_stmt(s: ReturnStmt):
    from ir2.gen_expr import ir2_expr

    if s.ret_expr is None:
        ir2_insert(RetInstr(None))
    else:
        v = ir2_expr(s.ret_expr)
        ir2_insert(RetInstr(v))


def ir2_if_stmt(s: IfStmt):
    from ir2.gen_expr import ir2_expr

    cond_var = ir2_expr(s.cond)
    block_if = Block(tmp_value_name("if.then"))
    block_end = Block(tmp_value_name("if.end"))
    block_false: Block
    if s.else_stmt is None:
        block_false = block_end
    else:
        block_false = Block(tmp_value_name("if.else"))
    ir2_insert(BranchInstr(block_if, block_false, cond_var))
    ir2_insert_b(block_if)
    ir2_stmt(s.then_stmt)
    ir2_insert(BranchInstr(block_end))
    if s.else_stmt is not None:
        ir2_insert_b(block_false)
        ir2_stmt(s.else_stmt)
        ir2_insert(BranchInstr(block_end))
    ir2_insert_b(block_end)


def ir2_do_stmt(s: DoStmt):
    from ir2.gen_expr import ir2_expr

    block_body = Block(tmp_value_name("do.body"))
    block_cond = Block(tmp_value_name("do.cond"))
    block_end = Block(tmp_value_name("do.end"))
    ir2_insert(BranchInstr(block_body))
    break_stack.append(block_end)
    continue_stack.append(block_cond)
    ir2_insert_b(block_body)
    ir2_stmt(s.body)
    break_stack.pop()
    continue_stack.pop()
    ir2_insert(BranchInstr(block_cond))
    ir2_insert_b(block_cond)
    cond_var = ir2_expr(s.expr)
    ir2_insert(BranchInstr(block_body, block_end, cond_var))
    ir2_insert_b(block_end)


def ir2_while_stmt(s: WhileStmt):
    from ir2.gen_expr import ir2_expr

    block_cond = Block(tmp_value_name("while.cond"))
    block_body = Block(tmp_value_name("while.body"))
    block_end = Block(tmp_value_name("while.end"))
    ir2_insert(BranchInstr(block_cond))

    ir2_insert_b(block_cond)
    break_stack.append(block_end)
    continue_stack.append(block_cond)
    cond_var = ir2_expr(s.cond)
    ir2_insert(BranchInstr(block_body, block_end, cond_var))

    ir2_insert_b(block_body)
    ir2_stmt(s.while_stmt)
    ir2_insert(BranchInstr(block_cond))
    break_stack.pop()
    continue_stack.pop()

    ir2_insert_b(block_end)


def ir2_break_stmt(_: BreakStmt):
    assert len(break_stack) > 0
    ir2_insert(BranchInstr(break_stack[-1]))


def ir2_continue_stmt(_: ContinueStmt):
    assert len(continue_stack) > 0
    ir2_insert(BranchInstr(continue_stack[-1]))


def ir2_stmt(s: Stmt):
    from ir2.gen_expr import ir2_expr

    if isinstance(s, NullStmt):
        pass
    elif isinstance(s, CompoundStmt):
        ir2_compound_stmt(s)
    elif isinstance(s, DeclStmt):
        ir2_decl_stmt(s)
    elif isinstance(s, ReturnStmt):
        ir2_return_stmt(s)
    elif isinstance(s, IfStmt):
        ir2_if_stmt(s)
    elif isinstance(s, DoStmt):
        ir2_do_stmt(s)
    elif isinstance(s, WhileStmt):
        ir2_while_stmt(s)
    elif isinstance(s, BreakStmt):
        ir2_break_stmt(s)
    elif isinstance(s, ContinueStmt):
        ir2_continue_stmt(s)
    elif isinstance(s, Expr):
        ir2_expr(s)
    else:
        assert False, f"unimplemented {s.__class__}"
