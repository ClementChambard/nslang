from ir2.gen_state import (
    ir2_insert,
    ir2_set_value_for_decl,
    start_function,
    end_function,
)
from ir2.gen_stmt import ir2_stmt
from ir2.data.instrs.memory import LocalVarInstr, StoreInstr
from ir2.data.prog import Function
from ir2.data.types import ir2_get_type
from ns_ast.nodes.decl import FnDecl


def move_locals_to_top(f: Function):
    local_vars = []
    for b in f.blocks:
        ins = []
        for i in b.instructions:
            if isinstance(i, LocalVarInstr):
                local_vars.append(i)
            else:
                ins.append(i)
        b.instructions = ins
    f.first_block.instructions = local_vars + f.first_block.instructions


def ir2_func(d: FnDecl) -> Function:
    global cur_insert_block, decl_value_assoc, cur_function
    f = start_function(d)
    for p_d, fp_d in zip(d.param_decls, f.arg_values):
        v = LocalVarInstr(f"{p_d.name}.local", fp_d.ty)
        ir2_set_value_for_decl(p_d, v)
        ir2_insert(v)
        ir2_insert(StoreInstr(v.ty, v, fp_d))
    ir2_stmt(d.body)
    move_locals_to_top(f)
    end_function(d)
    return f
