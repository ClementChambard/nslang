from ir2.data.instrs._base import Instr
from ir2.data.instrs.term import RetInstr, TermInstr
from ir2.data.prog import Block, Function
from ir2.data.types import IrType, ir2_get_type
from ir2.data.value import IrValue
from ns_ast.nodes.decl import FnDecl, ValueDecl
from ns_ast.nodes.types import FunctionType


cur_insert_block: Block
cur_function: Function
decl_value_assoc = dict()
tmp_value_nums = dict()


def get_cur_fn() -> Function:
    global cur_function
    return cur_function


def tmp_value_name(n: str = "") -> str:
    global tmp_value_nums
    if n not in tmp_value_nums.keys():
        if n == "":
            tmp_value_nums[n] = 1
            return "0"
        tmp_value_nums[n] = 0
        return n
    out = n + str(tmp_value_nums[n])
    tmp_value_nums[n] += 1
    return out


def make_tmp_value(ty: IrType, name="") -> IrValue:
    return IrValue(ty, tmp_value_name(name))


def ir2_get_value_for_decl(d: ValueDecl):
    global decl_value_assoc
    return decl_value_assoc[d.name]


def ir2_set_value_for_decl(d: ValueDecl, v: IrValue):
    global decl_value_assoc
    decl_value_assoc[d.name] = v


def ir2_insert(i: Instr):
    if cur_insert_block.is_terminated:
        return
    i.parent = cur_insert_block
    cur_insert_block.instructions.append(i)
    return i


def ir2_insert_b(b: Block):
    global cur_insert_block, cur_function
    cur_function.blocks.append(b)
    cur_insert_block = b


def start_function(d: FnDecl):
    global decl_value_assoc, cur_insert_block, cur_function, tmp_value_nums
    assert isinstance(d.ty, FunctionType)
    tmp_value_nums = dict()
    decl_value_assoc = dict()
    cur_function = Function(
        d.name,
        ir2_get_type(d.ty.return_type),
        d.is_lib,
        d.is_vararg,
    )
    cur_function.arg_values = [
        IrValue(ir2_get_type(p_d.ty), p_d.name) for p_d in d.param_decls
    ]
    first_block = Block(tmp_value_name("start"))
    cur_insert_block = first_block
    cur_function.blocks.append(first_block)
    return cur_function


def end_function(_: FnDecl):
    if not cur_insert_block.is_terminated:
        ir2_insert(RetInstr(None))
