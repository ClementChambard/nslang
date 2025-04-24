from .data import (
    IrBlock,
    IrConstant,
    IrFunction,
    IrInstrBinOp,
    IrInstrBranch,
    IrInstrExpr,
    IrInstrRet,
    IrTranslationUnit,
    IrInstr,
    IrValue,
)


def print_ir_value_or_constant_notype(vorc: IrValue | IrConstant):
    if isinstance(vorc, IrValue):
        print(f"${vorc.name}", end="")
    else:
        print(vorc.value, end="")


def print_ir_value_or_constant(vorc: IrValue | IrConstant):
    print(vorc.ty, end=" ")
    print_ir_value_or_constant_notype(vorc)


def print_ir_instr_branch(i: IrInstrBranch):
    print("br ", end="")
    if i.branch_cond is not None:
        assert i.branch_else is not None
        print_ir_value_or_constant(i.branch_cond)
        print(
            f", label ${i.branch_to.name}, label ${i.branch_else.name}",
            end="",
        )
    else:
        print(f"label ${i.branch_to.name}", end="")


def print_ir_instr_ret(i: IrInstrRet):
    print("ret", end="")
    if i.result is not None:
        print(" ", end="")
        print_ir_value_or_constant(i.result)


def print_ir_instr_binop(i: IrInstrBinOp):
    ty = i.lhs.ty
    print(f"{i.kind.name.lower()} {ty} ", end="")
    print_ir_value_or_constant_notype(i.lhs)
    print(", ", end="")
    print_ir_value_or_constant_notype(i.rhs)


def print_ir_instr(i: IrInstr):
    if isinstance(i, IrInstrExpr) and i.result is not None:
        print(f"${i.result.name} = ", end="")
    if isinstance(i, IrInstrBranch):
        print_ir_instr_branch(i)
    if isinstance(i, IrInstrRet):
        print_ir_instr_ret(i)
    if isinstance(i, IrInstrBinOp):
        print_ir_instr_binop(i)


def print_ir_block(b: IrBlock):
    print(f"{b.name}:")
    for i in b.instrs:
        print("    ", end="")
        print_ir_instr(i)
        print()


def print_ir_function(f: IrFunction):
    define_declare = ["define", "declare"][len(f.blocks) == 0]
    print(
        f"{f.linkage.name.lower()} {define_declare} function {f.return_ty} @{f.name}(",
        end="",
    )
    for i, a in enumerate(f.args):
        if i != 0:
            print(", ", end="")
        print(f"{a.ty} ${a.name}", end="")
    if f.is_vararg:
        if len(f.args) > 0:
            print(", ...", end="")
        else:
            print("...", end="")
    print(")", end="")
    if len(f.blocks) < 0:
        print()
        return
    print(" {")
    for b in f.blocks:
        print_ir_block(b)
    print("}")


def print_ir(ir: IrTranslationUnit):
    for f in ir.functions:
        print_ir_function(f)
        print()
