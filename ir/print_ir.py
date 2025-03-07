from . import IrInstr, IrInstrKind


def print_special_arg(op1, op2):
    if op1 is None:
        return
    if op1 == 0:
        print(f"    {op2}", end = "")
    if op1 == 1:
        print(f"    Var[{op2}]", end = "")
    if op1 == 2:
        print(f"    [{op2}]", end = "")
    if op1 == 3:
        print(f"    [global_{op2}]", end = "")


def print_ir_instr(i: IrInstr):
    if i.opcode == IrInstrKind.LBL:
        print(f" .{i.operand1}:")
        return
    print(f"    {i.opcode.name}", end = "")
    operand_count = 0
    match i.opcode:
        case IrInstrKind.ADD | IrInstrKind.SUB | IrInstrKind.PSH | IrInstrKind.MUL | IrInstrKind.DIV |  \
             IrInstrKind.REM | IrInstrKind.SHL | IrInstrKind.SHR | IrInstrKind.AND | IrInstrKind.IOR | IrInstrKind.XOR:
            print_special_arg(i.operand1, i.operand2)
        case IrInstrKind.PRM | IrInstrKind.STV | IrInstrKind.LBL | IrInstrKind.JMP | IrInstrKind.JZO | IrInstrKind.JNZ:
            operand_count = 1
        case IrInstrKind.CAL | IrInstrKind.BUI:
            operand_count = 2
        case _:
            operand_count = 0
    if operand_count > 0:
        print(f"    {i.operand1}", end = "")
    if operand_count > 1:
        print(f", {i.operand2}", end = "")
    print()


def print_ir(ir: dict):
    for key, value in ir.functions.items():
        if len(value.instructions) == 0:
            continue
        print(f"{key}:")
        for i in value.instructions:
            print_ir_instr(i)
    for i, g in enumerate(ir.globs):
        print(f"global_{i}:")
        print(f"    {g.data}")
