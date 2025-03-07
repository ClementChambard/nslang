from typing import List
from . import IrInstr, IrInstrKind, FullIr


def optimize_push_then_op(fn_ir: List[IrInstr]) -> List[IrInstr]:
    out = []
    skip = False
    for i in range(len(fn_ir) - 1):
        if skip:
            skip = False
            continue
        if fn_ir[i].opcode == IrInstrKind.PSH and fn_ir[i+1].opcode == IrInstrKind.DRP:
            skip = True
            continue
        if fn_ir[i].opcode == IrInstrKind.PSH and fn_ir[i+1].opcode in [IrInstrKind.ADD, IrInstrKind.SUB, IrInstrKind.MUL, \
                                                                        IrInstrKind.DIV, IrInstrKind.REM, IrInstrKind.SHL, \
                                                                        IrInstrKind.SHR, IrInstrKind.AND, IrInstrKind.IOR, \
                                                                        IrInstrKind.XOR, IrInstrKind.LEQ, IrInstrKind.LTH, \
                                                                        IrInstrKind.GEQ, IrInstrKind.GTH, IrInstrKind.EQU, \
                                                                        IrInstrKind.NEQ, IrInstrKind.PTR]:
            if fn_ir[i].operand1 is None:
                op = fn_ir[i+1]
                op.operand1 = fn_ir[i].operand1
                op.operand2 = fn_ir[i].operand2
                out.append(op)
                skip = True
                continue
        out.append(fn_ir[i])
    if not skip:
        out.append(fn_ir[-1])
    return out

def optimize_ir(ir: FullIr) -> FullIr:
    # for k, v in ir.functions.items():
    #     v.instructions = optimize_push_then_op(v.instructions)
    return ir
