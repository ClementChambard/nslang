from typing import List, Tuple
from . import IrInstr, IrInstrKind, FullIr, FunctionIr


def optimize_push_op(fn_ir: List[IrInstr]) -> Tuple[List[IrInstr], bool]:
    out = []
    skip = False
    changed = False
    for i in range(len(fn_ir) - 1):
        if skip:
            skip = False
            continue
        if fn_ir[i].opcode == IrInstrKind.PSH and fn_ir[i+1].opcode in [IrInstrKind.ADD, IrInstrKind.SUB, IrInstrKind.MUL, \
                                                                        IrInstrKind.DIV, IrInstrKind.REM, IrInstrKind.SHL, \
                                                                        IrInstrKind.SHR, IrInstrKind.AND, IrInstrKind.IOR, \
                                                                        IrInstrKind.XOR, IrInstrKind.LEQ, IrInstrKind.LTH, \
                                                                        IrInstrKind.GEQ, IrInstrKind.GTH, IrInstrKind.EQU, \
                                                                        IrInstrKind.NEQ]:
            if fn_ir[i+1].operand1 is None and fn_ir[i].operand1 < 3:
                op = fn_ir[i+1]
                op.operand1 = fn_ir[i].operand1
                op.operand2 = fn_ir[i].operand2
                out.append(op)
                skip = True
                changed = True
                continue
        out.append(fn_ir[i])
    if not skip:
        out.append(fn_ir[-1])
    return out, changed

def optimize_push_drop(fn_ir: List[IrInstr]) -> Tuple[List[IrInstr], bool]:
    out = []
    skip = False
    changed = False
    for i in range(len(fn_ir) - 1):
        if skip:
            skip = False
            continue
        if fn_ir[i].opcode == IrInstrKind.PSH and fn_ir[i+1].opcode == IrInstrKind.DRP:
            skip = True
            changed = True
            continue
        out.append(fn_ir[i])
    if not skip:
        out.append(fn_ir[-1])
    return out, changed

def optimize_nooplike_ops(fn_ir: List[IrInstr]) -> Tuple[List[IrInstr], bool]:
    out = []
    changed = False
    for i in fn_ir:
        if i.opcode in [IrInstrKind.ADD, IrInstrKind.SUB, IrInstrKind.SHL, \
                        IrInstrKind.SHR, IrInstrKind.IOR, IrInstrKind.XOR]:
            if i.operand1 is not None and i.operand1 == 0: # value
                if i.operand2 == 0: # skip operations with 0
                    changed = True
                    continue
        if i.opcode in [IrInstrKind.MUL, IrInstrKind.DIV]:
            if i.operand1 is not None and i.operand1 == 0: # value
                if i.operand2 == 1: # skip operations with 1
                    changed = True
                    continue
        out.append(i)
    return out, changed

def optimize_function_ir(ir: FunctionIr):
    if len(ir.instructions) == 0:
        return
    changed = True
    while changed:
        changed = False
        ir.instructions, c = optimize_push_drop(ir.instructions)
        changed = changed or c
        ir.instructions, _ = optimize_nooplike_ops(ir.instructions)
        changed = changed or c
        ir.instructions, _ = optimize_push_op(ir.instructions)
        changed = changed or c


def optimize_ir(ir: FullIr) -> FullIr:
    for _, v in ir.functions.items():
        optimize_function_ir(v)
    return ir
