from typing import Tuple

from codegen.x86_64.x86_ir import (
    X86_Body,
    X86_Instr,
    X86_InstrKind,
    X86_LabelPoint,
    X86_OperandKind,
    X86_Register,
)


def optimize_simple_instrs(code: X86_Body) -> Tuple[X86_Body, bool]:
    changed = False
    new_code = []
    for i in code:
        if isinstance(i, X86_LabelPoint):
            new_code.append(i)
            continue
        if i.kind == X86_InstrKind.MOV:
            if i.operands[0].kind == X86_OperandKind.REGISTER:
                lreg = i.operands[0].value
                assert isinstance(lreg, X86_Register)
                if i.operands[1].kind == X86_OperandKind.REGISTER:
                    rreg = i.operands[1].value
                    assert isinstance(rreg, X86_Register)
                    if rreg.kind == lreg.kind:
                        # move to itself: ignore
                        changed = True
                        continue
                elif i.operands[1].kind == X86_OperandKind.IMMEDIATE:
                    imm = i.operands[1].value
                    assert isinstance(imm, int)
                    if imm == 0:
                        # move zero: xor
                        changed = True
                        new_code.append(X86_Instr("xor", lreg, lreg))
                        continue
        if i.kind == X86_InstrKind.ADD or i.kind == X86_InstrKind.SUB:
            if i.operands[1].kind == X86_OperandKind.IMMEDIATE:
                imm = i.operands[1].value
                assert isinstance(imm, int)
                if imm == 0:
                    # add/sub 0: ignore
                    changed = True
                    continue
                if imm == 1 or imm == -1:
                    # add/sub 1/-1: inc/dec
                    ins_kind = (
                        "inc"
                        if imm == 1
                        and i.kind == X86_InstrKind.ADD
                        or imm == -1
                        and i.kind == X86_InstrKind.SUB
                        else "dec"
                    )
                    new_code.append(X86_Instr(ins_kind, i.operands[0]))
                    changed = True
                    continue
        if i.kind == X86_InstrKind.IMUL:
            if i.operands[1].kind == X86_OperandKind.IMMEDIATE:
                imm = i.operands[1].value
                assert isinstance(imm, int)
                if imm == 0:
                    new_code.append(X86_Instr("mov", i.operands[0], 0))
                    changed = True
                    continue
                if imm == 1:
                    changed = True
                    continue
                if imm == -1:
                    new_code.append(X86_Instr("neg", i.operands[0]))
                    changed = True
                    continue
                # if imm is power of 2 => use shl
        new_code.append(i)
    return new_code, changed


def optimize_push_pop_sequences(code: X86_Body) -> Tuple[X86_Body, bool]:
    changed = False
    new_code = []
    in_push_seq = 0
    push_seq = []
    for i in code:
        if isinstance(i, X86_LabelPoint):
            if in_push_seq > 0:
                new_code += push_seq
                push_seq = []
                in_push_seq = 0
            new_code.append(i)
            continue
        if i.kind == X86_InstrKind.PUSH:
            in_push_seq += 1
            push_seq.append(i)
        elif i.kind == X86_InstrKind.POP and in_push_seq > 0:
            in_push_seq -= 1
            matching_push = push_seq.pop()
            assert (
                len(i.operands) == 1 and i.operands[0].kind == X86_OperandKind.REGISTER
            )
            assert len(matching_push.operands) == 1
            changed = True
            new_code.append(X86_Instr("mov", i.operands[0], matching_push.operands[0]))
        else:
            if in_push_seq > 0:
                new_code += push_seq
                push_seq = []
                in_push_seq = 0
            new_code.append(i)
    if in_push_seq > 0:
        new_code += push_seq
    return new_code, changed


def optimize_function_code(code: X86_Body):
    changed = True
    while changed:
        changed = False

        code, c = optimize_simple_instrs(code)
        changed = changed or c

        # TODO: This creates segfaults
        # code, c = optimize_push_pop_sequences(code)
        # changed = changed or c
    return code
