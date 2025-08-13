from typing import Any, List, Tuple
from .x86_ir import (
    X86_Address,
    X86_Body,
    X86_Instr,
    X86_Label,
    X86_LabelPoint,
    X86_Memory,
    X86_Register,
)
from ir import IrInstrKind, IrInstr, FunctionIr, FullIr
from dataclasses import dataclass
from .optimize import optimize_function_code
from .abi import Abi

from ir.data import ParamEntry, StackFrameEntry

CUR_LABEL_ID = 0


def create_label(name):
    global CUR_LABEL_ID
    a = f".{name}_{CUR_LABEL_ID}"
    CUR_LABEL_ID += 1
    return X86_Label(a)


@dataclass
class StackVariables:
    variable_offsets: List[int]
    variable_sizes: List[int]
    variable_names: List[str]

    vararg_cur_int_loc: int
    vararg_cur_float_loc: int
    vararg_int_loc: int
    vararg_float_loc: int
    vararg_stack_loc: int

    stack_size: int

    def __init__(self, stack_frame: List[StackFrameEntry], is_vararg: bool):
        self.stack_size = 0
        if is_vararg:
            self.vararg_stack_loc = self.stack_size + 8
            self.stack_size += 8
            self.vararg_cur_int_loc = self.stack_size + 8
            self.stack_size += 8
            self.vararg_int_loc = self.stack_size + 8 * 6
            self.stack_size += 8 * 6
            # TODO: float
            # self.vararg_cur_float_loc = -self.stack_size - 8
            # self.stack_size += 8
            # self.vararg_float_loc = -self.stack_size - 16 * 7
            # self.stack_size += 16 * 7

        self.variable_offsets = []
        self.variable_sizes = []
        self.variable_names = []
        for item in stack_frame:
            if self.stack_size % item.align != 0:
                self.stack_size = (
                    self.stack_size - self.stack_size % item.align + item.align
                )
            self.variable_sizes.append(item.size)
            self.variable_offsets.append(-self.stack_size - item.size)
            self.variable_names.append(item.name)
            self.stack_size += item.size
        if self.stack_size % 16 != 0:  # For some reason, the stack must be 16B aligned
            self.stack_size = self.stack_size - self.stack_size % 16 + 16


CUR_STACK_VARS: StackVariables = StackVariables([], False)


def var_size(var_id: int):
    return CUR_STACK_VARS.variable_sizes[var_id]


def get_variable_address(var_id: int, stack_param: bool = False) -> X86_Address:
    i = 0
    if stack_param:
        i = 16 + var_id * 8
    else:
        i = CUR_STACK_VARS.variable_offsets[var_id]
    return X86_Address(X86_Register.get("rbp"), offset=i)


def get_variable_memory(
    var_id: int, stack_param: bool = False, size: int | None = None
) -> X86_Memory:
    if size is None:
        size = var_size(var_id)
    return X86_Memory(size, get_variable_address(var_id, stack_param))


def fn_params(params: List[ParamEntry]) -> Tuple[X86_Body, int, int, int]:
    out = []
    int_param_i = 0
    float_param_i = 0
    stack_param_i = 0
    for i, p in enumerate(params):
        if p.is_float:
            # for floats: xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6, xmm7, then stack
            # after: stack
            assert False, "float not implemented"
        arg_memory = get_variable_memory(i, size=p.size)
        if int_param_i < Abi.PARAM_INT_MAX_REGISTER:
            param_reg = Abi.param_int_register(int_param_i, size=p.size)
            out.append(X86_Instr("mov", arg_memory, param_reg))
            int_param_i += 1
        else:
            arg_stack_loc = get_variable_memory(stack_param_i, True, size=8)
            out.append(X86_Instr("mov", "rdi", arg_stack_loc))
            out.append(X86_Instr("mov", arg_memory, "rdi"))
            stack_param_i += 1
    return out, int_param_i, float_param_i, stack_param_i


def fn_start(f: FunctionIr) -> X86_Body:
    global CUR_STACK_VARS
    CUR_STACK_VARS = StackVariables(f.stack_frame, f.is_vararg)
    out = []
    out.append(X86_Instr("push", "rbp"))
    out.append(X86_Instr("mov", "rbp", "rsp"))
    if CUR_STACK_VARS.stack_size > 0:
        out.append(X86_Instr("sub", "rsp", CUR_STACK_VARS.stack_size))
    int_i = 0
    float_i = 0
    stack_i = 0
    if len(f.params) != 0:
        a, int_i, float_i, stack_i = fn_params(f.params)
        out += a
    if f.is_vararg:
        for i in range(6):
            register = Abi.param_int_register(i)
            out.append(
                X86_Instr(
                    "mov",
                    f"QWORD[rbp-{CUR_STACK_VARS.vararg_int_loc - 8 * i}]",
                    register,
                )
            )
        # TODO: same for floats
        out.append(X86_Instr("mov", "rdi", int_i * 8))
        out.append(
            X86_Instr("mov", f"QWORD[rbp-{CUR_STACK_VARS.vararg_cur_int_loc}]", "rdi")
        )
        out.append(X86_Instr("lea", "rdi", get_variable_address(stack_i, True)))
        out.append(
            X86_Instr("mov", f"QWORD[rbp-{CUR_STACK_VARS.vararg_stack_loc}]", "rdi")
        )
    return out


def value_get(operand1, operand2, operand_bits=64) -> Tuple[X86_Body, Any]:
    reg = X86_Register.get("rsi")
    op_reg = reg
    if operand_bits == 8:
        reg = X86_Register.get("rcx")
        op_reg = reg.with_size(1)
    if operand1 is None:
        return ([X86_Instr("pop", reg)], op_reg)
    elif operand1 == 0:
        return ([], operand2)
    elif operand1 == 1:
        if operand_bits == 8:
            return ([X86_Instr("mov", op_reg, get_variable_memory(operand2, size=1))], op_reg)
        return ([], get_variable_memory(operand2, size=operand_bits//8))
    elif operand1 == 2:
        return ([X86_Instr("lea", "rsi", get_variable_address(operand2))], op_reg)
    else:
        assert False


COMMENT_IR_IN_ASM = False


def instr_codegen(ins: IrInstr, ir: FullIr) -> X86_Body:
    out = []
    match ins.opcode:
        case IrInstrKind.STV:
            var_id = ins.operand1
            size = var_size(var_id)
            store_reg = X86_Register(size, "di")
            store_to = get_variable_memory(var_id, size=size)
            out.append(X86_Instr("pop", "rdi"))
            out.append(X86_Instr("mov", store_to, store_reg))
        case IrInstrKind.STA:
            size = ins.operand1
            addr_reg = X86_Register.get("rdi")
            data_reg = X86_Register.get("rsi")
            store_to = X86_Memory(size, addr_reg)
            out.append(X86_Instr("pop", addr_reg))
            out.append(X86_Instr("pop", data_reg))
            out.append(X86_Instr("mov", store_to, data_reg.with_size(size)))
        case IrInstrKind.LDA:
            size = ins.operand1
            addr_reg = X86_Register.get("rdi")
            data_reg = X86_Register.get("rsi")
            load_from = X86_Memory(size, addr_reg)
            if size != 8:
                out.append(X86_Instr("mov", data_reg, 0))
            out.append(X86_Instr("pop", addr_reg))
            out.append(X86_Instr("mov", data_reg.with_size(size), load_from))
            out.append(X86_Instr("push", data_reg))
        case IrInstrKind.PSH:
            if ins.operand1 == 0:
                out.append(X86_Instr("push", int(ins.operand2)))
            elif ins.operand1 == 1:
                size = var_size(ins.operand2)
                reg = X86_Register.get("rdi")
                if size != 8:
                    out.append(X86_Instr("mov", "rdi", 0))
                    out.append(
                        X86_Instr(
                            "mov",
                            reg.with_size(size),
                            get_variable_memory(ins.operand2, size=size),
                        )
                    )
                    out.append(X86_Instr("push", reg))
                else:
                    out.append(
                        X86_Instr("push", get_variable_memory(ins.operand2, size=size))
                    )
            elif ins.operand1 == 2:
                out.append(X86_Instr("lea", "rdi", get_variable_address(ins.operand2)))
                out.append(X86_Instr("push", "rdi"))
            elif ins.operand1 == 3:
                out.append(X86_Instr("lea", "rdi", f"[global_{ins.operand2}]"))
                out.append(X86_Instr("push", "rdi"))
            elif ins.operand1 == 4:
                # TODO: multiple sizes
                out.append(X86_Instr("push", f"QWORD[global_{ins.operand2}]"))
            else:
                assert False
        case IrInstrKind.DUP:
            out.append(X86_Instr("mov", "rdi", "QWORD[rsp]"))
            out.append(X86_Instr("push", "rdi"))
        case IrInstrKind.VAA:
            label_else = create_label("vaelse")
            label_end = create_label("vaend")
            non_stack_i = CUR_STACK_VARS.vararg_cur_int_loc
            non_stack_loc = CUR_STACK_VARS.vararg_int_loc
            non_stack_increment = 8
            if ins.operand1:
                assert False, "TODO: is_float = True"
                non_stack_i = CUR_STACK_VARS.vararg_cur_float_loc
                non_stack_loc = CUR_STACK_VARS.vararg_float_loc
                non_stack_increment = 8
            out.append(X86_Instr("mov", "rax", f"QWORD[rbp-{non_stack_i}]"))
            out.append(X86_Instr("cmp", "rax", 47))
            out.append(X86_Instr("ja", label_else))
            out.append(X86_Instr("lea", "rdi", f"[rbp-{non_stack_loc}]"))
            out.append(X86_Instr("add", "rdi", "rax"))
            out.append(X86_Instr("add", "rax", non_stack_increment))
            out.append(X86_Instr("mov", f"QWORD[rbp-{non_stack_i}]", "rax"))
            out.append(X86_Instr("jmp", label_end))
            out.append(X86_LabelPoint(label_else))
            out.append(
                X86_Instr("mov", "rdi", f"QWORD[rbp-{CUR_STACK_VARS.vararg_stack_loc}]")
            )
            out.append(X86_Instr("lea", "rax", "[rdi + 8]"))
            out.append(
                X86_Instr("mov", f"QWORD[rbp-{CUR_STACK_VARS.vararg_stack_loc}]", "rax")
            )
            out.append(X86_LabelPoint(label_end))
            # TODO: different for float ?
            out.append(X86_Instr("mov", "rdi", "QWORD[rdi]"))
            out.append(X86_Instr("push", "rdi"))
        case IrInstrKind.ADD:
            out.append(X86_Instr("pop", "rdi"))
            pre_code, operand2 = value_get(ins.operand1, ins.operand2)
            out += pre_code
            out.append(X86_Instr("add", "rdi", operand2))
            out.append(X86_Instr("push", "rdi"))
        case IrInstrKind.SUB:
            out.append(X86_Instr("pop", "rdi"))
            if ins.operand1 is None:
                out.append(X86_Instr("pop", "rsi"))
                out.append(X86_Instr("sub", "rsi", "rdi"))
                out.append(X86_Instr("push", "rsi"))
            else:
                pre_code, operand2 = value_get(ins.operand1, ins.operand2)
                out += pre_code
                out.append(X86_Instr("sub", "rdi", operand2))
                out.append(X86_Instr("push", "rdi"))
        case IrInstrKind.MUL:
            out.append(X86_Instr("pop", "rdi"))
            pre_code, operand2 = value_get(ins.operand1, ins.operand2)
            out += pre_code
            out.append(X86_Instr("imul", "rdi", operand2))
            out.append(X86_Instr("push", "rdi"))
        case IrInstrKind.DIV:
            if ins.operand1 is None:
                out.append(X86_Instr("pop", "rdi"))
                out.append(X86_Instr("pop", "rax"))
                out.append(X86_Instr("cqo"))
                out.append(X86_Instr("idiv", "rdi"))
            else:
                out.append(X86_Instr("pop", "rax"))
                pre_code, operand2 = value_get(ins.operand1, ins.operand2)
                out += pre_code
                out.append(X86_Instr("cqo"))
                if operand2 != "rsi":
                    out.append(X86_Instr("mov", "rsi", operand2))
                    operand2 = "rsi"
                out.append(X86_Instr("idiv", operand2))
            out.append(X86_Instr("push", "rax"))
        case IrInstrKind.REM:
            if ins.operand1 is None:
                out.append(X86_Instr("pop", "rdi"))
                out.append(X86_Instr("pop", "rax"))
                out.append(X86_Instr("cqo"))
                out.append(X86_Instr("idiv", "rdi"))
            else:
                out.append(X86_Instr("pop", "rax"))
                pre_code, operand2 = value_get(ins.operand1, ins.operand2)
                out += pre_code
                out.append(X86_Instr("cqo"))
                if operand2 != "rsi":
                    out.append(X86_Instr("mov", "rsi", operand2))
                    operand2 = "rsi"
                out.append(X86_Instr("idiv", operand2))
            out.append(X86_Instr("push", "rdx"))
        case IrInstrKind.SHL:
            if ins.operand1 is None:
                out.append(X86_Instr("pop", "rcx"))
                out.append(X86_Instr("pop", "rsi"))
                out.append(X86_Instr("shl", "rsi", "cl"))
                out.append(X86_Instr("push", "rsi"))
            else:
                out.append(X86_Instr("pop", "rdi"))
                pre_code, operand2 = value_get(ins.operand1, ins.operand2, 8)
                out += pre_code
                out.append(X86_Instr("shl", "rdi", operand2))
                out.append(X86_Instr("push", "rdi"))
        case IrInstrKind.SHR:
            if ins.operand1 is None:
                out.append(X86_Instr("pop", "rcx"))
                out.append(X86_Instr("pop", "rsi"))
                out.append(X86_Instr("shr", "rsi", "cl"))
                out.append(X86_Instr("push", "rsi"))
            else:
                out.append(X86_Instr("pop", "rdi"))
                pre_code, operand2 = value_get(ins.operand1, ins.operand2, 8)
                out += pre_code
                out.append(X86_Instr("shr", "rdi", operand2))
                out.append(X86_Instr("push", "rdi"))
        case IrInstrKind.AND:
            out.append(X86_Instr("pop", "rdi"))
            pre_code, operand2 = value_get(ins.operand1, ins.operand2)
            out += pre_code
            out.append(X86_Instr("and", "rdi", operand2))
            out.append(X86_Instr("push", "rdi"))
        case IrInstrKind.XOR:
            out.append(X86_Instr("pop", "rdi"))
            pre_code, operand2 = value_get(ins.operand1, ins.operand2)
            out += pre_code
            out.append(X86_Instr("xor", "rdi", operand2))
            out.append(X86_Instr("push", "rdi"))
        case IrInstrKind.IOR:
            out.append(X86_Instr("pop", "rdi"))
            pre_code, operand2 = value_get(ins.operand1, ins.operand2)
            out += pre_code
            out.append(X86_Instr("or", "rdi", operand2))
            out.append(X86_Instr("push", "rdi"))
        case (
            IrInstrKind.LTH
            | IrInstrKind.GTH
            | IrInstrKind.GEQ
            | IrInstrKind.LEQ
            | IrInstrKind.EQU
            | IrInstrKind.NEQ
        ):
            kind = {
                IrInstrKind.LTH: "l",
                IrInstrKind.GTH: "g",
                IrInstrKind.LEQ: "le",
                IrInstrKind.GEQ: "ge",
                IrInstrKind.EQU: "e",
                IrInstrKind.NEQ: "ne",
            }[ins.opcode]
            operand1 = ins.operand1
            operand2 = ins.operand2
            if operand1 is None:
                out.append(X86_Instr("pop", "rdi"))
                out.append(X86_Instr("pop", "rsi"))
                out.append(X86_Instr("cmp", "rsi", "rdi"))
            else:
                out.append(X86_Instr("pop", "rdi"))
                pre_code, operand2 = value_get(operand1, operand2)
                out += pre_code
                out.append(X86_Instr("cmp", "rdi", operand2))
            out.append(X86_Instr(f"set{kind}", "al"))
            out.append(X86_Instr("movzx", "rax", "al"))
            out.append(X86_Instr("push", "rax"))
        case IrInstrKind.NEG:
            out.append(X86_Instr("pop", "rdi"))
            out.append(X86_Instr("neg", "rdi"))
            out.append(X86_Instr("push", "rdi"))
        case IrInstrKind.INV:
            out.append(X86_Instr("pop", "rdi"))
            out.append(X86_Instr("not", "rdi"))
            out.append(X86_Instr("push", "rdi"))
        case IrInstrKind.NOT:
            out.append(X86_Instr("pop", "rdi"))
            out.append(X86_Instr("mov", "rax", 0))
            out.append(X86_Instr("test", "rdi", "rdi"))
            out.append(X86_Instr("sete", "al"))
            out.append(X86_Instr("mov", "rdi", "rax"))
            out.append(X86_Instr("push", "rdi"))
        case IrInstrKind.DRP:
            out.append(X86_Instr("pop", "rdi"))
        case IrInstrKind.RET:
            out.append(X86_Instr("leave"))
            out.append(X86_Instr("ret"))
        case IrInstrKind.RTV:
            out.append(X86_Instr("pop", "rax"))
            out.append(X86_Instr("leave"))
            out.append(X86_Instr("ret"))
        case IrInstrKind.CAL:
            call_name = ins.operand1
            pic_str = (
                "" if len(ir.functions[call_name].instructions) != 0 else " wrt ..plt"
            )
            param_count = ins.operand2
            for i in range(min(param_count, 6)):
                out.append(
                    X86_Instr("pop", ["rdi", "rsi", "rdx", "rcx", "r8", "r9"][i])
                )
            if ir.functions[call_name].is_vararg:
                # TODO: number of float arguments
                out.append(X86_Instr("mov", "rax", 0))
            out.append(X86_Instr("call", X86_Label(f"{call_name}{pic_str}")))
            if (n := max(0, param_count) - 6) > 0:
                out.append(X86_Instr("add", "rsp", n * 8))
            if ir.functions[call_name].returns_value:
                out.append(X86_Instr("push", "rax"))
        case IrInstrKind.LBL:
            out.append(X86_LabelPoint(X86_Label("." + ins.operand1)))
        case IrInstrKind.JMP:
            out.append(X86_Instr("jmp", X86_Label("." + ins.operand1)))
        case IrInstrKind.JZO:
            out.append(X86_Instr("pop", "rdi"))
            out.append(X86_Instr("test", "rdi", "rdi"))
            out.append(X86_Instr("je", X86_Label("." + ins.operand1)))
        case IrInstrKind.JNZ:
            out.append(X86_Instr("pop", "rdi"))
            out.append(X86_Instr("test", "rdi", "rdi"))
            out.append(X86_Instr("jne", X86_Label("." + ins.operand1)))
        case IrInstrKind.BUI:
            builtin_name = ins.operand1
            param_count = ins.operand2
            if builtin_name == "__builtin_syscall":
                for i in range(param_count):
                    out.append(
                        X86_Instr(
                            "pop", ["rax", "rdi", "rsi", "rdx", "r10", "r8", "r9"][i]
                        )
                    )
                out.append(X86_Instr("syscall"))
                out.append(X86_Instr("push", "rax"))
            else:
                assert False, builtin_name
    return out


def function_code(f, ir) -> X86_Body:
    out = []
    out += fn_start(f)
    for i in f.instructions:
        try:
            out += instr_codegen(i, ir)
        except Exception: 
            print(f"instruction '{i}' caused error (function {f})")
            assert False
    return optimize_function_code(out)


def gen_db_str(data: bytes) -> str:
    out = ""
    needs_comma = False
    while len(data) > 0:
        if needs_comma:
            out += ", "
        printable_count = 0
        while (
            printable_count < len(data)
            and chr(data[printable_count]).isprintable()
            and chr(data[printable_count]) != '"'
        ):
            printable_count += 1
        if printable_count > 0:
            data_str = str(data[:printable_count])[2:-1]
            out += f'"{data_str}"'
            data = data[printable_count:]
            needs_comma = True
            continue
        out += str(data[0])
        needs_comma = True
        data = data[1:]
    return out


def compile_ir(ir: FullIr, comment_ir_in_asm: bool = False) -> str:
    global COMMENT_IR_IN_ASM
    COMMENT_IR_IN_ASM = comment_ir_in_asm
    out = "default rel\n"
    out += "section .text\n"
    for n, f in ir.functions.items():
        if len(f.instructions) == 0:
            out += f"extern {n}\n"
        else:
            if f.is_lib or n == "main":
                out += f"global {n}\n"
            out += n + ":\n"
            f_code = function_code(f, ir)
            out += "\n".join([str(i) for i in f_code]) + "\n"
    cur = ".text"
    for i, g in enumerate(ir.globs):
        section = [".data", ".rodata"][g.is_ro]
        if section != cur:
            out += f"section {section}\n"
            cur = section
        if g.is_lib:
            out += f"global global_{i}\n"
        out += f"global_{i}:\n"
        data_str = gen_db_str(g.data)
        out += f"    db {data_str}\n"
    return out
