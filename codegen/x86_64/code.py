from ir import IrInstrKind, IrInstr, FunctionIr, FullIr
from dataclasses import dataclass

@dataclass
class StackVariables:
    variable_offsets: [int]
    variable_sizes: [int]
    stack_size: int

    def __init__(self, stack_frame: [int]):
        self.stack_size = 0
        self.variable_offsets = []
        self.variable_sizes = []
        for item in stack_frame:
            self.variable_sizes.append(item)
            self.variable_offsets.append(- self.stack_size - item)
            self.stack_size += item
            if self.stack_size % 8 != 0:
                self.stack_size = self.stack_size - self.stack_size % 8 + 8

CUR_STACK_VARS: StackVariables = StackVariables([])

def fn_start(stack_frame: [int]) -> str:
    global CUR_STACK_VARS
    CUR_STACK_VARS = StackVariables(stack_frame)
    out = ""
    out +=  "    push rbp\n"
    out +=  "    mov rbp, rsp\n"
    if len(stack_frame) > 0:
        out += f"    sub rsp, {CUR_STACK_VARS.stack_size}\n"
    return out

def var_size(var_id: int):
    return CUR_STACK_VARS.variable_sizes[var_id]

def var_addr(var_id: int, stack_param: bool = False) -> str:
    i = 0
    if stack_param:
        i = 16 + var_id * 8
    else:
        i = CUR_STACK_VARS.variable_offsets[var_id]
    if i == 0:
        return "[rbp]"
    elif i < 0:
        return f"[rbp - {abs(i)}]"
    else:
        return f"[rbp + {i}]"

def value_get(operand1, operand2, operand_bytes = 64):
    reg = "rsi"
    op_reg = "rsi"
    if operand_bytes == 8:
        reg = "rcx"
        op_reg = "cl"
    if operand1 is None:
        return (f"    pop {reg}\n", op_reg)
    elif operand1 == 0:
        return ("", f"{operand2}")
    elif operand1 == 1:
        return ("", f"QWORD{var_addr(operand2)}")
    elif operand1 == 2:
        return (f"    lea rsi, {var_addr(operand2)}", op_reg)
    else:
        assert False

def comp_code(operand1, operand2, kind):
    out = ""
    if operand1 is None:
        out += "    pop rdi\n"
        out += "    pop rsi\n"
        out += "    cmp rsi, rdi\n"
    else:
        out += "    pop rdi\n"
        pre_code, operand2 = value_get(operand1, operand2)
        out += pre_code
        out += f"    cmp rdi, {operand2}\n"
    out += f"    set{kind} al\n"
    out += f"    movzx rax, al\n"
    out += f"    push rax\n"
    return out

def get_reg_kind(base, size):
    reg = ""
    if base == "rdi":
        reg = ["dil", "di", "edi", "edi", "rdi", "rdi", "rdi", "rdi"][size - 1]
    elif base == "rsi":
        reg = ["sil", "si", "esi", "esi", "rsi", "rsi", "rsi", "rsi"][size - 1]
    elif base == "rdx":
        reg = ["dl", "dx", "edx", "edx", "rdx", "rdx", "rdx", "rdx"][size - 1]
    elif base == "rcx":
        reg = ["cl", "cx", "ecx", "ecx", "rcx", "rcx", "rcx", "rcx"][size - 1]
    elif base == "rax":
        reg = ["al", "ax", "eax", "eax", "rax", "rax", "rax", "rax"][size - 1]
    elif base == "rbx":
        reg = ["bl", "bx", "ebx", "ebx", "rbx", "rbx", "rbx", "rbx"][size - 1]
    elif base == "r8":
        reg = ["r8b", "r8w", "r8d", "r8d", "r8", "r8", "r8", "r8"][size - 1]
    elif base == "r9":
        reg = ["r9b", "r9w", "r9d", "r9d", "r9", "r9", "r9", "r9"][size - 1]
    elif base == "r10":
        reg = ["r10b", "r10w", "r10d", "r10d", "r10", "r10", "r10", "r10"][size - 1]
    # TODO: all
    return reg, ["BYTE", "WORD", "DWORD", "DWORD", "QWORD", "QWORD", "QWORD", "QWORD"][size - 1]


def instr_codegen(i: IrInstr, ir: FullIr) -> str:
    out = ""
    match i.opcode:
        case IrInstrKind.PRM:
            param_count = i.operand1
            for i in range(min(param_count, 6)):
                size = var_size(i)
                register = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"][i]
                store_reg, store_kind = get_reg_kind(register, size)
                out += f"    mov {store_kind}{var_addr(i)}, {store_reg}\n"
            if param_count > 6:
                for i in range(0, param_count - 6):
                    out += f"    mov rdi, QWORD{var_addr(i, True)}\n"
                    out += f"    mov QWORD{var_addr(i + 6)}, rdi\n"
        case IrInstrKind.STV:
            var_id = i.operand1
            size = var_size(var_id)
            store_reg, store_kind = get_reg_kind("rdi", size)
            out += f"    pop rdi\n"
            out += f"    mov {store_kind}{var_addr(var_id)}, {store_reg}\n"
        case IrInstrKind.STA:
            store_reg, store_kind = get_reg_kind("rsi", i.operand1)
            out += f"    pop rdi\n"
            out += f"    pop rsi\n"
            out += f"    mov {store_kind}[rdi], {store_reg}\n"
        case IrInstrKind.LDA:
            load_reg, load_kind = get_reg_kind("rsi", i.operand1)
            if i.operand1 != 8:
                out += f"    xor rsi, rsi\n"
            out += f"    pop rdi\n"
            out += f"    mov {load_reg}, {load_kind}[rdi]\n"
            out += f"    push rsi\n"
        case IrInstrKind.PSH:
            if i.operand1 == 0:
                out += f"    push {i.operand2}\n"
            elif i.operand1 == 1:
                size = var_size(i.operand2)
                load_reg, load_kind = get_reg_kind("rdi", size)
                if size != 8:
                    out += f"    xor rdi, rdi\n"
                out += f"    mov {load_reg}, {load_kind}{var_addr(i.operand2)}\n"
                out += f"    push rdi\n"
            elif i.operand1 == 2:
                out += f"    lea rdi, {var_addr(i.operand2)}\n"
                out += f"    push rdi\n"
            elif i.operand1 == 3:
                out += f"    lea rdi, [global_{i.operand2}]\n"
                out += f"    push rdi\n"
            elif i.operand1 == 4:
                # TODO: multiple sizes
                out += f"    mov rdi, QWORD[global_{i.operand2}]\n"
                out += f"    push rdi\n"
            else:
                assert False
        case IrInstrKind.ADD:
            out += "    pop rdi\n"
            pre_code, operand2 = value_get(i.operand1, i.operand2)
            out += pre_code
            out += f"    add rdi, {operand2}\n"
            out += "    push rdi\n"
        case IrInstrKind.SUB:
            out += "    pop rdi\n"
            if i.operand1 is None:
                out += "    pop rsi\n"
                out += "    sub rsi, rdi\n"
                out += "    push rsi\n"
            else:
                pre_code, operand2 = value_get(i.operand1, i.operand2)
                out += pre_code
                out += f"    sub rdi, {operand2}\n"
                out += "    push rdi\n"
        case IrInstrKind.MUL:
            out += "    pop rdi\n"
            pre_code, operand2 = value_get(i.operand1, i.operand2)
            out += pre_code
            out += f"    imul rdi, {operand2}\n"
            out += "    push rdi\n"
        case IrInstrKind.DIV:
            if i.operand1 is None:
                out += "    pop rdi\n"
                out += "    pop rax\n"
                out += "    cqo\n"
                out += "    idiv rdi\n"
            else:
                out += "    pop rax\n"
                pre_code, operand2 = value_get(i.operand1, i.operand2)
                out += pre_code
                out += "    cqo\n"
                if operand2 != "rsi":
                    out += f"    mov rsi, {operand2}\n"
                    operand2 = "rsi"
                out += f"    idiv {operand2}\n"
            out += "    push rax\n"
        case IrInstrKind.REM:
            if i.operand1 is None:
                out += "    pop rdi\n"
                out += "    pop rax\n"
                out += "    cqo\n"
                out += "    idiv rdi\n"
            else:
                out += "    pop rax\n"
                pre_code, operand2 = value_get(i.operand1, i.operand2)
                out += pre_code
                out += "    cqo\n"
                if operand2 != "rsi":
                    out += f"    mov rsi, {operand2}\n"
                    operand2 = "rsi"
                out += f"    idiv {operand2}\n"
            out += "    push rdx\n"
        case IrInstrKind.SHL:
            if i.operand1 is None:
                out += "    pop rcx\n"
                out += "    pop rsi\n"
                out += "    shl rsi, cl\n"
                out += "    push rsi\n"
            else:
                out += "    pop rdi\n"
                pre_code, operand2 = value_get(i.operand1, i.operand2, 8)
                out += pre_code
                out += f"    shl rdi, {operand2}\n"
                out += "    push rdi\n"
        case IrInstrKind.SHR:
            if i.operand1 is None:
                out += "    pop rcx\n"
                out += "    pop rsi\n"
                out += "    shr rsi, cl\n"
                out += "    push rsi\n"
            else:
                out += "    pop rdi\n"
                pre_code, operand2 = value_get(i.operand1, i.operand2, 8)
                out += pre_code
                out += f"    shr rdi, {operand2}\n"
                out += "    push rdi\n"
        case IrInstrKind.AND:
            out += "    pop rdi\n"
            pre_code, operand2 = value_get(i.operand1, i.operand2)
            out += pre_code
            out += f"    and rdi, {operand2}\n"
            out += "    push rdi\n"
        case IrInstrKind.XOR:
            out += "    pop rdi\n"
            pre_code, operand2 = value_get(i.operand1, i.operand2)
            out += pre_code
            out += f"    xor rdi, {operand2}\n"
            out += "    push rdi\n"
        case IrInstrKind.IOR:
            out += "    pop rdi\n"
            pre_code, operand2 = value_get(i.operand1, i.operand2)
            out += pre_code
            out += f"    or rdi, {operand2}\n"
            out += "    push rdi\n"
        case IrInstrKind.LTH:
            out += comp_code(i.operand1, i.operand2, "l")
        case IrInstrKind.GTH:
            out += comp_code(i.operand1, i.operand2, "g")
        case IrInstrKind.LEQ:
            out += comp_code(i.operand1, i.operand2, "le")
        case IrInstrKind.GEQ:
            out += comp_code(i.operand1, i.operand2, "ge")
        case IrInstrKind.EQU:
            out += comp_code(i.operand1, i.operand2, "e")
        case IrInstrKind.NEQ:
            out += comp_code(i.operand1, i.operand2, "ne")
        case IrInstrKind.NEG:
            out += "    pop rdi\n"
            out += "    neg rdi\n"
            out += "    push rdi\n"
        case IrInstrKind.INV:
            out += "    pop rdi\n"
            out += "    not rdi\n"
            out += "    push rdi\n"
        case IrInstrKind.NOT:
            out += "    pop rdi\n"
            out += "    xor rax, rax\n"
            out += "    test rdi, rdi\n"
            out += "    sete al\n"
            out += "    mov rdi, rax\n"
            out += "    push rdi\n"
        case IrInstrKind.DRP:
            out += "    pop rdi\n"
        case IrInstrKind.RET:
            out +=  "    leave\n"
            out += f"    ret\n"
        case IrInstrKind.RTV:
            out +=  "    pop rax\n"
            out +=  "    leave\n"
            out += f"    ret\n"
        case IrInstrKind.CAL:
            call_name = i.operand1
            param_count = i.operand2
            for i in range(min(param_count, 6)):
                out += "    pop " + ["rdi", "rsi", "rdx", "rcx", "r8", "r9"][i] + "\n"
            out += f"    call {call_name}\n"
            if (n := max(0, param_count) - 6) > 0:
                out += "    add rsp, {}\n".format(n*8)
            if ir.functions[call_name].returns_value:
                out += "    push rax\n"
        case IrInstrKind.LBL:
            label_name = i.operand1
            out += f".{label_name}:\n"
        case IrInstrKind.JMP:
            to_label = i.operand1
            out += f"    jmp .{to_label}\n"
        case IrInstrKind.JZO:
            to_label = i.operand1
            out +=  "    pop rdi\n"
            out +=  "    test rdi, rdi\n"
            out += f"    je .{to_label}\n"
        case IrInstrKind.JNZ:
            to_label = i.operand1
            out +=  "    pop rdi\n"
            out +=  "    test rdi, rdi\n"
            out += f"    jne .{to_label}\n"
        case IrInstrKind.BUI:
            builtin_name = i.operand1
            param_count = i.operand2
            if builtin_name == "__builtin_hexdump":
                assert param_count == 1
                out += "    pop rdi\n"
                out += "    call __builtin_hexdump_fncall\n"
                out += "    push 0\n"
            elif builtin_name == "__builtin_syscall":
                for i in range(param_count):
                    out += "    pop " + ["rax", "rdi", "rsi", "rdx", "r10", "r8", "r9"][i] + "\n"
                out += "    syscall\n"
                out += "    push rax\n"
            else:
                assert False, builtin_name
        case _:
            assert False, i.opcode
    return out

def end_main() -> str:
    out = ""
    out += "    mov rax, 60\n"
    out += "    mov rdi, 0\n" # TODO: return value
    out += "    syscall\n\n"
    return out

def remove_push_pop(instrs: str) -> str:
    return instrs
    out_lines = []
    lines = instrs.split('\n')
    if len(lines) == 0:
        return ""
    skip_n = 0
    for i in range(len(lines) - 1):
        if skip_n > 0:
            skip_n -= 1
            continue
        l = lines[i]
        if l.startswith("    push "):
            lp1 = lines[i+1]
            if lp1.startswith("    pop "):
                reg0 = l[9:]
                reg1 = lp1[8:]
                if reg0 == reg1:
                    skip_n = 1
                    continue
                out_lines.append(f"    mov {reg1}, {reg0}")
                skip_n = 1
                continue
        if l.startswith("    pop "):
            lp1 = lines[i+1]
            if lp1.startswith("    push "):
                reg0 = l[8]
                reg1 = lp1[9]
                if reg0 == reg1:
                    skip_n = 1
                    continue

        out_lines.append(l)
    out_lines.append(lines[-1])
    return '\n'.join(out_lines) + '\n'

def function_code(f, ir, is_main: bool) -> str:
    out = ""
    out += fn_start(f.stack_frame)
    for i in f.instructions:
        if is_main and i.opcode == IrInstrKind.RET:
            out += end_main()
        else:
            out += instr_codegen(i, ir)
    return out

def gen_db_str(data: bytes) -> str:
    out = ""
    needs_comma = False
    while len(data) > 0:
        if needs_comma:
            out += ", "
        printable_count = 0
        while printable_count < len(data) and chr(data[printable_count]).isprintable():
            printable_count += 1
        if printable_count > 0:
            data_str = str(data[:printable_count])[2:-1]
            out += f"\"{data_str}\""
            data = data[printable_count:]
            needs_comma = True
            continue
        out += str(data[0])
        needs_comma = True
        data = data[1:]
    return out

def compile_ir(ir: FullIr) -> str:
    out = "section .text\n"
    for n, f in ir.functions.items():
        if n == "main":
            out += "global _start\n"
            out += "_start:\n"
            out += remove_push_pop(function_code(f, ir, True))
        elif len(f.instructions) == 0:
            out += f"extern {n}\n"
        else:
            if f.is_lib:
                out += f"global {n}\n"
            out += n + ":\n"
            out += remove_push_pop(function_code(f, ir, False))
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
