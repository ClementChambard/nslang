from enum import Enum
import struct
from dataclasses import dataclass
from types import MemberDescriptorType
from typing import Any, List, Tuple
from ir.data import FullIr, IrGlobal, IrInstr, IrInstrKind, FunctionIr, StackFrameEntry


def struct_int(size: int) -> str:
    if size == 2:
        return "W"
    if size == 4:
        return "I"
    if size == 8:
        return "Q"
    return "B"


@dataclass
class ExecCtx:
    f_name: str
    cur_function: FunctionIr
    cur_instr: int
    local_offsets: List[Tuple[int, int]]
    base_stack_offset: int
    va_args: list  # always ints ?

    def __init__(self, f_name: str, f: FunctionIr, base_stack_offset=0) -> None:
        self.f_name = f_name
        self.cur_function = f
        self.cur_instr = -1
        self.base_stack_offset = base_stack_offset
        self.local_offsets = []
        for local in f.stack_frame:
            self.local_offsets.append((base_stack_offset, local.size))
            base_stack_offset += local.size

    def setup_stack(self, args: List[int]) -> bytes:
        # also set up va_args
        regular_param_count = len(self.cur_function.params)
        args, self.va_args = args[:regular_param_count], args[regular_param_count:]
        out = b""
        for i in range(regular_param_count):
            out += struct.pack(struct_int(self.local_offsets[i][1]), args[i])
        for i in range(regular_param_count, len(self.local_offsets)):
            out += b"\x00" * self.local_offsets[i][1]
        return out

    def get_var(self, var_id) -> Tuple[int, int]:
        return self.local_offsets[var_id]


class MemKind(Enum):
    NONE = 0
    STACK = 1
    HEAP = 2
    GLOB = 3
    COUNT = 4

    def offset(self) -> int:
        return self.value * 0x100000000

    @classmethod
    def decompose_addr(cls, addr: int) -> Tuple[int, int]:
        return addr >> 0x20, addr & 0xFFFFFFFF


@dataclass
class Interpreter:
    memory: List[bytes]
    globals: list
    ir_globals: list
    ctx: ExecCtx
    done: bool
    ctx_stack: List[ExecCtx]
    functions: dict
    exit_code: int

    def __init__(self, ir: FullIr) -> None:
        self.memory = [b"", b"", b"", b""]
        self.globals = []
        self.ir_globals = ir.globs
        self.prepare_globals(ir.globs)
        self.functions = ir.functions
        self.ctx = ExecCtx("main", self.functions["main"])
        self.memory[MemKind.STACK.value] = self.ctx.setup_stack([])
        self.ctx_stack = []
        self.exit_code = 0
        self.done = False

    def prepare_globals(self, globs: List[IrGlobal]):
        cur_offset = 0
        for g in globs:
            self.globals.append(
                (cur_offset, g.name, [0, 1][g.is_lib] | [0, 2][g.is_ro])
            )
            self.memory[MemKind.GLOB.value] += g.data
            cur_offset += len(g.data)

    def syscall(self, n, args) -> int:
        import os

        if n == 0:  # read
            b = os.read(args[0], args[2])
            self.store_bytes(args[1], b)
            return len(b)
        if n == 1:  # write
            b = self.load_bytes(args[2], args[1])
            return os.write(args[0], b)
        if n == 2:  # open
            fname = self.load_cstr(args[0])
            return os.open(fname, args[1], args[2])
        if n == 3:  # close
            os.close(args[0])
            return 0
        if n == 8:  # lseek
            return os.lseek(args[0], args[1], args[2])
        if n == 12:  # brk
            cur_brk = len(self.memory[MemKind.HEAP.value])
            new_brk = args[0] - MemKind.HEAP.offset()
            if new_brk > cur_brk:
                to_add = new_brk - cur_brk
                self.memory[MemKind.HEAP.value] += b"\x00" * to_add
            return len(self.memory[MemKind.HEAP.value]) + MemKind.HEAP.offset()
        print(f"syscall[{n}]({args})")
        return 0

    def push(self, int64_value):
        input_bytes = struct.pack("q", int64_value)
        self.memory[MemKind.STACK.value] += input_bytes

    def pop(self) -> int:
        stack_ptr = len(self.memory[MemKind.STACK.value])
        self.memory[MemKind.STACK.value], output_bytes = (
            self.memory[MemKind.STACK.value][: stack_ptr - 8],
            self.memory[MemKind.STACK.value][stack_ptr - 8 :],
        )
        return struct.unpack("q", output_bytes)[0]

    def call(self, f_name: str, arg_count: int):
        args = [self.pop() for _ in range(arg_count)]
        self.ctx_stack.append(self.ctx)
        self.ctx = ExecCtx(
            f_name, self.functions[f_name], len(self.memory[MemKind.STACK.value])
        )
        self.memory[MemKind.STACK.value] += self.ctx.setup_stack(args)

    def ret(self, return_value: bool = False):
        ret_val = 0
        if return_value:
            ret_val = self.pop()
        self.memory[MemKind.STACK.value] = self.memory[MemKind.STACK.value][
            : self.ctx.base_stack_offset
        ]
        if len(self.ctx_stack) == 0:
            self.done = True
            self.exit_code = ret_val
            return
        self.ctx = self.ctx_stack.pop()
        if return_value:
            self.push(ret_val)

    def jump(self, lbl_name: str):
        for i, ins in enumerate(self.ctx.cur_function.instructions):
            if ins.opcode != IrInstrKind.LBL:
                continue
            if ins.operand1 == lbl_name:
                self.ctx.cur_instr = i
                return
        assert False, f"label not found: {lbl_name}"

    def exec_builtin(self, name: str, arg_count: int):
        args = [self.pop() for _ in range(arg_count)]
        if name == "__builtin_syscall":
            self.push(self.syscall(args[0], args[1:]))
        else:
            assert False, f"builtin not found: {name}"

    def store_bytes(self, addr: int, input_bytes: bytes):
        mem_type, offset = MemKind.decompose_addr(addr)
        self.memory[mem_type] = (
            self.memory[mem_type][:offset]
            + input_bytes
            + self.memory[mem_type][offset + len(input_bytes) :]
        )

    def store(self, size: int, addr: int, val: int):
        self.store_bytes(addr, struct.pack(struct_int(size), val))

    def load_bytes(self, size: int, addr: int) -> bytes:
        mem_type, offset = MemKind.decompose_addr(addr)
        mem = self.memory[mem_type]
        assert len(mem) > offset, (
            f"Out of bound access => offset={offset}, size={size}, max={len(mem)}"
        )
        assert len(mem) >= offset + size, (
            f"Out of bound access (size) => offset={offset}, size={size}, max={len(mem)}"
        )
        return mem[offset : offset + size]

    def load_cstr(self, addr: int) -> str:
        len = 0
        while self.load(1, addr + len) != 0:
            len += 1
        return self.load_bytes(len, addr).decode("utf-8")

    def load(self, size: int, addr: int) -> int:
        out_bytes = self.load_bytes(size, addr)
        assert len(out_bytes) == size, f"expected {size} bytes, got {len(out_bytes)}"
        return struct.unpack(struct_int(size), out_bytes)[0]

    def get_thing(self, operand1, operand2) -> int:
        if operand1 == 0:
            return int(operand2)
        elif operand1 == 1:
            offset, size = self.ctx.get_var(operand2)
            return self.load(size, MemKind.STACK.offset() + offset)
        elif operand1 == 2:
            return MemKind.STACK.offset() + self.ctx.get_var(operand2)[0]
        elif operand1 == 3:
            return MemKind.GLOB.offset() + self.globals[operand2][0]
        elif operand1 == 4:
            return self.load(8, MemKind.GLOB.offset() + self.globals[operand2][0])
        else:
            assert False

    def arithmetic(self, operand1, operand2, f):
        if operand1 is None:
            b = self.pop()
            a = self.pop()
            self.push(int(f(a, b)))
            return
        a = self.pop()
        b = self.get_thing(operand1, operand2)
        self.push(int(f(a, b)))

    def exec_instr(self, ins: IrInstr):
        match ins.opcode:
            case IrInstrKind.STV:
                val = self.pop()
                offset, size = self.ctx.get_var(ins.operand1)
                self.store(size, MemKind.STACK.offset() + offset, val)
            case IrInstrKind.STA:
                addr = self.pop()
                val = self.pop()
                self.store(ins.operand1, addr, val)
            case IrInstrKind.LDA:
                addr = self.pop()
                self.push(self.load(ins.operand1, addr))
            case IrInstrKind.PSH:
                self.push(self.get_thing(ins.operand1, ins.operand2))
            case IrInstrKind.DUP:
                a = self.pop()
                self.push(a)
                self.push(a)
            case IrInstrKind.DRP:
                self.pop()
            case IrInstrKind.VAA:
                a = self.ctx.va_args[0]
                self.ctx.va_args = self.ctx.va_args[1:]
                self.push(a)
            case IrInstrKind.LBL:
                pass
            case IrInstrKind.JMP:
                self.jump(ins.operand1)
            case IrInstrKind.JZO:
                if self.pop() == 0:
                    self.jump(ins.operand1)
            case IrInstrKind.JNZ:
                if self.pop() != 0:
                    self.jump(ins.operand1)
            case IrInstrKind.CAL:
                self.call(ins.operand1, ins.operand2)
            case IrInstrKind.BUI:
                self.exec_builtin(ins.operand1, ins.operand2)
            case IrInstrKind.RET:
                self.ret()
            case IrInstrKind.RTV:
                self.ret(True)
            case IrInstrKind.NEG:
                self.push(-self.pop())
            case IrInstrKind.INV:
                self.push(~self.pop())
            case IrInstrKind.NOT:
                self.push(int(not self.pop()))
            case IrInstrKind.ADD:
                self.arithmetic(ins.operand1, ins.operand2, lambda x, y: x + y)
            case IrInstrKind.SUB:
                self.arithmetic(ins.operand1, ins.operand2, lambda x, y: x - y)
            case IrInstrKind.MUL:
                self.arithmetic(ins.operand1, ins.operand2, lambda x, y: x * y)
            case IrInstrKind.DIV:
                self.arithmetic(ins.operand1, ins.operand2, lambda x, y: x // y)
            case IrInstrKind.REM:
                self.arithmetic(ins.operand1, ins.operand2, lambda x, y: x % y)
            case IrInstrKind.SHL:
                self.arithmetic(ins.operand1, ins.operand2, lambda x, y: x << y)
            case IrInstrKind.SHR:
                self.arithmetic(ins.operand1, ins.operand2, lambda x, y: x >> y)
            case IrInstrKind.AND:
                self.arithmetic(ins.operand1, ins.operand2, lambda x, y: x & y)
            case IrInstrKind.XOR:
                self.arithmetic(ins.operand1, ins.operand2, lambda x, y: x ^ y)
            case IrInstrKind.IOR:
                self.arithmetic(ins.operand1, ins.operand2, lambda x, y: x | y)
            case IrInstrKind.LTH:
                self.arithmetic(ins.operand1, ins.operand2, lambda x, y: x < y)
            case IrInstrKind.GTH:
                self.arithmetic(ins.operand1, ins.operand2, lambda x, y: x > y)
            case IrInstrKind.LEQ:
                self.arithmetic(ins.operand1, ins.operand2, lambda x, y: x <= y)
            case IrInstrKind.GEQ:
                self.arithmetic(ins.operand1, ins.operand2, lambda x, y: x >= y)
            case IrInstrKind.EQU:
                self.arithmetic(ins.operand1, ins.operand2, lambda x, y: x == y)
            case IrInstrKind.NEQ:
                self.arithmetic(ins.operand1, ins.operand2, lambda x, y: x != y)

    def run(self):
        while not self.done:
            self.ctx.cur_instr += 1
            self.exec_instr(self.ctx.cur_function.instructions[self.ctx.cur_instr])

    def debug(self):
        print_ins = True
        cur_ctx = self.ctx
        breakpoints = []
        run_until = None
        running = False
        last = ""
        while True:
            if self.done:
                print(f"Program exited with code {self.exit_code}")
                print("Type 'exit' to quit, or 'restart' to restart the program")
                running = False
                while True:
                    cmd = input("dbg> ")
                    if cmd == "exit":
                        return
                    if cmd == "restart" or cmd == "r":
                        self.done = False
                        self.memory = [b"", b"", b"", b""]
                        self.ctx = ExecCtx("main", self.functions["main"])
                        self.memory[MemKind.STACK.value] = self.ctx.setup_stack([])
                        self.prepare_globals(self.ir_globals)
                        break
                print_ins = True
                cur_ctx = self.ctx
                continue
            if running:
                self.ctx.cur_instr += 1
                try:
                    self.exec_instr(
                        self.ctx.cur_function.instructions[self.ctx.cur_instr]
                    )
                except Exception:
                    self.ctx.cur_instr -= 1
                    print("Exception occured on this instruction: ")
                    print_ins = True
                    running = False
                    cur_ctx = self.ctx
                    continue
                if run_until is not None:
                    if (
                        run_until[0] == self.ctx.f_name
                        and run_until[1] == self.ctx.cur_instr
                    ):
                        running = False
                        print_ins = True
                        run_until = None
                        cur_ctx = self.ctx
                        continue
                for i, bp in enumerate(breakpoints):
                    if bp[0] == self.ctx.f_name and bp[1] == self.ctx.cur_instr:
                        running = False
                        run_until = None
                        print_ins = True
                        print(f"Hit breakpoint[{i}] at {bp[0]}:{bp[1] + 1}")
                        cur_ctx = self.ctx
                        continue

                # Check breakpoints
                continue
            if print_ins:
                n, o = cur_ctx.f_name, cur_ctx.cur_instr + 1
                print(f"{n}:{o}")
                print(cur_ctx.cur_function.instructions[o])
                print_ins = False
            cmd = input("dbg> ")
            if cmd == "":
                cmd = last
            last = cmd
            cmd_kind = cmd.split(" ", 1)[0]
            if cmd_kind == "n" or cmd_kind == "next":
                self.ctx.cur_instr += 1
                try:
                    self.exec_instr(
                        self.ctx.cur_function.instructions[self.ctx.cur_instr]
                    )
                except Exception:
                    self.ctx.cur_instr -= 1
                    print("Exception occured on this instruction: ")
                print_ins = True
                cur_ctx = self.ctx
                continue
            if cmd_kind == "c" or cmd_kind == "continue":
                running = True
                continue
            if cmd_kind == "b" or cmd_kind == "break":
                args = cmd.split(" ")[1:]
                fst_arg_is_line = False
                line = -1
                fn = cur_ctx.f_name
                if len(args) > 0:
                    if args[0][0].isdigit():
                        line = int(args[0]) - 1
                        fst_arg_is_line = True
                    else:
                        fn = args[0]
                if len(args) > 1:
                    if fst_arg_is_line:
                        fn = args[1]
                    else:
                        line = int(args[1]) - 1
                print(f"Added breakpoint[{len(breakpoints)}] at {fn}:{line + 1}")
                breakpoints.append((fn, line))
                continue
            if cmd_kind == "step" or cmd_kind == "s":
                run_until = (self.ctx.f_name, self.ctx.cur_instr + 1)
                running = True
                continue
            if cmd_kind == "stepout" or cmd_kind == "so":
                if len(self.ctx_stack) == 0:
                    print("Can't step out main function")
                    continue
                par_ctx = self.ctx_stack[-1]
                run_until = (par_ctx.f_name, par_ctx.cur_instr)
                running = True
                continue
            if cmd_kind == "stack":
                print_stack(cur_ctx, self.memory[MemKind.STACK.value])
                continue
            if cmd_kind == "heap":
                args = cmd.split(" ")[1:]
                if len(args) == 0:
                    print(self.memory[MemKind.HEAP.value])
                    continue
                offset = 0
                if args[0].startswith("0x"):
                    offset = int(args[0][2:], base=16)
                else:
                    offset = int(args[0])
                print(self.memory[MemKind.HEAP.value][offset:])
                continue
            if cmd_kind == "globals":
                print_globals(self.globals, self.memory[MemKind.GLOB.value])
                continue
            if cmd_kind == "func":

                def p(name, f):
                    print(f"function {name}:")
                    for i, ins in enumerate(f.instructions):
                        print(f"#{i:03} {ins}")

                args = cmd.split(" ")[1:]
                if len(args) == 0:
                    p(cur_ctx.f_name, cur_ctx.cur_function)
                else:
                    f_name = args[0]
                    if f_name not in self.functions.keys():
                        print(f"Unknown function '{f_name}'")
                        continue
                    f = self.functions[f_name]
                    p(f_name, f)
                continue
            if cmd_kind == "call_stack":
                print_call_stack(cur_ctx, self.ctx_stack + [self.ctx])
                continue
            if cmd_kind == "up" or cmd_kind == "u":
                cur_ctx, print_ins = call_stack_navigate(
                    cur_ctx, self.ctx_stack + [self.ctx], -1
                )
                continue
            if cmd_kind == "down" or cmd_kind == "d":
                cur_ctx, print_ins = call_stack_navigate(
                    cur_ctx, self.ctx_stack + [self.ctx], 1
                )
                continue
            if cmd_kind == "exit":
                return


def call_stack_navigate(
    cur_ctx: ExecCtx, stack: List[ExecCtx], dir: int
) -> Tuple[ExecCtx, bool]:
    if cur_ctx == stack[0] and dir == -1:
        print("Can't go any more up")
        return cur_ctx, False
    if cur_ctx == stack[-1] and dir == 1:
        print("Can't go any more down")
        return cur_ctx, False
    for i, ctx in enumerate(stack):
        if ctx == cur_ctx:
            cur_ctx = stack[i + dir]
            break
    return cur_ctx, True


def print_call_stack(cur_ctx: ExecCtx, stack: List[ExecCtx]):
    for i, ctx in enumerate(reversed(stack)):
        start = ["  ", "->"][cur_ctx == ctx]
        word = ["from", "  at"][i == 0]
        n, o = ctx.f_name, (ctx.cur_instr + int(i == 0))
        print(f"{start} {word} {n}:{o}")


def print_stack(ctx: ExecCtx, stack_memory: bytes):
    print("Stack variables:")
    for ent, (o, s) in zip(ctx.cur_function.stack_frame, ctx.local_offsets):
        addr = o + MemKind.STACK.offset()
        data = stack_memory[o : o + s]
        print(f"  #{addr:09X} ({ent.name}): {data}")
    end_o = 0
    if len(ctx.local_offsets) > 0:
        end_o = ctx.local_offsets[-1][0] + ctx.local_offsets[-1][1]
    stack = stack_memory[end_o:]
    stack_count = (len(stack_memory) - end_o) // 8
    print("Stack:")
    for i in range(stack_count):
        name = f"STACK-{stack_count - i + 1}"
        addr = MemKind.STACK.offset() + end_o + i * 8
        d = struct.unpack("Q", stack[i * 8 : i * 8 + 8])[0]
        print(f"  #{addr:09X} ({name}): 0x{d:X}")


def print_globals(globals, memory):
    if len(globals) == 0:
        print("No global variables.")
        return
    print("Global variables:")
    for i in range(len(globals) - 1):
        n = globals[i][1]
        o = globals[i][0]
        s = globals[i + 1][0] - o
        addr = o + MemKind.GLOB.offset()
        data = memory[o : o + s]
        print(f"  #{addr:09X} ({n}): {data}")
    n = globals[-1][1]
    o = globals[-1][0]
    addr = o + MemKind.GLOB.offset()
    data = memory[o:]
    print(f"  #{addr:09X} ({n}): {data}")
