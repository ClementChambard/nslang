from typing import List
from dataclasses import dataclass
from . import IrInstrKind, FullIr


@dataclass
class InstrPtr:
    fn_name: str
    instr_num: int


@dataclass
class StackFrame:
    stack: List[int]
    variables: List[int]
    old_ip: InstrPtr | None


@dataclass
class InterpState:
    ip: InstrPtr | None
    cur_stack_frame: StackFrame
    stack_frames: List[StackFrame]
    ir: FullIr

    def __init__(self, ir: FullIr):
        self.ir = ir
        self.stack_frames = []
        self.cur_stack_frame = StackFrame([], [0] * len(ir.functions["main"].stack_frame), None)
        self.ip = InstrPtr("main", 0)

    def run(self):
        while self.ip is not None:
            self.exec_instr()

    def stack_push(self, val: int):
        self.cur_stack_frame.stack.append(val)

    def stack_pop(self) -> int:
        return self.cur_stack_frame.stack.pop()

    def set_var(self, i: int, val: int):
        self.cur_stack_frame.variables[i] = val

    def get_var(self, i: int) -> int:
        return self.cur_stack_frame.variables[i]

    def push_context(self, fn_name: str, args: List[int]):
        self.stack_frames.append(self.cur_stack_frame)
        self.cur_stack_frame = StackFrame(args, [0] * len(self.ir.functions[fn_name].stack_frame), self.ip)
        self.ip = InstrPtr(fn_name, 0)

    def pop_context(self):
        self.ip = self.cur_stack_frame.old_ip
        if self.ip is not None:
            self.cur_stack_frame = self.stack_frames.pop()

    def jump_to_label(self, lbl):
        for i, ins in enumerate(self.ir.functions[self.ip.fn_name].instructions):
            if ins.opcode != IrInstrKind.LBL:
                continue
            if ins.operand1 == lbl:
                self.ip.instr_num = i + 1
                return
        assert False, f"Error: LABEL '{lbl}' NOT FOUND"

    def get_val(self, operand1, operand2):
        if operand1 is None:
            return self.stack_pop(), True
        elif operand1 == 0:
            return operand2, False
        elif operand1 == 1:
            return self.get_var(operand2), False
        elif operand1 == 2:
            return operand2 + 0x100000, False # * stack_id
        elif operand1 == 3:
            return operand2 + 0x80000000, False
        else:
            assert False

    def exec_instr(self):
        assert self.ip is not None, "No instruction to execute (Invalid IP)"
        i = self.ir.functions[self.ip.fn_name].instructions[self.ip.instr_num]
        self.ip.instr_num += 1
        match i.opcode:
            case IrInstrKind.PRM:
                args = self.cur_stack_frame.stack
                assert len(args) == i.operand1
                self.cur_stack_frame.stack = []
                for i, v in enumerate(args):
                    self.set_var(i, v)
            case IrInstrKind.STV:
                self.set_var(i.operand1, self.stack_pop())
            case IrInstrKind.STA:
                addr = self.stack_pop()
                to_store = self.stack_pop()
                print(f"STA({i.operand1}) #{addr} => {to_store}")
            case IrInstrKind.LDA:
                addr = self.stack_pop()
                print(f"LDA({i.operand1}) #{addr}")
                self.stack_push(0)
            case IrInstrKind.PSH:
                self.stack_push(self.get_val(i.operand1, i.operand2)[0])
            case IrInstrKind.ADD:
                op_1 = self.stack_pop()
                op_2, _ = self.get_val(i.operand1, i.operand2)
                self.stack_push(op_1 + op_2)
            case IrInstrKind.MUL:
                op_1 = self.stack_pop()
                op_2, _ = self.get_val(i.operand1, i.operand2)
                self.stack_push(op_1 * op_2)
            case IrInstrKind.AND:
                op_1 = self.stack_pop()
                op_2, _ = self.get_val(i.operand1, i.operand2)
                self.stack_push(op_1 & op_2)
            case IrInstrKind.XOR:
                op_1 = self.stack_pop()
                op_2, _ = self.get_val(i.operand1, i.operand2)
                self.stack_push(op_1 ^ op_2)
            case IrInstrKind.IOR:
                op_1 = self.stack_pop()
                op_2, _ = self.get_val(i.operand1, i.operand2)
                self.stack_push(op_1 | op_2)
            case IrInstrKind.SUB:
                op_1 = self.stack_pop()
                op_2, s = self.get_val(i.operand1, i.operand2)
                if s:
                    self.stack_push(op_2 - op_1)
                else:
                    self.stack_push(op_1 - op_2)
            case IrInstrKind.DIV:
                op_1 = self.stack_pop()
                op_2, s = self.get_val(i.operand1, i.operand2)
                if s:
                    self.stack_push(op_2 // op_1)
                else:
                    self.stack_push(op_1 // op_2)
            case IrInstrKind.REM:
                op_1 = self.stack_pop()
                op_2, s = self.get_val(i.operand1, i.operand2)
                if s:
                    self.stack_push(op_2 % op_1)
                else:
                    self.stack_push(op_1 % op_2)
            case IrInstrKind.SHL:
                op_1 = self.stack_pop()
                op_2, s = self.get_val(i.operand1, i.operand2)
                if s:
                    self.stack_push(op_2 << op_1)
                else:
                    self.stack_push(op_1 << op_2)
            case IrInstrKind.SHR:
                op_1 = self.stack_pop()
                op_2, s = self.get_val(i.operand1, i.operand2)
                if s:
                    self.stack_push(op_2 >> op_1)
                else:
                    self.stack_push(op_1 >> op_2)
            case IrInstrKind.GTH:
                op_1 = self.stack_pop()
                op_2, s = self.get_val(i.operand1, i.operand2)
                if s:
                    self.stack_push(int(op_2 > op_1))
                else:
                    self.stack_push(int(op_1 > op_2))
            case IrInstrKind.LTH:
                op_1 = self.stack_pop()
                op_2, s = self.get_val(i.operand1, i.operand2)
                if s:
                    self.stack_push(int(op_2 < op_1))
                else:
                    self.stack_push(int(op_1 < op_2))
            case IrInstrKind.GEQ:
                op_1 = self.stack_pop()
                op_2, s = self.get_val(i.operand1, i.operand2)
                if s:
                    self.stack_push(int(op_2 >= op_1))
                else:
                    self.stack_push(int(op_1 >= op_2))
            case IrInstrKind.LEQ:
                op_1 = self.stack_pop()
                op_2, s = self.get_val(i.operand1, i.operand2)
                if s:
                    self.stack_push(int(op_2 <= op_1))
                else:
                    self.stack_push(int(op_1 <= op_2))
            case IrInstrKind.EQU:
                op_1 = self.stack_pop()
                op_2, s = self.get_val(i.operand1, i.operand2)
                if s:
                    self.stack_push(int(op_2 == op_1))
                else:
                    self.stack_push(int(op_1 == op_2))
            case IrInstrKind.NEQ:
                op_1 = self.stack_pop()
                op_2, s = self.get_val(i.operand1, i.operand2)
                if s:
                    self.stack_push(int(op_2 != op_1))
                else:
                    self.stack_push(int(op_1 != op_2))
            case IrInstrKind.NEG:
                self.stack_push(-self.stack_pop())
            case IrInstrKind.INV:
                self.stack_push(~self.stack_pop())
            case IrInstrKind.NOT:
                self.stack_push(int(not self.stack_pop()))
            case IrInstrKind.DRP:
                self.stack_pop()
            case IrInstrKind.RET:
                self.pop_context()
            case IrInstrKind.RTV:
                ret_val = self.stack_pop()
                self.pop_context()
                self.stack_push(ret_val)
            case IrInstrKind.CAL:
                args = [self.stack_pop() for _ in range(i.operand2)]
                self.push_context(i.operand1, args)
            case IrInstrKind.LBL:
                self.exec_instr()
            case IrInstrKind.JMP:
                self.jump_to_label(i.operand1)
            case IrInstrKind.JZO:
                top = self.stack_pop()
                if top == 0:
                    self.jump_to_label(i.operand1)
            case IrInstrKind.JNZ:
                top = self.stack_pop()
                if top != 0:
                    self.jump_to_label(i.operand1)
            case IrInstrKind.BUI:
                if i.operand1 == "__builtin_hexdump" and i.operand2 == 1:
                    print(f"0x{self.stack_pop():X}")
                    self.stack_push(0)
                elif i.operand1 == "__builtin_syscall":
                    print("syscall ", end='')
                    print(self.stack_pop(), end= '')
                    for _ in range(i.operand2 - 1):
                        print(",", self.stack_pop(), end= '')
                    print()
                    self.stack_push(0)
            case _: assert False, f"{i.opcode}"


def run_ir_interpreter(ir: FullIr):
    # interp = InterpState(ir)
    # interp.run()
    pass
