from typing import List, Any
from dataclasses import dataclass
from enum import Enum, auto


class IrInstrKind(Enum):
    # memory/stack
    STV = auto()    # STORE_VAR     STV  var_id: int                       => pops value into slot var_id
    STA = auto()    # STORE_ADDR    STA  store_size: int                   => pops address and value, then store at that address
    LDA = auto()    # LOAD_ADDR     LDA  load_size: int                    => pops address and pushes value found at that address
    PSH = auto()    # PUSH          PSH  v: thing                          => push value onto the stack
    DUP = auto()    # DUP           DUP                                    => duplicates top of stack
    DRP = auto()    # DROP          DRP                                    => drops the top of the stack
    VAA = auto()    # VAARG         VAA  is_float: bool                    => push the next va argument onto the stack

    # control
    LBL = auto()    # LABEL         LBL  lbl: str                          => creates a label to jump to
    JMP = auto()    # JMP           JMP  lbl: str                          => jumps to a label
    JZO = auto()    # JZ            JZO  lbl: str                          => pops top of stack and jumps if zero
    JNZ = auto()    # JNZ           JNZ  lbl: str                          => pops top of stack and jumps if not zero

    # call
    CAL = auto()    # CALL          CAL  fn: str       nparams: int        => calls a function by name with nparams on the stack. puts the return value on the stack if it exists
    BUI = auto()    # BUILTIN       BUI  fn: str       nparams: int        => same as call but for builtins
    RET = auto()    # RET           RET                                    => exits the function
    RTV = auto()    # RETVAL        RTV                                    => pops the top of the stack and returns it

    # Arithmetic
    NEG = auto()    # NEG           NEG                                    => negates the top of the stack
    INV = auto()    # INV           INV                                    => bitwise invert the top of the stack
    NOT = auto()    # NOT           NOT                                    => logical not to top of stack
    ADD = auto()    # ADD           ADD  v: thing                          => adds the top of the stack (maybe with thing)
    SUB = auto()    # SUB           SUB  v: thing                          => subtracts the top of the stack (maybe with thing)
    MUL = auto()    # MUL           MUL  v: thing                          => multiply
    DIV = auto()    # DIV           DIV  v: thing                          => divide
    REM = auto()    # REM           REM  v: thing                          => remainder
    SHL = auto()    # SHL           SHL  v: thing                          => left shift
    SHR = auto()    # SHR           SHR  v: thing                          => right shift
    AND = auto()    # AND           AND  v: thing                          => binary and
    XOR = auto()    # XOR           XOR  v: thing                          => binary xor
    IOR = auto()    # IOR           IOR  v: thing                          => binary or
    LTH = auto()    # LTH           LTH  v: thing                          => less than
    GTH = auto()    # GTH           GTH  v: thing                          => greater than
    LEQ = auto()    # LEQ           LEQ  v: thing                          => less equal
    GEQ = auto()    # GEQ           GEQ  v: thing                          => greater equal
    EQU = auto()    # EQU           EQU  v: thing                          => equals
    NEQ = auto()    # NEQ           NEQ  v: thing                          => not equals



@dataclass
class IrInstr:
    opcode: IrInstrKind
    operand1: Any
    operand2: Any

    def special_arg_str(self) -> str:
        if self.operand1 is None:
            return ""
        if self.operand1 == 0:
            return f"    {self.operand2}"
        if self.operand1 == 1:
            return f"    Var[{self.operand2}]"
        if self.operand1 == 2:
            return f"    [{self.operand2}]"
        if self.operand1 == 3:
            return f"    [global_{self.operand2}]"
        if self.operand1 == 4:
            return f"    Var[global_{self.operand2}]"
        return ""

    def __str__(self) -> str:
        if self.opcode == IrInstrKind.LBL:
            return f" .{self.operand1}:"
        out = f"    {self.opcode.name}"
        operand_count = 0
        match self.opcode:
            case IrInstrKind.ADD | IrInstrKind.SUB | IrInstrKind.PSH | IrInstrKind.MUL | IrInstrKind.DIV |  \
                IrInstrKind.REM | IrInstrKind.SHL | IrInstrKind.SHR | IrInstrKind.AND | IrInstrKind.IOR |  \
                IrInstrKind.XOR | IrInstrKind.EQU | IrInstrKind.NEQ | IrInstrKind.GTH | IrInstrKind.LTH |  \
                IrInstrKind.LEQ | IrInstrKind.GEQ:
                out += self.special_arg_str()
            case IrInstrKind.STV | IrInstrKind.LBL | IrInstrKind.JMP | IrInstrKind.JZO | IrInstrKind.JNZ | IrInstrKind.VAA:
                operand_count = 1
            case IrInstrKind.CAL | IrInstrKind.BUI:
                operand_count = 2
            case _:
                operand_count = 0
        if operand_count > 0:
            out += f"    {self.operand1}"
        if operand_count > 1:
            out += f", {self.operand2}"
        return out


@dataclass
class IrGlobal:
    data: bytes
    is_lib: bool
    is_ro: bool
    name: str

    def __init__(self, data, is_lib, is_ro, name = "####"):
        self.data = data
        self.is_lib = is_lib
        self.is_ro = is_ro
        self.name = name


@dataclass
class StackFrameEntry:
    size: int
    align: int
    name: str # for debugging


@dataclass
class ParamEntry:
    size: int
    is_float: bool


@dataclass
class FunctionIr:
    returns_value: bool
    instructions: List[IrInstr]
    stack_frame: List[StackFrameEntry] # includes non VAARGS params and local variables
    params: List[ParamEntry]
    is_vararg: bool
    is_lib: bool


@dataclass
class FullIr:
    functions: dict
    globs: List[IrGlobal]

    def has_global(self, n):
        for i, g in enumerate(self.globs):
            if g.name == n:
                return i
        return -1

    def __init__(self):
        self.functions = {}
        self.globs = []
