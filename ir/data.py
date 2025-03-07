from typing import List, Any
from dataclasses import dataclass

from utils.my_enum import Enum, ENUM_INIT, ENUM_N


class IrInstrKind(Enum):
    PRM = ENUM_INIT() # SETUP_PARAMS  PRM  nparams: int                      => puts the params in their var slots
    STV = ENUM_N()    # STORE_VAR     STV  var_id: int                       => pops value into slot var_id
    STA = ENUM_N()    # STORE_ADDR    STA  store_size: int                   => pops address and value, then store at that address
    LDA = ENUM_N()    # LOAD_ADDR     LDA  load_size: int                    => pops address and pushes value found at that address
    PSH = ENUM_N()    # PUSH          PSH  v: thing                          => push value onto the stack
    DUP = ENUM_N()    # DUP           DUP                                    => duplicates top of stack
    DRP = ENUM_N()    # DROP          DRP                                    => drops the top of the stack
    RET = ENUM_N()    # RET           RET                                    => exits the function
    RTV = ENUM_N()    # RETVAL        RTV                                    => pops the top of the stack and returns it
    CAL = ENUM_N()    # CALL          CAL  fn: str       nparams: int        => calls a function by name with nparams on the stack. puts the return value on the stack if it exists
    LBL = ENUM_N()    # LABEL         LBL  lbl: str                          => creates a label to jump to
    JMP = ENUM_N()    # JMP           JMP  lbl: str                          => jumps to a label
    JZO = ENUM_N()    # JZ            JZO  lbl: str                          => pops top of stack and jumps if zero
    JNZ = ENUM_N()    # JNZ           JNZ  lbl: str                          => pops top of stack and jumps if not zero
    BUI = ENUM_N()    # BUILTIN       BUI  fn: str       nparams: int        => same as call but for builtins
    NEG = ENUM_N()    # NEG           NEG                                    => negates the top of the stack
    INV = ENUM_N()    # INV           INV                                    => bitwise invert the top of the stack
    NOT = ENUM_N()    # NOT           NOT                                    => logical not to top of stack
    ADD = ENUM_N()    # ADD           ADD  v: thing                          => adds the top of the stack (maybe with thing)
    SUB = ENUM_N()    # SUB           SUB  v: thing                          => subtracts the top of the stack (maybe with thing)
    MUL = ENUM_N()    # MUL           MUL  v: thing                          => multiply
    DIV = ENUM_N()    # DIV           DIV  v: thing                          => divide
    REM = ENUM_N()    # REM           REM  v: thing                          => remainder
    SHL = ENUM_N()    # SHL           SHL  v: thing                          => left shift
    SHR = ENUM_N()    # SHR           SHR  v: thing                          => right shift
    AND = ENUM_N()    # AND           AND  v: thing                          => binary and
    XOR = ENUM_N()    # XOR           XOR  v: thing                          => binary xor
    IOR = ENUM_N()    # IOR           IOR  v: thing                          => binary or
    LTH = ENUM_N()    # LTH           LTH  v: thing                          => less than
    GTH = ENUM_N()    # GTH           GTH  v: thing                          => greater than
    LEQ = ENUM_N()    # LEQ           LEQ  v: thing                          => less equal
    GEQ = ENUM_N()    # GEQ           GEQ  v: thing                          => greater equal
    EQU = ENUM_N()    # EQU           EQU  v: thing                          => equals
    NEQ = ENUM_N()    # NEQ           NEQ  v: thing                          => not equals



@dataclass
class IrInstr:
    opcode: IrInstrKind
    operand1: Any
    operand2: Any


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
class FunctionIr:
    returns_value: bool
    instructions: List[IrInstr]
    stack_frame: List[int]
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
