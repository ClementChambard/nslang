from .data import IrInstrKind, IrInstr, FunctionIr, IrGlobal, FullIr, StackFrameEntry, ParamEntry
from .from_ast import generate_ir
from .print_ir import print_ir
from .optimize import optimize_ir
from .interpreter import Interpreter
