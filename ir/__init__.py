from .data import IrInstrKind, IrInstr, FunctionIr, IrGlobal, FullIr
from .from_ast import generate_ir
from .interpreter import run_ir_interpreter
from .print_ir import print_ir
from .optimize import optimize_ir
