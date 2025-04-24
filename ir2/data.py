from dataclasses import dataclass
from enum import Enum, auto
from typing import Any, List


@dataclass
class IrType:
    pass


@dataclass
class IrTypeInt(IrType):
    bitsize: int

    def __str__(self) -> str:
        return f"i{self.bitsize}"


@dataclass
class IrValue:
    ty: IrType
    name: str


@dataclass
class IrConstant:  # TODO: would this be only for int
    ty: IrType
    value: Any


@dataclass
class IrInstr:
    pass


@dataclass
class IrBlock:
    name: str
    instrs: List[IrInstr]


@dataclass
class IrInstrExpr:
    result: IrValue | None


@dataclass
class IrInstrTerm(IrInstr):
    pass


@dataclass
class IrInstrRet(IrInstrTerm):
    result: IrValue | IrConstant | None


@dataclass
class IrInstrBranch(IrInstrTerm):
    branch_to: IrBlock
    branch_cond: IrValue | IrConstant | None
    branch_else: IrBlock | None


class IrBinOpKind(Enum):
    ADD = auto()
    SUB = auto()


@dataclass
class IrInstrBinOp(IrInstrExpr):
    kind: IrBinOpKind
    lhs: IrValue | IrConstant
    rhs: IrValue | IrConstant


class IrLinkage(Enum):
    EXTERN = auto()
    PUBLIC = auto()
    PRIVATE = auto()


@dataclass
class IrGlobal:
    name: str
    linkage: IrLinkage


@dataclass
class IrFunction(IrGlobal):
    args: List[IrValue]
    return_ty: IrType
    blocks: List[IrBlock]
    is_vararg: bool


@dataclass
class IrTranslationUnit:
    functions: List[IrFunction]
