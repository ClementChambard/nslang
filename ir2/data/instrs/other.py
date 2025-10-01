from dataclasses import dataclass
from enum import Enum, auto
from typing import List

from ir2.data.instrs._base import Instr
from ir2.data.types import IrType
from ir2.data.value import IrValue, Use


class IcmpCondKind(Enum):
    EQ = auto()
    NE = auto()
    UGT = auto()
    ULT = auto()
    UGE = auto()
    ULE = auto()
    SGT = auto()
    SLT = auto()
    SGE = auto()
    SLE = auto()

    def is_equality(self) -> bool:
        return self == self.EQ or self == self.NE


@dataclass
class IcmpInstr(Instr):
    cond: IcmpCondKind

    def __init__(
        self, name: str, ty: IrType, cond: IcmpCondKind, lhs: IrValue, rhs: IrValue
    ):
        super().__init__(ty, name)
        self.operands.append(Use(lhs, self))
        self.operands.append(Use(rhs, self))
        self.cond = cond

    @property
    def lhs(self) -> IrValue:
        return self.operands[0].get()

    @lhs.setter
    def lhs(self, v: IrValue):
        return self.operands[0].set(v)

    @property
    def rhs(self) -> IrValue:
        return self.operands[1].get()

    @rhs.setter
    def rhs(self, v: IrValue):
        return self.operands[1].set(v)

    def to_txt(self) -> str:
        return f"{self.vname()} = icmp {self.cond.name.lower()} {self.ty} {self.lhs.vname()} {self.rhs.vname()}"


@dataclass
class CallInstr(Instr):
    def __init__(self, name: str, ty: IrType, function: IrValue, args: List[IrValue]):
        super().__init__(ty, name)
        self.operands.append(Use(function, self))
        [self.operands.append(Use(a, self)) for a in args]

    @property
    def function(self) -> IrValue:
        return self.operands[0].get()

    @function.setter
    def function(self, v: IrValue):
        return self.operands[0].set(v)

    @property
    def args(self) -> List[IrValue]:
        return [a.get() for a in self.operands[1:]]

    def set_arg(self, i: int, v: IrValue):
        self.operands[1 + i].set(v)

    def to_txt(self) -> str:
        out = f"{self.vname()} = call {self.ty} {self.function.vname()}"
        for a in self.args:
            out += f" {a.vname()}"
        return out


@dataclass
class PhiInstr(Instr):
    def __init__(self, name: str, ty: IrType):
        super().__init__(ty, name)

    @property
    def coerce_vals(self) -> List[IrValue]:
        return [a.get() for a in self.operands[::2]]

    @property
    def coerce_blocks(self) -> List["Block"]:
        return [a.get() for a in self.operands[1::2]]

    @property
    def num_incoming(self) -> int:
        return len(self.operands) // 2

    def to_txt(self) -> str:
        out = f"{self.vname()} = phi {self.ty}"
        for b, v in zip(self.coerce_blocks, self.coerce_vals):
            out += f" [ .{b.name}, {v.vname()} ]"
        return out

    def add_incoming(self, v: IrValue, b: "Block"):
        self.operands.append(Use(v, self))
        self.operands.append(Use(b, self))

    def get_val_operand_for_incoming_block(self, b: "Block") -> Use | None:
        i = 0
        for i in range(self.num_incoming):
            if self.operands[i * 2+ 1].v is b:
                return self.operands[i * 2]
        return None

    def is_identical_to_when_defined(self, o: Instr) -> bool:
        if not isinstance(o, PhiInstr):
            return False
        if self.ty != o.ty:
            return False
        if self.num_incoming != o.num_incoming:
            return False
        blocks1 = set([id(b) for b in self.coerce_blocks])
        blocks2 = set([id(b) for b in o.coerce_blocks])
        if blocks1 != blocks2:
            return False
        return self.coerce_vals == o.coerce_vals
