from dataclasses import dataclass
from typing import List, Tuple
from ir2.data.instrs._base import Instr
from ir2.data.types import IrVoidType
from ir2.data.value import IrValue, Use


@dataclass
class TermInstr(Instr):
    def __init__(self):
        super().__init__(IrVoidType(), None)

    @property
    def successors(self) -> List["Block"]:
        return []


@dataclass
class RetInstr(TermInstr):
    def __init__(self, ret_val: IrValue | None = None):
        super().__init__()
        if ret_val:
            self.operands.append(Use(ret_val, self))

    @property
    def ret_val(self) -> IrValue | None:
        if len(self.operands) == 0:
            return None
        return self.operands[0].get()

    @ret_val.setter
    def ret_val(self, v: IrValue | None):
        if v is None:
            if len(self.operands) > 0:
                self.operands[0].remove_use()
                self.operands = []
        else:
            if len(self.operands) == 0:
                self.operands.append(Use(v, self))
            else:
                self.operands[0].set(v)

    def to_txt(self) -> str:
        out = "ret"
        if self.ret_val:
            out += f" {self.ret_val.ty} {self.ret_val.vname()}"
        else:
            out += " void"
        return out

    def clone(self) -> Instr:
        return RetInstr(self.ret_val)


@dataclass
class BranchInstr(TermInstr):
    def __init__(
        self,
        true_block: "Block",
        false_block: "Block | None" = None,
        cond_val: IrValue | None = None,
    ):
        super().__init__()
        assert (false_block is None) == (cond_val is None)
        self.operands.append(Use(true_block, self))
        if false_block:
            self.operands.append(Use(false_block, self))
        if cond_val:
            self.operands.append(Use(cond_val, self))

    @property
    def true_block(self) -> "Block":
        return self.operands[0].get()

    @true_block.setter
    def true_block(self, v: "Block"):
        self.operands[0].set(v)

    @property
    def false_block(self) -> "Block":
        assert self.is_conditional
        return self.operands[1].get()

    @false_block.setter
    def false_block(self, v: "Block"):
        assert self.is_conditional
        self.operands[1].set(v)

    @property
    def cond(self) -> IrValue:
        assert self.is_conditional
        return self.operands[2].get()

    @cond.setter
    def cond(self, v: IrValue):
        assert self.is_conditional
        self.operands[2].set(v)

    def to_txt(self) -> str:
        out = "br "
        if self.is_conditional:
            out += f"{self.cond.ty} {self.cond.vname()} .{self.true_block.name} .{self.false_block.name}"
        else:
            out += f".{self.true_block.name}"
        return out

    @property
    def is_conditional(self) -> bool:
        return len(self.operands) > 1

    @property
    def successors(self) -> List["Block"]:
        if self.is_conditional:
            return [self.true_block, self.false_block]
        else:
            return [self.true_block]


@dataclass
class UnreachableInstr(TermInstr):
    def __init__(self):
        super().__init__()

    def to_txt(self) -> str:
        return "unreachable"
