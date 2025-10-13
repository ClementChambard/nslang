from dataclasses import dataclass
from typing_extensions import List
from ir2.data.instrs._base import Instr
from ir2.data.types import IrType
from ir2.data.value import IrValue, Use


@dataclass
class StoreInstr(Instr):
    ty: IrType

    def __init__(self, ty: IrType, addr: IrValue, val: IrValue):
        super().__init__(ty)
        self.operands.append(Use(addr, self))
        self.operands.append(Use(val, self))
        self.ty = ty

    @property
    def addr(self) -> IrValue:
        return self.operands[0].get()

    @addr.setter
    def addr(self, v: IrValue):
        self.operands[0].set(v)

    @property
    def val(self) -> IrValue:
        return self.operands[1].get()

    @val.setter
    def val(self, v: IrValue):
        self.operands[1].set(v)

    def to_txt(self) -> str:
        return f"store {self.ty} {self.addr.vname()} {self.val.vname()}"


@dataclass
class LoadInstr(Instr):
    def __init__(self, name: str, ty: IrType, addr: IrValue):
        super().__init__(ty, name)
        self.operands.append(Use(addr, self))

    @property
    def addr(self) -> IrValue:
        return self.operands[0].get()

    @addr.setter
    def addr(self, v: IrValue):
        self.operands[0].set(v)

    def to_txt(self) -> str:
        return f"{self.vname()} = load {self.ty} {self.addr.vname()}"


@dataclass
class LocalVarInstr(Instr):
    def __init__(self, name: str, ty: IrType):
        super().__init__(ty, name)

    def to_txt(self) -> str:
        return f"{self.vname()} = localvar {self.ty}"


@dataclass
class VaargInstr(Instr):
    def __init__(self, name: str, ty: IrType):
        super().__init__(ty, name)

    def to_txt(self) -> str:
        return f"{self.vname()} = vaarg {self.ty}"
