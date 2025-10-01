from dataclasses import dataclass
from typing_extensions import List
from ir2.data.instrs._base import Instr
from ir2.data.types import IrType
from ir2.data.value import IrValue, Use


@dataclass
class BinaryInstr(Instr):
    def __init__(self, name: str, lhs: IrValue, rhs: IrValue):
        assert lhs.ty == rhs.ty
        super().__init__(lhs.ty, name)
        self.operands.append(Use(lhs, self))
        self.operands.append(Use(rhs, self))

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

    def instr_name(self) -> str:
        return self.__class__.__name__.lower().removesuffix("instr")

    def to_txt(self) -> str:
        return f"{self.vname()} = {self.instr_name()} {self.ty} {self.lhs.vname()} {self.rhs.vname()}"

    def set_operands(self, operands: List[IrValue]):
        assert len(operands) == 2
        self.lhs = operands[0]
        self.rhs = operands[1]


@dataclass
class AddInstr(BinaryInstr):
    no_unsigned_wrap: bool
    no_signed_wrap: bool

    def __init__(
        self,
        name: str,
        lhs: IrValue,
        rhs: IrValue,
        nuw: bool = False,
        nsw: bool = False,
    ):
        super().__init__(name, lhs, rhs)
        self.no_unsigned_wrap = nuw
        self.no_signed_wrap = nsw

    def instr_name(self) -> str:
        return (
            "add"
            + ("", " nuw")[self.no_unsigned_wrap]
            + ("", " nsw")[self.no_signed_wrap]
        )


@dataclass
class SubInstr(BinaryInstr):
    no_unsigned_wrap: bool
    no_signed_wrap: bool

    def __init__(
        self,
        name: str,
        lhs: IrValue,
        rhs: IrValue,
        nuw: bool = False,
        nsw: bool = False,
    ):
        super().__init__(name, lhs, rhs)
        self.no_unsigned_wrap = nuw
        self.no_signed_wrap = nsw

    def instr_name(self) -> str:
        return (
            "sub"
            + ("", " nuw")[self.no_unsigned_wrap]
            + ("", " nsw")[self.no_signed_wrap]
        )


@dataclass
class MulInstr(BinaryInstr):
    no_unsigned_wrap: bool
    no_signed_wrap: bool

    def __init__(
        self,
        name: str,
        lhs: IrValue,
        rhs: IrValue,
        nuw: bool = False,
        nsw: bool = False,
    ):
        super().__init__(name, lhs, rhs)
        self.no_unsigned_wrap = nuw
        self.no_signed_wrap = nsw

    def instr_name(self) -> str:
        return (
            "mul"
            + ("", " nuw")[self.no_unsigned_wrap]
            + ("", " nsw")[self.no_signed_wrap]
        )


@dataclass
class UdivInstr(BinaryInstr):
    exact: bool

    def __init__(
        self,
        name: str,
        lhs: IrValue,
        rhs: IrValue,
        exact: bool = False,
    ):
        super().__init__(name, lhs, rhs)
        self.exact = exact

    def instr_name(self) -> str:
        return "udiv" + ("", " exact")[self.exact]


@dataclass
class SdivInstr(BinaryInstr):
    exact: bool

    def __init__(
        self,
        name: str,
        lhs: IrValue,
        rhs: IrValue,
        exact: bool = False,
    ):
        super().__init__(name, lhs, rhs)
        self.exact = exact

    def instr_name(self) -> str:
        return "sdiv" + ("", " exact")[self.exact]


@dataclass
class UremInstr(BinaryInstr):
    def __init__(
        self,
        name: str,
        lhs: IrValue,
        rhs: IrValue,
    ):
        super().__init__(name, lhs, rhs)


@dataclass
class SremInstr(BinaryInstr):
    def __init__(
        self,
        name: str,
        lhs: IrValue,
        rhs: IrValue,
    ):
        super().__init__(name, lhs, rhs)


@dataclass
class ShlInstr(BinaryInstr):
    no_unsigned_wrap: bool
    no_signed_wrap: bool

    def __init__(
        self,
        name: str,
        lhs: IrValue,
        rhs: IrValue,
        nuw: bool = False,
        nsw: bool = False,
    ):
        super().__init__(name, lhs, rhs)
        self.no_unsigned_wrap = nuw
        self.no_signed_wrap = nsw

    def instr_name(self) -> str:
        return (
            "shl"
            + ("", " nuw")[self.no_unsigned_wrap]
            + ("", " nsw")[self.no_signed_wrap]
        )


@dataclass
class AshrInstr(BinaryInstr):
    exact: bool

    def __init__(
        self,
        name: str,
        lhs: IrValue,
        rhs: IrValue,
        exact: bool = False,
    ):
        super().__init__(name, lhs, rhs)
        self.exact = exact

    def instr_name(self) -> str:
        return "ashr" + ("", " exact")[self.exact]


@dataclass
class LshrInstr(BinaryInstr):
    exact: bool

    def __init__(
        self,
        name: str,
        lhs: IrValue,
        rhs: IrValue,
        exact: bool = False,
    ):
        super().__init__(name, lhs, rhs)
        self.exact = exact

    def instr_name(self) -> str:
        return "lshr" + ("", " exact")[self.exact]


@dataclass
class AndInstr(BinaryInstr):
    def __init__(
        self,
        name: str,
        lhs: IrValue,
        rhs: IrValue,
    ):
        super().__init__(name, lhs, rhs)


@dataclass
class OrInstr(BinaryInstr):
    def __init__(
        self,
        name: str,
        lhs: IrValue,
        rhs: IrValue,
    ):
        super().__init__(name, lhs, rhs)


@dataclass
class XorInstr(BinaryInstr):
    def __init__(
        self,
        name: str,
        lhs: IrValue,
        rhs: IrValue,
    ):
        super().__init__(name, lhs, rhs)
