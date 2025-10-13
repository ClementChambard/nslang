from dataclasses import dataclass

from ir2.data.instrs._base import Instr
from ir2.data.types import IrType, IrTypeInt
from ir2.data.value import IrValue, Use


@dataclass
class ConvertInstr(Instr):
    from_ty: IrType

    def __init__(self, name: str, arg: IrValue, from_ty: IrType, to_type: IrType):
        super().__init__(to_type, name)
        self.from_ty = from_ty
        self.operands.append(Use(arg, self))
        assert self.from_ty == self.arg.ty

    @property
    def arg(self) -> IrValue:
        return self.operands[0].get()

    @arg.setter
    def arg(self, v: IrValue):
        self.operands[0].set(v)

    def instr_name(self) -> str:
        return self.__class__.__name__.lower().removesuffix("instr")

    def to_txt(self) -> str:
        return f"{self.vname()} = {self.instr_name()} {self.from_ty} {self.arg.vname()} to {self.ty}"


@dataclass
class TruncInstr(ConvertInstr):
    no_unsigned_wrap: bool
    no_signed_wrap: bool

    def __init__(
        self,
        name: str,
        arg0: IrValue,
        from_ty: IrType,
        to_type: IrType,
        nuw: bool = False,
        nsw: bool = False,
    ):
        super().__init__(name, arg0, from_ty, to_type)
        self.no_unsigned_wrap = nuw
        self.no_signed_wrap = nsw
        assert isinstance(from_ty, IrTypeInt) and isinstance(to_type, IrTypeInt)
        assert from_ty.size >= to_type.size

    def instr_name(self) -> str:
        return (
            "trunc"
            + ("", " nuw")[self.no_unsigned_wrap]
            + ("", " nsw")[self.no_signed_wrap]
        )


@dataclass
class SextInstr(ConvertInstr):
    def __init__(self, name: str, arg0: IrValue, from_ty: IrType, to_type: IrType):
        super().__init__(name, arg0, from_ty, to_type)
        assert isinstance(from_ty, IrTypeInt) and isinstance(to_type, IrTypeInt)
        assert from_ty.size <= to_type.size


@dataclass
class ZextInstr(ConvertInstr):
    def __init__(self, name: str, arg0: IrValue, from_ty: IrType, to_type: IrType):
        super().__init__(name, arg0, from_ty, to_type)
        assert isinstance(from_ty, IrTypeInt) and isinstance(to_type, IrTypeInt)
        assert from_ty.size <= to_type.size
