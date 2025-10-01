from dataclasses import dataclass
from enum import Enum

from ns_ast.nodes.types import BuiltinType, EnumType, PointerType, Type, type_is_void


@dataclass
class IrType:
    def __str__(self) -> str:
        return "unk"


@dataclass
class IrTypeInt(IrType):
    size: int

    def __str__(self) -> str:
        return f"i{self.size}"


@dataclass
class IrPtrType(IrType):
    def __str__(self) -> str:
        return "ptr"

@dataclass
class IrLabelType(IrType):
    def __str__(self) -> str:
        return "label"


@dataclass
class IrVoidType(IrType):
    def __str__(self) -> str:
        return "void"


def ir2_get_type(ty: Type | None):
    if ty is None or type_is_void(ty):
        return IrVoidType()
    if isinstance(ty, BuiltinType):
        if ty.is_integer_type():
            return IrTypeInt(ty.get_bit_width())
    if isinstance(ty, PointerType):
        return IrPtrType()
    if isinstance(ty, EnumType):
        return ir2_get_type(ty.aliased_type) if ty.aliased_type else IrTypeInt(64)
    assert False, "unimplemented"
