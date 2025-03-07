from dataclasses import dataclass
from utils.my_enum import Enum, ENUM_INIT, ENUM_N
from lex import Token, Tok


@dataclass
class Type:
    def get_size(self) -> int:
        return 0

    def get_align(self) -> int:
        return 1

    def is_arithmetic_type(self) -> bool:
        return False

    def is_integer_type(self) -> bool:
        return False

    def is_integral_or_enumeration_type(self) -> bool:
        return self.is_integer_type()

    def get_unqualified(self):
        return self

    def is_scalar_type(self) -> bool:
        return False

    def __str__(self) -> str:
        return "void"


class BuiltinTypeKind(Enum):
    I8 = ENUM_INIT()
    I16 = ENUM_N()
    I32 = ENUM_N()
    I64 = ENUM_N()
    U8 = ENUM_N()
    U16 = ENUM_N()
    U32 = ENUM_N()
    U64 = ENUM_N()
    BOOL = ENUM_N()


@dataclass
class AliasType(Type):
    aliased_type: Type

    def get_size(self) -> int:
        return self.aliased_type.get_size()

    def get_align(self) -> int:
        return self.aliased_type.get_align()

    def get_aliased_type(self) -> Type:
        t = self
        while isinstance(t.aliased_type, AliasType) or isinstance(t.aliased_type, EnumType):
            t = t.aliased_type
        return t.aliased_type

    def __str__(self) -> str:
        return f"{self.get_aliased_type()}"


@dataclass
class EnumType(Type):
    name: str
    aliased_type: Type | None
    variants: "List[EnumVariantDecl]"
    def get_size(self) -> int:
        return self.aliased_type.get_size() if self.aliased_type is not None else 8

    def get_align(self) -> int:
        return self.aliased_type.get_align() if self.aliased_type is not None else 8

    def get_aliased_type(self) -> Type:
        t = self
        while isinstance(t.aliased_type, AliasType) or isinstance(t.aliased_type, EnumType):
            t = t.aliased_type
        return t.aliased_type

    def __str__(self) -> str:
        return f"enum {self.name}" if self.aliased_type is None else f"{self.get_aliased_type()}"


@dataclass
class StructType(Type):
    name: str
    fields: "List[FieldDecl]"
    is_incomplete: bool

    def __str__(self) -> str:
        return f"struct {self.name}"

    def get_align(self) -> int:
        cur_align = 1
        for f in self.fields:
            f_align = f.ty.get_align()
            if f_align > cur_align:
                cur_align = f_align
        return cur_align

    def get_size(self) -> int:
        cur_size = 0
        for f in self.fields:
            needs_align = f.ty.get_align()
            if (a := (cur_size % needs_align)) != 0:
                cur_size += needs_align - a
            cur_size += f.ty.get_size()
        return cur_size


@dataclass
class BuiltinType(Type):
    kind: BuiltinTypeKind

    @staticmethod
    def get_from_tok(token: Token) -> "BuiltinType":
        match token.ty:
            case Tok.KW_I8:
                return BuiltinType(BuiltinTypeKind.I8)
            case Tok.KW_I16:
                return BuiltinType(BuiltinTypeKind.I16)
            case Tok.KW_I32:
                return BuiltinType(BuiltinTypeKind.I32)
            case Tok.KW_I64:
                return BuiltinType(BuiltinTypeKind.I64)
            case Tok.KW_U8:
                return BuiltinType(BuiltinTypeKind.U8)
            case Tok.KW_U16:
                return BuiltinType(BuiltinTypeKind.U16)
            case Tok.KW_U32:
                return BuiltinType(BuiltinTypeKind.U32)
            case Tok.KW_U64:
                return BuiltinType(BuiltinTypeKind.U64)
            case Tok.KW_BOOL:
                return BuiltinType(BuiltinTypeKind.BOOL)
            case _:
                assert False, f"unknown builtin type {token.value}"

    def get_size(self) -> int:
        match self.kind:
            case BuiltinTypeKind.I64 | BuiltinTypeKind.U64: return 8
            case BuiltinTypeKind.I32 | BuiltinTypeKind.U32: return 4
            case BuiltinTypeKind.I16 | BuiltinTypeKind.U16: return 2
            case BuiltinTypeKind.I8 | BuiltinTypeKind.U8: return 1
            case BuiltinTypeKind.BOOL: return 1

    def get_align(self) -> int:
        return self.get_size()

    def is_arithmetic_type(self) -> bool:
        return self.kind.value >= BuiltinTypeKind.I8.value and self.kind.value <= BuiltinTypeKind.BOOL.value

    def is_integer_type(self) -> bool:
        return self.kind.value >= BuiltinTypeKind.I8.value and self.kind.value <= BuiltinTypeKind.BOOL.value

    def is_scalar_type(self) -> bool:
        return self.kind.value >= BuiltinTypeKind.I8.value and self.kind.value <= BuiltinTypeKind.BOOL.value

    def __str__(self) -> str:
        return self.kind.name.lower()


@dataclass
class PointerType(Type):
    subtype: Type

    def get_size(self) -> int:  # TODO: might be architecture dependant
        return 8

    def get_align(self) -> int:
        return 8

    def __str__(self) -> str:
        subtype_str = f"{self.subtype}"
        if not subtype_str.endswith("*"):
            subtype_str += " "
        return subtype_str + "*"

    def is_scalar_type(self) -> bool:
        return True


@dataclass
class ArrayType(Type):
    subtype: Type
    count: int

    def get_size(self) -> int:
        return self.subtype.get_size() * self.count

    def get_align(self) -> int:
        return self.subtype.get_align()

    def __str__(self) -> str:
        return f"{self.subtype}[{self.count}]"


@dataclass
class FunctionType(Type):
    return_type: Type | None
    param_types: [Type]

    def __str__(self) -> str:
        rt_str = f"{self.return_type}"
        ptype_str = ""
        if len(self.param_types) != 0:
            ptype_str = f"{self.param_types[0]}"
            if len(self.param_types) > 1:
                for d in self.param_types[1:]:
                    ptype_str = f"{ptype_str}, {d}"
        return f"{rt_str} ({ptype_str})"


def type_is_void(t: Type) -> bool:
    return t is None or t == Type()
