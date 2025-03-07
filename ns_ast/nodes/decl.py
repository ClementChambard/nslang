from dataclasses import dataclass
from typing import List
from . import Type, FunctionType
from lex import LocRge


@dataclass
class Decl:
    src_range: LocRge
    is_lib: bool

    def __init__(self, src_range = (0, 0)):
        self.src_range = src_range
        self.is_lib = False

    def get_range(self) -> LocRge:
        return self.src_range


@dataclass
class NamedDecl(Decl):
    name: str

    def __init__(self, src_range = (0, 0), name = ""):
        super().__init__(src_range)
        self.name = name


@dataclass
class TypeDecl(NamedDecl):
    ty: Type
    def __init__(self, src_range = (0, 0), name = "", ty = Type()):
        super().__init__(src_range, name)
        self.ty = ty


@dataclass
class ValueDecl(NamedDecl):
    ty: Type

    def __init__(self, src_range = (0, 0), name = "", ty = Type()):
        super().__init__(src_range, name)
        self.ty = ty


@dataclass
class FieldDecl(ValueDecl):
    def __init__(self, src_range = (0, 0), name = "", ty = Type()):
        super().__init__(src_range, name, ty)


@dataclass
class EnumVariantDecl(ValueDecl):
    val: int
    def __init__(self, src_range = (0, 0), name = "", ty = Type(), val: int = 0):
        super().__init__(src_range, name, ty)
        self.val = val


@dataclass
class VarDecl(ValueDecl):
    def __init__(self, src_range = (0, 0), name = "", ty = Type()):
        super().__init__(src_range, name, ty)


@dataclass
class ParamDecl(ValueDecl):
    def __init__(self, src_range = (0, 0), name = "", ty = Type()):
        super().__init__(src_range, name, ty)


@dataclass
class FnDecl(ValueDecl):
    param_decls: List[ParamDecl]
    body: "CompoundStmt"

    def __init__(self, src_range, name, param_decls, return_type):
        if return_type is None:
            return_type = Type()
        super().__init__(src_range, name, FunctionType(return_type, [p.ty for p in param_decls]))
        self.body = None
        self.param_decls = param_decls

    def set_body(self, body: "CompoundStmt"):
        self.body = body
        self.src_range = (self.src_range[0], body.get_range()[1])
