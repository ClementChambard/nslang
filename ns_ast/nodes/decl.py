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
class TranslationUnitDecl(Decl):
    decls: List[Decl]

    def __init__(self, decls):
        src_range = (1, 1)
        if len(decls) > 0:
            src_range = (decls[0].get_range()[0], decls[-1].get_range()[1])
        super().__init__(src_range)
        self.decls = decls
        self.is_lib = False


@dataclass
class NamedDecl(Decl):
    name: str

    def __init__(self, src_range = (0, 0), name = ""):
        super().__init__(src_range)
        self.name = name


@dataclass
class ValueDecl(NamedDecl):
    ty: Type

    def __init__(self, src_range = (0, 0), name = "", ty = Type()):
        super().__init__(src_range, name)
        self.ty = ty


@dataclass
class TypeDecl(NamedDecl):
    ty: Type
    def __init__(self, src_range = (0, 0), name = "", ty = Type()):
        super().__init__(src_range, name)
        self.ty = ty


@dataclass
class EnumVariantDecl(ValueDecl):
    val: int
    def __init__(self, src_range = (0, 0), name = "", ty = Type(), val: int = 0):
        super().__init__(src_range, name, ty)
        self.val = val


@dataclass
class EnumDecl(TypeDecl):
    variants: List[EnumVariantDecl]
    def __init__(self, src_range = (0, 0), name = "", ty = Type()):
        super().__init__(src_range, name, ty)
        self.variants = []


@dataclass
class FieldDecl(ValueDecl):
    def __init__(self, src_range = (0, 0), name = "", ty = Type()):
        super().__init__(src_range, name, ty)


@dataclass
class StructDecl(TypeDecl):
    fields: List[FieldDecl]
    def __init__(self, src_range = (0, 0), name = "", ty = Type()):
        super().__init__(src_range, name, ty)
        self.fields = []


@dataclass
class VarDecl(ValueDecl):
    def __init__(self, src_range = (0, 0), name = "", ty = Type()):
        super().__init__(src_range, name, ty)


@dataclass
class ParamDecl(VarDecl):
    def __init__(self, src_range = (0, 0), name = "", ty = Type()):
        super().__init__(src_range, name, ty)


@dataclass
class FnDecl(ValueDecl):
    param_decls: List[ParamDecl]
    is_vararg: bool
    body: "CompoundStmt"

    def __init__(self, src_range, name, param_decls, return_type, is_vararg: bool = False):
        if return_type is None:
            return_type = Type()
        super().__init__(src_range, name, FunctionType(return_type, [p.ty for p in param_decls], is_vararg))
        self.body = None
        self.param_decls = param_decls
        self.is_vararg = is_vararg

    def set_body(self, body: "CompoundStmt"):
        self.body = body
        self.src_range = (self.src_range[0], body.get_range()[1])

    def get_params_range(self) -> LocRge:
        # TODO:
        if len(self.param_decls) > 0:
            return (self.param_decls[0].get_range()[0], self.param_decls[-1].get_range()[1])
        if self.body is None:
            return self.get_range()
        return (self.get_range()[0], self.body.get_range()[0])
