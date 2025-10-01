from dataclasses import dataclass
from typing import List

from .types import IrType


@dataclass
class Use:
    v: "IrValue"
    user: "User"

    def __init__(self, v: "IrValue", u: "User"):
        self.user = u
        self.v = v
        v.uses.append(self)

    def remove_use(self):
        for i in range(len(self.v.uses)):
            if self.v.uses[i] is self:
                self.v.uses.pop(i)
                break

    def set(self, new_value: "IrValue"):
        self.remove_use()
        self.v = new_value
        self.v.uses.append(self)

    def get(self) -> "IrValue":
        return self.v


@dataclass
class IrValue:
    ty: IrType
    uses: List[Use]
    name: str | None

    def __init__(self, ty: IrType, name: str | None = None):
        self.ty = ty
        self.uses = []
        self.name = name

    def __eq__(self, o):
        return self is o

    def vname(self) -> str:
        return "%unk%" if self.name is None else f"%{self.name}"

    def replace_uses(self, new: "IrValue"):
        u_cpy = [u for u in self.uses]
        for u in u_cpy:
            u.set(new)


@dataclass
class User(IrValue):
    operands: List[Use]

    def __init__(self, ty: IrType, name: str | None = None):
        super().__init__(ty, name)
        self.operands = []

    def remove_operands(self):
        for o in self.operands:
            o.remove_use()
        self.operands = []


@dataclass
class Constant(User):
    def __init__(self, ty: IrType):
        super().__init__(ty, None)


@dataclass
class GlobalValue(Constant):
    # visibility
    def __init__(self, ty: IrType, name: str):
        super().__init__(ty)
        self.name = name

    def vname(self) -> str:
        assert self.name
        return f"@{self.name}"


@dataclass
class GlobalObject(GlobalValue):
    def __init__(self, ty: IrType, name: str):
        super().__init__(ty, name)


@dataclass
class ConstantData(Constant):
    def __init__(self, ty: IrType):
        super().__init__(ty)


@dataclass
class ConstantInt(IrValue):
    val: int

    def __eq__(self, o: IrValue):
        if not isinstance(o, ConstantInt):
            return False
        return self.val == o.val and self.ty == o.ty

    def __init__(self, ty: IrType, val: int):
        super().__init__(ty)
        self.val = val

    def vname(self):
        return f"${self.val}"


@dataclass
class IrPoisonValue(IrValue):
    def __init__(self):
        pass

    def vname(self):
        return "poison"

@dataclass
class IrUndefValue(IrValue):
    def __init__(self):
        pass

    def vname(self):
        return "undef"
