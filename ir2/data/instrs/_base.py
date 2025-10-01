from dataclasses import dataclass
from typing import Self

from ir2.data.types import IrType
from ir2.data.value import User


@dataclass
class Instr(User):
    parent: "Block | None"

    def __init__(self, ty: IrType, name: str | None = None):
        super().__init__(ty, name)
        self.parent = None

    def __eq__(self, o) -> bool:
        return self is o

    def to_txt(self) -> str:
        return "nop"

    def clone(self) -> Self:
        assert False, f"clone not implemented for instruction {self.__class__}"

    def is_identical_to_when_defined(self, o: Self) -> bool:
        return False

    def erase_from_parent(self):
        assert self.parent
        self.remove_operands()
        for i in range(len(self.parent.instructions)):
            if self.parent.instructions[i] is self:
                self.parent.instructions.pop(i)
                break
        self.parent = None

    def is_trivially_dead(self) -> bool:
        if len(self.uses) != 0:
            return False
        return False # TODO:
