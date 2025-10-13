from dataclasses import dataclass
from typing import Self
from . import Loc, LOC_INVALID, OpenedFile, Tok, IdentInfo


@dataclass
class Token:
    ty: Tok
    value: str | IdentInfo | None
    loc: Loc

    def __init__(self):
        self.ty = Tok.UNKNOWN
        self.value = None
        self.loc = LOC_INVALID

    def dump(self):
        ty = self.ty.name
        print(ty, self.value, OpenedFile.get_loc(self.loc))

    def copy_from(self, tok: Self):
        self.ty = tok.ty
        self.value = tok.value
        self.loc = tok.loc

    def get_end_loc(self) -> Loc:
        if isinstance(self.value, IdentInfo):
            return self.loc + len(self.value.val)
        elif isinstance(self.value, str):
            return self.loc + len(self.value)
        else:
            return self.loc

    def ident(self) -> IdentInfo:
        assert self.ty == Tok.IDENT, "ident method can only be called on identifiers"
        assert isinstance(self.value, IdentInfo)
        return self.value

    def value_str(self) -> str:
        if isinstance(self.value, IdentInfo):
            return self.value.val
        assert isinstance(self.value, str)
        return self.value
