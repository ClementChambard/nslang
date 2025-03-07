from typing import Self
from . import Loc, LOC_INVALID, OpenedFile, Tok, IdentInfo

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

    def get_end_loc(self):
        if isinstance(self.value, IdentInfo):
            return loc + len(self.value.val)
        else:
            return loc + len(self.value)
