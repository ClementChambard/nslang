from dataclasses import dataclass
from typing import ClassVar, List, Self, Tuple
from . import Loc, LOC_INVALID

@dataclass
class OpenedFile:
    opened_files: ClassVar[List[Self]] = []

    filename: str
    source: str
    src_len: int
    pos_offset: int
    line_cache: List[int]

    def __init__(self, filename: str):
        self.filename = filename
        with open(filename, "r") as f:
            self.source = f.read() + "\0"
        self.src_len = len(self.source)
        if len(self.opened_files) == 0:
            self.pos_offset = 1
        else:
            self.pos_offset = self.opened_files[-1].pos_offset + self.opened_files[-1].src_len
        self.opened_files.append(self)
        self.make_lines_cache()

    def make_lines_cache(self):
        pos = 0
        nl_offsets = []
        while pos < self.src_len:
            if self.source[pos] == "\r":
                pos += 1
                if self.source[pos] == "\n":
                    pos += 1
                nl_offsets.append(pos)
            elif self.source[pos] == "\n":
                pos += 1
                if self.source[pos] == "\r":
                    pos += 1
                nl_offsets.append(pos)
            else:
                pos += 1
        self.line_cache = nl_offsets

    def line_offset(self, line: int) -> int:
        if line == 1:
            return 0
        return self.line_cache[line-2]

    @classmethod
    def find(cls, filename: str) -> Self | None: 
        for f in cls.opened_files:
            if f.filename == filename:
                return f
        return None

    @classmethod
    def find_by_loc(cls, loc: Loc) -> Self | None:
        if loc == LOC_INVALID or len(cls.opened_files) == 0:
            return None
        file_i = 0
        while (file_i + 1 < len(cls.opened_files) and loc >= cls.opened_files[file_i + 1].pos_offset):
            file_i += 1
        return cls.opened_files[file_i]

    @classmethod
    def get_loc(cls, location: Loc) -> Tuple[str, int, int] | None:
        f = OpenedFile.find_by_loc(location)
        if f is None:
            return None
        line_cache = f.line_cache
        offset = location - f.pos_offset
        line = 0
        col = offset
        while line < len(line_cache) and offset >= line_cache[line]:
            line += 1
        if line > 0:
            col = offset - line_cache[line - 1]
        return (f.filename, line + 1, col + 1)
