from typing import List, Tuple
from . import Loc, LOC_INVALID


OPENED_FILES = []


class OpenedFile:
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
        if len(OPENED_FILES) == 0:
            self.pos_offset = 1
        else:
            self.pos_offset = OPENED_FILES[-1].pos_offset + OPENED_FILES[-1].src_len
        OPENED_FILES.append(self)
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

    @staticmethod
    def find(filename: str):  # ->
        for f in OPENED_FILES:
            if f.filename == filename:
                return f
        return None

    @staticmethod
    def find_by_loc(loc: Loc):  # ->
        if loc == LOC_INVALID or len(OPENED_FILES) == 0:
            return None
        file_i = 0
        while (file_i + 1 < len(OPENED_FILES) and loc >= OPENED_FILES[file_i + 1].pos_offset):
            file_i += 1
        return OPENED_FILES[file_i]

    @staticmethod
    def get_loc(location: Loc) -> Tuple[str, int, int] | None:
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
