from dataclasses import dataclass
from typing import List, Self, Tuple

from ir2.data.instrs.other import PhiInstr
from ir2.data.instrs.term import TermInstr
from ir2.data.value import GlobalObject, IrValue

from .instrs._base import Instr
from .types import IrLabelType, IrPtrType, IrType


@dataclass
class Block(IrValue):
    instructions: List[Instr]
    parent: "Function"

    def __eq__(self, o) -> bool:
        return self is o

    def __init__(self, name: str, function: "Function | None" = None):
        from ir2.gen_state import get_cur_fn

        super().__init__(IrLabelType(), name)
        self.instructions = []
        self.parent = function or get_cur_fn()

    def vname(self) -> str:
        assert self.name
        return f".{self.name}"

    def to_txt(self) -> str:
        out = f"{self.vname()}:\n"
        for i in self.instructions:
            out += "  " + i.to_txt() + "\n"
        return out

    def get_terminator(self) -> TermInstr | None:
        if not self.is_terminated:
            return None
        assert len(self.instructions) > 0, f"in block .{self.name}"
        assert isinstance(self.instructions[-1], TermInstr)
        return self.instructions[-1]

    @property
    def is_terminated(self) -> bool:
        return len(self.instructions) > 0 and isinstance(
            self.instructions[-1], TermInstr
        )

    @property
    def successors(self) -> List[Self]:
        if not self.is_terminated:
            return []
        term = self.get_terminator()
        assert term
        return term.successors

    def get_predecessors(self) -> List["Block"]:
        return self.parent.get_preds(self)

    def get_single_predecessor(self) -> "Block | None":
        preds = self.get_predecessors()
        return preds[0] if len(preds) == 1 else None

    def phis(self) -> List[PhiInstr]:
        i = 0
        out = []
        while i < len(self.instructions) and isinstance(self.instructions[i], PhiInstr):
            out.append(self.instructions[i])
            i += 1
        return out

    def non_phis_instrs(self) -> List[Instr]:
        i = 0
        while i < len(self.instructions) and isinstance(self.instructions[i], PhiInstr):
            i += 1
        return self.instructions[i:]

    def erase_from_parent(self):
        assert self.parent
        for i in range(len(self.parent.blocks)):
            if self.parent.blocks[i] is self:
                self.parent.blocks.pop(i)
                break


@dataclass
class Function(GlobalObject):
    blocks: List[Block]
    arg_values: List[IrValue]
    result_type: IrType
    is_lib: bool
    is_vararg: bool

    def __init__(self, name, result_type, is_lib, is_vararg):
        super().__init__(IrPtrType(), name)
        self.blocks = []
        self.arg_values = []
        self.result_type = result_type
        self.is_lib = is_lib
        self.is_vararg = is_vararg

    def __iter__(self):
        return self.blocks.__iter__()

    @property
    def first_block(self) -> Block:
        return self.blocks[0]

    def to_txt(self) -> str:
        out = ""
        if self.is_lib:
            out = "lib "
        out += f"fn {self.name}("
        for i, a in enumerate(self.arg_values):
            if i != 0:
                out += ", "
            out += a.vname() + ": " + str(a.ty)
        if self.is_vararg:
            if len(self.arg_values) > 0:
                out += ", "
            out += "..."
        out += ") -> "
        out += str(self.result_type)
        out += " {\n"
        for b in self.blocks:
            out += b.to_txt()
        out += "}\n"
        return out

    def get_backedges(self) -> List[Tuple[Block, Block]]:
        b = self.first_block
        if len(b.successors) == 0:
            return []

        result = []

        visited = list()
        visit_stack = []
        in_stack = list()

        visited.append(b)
        visit_stack.append([b, b.successors])
        in_stack.append(b)
        while len(visit_stack) > 0:
            top = visit_stack[-1]
            parent_bb = top[0]
            i = top[1]

            found_new = False
            for bb in i:
                b = bb
                if b not in visited:
                    visited.append(b)
                    found_new = True
                    break
                if b in in_stack:
                    result.append([parent_bb, b])

            if found_new:
                if b not in in_stack:
                    in_stack.append(b)
                visit_stack.append([b, b.successors])
            else:
                in_stack.remove(visit_stack.pop()[0])

        return result

    def get_preds(self, bb: Block) -> List[Block]:
        out = []
        for b in self.blocks:
            if bb in b.successors and b not in out:
                out.append(b)
        return out
