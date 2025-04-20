from typing import Self, List, Any
from dataclasses import dataclass
from ns_ast import Decl, NamedDecl
import enum


class ScopeFlags(enum.IntFlag):
    NO = 0x000
    FN = 0x001
    BREAK = 0x002
    CONTINUE = 0x004
    DECL = 0x008
    CONTROL = 0x010
    CLASS = 0x020
    BLOCK = 0x040
    FUNCTION_PROTO = 0x100
    FUNCTION_DECL = 0x200
    SWITCH = 0x1000
    ENUM = 0x40000
    COMPOUND_STMT = 0x400000
    CONDITION_VAR = 0x2000000
    LAMBDA = 0x8000000
    TYPE_ALIAS = 0x20000000
    FRIEND = 0x40000000


@dataclass
class Scope:
    any_parent: Self | None
    flags: int
    depth: int
    prototype_depth: int
    prototype_index: int
    fn_parent: Self | None
    break_parent: Self | None
    continue_parent: Self | None
    block_parent: Self | None
    decl_parent: Self | None
    decls_in_scope: List[Decl]
    decl_context: Any
    # using directives

    def set_flags_pr(self, parent: Self | None, f: int):
        self.any_parent = parent
        self.flags = f

        if parent is not None and (f & ScopeFlags.FN) == 0:
            self.break_parent = parent.break_parent
            self.continue_parent = parent.continue_parent
        else:
            self.break_parent = None
            self.continue_parent = None

        if parent is not None:
            self.depth = parent.depth + 1
            self.prototype_depth = parent.prototype_depth
            self.prototype_index = 0
            self.fn_parent = parent.fn_parent
            self.block_parent = parent.block_parent
            self.decl_parent = parent.decl_parent
            if (
                f
                & (
                    ScopeFlags.FN
                    | ScopeFlags.CLASS
                    | ScopeFlags.BLOCK
                    | ScopeFlags.FUNCTION_PROTO
                )
            ) == 0:
                self.flags |= parent.flags
        else:
            self.depth = 0
            self.prototype_depth = 0
            self.prototype_index = 0
            self.decl_parent = None
            self.fn_parent = None
            self.block_parent = None

        if (f & ScopeFlags.FN) != 0:
            self.fn_parent = self

        if (f & ScopeFlags.BREAK) != 0:
            self.break_parent = self

        if (f & ScopeFlags.CONTINUE) != 0:
            self.continue_parent = self

        if (f & ScopeFlags.BLOCK) != 0:
            self.block_parent = self

        if (f & ScopeFlags.FUNCTION_PROTO) != 0 and (f & ScopeFlags.LAMBDA) == 0:
            self.prototype_depth += 1

        if (f & ScopeFlags.DECL) != 0:
            self.decl_parent = self

    def set_flags(self, f: int):
        self.set_flags_pr(self.get_parent(), f)

    def get_parent(self) -> Self | None:
        return self.any_parent

    def is_block_scope(self) -> bool:
        return (self.flags & ScopeFlags.BLOCK) != 0

    def is_condition_var_scope(self) -> bool:
        return (self.flags & ScopeFlags.CONDITION_VAR) != 0

    def is_function_proto_scope(self) -> bool:
        return (self.flags & ScopeFlags.FUNCTION_PROTO) != 0

    def is_function_decl_scope(self) -> bool:
        return (self.flags & ScopeFlags.FUNCTION_DECL) != 0

    def is_switch_scope(self) -> bool:
        s = self
        while s is not None:
            if (s.flags & ScopeFlags.SWITCH) != 0:
                return True
            elif (
                s.flags
                & (
                    ScopeFlags.FN
                    | ScopeFlags.CLASS
                    | ScopeFlags.BLOCK
                    | ScopeFlags.FUNCTION_PROTO
                )
            ) != 0:
                return False
            s = s.get_parent()
        return False

    def is_loop_scope(self) -> bool:
        return (self.flags & ScopeFlags.BREAK) != 0 and (
            self.flags & ScopeFlags.SWITCH
        ) == 0

    def is_continue_scope(self) -> bool:
        return (self.flags & ScopeFlags.CONTINUE) != 0

    def is_compound_stmt_scope(self) -> bool:
        return (self.flags & ScopeFlags.COMPOUND_STMT) != 0

    def is_control_scope(self) -> bool:
        return (self.flags & ScopeFlags.CONTROL) != 0

    def is_type_alias_scope(self) -> bool:
        return (self.flags & ScopeFlags.TYPE_ALIAS) != 0

    def is_friend_scope(self) -> bool:
        return (self.flags & ScopeFlags.FRIEND) != 0

    def is_function_scope(self) -> bool:
        return (self.flags & ScopeFlags.FN) != 0

    def is_class_scope(self) -> bool:
        return (self.flags & ScopeFlags.CLASS) != 0

    def contains(self, other: Self) -> bool:
        return self.depth < other.depth

    def contained_in_prototype_scope(self) -> bool:
        s = self
        while s is not None:
            if s.is_function_proto_scope():
                return True
            s = s.get_parent()
        return False

    # using dir

    def add_flags(self, flags: int):
        assert (flags & ~(ScopeFlags.BREAK | ScopeFlags.CONTINUE)) == 0, (
            "Unsupported scope flags"
        )
        if (flags & ScopeFlags.BREAK) != 0:
            assert (self.flags & ScopeFlags.BREAK) == 0, "Already set"
            self.break_parent = self
        if (flags & ScopeFlags.CONTINUE) != 0:
            assert (self.flags & ScopeFlags.CONTINUE) == 0, "Already set"
            self.continue_parent = self
        self.flags |= flags

    def dump(self) -> str:
        out = ""
        flags = self.flags
        has_flags = flags != 0
        if has_flags:
            out += "Flags: "
        flag_info = {
            ScopeFlags.FN: "FnScope",
            ScopeFlags.BREAK: "BreakScope",
            ScopeFlags.CONTINUE: "ContinueScope",
            ScopeFlags.DECL: "DeclScope",
            ScopeFlags.CONTROL: "ControlScope",
            ScopeFlags.CLASS: "ClassScope",
            ScopeFlags.BLOCK: "BlockScope",
            ScopeFlags.FUNCTION_PROTO: "FunctionPrototypeScope",
            ScopeFlags.FUNCTION_DECL: "FunctionDeclarationScope",
            ScopeFlags.SWITCH: "SwitchScope",
            ScopeFlags.ENUM: "EnumScope",
            ScopeFlags.COMPOUND_STMT: "CompoundStmtScope",
            ScopeFlags.CONDITION_VAR: "ConditionVarScope",
            ScopeFlags.LAMBDA: "LambdaScope",
            ScopeFlags.TYPE_ALIAS: "TypeAliasScope",
            ScopeFlags.FRIEND: "FriendScope",
        }

        for k, v in flag_info.items():
            if (flags & k) != 0:
                out += v
                flags &= ~k
                if flags != 0:
                    out += " | "

        assert flags == 0, "Unknown scope flags"

        if has_flags:
            out += "\n"

        if self.any_parent is not None:
            out += f"Parent: {self.any_parent}\n"
        out += f"Depth: {self.depth}\n"
        if self.decl_context is not None:
            out += f"Entity: {self.decl_context}\n"
        return out

    def set_is_condition_var_scope(self, val: bool):
        self.flags = (self.flags & ~ScopeFlags.CONDITION_VAR) | (
            int(val) * ScopeFlags.CONDITION_VAR
        )

    def get_next_function_prototype_index(self) -> int:
        assert (self.flags & ScopeFlags.FUNCTION_PROTO) != 0
        i = self.prototype_index
        self.prototype_index += 1
        return i

    def add_decl(self, d: Decl):
        self.decls_in_scope.append(d)

    def remove_decl(self, d: Decl):
        self.decls_in_scope.remove(d)

    def contains_decl(self, d: Decl):
        return d in self.decls_in_scope

    def __init__(self, parent: Self | None, flags: ScopeFlags):
        self.any_parent = None
        self.flags = 0
        self.depth = 0
        self.prototype_depth = 0
        self.prototype_index = 0
        self.fn_parent = None
        self.break_parent = None
        self.continue_parent = None
        self.block_parent = None
        self.decl_parent = None
        self.decls_in_scope = []
        self.decl_context = None

        self.set_flags_pr(parent, flags)
        self.parent_scope = parent
        self.flags = flags
        self.decls_in_scope = []

    def lookup_named_decl(self, decl_name) -> NamedDecl | None:
        for d in self.decls_in_scope:
            if not isinstance(d, NamedDecl):
                continue
            if d.name == decl_name:
                return d
        if self.parent_scope is not None:
            return self.parent_scope.lookup_named_decl(decl_name)
        return None
