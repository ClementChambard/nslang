from dataclasses import dataclass
from enum import Enum, auto
from typing import List, Self
from typing_extensions import Tuple


class X86_InstrKind(Enum):
    PUSH = auto()
    POP = auto()
    MOV = auto()
    XOR = auto()
    AND = auto()
    OR = auto()
    SHR = auto()
    SHL = auto()
    LEA = auto()
    ADD = auto()
    SUB = auto()
    CALL = auto()
    LEAVE = auto()
    RET = auto()
    TEST = auto()
    JE = auto()
    JNE = auto()
    JA = auto()
    JMP = auto()
    NEG = auto()
    IMUL = auto()
    IDIV = auto()
    INC = auto()
    DEC = auto()
    CMP = auto()
    CQO = auto()
    SYSCALL = auto()
    SETE = auto()
    SETNE = auto()
    SETL = auto()
    SETLE = auto()
    SETG = auto()
    SETGE = auto()
    MOVZX = auto()

    def __str__(self) -> str:
        return self.name.lower()

    @classmethod
    def get(cls, name: str) -> Self:
        return cls[name.upper()]


class X86_OperandKind(Enum):
    REGISTER = auto()
    IMMEDIATE = auto()
    MEMORY = auto()
    ADDRESS = auto()
    LABEL = auto()


class X86_RegisterKind(Enum):
    A = auto()
    B = auto()
    C = auto()
    D = auto()
    DI = auto()
    SI = auto()
    SP = auto()
    BP = auto()
    R8 = auto()
    R9 = auto()
    R10 = auto()
    R11 = auto()
    R12 = auto()
    R13 = auto()
    R14 = auto()
    R15 = auto()

    @classmethod
    def get(cls, name: str) -> Self:
        match name.lower():
            case "a":
                return cls.A
            case "b":
                return cls.B
            case "c":
                return cls.C
            case "d":
                return cls.D
            case "di":
                return cls.DI
            case "si":
                return cls.SI
            case "sp":
                return cls.SP
            case "bp":
                return cls.BP
            case "r8":
                return cls.R8
            case "r9":
                return cls.R9
            case "r10":
                return cls.R10
            case "r11":
                return cls.R11
            case "r12":
                return cls.R12
            case "r13":
                return cls.R13
            case "r14":
                return cls.R14
            case "r15":
                return cls.R15
            case _:
                raise ValueError("Invalid register name")


class X86_Size(Enum):
    BYTE = auto()
    WORD = auto()
    DWORD = auto()
    QWORD = auto()
    REG_HI_BYTE = auto()  # for registers only

    @classmethod
    def get(cls, size: int) -> Self:
        match size:
            case 1:
                return cls.BYTE
            case 2:
                return cls.WORD
            case 4:
                return cls.DWORD
            case 8:
                return cls.QWORD
            case _:
                raise ValueError("Invalid size")


@dataclass
class X86_Register:
    size: X86_Size
    kind: X86_RegisterKind

    def __init__(self, size: int | X86_Size, kind: X86_RegisterKind | str):
        if isinstance(size, int):
            size = X86_Size.get(size)
        if isinstance(kind, str):
            kind = X86_RegisterKind.get(kind)
        self.size = size
        self.kind = kind

    @classmethod
    def get(cls, name: str) -> Self:
        if name.startswith("r"):
            if name[1].isdigit():
                take = [2, 3][name[1] == "1"]
                name, suffix = name[:take], name[take:]
                size = {"": 8, "d": 4, "w": 2, "b": 1}[suffix]
                return X86_Register(size, name)
            name = name[1:]
            if name.endswith("x"):
                name = name[:-1]
            return X86_Register(8, name)
        if name.startswith("e"):
            name = name[1:]
            if name.endswith("x"):
                name = name[:-1]
            return X86_Register(4, name)
        if name.endswith("x"):
            return X86_Register(2, name[:-1])
        if name.endswith("l"):
            return X86_Register(1, name[:-1])
        if name.endswith("h"):
            return X86_Register(X86_Size.REG_HI_BYTE, name[:-1])
        return X86_Register(2, name)

    def with_size(self, size: int | X86_Size) -> Self:
        return X86_Register(size, self.kind)

    def __str__(self) -> str:
        match self.kind:
            case (
                X86_RegisterKind.A
                | X86_RegisterKind.B
                | X86_RegisterKind.C
                | X86_RegisterKind.D
            ):
                name = self.kind.name.lower()
                match self.size:
                    case X86_Size.BYTE:
                        return f"{name}l"
                    case X86_Size.REG_HI_BYTE:
                        return f"{name}h"
                    case X86_Size.WORD:
                        return f"{name}x"
                    case X86_Size.DWORD:
                        return f"e{name}x"
                    case X86_Size.QWORD:
                        return f"r{name}x"
            case (
                X86_RegisterKind.DI
                | X86_RegisterKind.SI
                | X86_RegisterKind.SP
                | X86_RegisterKind.BP
            ):
                name = self.kind.name.lower()
                match self.size:
                    case X86_Size.BYTE:
                        return f"{name}l"
                    case X86_Size.REG_HI_BYTE:
                        assert False
                    case X86_Size.WORD:
                        return f"{name}"
                    case X86_Size.DWORD:
                        return f"e{name}"
                    case X86_Size.QWORD:
                        return f"r{name}"
            case _:
                name = self.kind.name.lower()
                match self.size:
                    case X86_Size.BYTE:
                        return f"{name}b"
                    case X86_Size.REG_HI_BYTE:
                        assert False
                    case X86_Size.WORD:
                        return f"{name}w"
                    case X86_Size.DWORD:
                        return f"{name}d"
                    case X86_Size.QWORD:
                        return f"{name}"


@dataclass
class X86_Label:
    name: str

    def __str__(self):
        return self.name


@dataclass
class X86_Address:
    base: X86_Register | X86_Label
    offset: X86_Register | int | None
    offset_amt: int
    offset_is_neg: bool

    def __init__(self, addr_base, offset=None, neg=False, amt=1):
        self.base = addr_base
        if isinstance(addr_base, str):
            # addr or register
            try:
                self.base = X86_Register.get(addr_base)
            except:
                self.base = X86_Label(addr_base)
        elif isinstance(addr_base, X86_Register):
            pass
        else:
            assert False
        if isinstance(self.base, X86_Register):
            if isinstance(offset, int) and offset < 0:
                offset = -offset
                neg = not neg
            if isinstance(offset, int) and offset == 0:
                offset = None
            self.offset = offset
            self.offset_is_neg = neg
            self.offset_amt = amt
        else:
            assert offset is None
            self.offset = None
            self.offset_is_neg = False
            self.offset_amt = 1

    @staticmethod
    def tokenize_addr(name: str) -> List[str]:
        def lex(s: str) -> Tuple[str, str]:
            assert len(s) > 0 and not s[0].isspace()
            if s[0] in ["+", "-", "*"]:
                return (s[0], s[1:])
            if s[0].isalnum() or s[0] in [".", "_"]:
                out = ""
                while len(s) > 0 and (s[0].isalnum() or s[0] in [".", "_"]):
                    out += s[0]
                    s = s[1:]
                return (out, s)
            assert False

        out = []
        name = name.strip()
        while len(name) > 0:
            tok, name = lex(name)
            name = name.strip()
            out.append(tok)
        return out

    @classmethod
    def get(cls, name: str) -> Self:
        name = name.strip()
        assert name.startswith("[") and name.endswith("]")
        name = name[1:-1]
        toks = cls.tokenize_addr(name)
        base = toks[0]
        toks = toks[1:]
        offset = None
        offset_amt = 0
        offset_is_neg = False
        if len(toks) > 0:
            offset_is_neg = {"+": False, "-": True}[toks[0]]
            try:
                offset = int(toks[1])
            except:
                offset = X86_Register.get(toks[1])
            offset_amt = 1
            toks = toks[2:]
        if len(toks) > 0:
            assert toks[0] == "*"
            toks = toks[1:]
            assert len(toks) == 1
            offset_amt = int(toks[0])
        return X86_Address(base, offset, offset_amt, offset_is_neg)

    def __str__(self) -> str:
        addr = str(self.base)
        if self.offset is not None and self.offset_amt != 0:
            if isinstance(self.offset, int):
                if self.offset != 0:
                    addr += [" + ", " - "][self.offset_is_neg]
                    addr += str(self.offset * self.offset_amt)
            else:
                addr += [" + ", " - "][self.offset_is_neg]
                addr += str(self.offset)
                if self.offset_amt != 1:
                    addr += f" * {self.offset_amt}"
        return f"[{addr}]"


@dataclass
class X86_Memory:
    size: X86_Size
    addr: X86_Address

    def __init__(self, size: int | X86_Size, addr_base, offset=None, neg=False, amt=1):
        if isinstance(size, int):
            size = X86_Size.get(size)
        self.size = size
        if isinstance(addr_base, X86_Address):
            self.addr = addr_base
        elif (
            isinstance(addr_base, str)
            and addr_base.startswith("[")
            and addr_base.endswith("]")
        ):
            self.addr = X86_Address.get(addr_base)
        else:
            self.addr = X86_Address(addr_base, offset, neg, amt)

    @classmethod
    def get(cls, name: str) -> Self:
        [size, address] = name.split("[")
        assert address.endswith("]")
        match size.strip().upper():
            case "BYTE":
                size = X86_Size.BYTE
            case "WORD":
                size = X86_Size.WORD
            case "DWORD":
                size = X86_Size.DWORD
            case "QWORD":
                size = X86_Size.QWORD
            case _:
                assert False
        return X86_Memory(size, "[" + address)

    def __str__(self) -> str:
        return f"{self.size.name}{self.addr}"


@dataclass
class X86_Operand:
    kind: X86_OperandKind
    value: X86_Register | int | X86_Memory | X86_Address | X86_Label

    def __str__(self) -> str:
        return str(self.value)

    @classmethod
    def register(cls, *args) -> Self:
        if len(args) == 1:
            [name] = args
            return X86_Operand(X86_OperandKind.REGISTER, X86_Register.get(name))
        elif len(args) == 2:
            [name, size] = args
            return X86_Operand(X86_OperandKind.REGISTER, X86_Register(size, name))
        else:
            raise ValueError()

    @classmethod
    def immediate(cls, num: int) -> Self:
        return X86_Operand(X86_OperandKind.IMMEDIATE, num)

    @classmethod
    def memory(cls, *args) -> Self:
        # TODO:
        [name] = args
        return X86_Operand(X86_OperandKind.MEMORY, X86_Memory.get(name))

    @classmethod
    def address(cls, *args) -> Self:
        # TODO:
        [name] = args
        return X86_Operand(X86_OperandKind.ADDRESS, X86_Address.get(name))

    @classmethod
    def label(cls, *args) -> Self:
        # TODO:
        [name] = args
        return X86_Operand(X86_OperandKind.LABEL, X86_Label(name))

    @classmethod
    def from_raw(cls, raw: int | str | X86_Register | X86_Memory | X86_Address) -> Self:
        if isinstance(raw, int):
            return cls.immediate(raw)
        if isinstance(raw, X86_Register):
            return X86_Operand(X86_OperandKind.REGISTER, raw)
        if isinstance(raw, X86_Memory):
            return X86_Operand(X86_OperandKind.MEMORY, raw)
        if isinstance(raw, X86_Address):
            return X86_Operand(X86_OperandKind.ADDRESS, raw)
        if isinstance(raw, X86_Label):
            return X86_Operand(X86_OperandKind.LABEL, raw)
        if isinstance(raw, str):
            if raw.isdigit():
                return cls.immediate(int(raw))
            if raw.startswith("[") and raw.endswith("]"):
                return cls.address(raw)
            if "[" in raw and raw.endswith("]"):
                return cls.memory(raw)
            try:
                return cls.register(raw)
            except:
                return cls.label(raw)


@dataclass
class X86_Instr:
    kind: X86_InstrKind
    operands: List[X86_Operand]

    def __init__(self, name, *operands):
        self.kind = X86_InstrKind.get(name)
        self.operands = [
            op if isinstance(op, X86_Operand) else X86_Operand.from_raw(op)
            for op in operands
        ]

    @classmethod
    def get(cls, name: str) -> Self:
        ins_name = ""
        while len(name) > 0 and name[0] != " ":
            ins_name += name[0]
            name = name[1:]
        if len(name) == 0:
            return X86_Instr(ins_name)
        operands = [op.strip() for op in name.split(",")]
        return X86_Instr(ins_name, *operands)

    def __str__(self):
        out = f"    {self.kind}"
        if len(self.operands) == 0:
            return out
        out += " " * (4 - len(out) % 4) + str(self.operands[0])
        for op in self.operands[1:]:
            out += ", " + str(op)
        return out


@dataclass
class X86_LabelPoint:
    label: X86_Label

    def __str__(self) -> str:
        return f"{self.label}:"


type X86_Body = List[X86_Instr | X86_LabelPoint]


def tests():
    code = [
        X86_Instr("mov", "rax", "123"),
        X86_Instr("mov", "WORD[rsp + 8]", "ax"),
    ]
    print("\n".join([f"    {i}" for i in code]))


if __name__ == "__main__":
    tests()
