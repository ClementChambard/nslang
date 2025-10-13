from ns_ast.nodes import Type
from lex import Token, Tok
from utils import HEX_DIGITS, OCTAL_DIGITS, DECIMAL_DIGITS


class NumLiteralParser:
    res: int
    ty: Type
    had_error: bool

    def parse_int(self, s, digits, base):
        n = ""
        while s != "":
            if s[0] not in digits:
                break
            n += s[0]
            s = s[1:]
        try:
            self.res = int(n, base)
        except Exception as e:
            print(f"error parsing number {e}")
            self.res = 0
            self.had_error = True
        return s

    def parse_num(self, s):
        if s.startswith("0x"):
            return self.parse_int(s[2:], HEX_DIGITS, 16)
        elif s.startswith("0b"):
            return self.parse_int(s[2:], ["0", "1"], 2)
        elif s.startswith("0"):
            # TODO: float that start with 0
            return self.parse_int(s, OCTAL_DIGITS, 8)
        else:
            # TODO: float
            return self.parse_int(s, DECIMAL_DIGITS, 10)

    def parse_suffix(self, s):
        from semantic_analysis import TYPES

        if s == "i8":
            self.ty = TYPES["i8"]
        elif s == "i16":
            self.ty = TYPES["i16"]
        elif s == "i32":
            self.ty = TYPES["i32"]
        elif s == "i64":
            self.ty = TYPES["i64"]
        elif s == "u8":
            self.ty = TYPES["u8"]
        elif s == "u16":
            self.ty = TYPES["u16"]
        elif s == "u32":
            self.ty = TYPES["u32"]
        elif s == "u64":
            self.ty = TYPES["u64"]
        # TODO: float suffixes
        else:
            # diagnose invalid suffix
            print(f"invalid number suffix '{s}'")
            self.had_error = True

    def parse(self, tok: Token):
        assert tok.ty == Tok.NUM
        val_end = self.parse_num(tok.value)
        if len(val_end) > 0:
            self.parse_suffix(val_end)

    def __init__(self, tok: Token):
        from semantic_analysis import TYPES

        self.res = 0
        self.ty = TYPES["i64"]
        self.had_error = False
        self.parse(tok)
