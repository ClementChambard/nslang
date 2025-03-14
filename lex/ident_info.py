from . import Tok


IDENTIFIERS = {}


class IdentInfo:
    ty: Tok
    val: str

    def __init__(self, name: str, ty: Tok = Tok.IDENT):
        self.ty = ty
        self.val = name
        IDENTIFIERS[name] = self

    def needs_handling(self):
        return False

    @staticmethod
    def find(name: str):  # -> self
        if name not in IDENTIFIERS.keys():
            _ = IdentInfo(name)
        return IDENTIFIERS[name]


IdentInfo("fn", Tok.KW_FN)
IdentInfo("let", Tok.KW_LET)
IdentInfo("lib", Tok.KW_LIB)
IdentInfo("type", Tok.KW_TYPE)
IdentInfo("struct", Tok.KW_STRUCT)
IdentInfo("enum", Tok.KW_ENUM)
IdentInfo("i8", Tok.KW_I8)
IdentInfo("i16", Tok.KW_I16)
IdentInfo("i32", Tok.KW_I32)
IdentInfo("i64", Tok.KW_I64)
IdentInfo("u8", Tok.KW_U8)
IdentInfo("u16", Tok.KW_U16)
IdentInfo("u32", Tok.KW_U32)
IdentInfo("u64", Tok.KW_U64)
IdentInfo("bool", Tok.KW_BOOL)
IdentInfo("void", Tok.KW_VOID)
IdentInfo("sizeof", Tok.KW_SIZEOF)
IdentInfo("cast", Tok.KW_CAST)
IdentInfo("if", Tok.KW_IF)
IdentInfo("else", Tok.KW_ELSE)
IdentInfo("true", Tok.KW_TRUE)
IdentInfo("false", Tok.KW_FALSE)
IdentInfo("nullptr", Tok.KW_NULLPTR)
IdentInfo("case", Tok.KW_CASE)
IdentInfo("default", Tok.KW_DEFAULT)
IdentInfo("switch", Tok.KW_SWITCH)
IdentInfo("while", Tok.KW_WHILE)
IdentInfo("do", Tok.KW_DO)
IdentInfo("for", Tok.KW_FOR)
IdentInfo("continue", Tok.KW_CONTINUE)
IdentInfo("break", Tok.KW_BREAK)
IdentInfo("return", Tok.KW_RETURN)
IdentInfo("__builtin_syscall", Tok.BUILTIN_SYSCALL)
IdentInfo("__builtin_hexdump", Tok.BUILTIN_HEXDUMP)
