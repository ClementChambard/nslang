from utils.my_enum import Enum, ENUM_INIT, ENUM_N

class Tok(Enum):
    # SPECIAL
    UNKNOWN = ENUM_INIT()
    EOF = ENUM_N()

    # IDENTIFIER
    IDENT = ENUM_N()

    # CONSTANTS
    NUM = ENUM_N()
    STR = ENUM_N()
    CHR = ENUM_N()

    # PUNCTUATION
    LPAREN = ENUM_N()
    RPAREN = ENUM_N()
    LBRACE = ENUM_N()
    RBRACE = ENUM_N()
    LSQUARE = ENUM_N()
    RSQUARE = ENUM_N()
    SEMI = ENUM_N()
    COLON = ENUM_N()
    PERIOD = ENUM_N()
    ARROW = ENUM_N()
    ELLIPSIS = ENUM_N()
    GREATER = ENUM_N()
    GREATERGREATER = ENUM_N()
    COMMA = ENUM_N()
    EQUAL = ENUM_N()
    EXCLAIM = ENUM_N()
    STAREQUAL = ENUM_N()
    SLASHEQUAL = ENUM_N()
    PERCENTEQUAL = ENUM_N()
    PLUSEQUAL = ENUM_N()
    MINUSEQUAL = ENUM_N()
    LESSLESSEQUAL = ENUM_N()
    GREATERGREATEREQUAL = ENUM_N()
    AMPEQUAL = ENUM_N()
    CARETEQUAL = ENUM_N()
    PIPEEQUAL = ENUM_N()
    QUESTION = ENUM_N()
    PIPEPIPE = ENUM_N()
    AMPAMP = ENUM_N()
    PIPE = ENUM_N()
    TILDE = ENUM_N()
    CARET = ENUM_N()
    AMP = ENUM_N()
    EXCLAIMEQUAL = ENUM_N()
    EQUALEQUAL = ENUM_N()
    LESSEQUAL = ENUM_N()
    LESS = ENUM_N()
    GREATEREQUAL = ENUM_N()
    SPACESHIP = ENUM_N()
    LESSLESS = ENUM_N()
    PLUS = ENUM_N()
    PLUSPLUS = ENUM_N()
    MINUS = ENUM_N()
    MINUSMINUS = ENUM_N()
    PERCENT = ENUM_N()
    SLASH = ENUM_N()
    STAR = ENUM_N()
    PERIODSTAR = ENUM_N()
    ARROWSTAR = ENUM_N()

    # KEYWORDS
    KW_FN = ENUM_N()
    KW_LET = ENUM_N()
    KW_LIB = ENUM_N()
    KW_TYPE = ENUM_N()
    KW_STRUCT = ENUM_N()
    KW_ENUM = ENUM_N()
    KW_I8 = ENUM_N()
    KW_I16 = ENUM_N()
    KW_I32 = ENUM_N()
    KW_I64 = ENUM_N()
    KW_U8 = ENUM_N()
    KW_U16 = ENUM_N()
    KW_U32 = ENUM_N()
    KW_U64 = ENUM_N()
    KW_BOOL = ENUM_N()
    KW_VOID = ENUM_N()
    KW_SIZEOF = ENUM_N()
    KW_CAST = ENUM_N()
    KW_IF = ENUM_N()
    KW_ELSE = ENUM_N()
    KW_TRUE = ENUM_N()
    KW_FALSE = ENUM_N()
    KW_NULLPTR = ENUM_N()
    KW_CASE = ENUM_N()
    KW_DEFAULT = ENUM_N()
    KW_SWITCH = ENUM_N()
    KW_WHILE = ENUM_N()
    KW_DO = ENUM_N()
    KW_FOR = ENUM_N()
    KW_CONTINUE = ENUM_N()
    KW_BREAK = ENUM_N()
    KW_RETURN = ENUM_N()

    # BUILTINS
    BUILTIN_SYSCALL = ENUM_N()
    BUILTIN_HEXDUMP = ENUM_N()

    def is_keyword(self):
        return self.value >= self.KW_FN.value and self.value <= self.KW_RETURN.value

    def is_builtin(self):
        return self.value >= self.BUILTIN_SYSCALL.value and self.value <= self.BUILTIN_HEXDUMP.value

    def is_builtin_type(self):
        return self.value >= self.KW_I8.value and self.value <= self.KW_BOOL.value

    def __str__(self) -> str:
        return self.get_name()

    def get_name(self):
        match self:
            case self.UNKNOWN: return "????"
            case self.EOF: return "end of file"
            case self.IDENT: return "identifier"
            case self.NUM: return "numeral constant"
            case self.LPAREN: return "("
            case self.RPAREN: return ")"
            case self.LBRACE: return "{"
            case self.RBRACE: return "}"
            case self.LSQUARE: return "["
            case self.RSQUARE: return "]"
            case self.SEMI: return ";"
            case self.COLON: return ":"
            case self.PERIOD: return "."
            case self.ARROW: return "->"
            case self.ELLIPSIS: return "..."
            case self.GREATER: return ">"
            case self.GREATERGREATER: return ">>"
            case self.COMMA: return ","
            case self.EQUAL: return "="
            case self.EXCLAIM: return "!"
            case self.STAREQUAL: return "*="
            case self.SLASHEQUAL: return "/="
            case self.PERCENTEQUAL: return "%="
            case self.PLUSEQUAL: return "+="
            case self.MINUSEQUAL: return "-="
            case self.LESSLESSEQUAL: return "<<="
            case self.GREATERGREATEREQUAL: return ">>="
            case self.AMPEQUAL: return "&="
            case self.CARETEQUAL: return "^="
            case self.PIPEEQUAL: return "|="
            case self.QUESTION: return "?"
            case self.PIPEPIPE: return "||"
            case self.AMPAMP: return "&&"
            case self.PIPE: return "|"
            case self.CARET: return "^"
            case self.AMP: return "&"
            case self.TILDE: return "~"
            case self.EXCLAIMEQUAL: return "!="
            case self.EQUALEQUAL: return "=="
            case self.LESSEQUAL: return "<="
            case self.LESS: return "<"
            case self.GREATEREQUAL: return ">="
            case self.SPACESHIP: return "<=>"
            case self.LESSLESS: return "<<"
            case self.PLUS: return "+"
            case self.PLUSPLUS: return "++"
            case self.MINUS: return "-"
            case self.MINUSMINUS: return "--"
            case self.PERCENT: return "%"
            case self.SLASH: return "/"
            case self.STAR: return "*"
            case self.PERIODSTAR: return ".*"
            case self.ARROWSTAR: return "->*"
            case self.KW_FN: return "fn"
            case self.KW_LET: return "let"
            case self.KW_TYPE: return "type"
            case self.KW_VOID: return "void"
            case self.KW_LIB: return "lib"
            case self.KW_STRUCT: return "struct"
            case self.KW_ENUM: return "enum"
            case self.KW_I8: return "i8"
            case self.KW_I16: return "i16"
            case self.KW_I32: return "i32"
            case self.KW_I64: return "i64"
            case self.KW_U8: return "u8"
            case self.KW_U16: return "u16"
            case self.KW_U32: return "u32"
            case self.KW_U64: return "u64"
            case self.KW_BOOL: return "bool"
            case self.KW_SIZEOF: return "sizeof"
            case self.KW_CAST: return "cast"
            case self.KW_IF: return "if"
            case self.KW_ELSE: return "else"
            case self.KW_TRUE: return "true"
            case self.KW_FALSE: return "false"
            case self.KW_NULLPTR: return "nullptr"
            case self.KW_CASE: return "case"
            case self.KW_DEFAULT: return "default"
            case self.KW_SWITCH: return "switch"
            case self.KW_WHILE: return "while"
            case self.KW_DO: return "do"
            case self.KW_FOR: return "for"
            case self.KW_CONTINUE: return "continue"
            case self.KW_BREAK: return "break"
            case self.KW_RETURN: return "return"
            case self.BUILTIN_SYSCALL: return "__builtin_syscall"
            case self.BUILTIN_HEXDUMP: return "__builtin_hexdump"
