from enum import Enum, auto

class Tok(Enum):
    # SPECIAL
    UNKNOWN = auto()
    EOF = auto()

    # IDENTIFIER
    IDENT = auto()

    # CONSTANTS
    NUM = auto()
    STR = auto()
    CHR = auto()

    # PUNCTUATION
    LPAREN = auto()
    RPAREN = auto()
    LBRACE = auto()
    RBRACE = auto()
    LSQUARE = auto()
    RSQUARE = auto()
    SEMI = auto()
    COLON = auto()
    COLONCOLON = auto()
    PERIOD = auto()
    ARROW = auto()
    ELLIPSIS = auto()
    GREATER = auto()
    GREATERGREATER = auto()
    COMMA = auto()
    EQUAL = auto()
    EXCLAIM = auto()
    STAREQUAL = auto()
    SLASHEQUAL = auto()
    PERCENTEQUAL = auto()
    PLUSEQUAL = auto()
    MINUSEQUAL = auto()
    LESSLESSEQUAL = auto()
    GREATERGREATEREQUAL = auto()
    AMPEQUAL = auto()
    CARETEQUAL = auto()
    PIPEEQUAL = auto()
    QUESTION = auto()
    PIPEPIPE = auto()
    AMPAMP = auto()
    PIPE = auto()
    TILDE = auto()
    CARET = auto()
    AMP = auto()
    EXCLAIMEQUAL = auto()
    EQUALEQUAL = auto()
    LESSEQUAL = auto()
    LESS = auto()
    GREATEREQUAL = auto()
    SPACESHIP = auto()
    LESSLESS = auto()
    PLUS = auto()
    PLUSPLUS = auto()
    MINUS = auto()
    MINUSMINUS = auto()
    PERCENT = auto()
    SLASH = auto()
    STAR = auto()
    PERIODSTAR = auto()
    ARROWSTAR = auto()

    # KEYWORDS
    KW_FN = auto()
    KW_LET = auto()
    KW_LIB = auto()
    KW_TYPE = auto()
    KW_STRUCT = auto()
    KW_ENUM = auto()
    KW_I8 = auto()
    KW_I16 = auto()
    KW_I32 = auto()
    KW_I64 = auto()
    KW_U8 = auto()
    KW_U16 = auto()
    KW_U32 = auto()
    KW_U64 = auto()
    KW_BOOL = auto()
    KW_VOID = auto()
    KW_SIZEOF = auto()
    KW_CAST = auto()
    KW_IF = auto()
    KW_ELSE = auto()
    KW_TRUE = auto()
    KW_FALSE = auto()
    KW_NULLPTR = auto()
    KW_VAARG = auto()
    KW_CASE = auto()
    KW_DEFAULT = auto()
    KW_SWITCH = auto()
    KW_WHILE = auto()
    KW_DO = auto()
    KW_FOR = auto()
    KW_CONTINUE = auto()
    KW_BREAK = auto()
    KW_RETURN = auto()

    def is_keyword(self):
        return self.value >= self.KW_FN.value and self.value <= self.KW_RETURN.value

    def is_builtin_type(self):
        return self.value >= self.KW_I8.value and self.value <= self.KW_BOOL.value

    def __str__(self) -> str:
        return self.get_name()

    def get_name(self) -> str:
        match self:
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
            case self.COLONCOLON: return "::"
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
            case self.KW_VAARG: return "vaarg"
            case self.KW_CASE: return "case"
            case self.KW_DEFAULT: return "default"
            case self.KW_SWITCH: return "switch"
            case self.KW_WHILE: return "while"
            case self.KW_DO: return "do"
            case self.KW_FOR: return "for"
            case self.KW_CONTINUE: return "continue"
            case self.KW_BREAK: return "break"
            case self.KW_RETURN: return "return"
            case _: return "????"
