import enum
from lex import Tok

class Prec(enum.Enum):
    UNKNOWN = enum.auto()
    COMMA = enum.auto()
    ASSIGN = enum.auto()
    COND = enum.auto()
    OR = enum.auto()
    AND = enum.auto()
    BOR = enum.auto()
    XOR = enum.auto()
    BAND = enum.auto()
    EQ = enum.auto()
    COMP = enum.auto()  # CPP -> spaceship after
    SHIFT = enum.auto()
    PLUS = enum.auto()
    STAR = enum.auto()  # CPP -> ptr to member after

    @staticmethod
    def from_bin_op(token_kind: Tok) -> "Prec":
        match token_kind:
            case (
                Tok.EQUAL
                | Tok.STAREQUAL
                | Tok.SLASHEQUAL
                | Tok.PERCENTEQUAL
                | Tok.PLUSEQUAL
                | Tok.MINUSEQUAL
                | Tok.LESSLESSEQUAL
                | Tok.GREATERGREATEREQUAL
                | Tok.AMPEQUAL
                | Tok.CARETEQUAL
                | Tok.PIPEEQUAL
            ):
                return Prec.ASSIGN
            case Tok.QUESTION:
                return Prec.COND
            case Tok.PIPEPIPE:
                return Prec.OR
            case Tok.AMPAMP:
                return Prec.AND
            case Tok.PIPE:
                return Prec.BOR
            case Tok.CARET:
                return Prec.XOR
            case Tok.AMP:
                return Prec.BAND
            case Tok.EXCLAIMEQUAL | Tok.EQUALEQUAL:
                return Prec.EQ
            case Tok.LESSEQUAL | Tok.LESS | Tok.GREATER | Tok.GREATEREQUAL:
                return Prec.COMP
            case Tok.LESSLESS | Tok.GREATERGREATER:
                return Prec.SHIFT
            case Tok.PLUS | Tok.MINUS:
                return Prec.PLUS
            case Tok.PERCENT | Tok.SLASH | Tok.STAR:
                return Prec.STAR
            case _:
                return Prec.UNKNOWN
