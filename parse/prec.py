from utils.my_enum import Enum, ENUM_INIT, ENUM_N
from lex import Tok

class Prec(Enum):
    UNKNOWN = ENUM_INIT()
    COMMA = ENUM_N()
    ASSIGN = ENUM_N()
    COND = ENUM_N()
    OR = ENUM_N()
    AND = ENUM_N()
    BOR = ENUM_N()
    XOR = ENUM_N()
    BAND = ENUM_N()
    EQ = ENUM_N()
    COMP = ENUM_N()  # CPP -> spaceship after
    SHIFT = ENUM_N()
    PLUS = ENUM_N()
    STAR = ENUM_N()  # CPP -> ptr to member after

    @staticmethod
    def from_bin_op(token_kind: Tok, greater_than_is_operator: bool) -> "Prec":
        match token_kind:
            case Tok.GREATER:
                return [Prec.UNKNOWN, Prec.COMP][greater_than_is_operator]
            case Tok.GREATERGREATER:
                return [Prec.UNKNOWN, Prec.SHIFT][greater_than_is_operator]
            case Tok.COMMA:
                return Prec.COMMA
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
            case Tok.LESSEQUAL | Tok.LESS | Tok.GREATEREQUAL:
                return Prec.COMP
            case Tok.LESSLESS:
                return Prec.SHIFT
            case Tok.PLUS | Tok.MINUS:
                return Prec.PLUS
            case Tok.PERCENT | Tok.SLASH | Tok.STAR:
                return Prec.STAR
            case _:
                return Prec.UNKNOWN
