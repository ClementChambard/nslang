from utils.my_enum import Enum, ENUM_INIT, ENUM_N
from . import Stmt, ValueDecl, Type, BuiltinType, BuiltinTypeKind
from lex import Loc, LocRge, Tok, IdentInfo
from dataclasses import dataclass
from typing import List


class ValueKind(Enum):
    PRVALUE = (ENUM_INIT(),)
    LVALUE = (ENUM_N(),)
    XVALUE = (ENUM_N(),)


@dataclass
class Expr(Stmt):
    value_kind: ValueKind
    ty: Type

    def ignore_parens(self):
        return ignore_expr_nodes(self, ignore_parens_single_step)


@dataclass
class DeclRefExpr(Expr):
    decl: ValueDecl
    ii: IdentInfo
    loc: Loc
    qualifier: None

    def __init__(self, nns: None, d: ValueDecl, name: str, ii: IdentInfo, nameloc: Loc, ty: Type, vk: ValueKind):
        self.decl = d
        self.ii = ii
        self.loc = nameloc
        self.ty = ty
        self.value_kind = vk
        self.qualifier = nns

    def get_range(self) -> LocRge:
        return (self.loc, self.loc)


@dataclass
class IntegerLiteral(Expr):
    value: int
    tok_loc: Loc

    def __init__(self, v: int, ty: Type, l: Loc):
        self.ty = ty
        self.value_kind = ValueKind.PRVALUE
        self.tok_loc = l
        self.value = v

    def get_range(self) -> LocRge:
        return (self.tok_loc, self.tok_loc)


@dataclass
class BoolLiteral(Expr):
    value: bool
    loc: Loc

    def __init__(self, val: bool, ty: Type, loc: Loc):
        self.ty = ty
        self.value_kind = ValueKind.PRVALUE #ok ordinary
        self.value = val
        self.loc = loc

    def get_range(self) -> LocRge:
        return (self.loc, self.loc)


@dataclass
class StringLiteral(Expr):
    value: bytes
    locs: List[Loc]
    kind: str

    def __init__(self, val: bytes, locs: List[Loc], kind: str, ty: Type):
        self.ty = ty
        self.value_kind = ValueKind.LVALUE
        self.kind = kind
        self.locs = locs
        self.value = val

    def get_range(self) -> LocRge:
        return (self.locs[0], self.locs[-1])

@dataclass
class ParenExpr(Expr):
    l: Loc
    r: Loc
    val: Expr
    def __init__(self, l: Loc, r: Loc, val: Expr):
        self.l = l
        self.r = r
        self.val = val
        self.value_kind = val.value_kind
        self.ty = val.ty

    def get_range(self) -> LocRge:
        return (self.l, self.r)


class UnaryOperatorKind(Enum):
    POSTINC = ENUM_INIT()
    POSTDEC = ENUM_N()
    PREINC = ENUM_N()
    PREDEC = ENUM_N()
    ADDROF = ENUM_N()
    DEREF = ENUM_N()
    PLUS = ENUM_N()
    MINUS = ENUM_N()
    NOT = ENUM_N()
    LNOT = ENUM_N()

    @staticmethod
    def from_tok(t: Tok):
        match t:
            case Tok.PLUSPLUS: return UnaryOperatorKind.PREINC
            case Tok.MINUSMINUS: return UnaryOperatorKind.PREDEC
            case Tok.AMP: return UnaryOperatorKind.ADDROF
            case Tok.STAR: return UnaryOperatorKind.DEREF
            case Tok.PLUS: return UnaryOperatorKind.PLUS
            case Tok.MINUS: return UnaryOperatorKind.MINUS
            case Tok.TILDE: return UnaryOperatorKind.NOT
            case Tok.EXCLAIM: return UnaryOperatorKind.LNOT


    def as_str(self) -> str:
        match self:
            case self.POSTINC: return "post ++"
            case self.POSTDEC: return "post --"
            case self.PREINC: return "++"
            case self.PREDEC: return "--"
            case self.ADDROF: return "&"
            case self.DEREF: return "*"
            case self.PLUS: return "+"
            case self.MINUS: return "-"
            case self.NOT: return "~"
            case self.LNOT: return "!"


@dataclass
class UnaryExpr(Expr):
    opc: UnaryOperatorKind
    # can_overflow: bool
    # has_fp_features: bool
    loc: Loc
    arg: Expr

    def __init__(self, arg: Expr, opc: UnaryOperatorKind, ty: Type, vk: ValueKind, l: Loc): #ctx, ok, can_overflow, fp_features
        self.opc = opc
        self.loc = l
        self.value_kind = vk
        self.ty = ty
        self.arg = arg
        # ok, can_overflow and fp_fearures ...

    def is_postfix(self) -> bool:
        return self.opc == UnaryOperatorKind.POSTINC or self.opc == UnaryOperatorKind.POSTDEC

    def get_range(self) -> LocRge:
        return (self.arg.get_range()[0], self.loc) if self.is_postfix() else (self.loc, self.arg.get_range()[1])


class BinaryOperatorKind(Enum):
    MUL = ENUM_INIT()
    DIV = ENUM_N()
    REM = ENUM_N()
    ADD = ENUM_N()
    SUB = ENUM_N()
    SHL = ENUM_N()
    SHR = ENUM_N()
    LT = ENUM_N()
    GT = ENUM_N()
    LE = ENUM_N()
    GE = ENUM_N()
    EQ = ENUM_N()
    NE = ENUM_N()
    AND = ENUM_N()
    XOR = ENUM_N()
    OR = ENUM_N()
    LAND = ENUM_N()
    LOR = ENUM_N()
    ASSIGN = ENUM_N()
    MULASSIGN = ENUM_N()
    DIVASSIGN = ENUM_N()
    REMASSIGN = ENUM_N()
    ADDASSIGN = ENUM_N()
    SUBASSIGN = ENUM_N()
    SHLASSIGN = ENUM_N()
    SHRASSIGN = ENUM_N()
    ANDASSIGN = ENUM_N()
    XORASSIGN = ENUM_N()
    ORASSIGN = ENUM_N()

    def is_compound_assignment(self):
        return self in [BinaryOperatorKind.MULASSIGN, BinaryOperatorKind.DIVASSIGN, BinaryOperatorKind.REMASSIGN, BinaryOperatorKind.ADDASSIGN, BinaryOperatorKind.SUBASSIGN, BinaryOperatorKind.SHLASSIGN,
                        BinaryOperatorKind.SHRASSIGN, BinaryOperatorKind.ANDASSIGN, BinaryOperatorKind.XORASSIGN, BinaryOperatorKind.ORASSIGN]

    @staticmethod
    def from_tok(t: Tok):
        match t:
            case Tok.STAR: return BinaryOperatorKind.MUL
            case Tok.SLASH: return BinaryOperatorKind.DIV
            case Tok.PERCENT: return BinaryOperatorKind.REM
            case Tok.PLUS: return BinaryOperatorKind.ADD
            case Tok.MINUS: return BinaryOperatorKind.SUB
            case Tok.LESSLESS: return BinaryOperatorKind.SHL
            case Tok.GREATERGREATER: return BinaryOperatorKind.SHR
            case Tok.LESS: return BinaryOperatorKind.LT
            case Tok.GREATER: return BinaryOperatorKind.GT
            case Tok.LESSEQUAL: return BinaryOperatorKind.LE
            case Tok.GREATEREQUAL: return BinaryOperatorKind.GE
            case Tok.EQUALEQUAL: return BinaryOperatorKind.EQ
            case Tok.EXCLAIMEQUAL: return BinaryOperatorKind.NE
            case Tok.AMP: return BinaryOperatorKind.AND
            case Tok.CARET: return BinaryOperatorKind.XOR
            case Tok.PIPE: return BinaryOperatorKind.OR
            case Tok.AMPAMP: return BinaryOperatorKind.LAND
            case Tok.PIPEPIPE: return BinaryOperatorKind.LOR
            case Tok.EQUAL: return BinaryOperatorKind.ASSIGN
            case Tok.STAREQUAL: return BinaryOperatorKind.MULASSIGN
            case Tok.SLASHEQUAL: return BinaryOperatorKind.DIVASSIGN
            case Tok.PERCENTEQUAL: return BinaryOperatorKind.REMASSIGN
            case Tok.PLUSEQUAL: return BinaryOperatorKind.ADDASSIGN
            case Tok.MINUSEQUAL: return BinaryOperatorKind.SUBASSIGN
            case Tok.LESSLESSEQUAL: return BinaryOperatorKind.SHLASSIGN
            case Tok.GREATERGREATEREQUAL: return BinaryOperatorKind.SHRASSIGN
            case Tok.AMPEQUAL: return BinaryOperatorKind.ANDASSIGN
            case Tok.CARETEQUAL: return BinaryOperatorKind.XORASSIGN
            case Tok.PIPEEQUAL: return BinaryOperatorKind.ORASSIGN
            case _: assert False, "Invalid token for bin op"

    def as_str(self) -> str:
        match self:
            case self.MUL: return "*"
            case self.DIV: return "/"
            case self.REM: return "%"
            case self.ADD: return "+"
            case self.SUB: return "-"
            case self.SHL: return "<<"
            case self.SHR: return ">>"
            case self.LT: return "<"
            case self.GT: return ">"
            case self.LE: return "<="
            case self.GE: return ">="
            case self.EQ: return "=="
            case self.NE: return "!="
            case self.AND: return "&"
            case self.XOR: return "^"
            case self.OR: return "|"
            case self.LAND: return "&&"
            case self.LOR: return "||"
            case self.ASSIGN: return "="
            case self.MULASSIGN: return "*="
            case self.DIVASSIGN: return "/="
            case self.REMASSIGN: return "%="
            case self.ADDASSIGN: return "+="
            case self.SUBASSIGN: return "-="
            case self.SHLASSIGN: return "<<="
            case self.SHRASSIGN: return ">>="
            case self.ANDASSIGN: return "&="
            case self.XORASSIGN: return "^="
            case self.ORASSIGN: return "|="


@dataclass
class BinaryExpr(Expr):
    opc: BinaryOperatorKind
    # has_fp_features: bool
    # excluded_overflow_pattern: bool
    op_loc: Loc
    lhs: Expr
    rhs: Expr

    def __init__(self, lhs: Expr, rhs: Expr, opc: BinaryOperatorKind, res_ty: Type, vk: ValueKind, op_loc: Loc): # ctx, ok, fpfeatures
        self.ty = res_ty
        self.value_kind = vk # ok
        self.opc = opc
        # assert(!is_compound_assignment_op())
        self.op_loc = op_loc
        self.lhs = lhs
        self.rhs = rhs
        # excluded_overflow_pattern = False, fpfeatures ...

    def get_range(self) -> LocRge:
        return (self.lhs.get_range()[0], self.rhs.get_range()[1])


@dataclass
class CompoundAssignExpr(Expr):
    opc: BinaryOperatorKind
    op_loc: Loc
    lhs: Expr
    rhs: Expr
    def __init__(self, lhs: Expr, rhs: Expr, opc: BinaryOperatorKind, res_ty: Type, vk: ValueKind, op_loc: Loc): # ctx, ok, fpfeatures
        self.ty = res_ty
        self.value_kind = vk # ok
        self.opc = opc
        # assert(!is_compound_assignment_op())
        self.op_loc = op_loc
        self.lhs = lhs
        self.rhs = rhs
        # excluded_overflow_pattern = False, fpfeatures ...

    def get_range(self) -> LocRge:
        return (self.lhs.get_range()[0], self.rhs.get_range()[1])


@dataclass
class CallExpr(Expr):
    rparen_loc: Loc
    fn: Expr
    args: [Expr]

    def __init__(self, fn: Expr, args: [Expr], ty: Type, vk: ValueKind, rparen_loc: Loc): # fpfeatures, min_num_args = 0, uses_adl = NotADL
        self.ty = ty
        self.value_kind = vk
        self.fn = fn
        self.args = args
        self.rparen_loc = rparen_loc

    def get_range(self) -> LocRge:
        return (self.fn.get_range()[0], self.rparen_loc)


@dataclass
class SizeofExpr(Expr):
    sizeof_loc: Loc
    rparen_loc: Loc
    expr: Expr | None
    ty_of_sizeof: Type

    def __init__(self, ty, expr, sl, rl):
        from semantic_analysis import TYPES
        self.sizeof_loc = sl
        self.rparen_loc = rl
        self.expr = expr
        self.ty_of_sizeof = ty
        self.ty = TYPES["i64"]
        self.value_kind = ValueKind.PRVALUE

    def get_range(self) -> LocRge:
        return (self.sizeof_loc, self.rparen_loc)


@dataclass
class MemberExpr(Expr):
    # TODO:
    base: Expr
    field_offset: int
    name: str
    is_arrow: bool
    oploc: Loc

    def __init__(self, base, is_arrow, oploc, name: str, ty: Type, vk: ValueKind, field_offset):
        self.ty = ty
        self.value_kind = vk
        self.name = name
        self.base = base
        self.is_arrow = is_arrow
        self.oploc = oploc
        self.field_offset = field_offset

    def get_range(self) -> LocRge:
        return (self.base.get_range()[0], self.oploc)


@dataclass
class ArraySubscriptExpr(Expr):
    lhs: Expr
    rhs: Expr
    rbracket_loc: Loc

    def __init__(self, lhs: Expr, rhs: Expr, t: Type, vk: ValueKind, rbracket_loc: Loc): # ok
        self.ty = t
        self.value_kind = vk
        self.lhs = lhs
        self.rhs = rhs
        self.rbracket_loc = rbracket_loc

    def get_range(self) -> LocRge:
        return (self.lhs.get_range()[0], self.rbracket_loc)


@dataclass
class ConditionalExpr(Expr):
    cond: Expr
    question_loc: Loc
    lhs: Expr
    colon_loc: Loc
    rhs: Expr

    def __init__(self, cond: Expr, qloc: Loc, lhs: Expr, cloc: Loc, rhs: Expr, t: Type, vk: ValueKind): # ok
        self.ty = t
        self.value_kind = vk # ok
        self.question_loc = qloc
        self.colon_loc = cloc
        self.cond = cond
        self.lhs = lhs
        self.rhs = rhs

    def get_range(self) -> LocRge:
        return (self.cond.get_range()[0], self.rhs.get_range[1])


@dataclass
class RecoveryExpr(Expr):
    begin_loc: Loc
    end_loc: Loc
    sub_exprs: List[Expr]

    def __init__(self, t: Type, begin_loc: Loc, end_loc: Loc, sub_exprs: List[Expr]):
        self.ty = t # more complex
        self.value_kind = ValueKind.PRVALUE # more complex
        self.begin_loc = begin_loc
        self.end_loc = end_loc
        self.sub_exprs = sub_exprs

    def get_range(self) -> LocRge:
        return (self.begin_loc, self.end_loc)


@dataclass
class BuiltinExpr(Expr):
    builtin_name: str
    builtin_loc: Loc
    args: List[Expr]
    rparen_loc: Loc

    def __init__(self, builtin: str, loc: Loc, args: List[Expr], rparen_loc: Loc):
        self.ty = BuiltinType(BuiltinTypeKind.I64)
        self.value_kind = ValueKind.PRVALUE
        self.builtin_name = builtin
        self.builtin_loc = loc
        self.args = args
        self.rparen_loc = rparen_loc

    def get_range(self) -> LocRge:
        return (self.builtin_loc, self.rparen_loc)


class CastKind(Enum):
    NOOP = ENUM_INIT()
    TO_VOID = ENUM_N()
    LVALUE_TO_RVALUE = ENUM_N()
    POINTER_TO_BOOLEAN = ENUM_N()
    INTEGRAL_TO_BOOLEAN = ENUM_N()
    INTEGRAL_CAST = ENUM_N()
    ARRAY_TO_POINTER_DECAY = ENUM_N()
    FUNCTION_TO_POINTER_DECAY = ENUM_N()

    def __str__(self) -> str:
        match self:
            case CastKind.NOOP:
                return "Noop"
            case CastKind.TO_VOID:
                return "ToVoid"
            case CastKind.POINTER_TO_BOOLEAN:
                return "PointerToBoolean"
            case CastKind.INTEGRAL_TO_BOOLEAN:
                return "IntegralToBoolean"
            case CastKind.INTEGRAL_CAST:
                return "IntegralCast"
            case CastKind.LVALUE_TO_RVALUE:
                return "LValueToRValue"
            case CastKind.ARRAY_TO_POINTER_DECAY:
                return "ArrayToPointerDecay"
            case CastKind.FUNCTION_TO_POINTER_DECAY:
                return "FunctionToPointerDecay"
            case _:
                assert False, "Unhandled match case"


@dataclass
class CastExpr(Expr):
    op: Expr
    kind: CastKind

    def __init__(self, ty: Type, vk: ValueKind, kind: CastKind, op: Expr):
        self.ty = ty
        self.value_kind = vk
        self.kind = kind
        self.op = op

    def get_range(self) -> LocRge:
        return self.op.get_range()


@dataclass
class ImplicitCastExpr(CastExpr):
    def __init__(self, ty: Type, kind: CastKind, op: Expr, vk: ValueKind):
        super().__init__(ty, vk, kind, op)


def ignore_parens_single_step(e: Expr) -> Expr:
    if isinstance(e, ParenExpr):
        return e.val
    return e;


def ignore_expr_nodes(e: Expr, *args):
    last_e = None
    while e != last_e:
        last_e = e
        for a in args:
            e = a(e)
    return e
