from ns_ast.nodes.stmt import StmtChild
from . import Stmt, ValueDecl, Type, BuiltinType, BuiltinTypeKind, FnDecl
from lex import Loc, LocRge, Tok, IdentInfo
from dataclasses import dataclass
from typing import List, Self
import enum


class ValueKind(enum.Enum):
    PRVALUE = (enum.auto(),)
    LVALUE = (enum.auto(),)
    XVALUE = (enum.auto(),)


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

    def __init__(
        self,
        nns: None,
        d: ValueDecl,
        name: str,
        ii: IdentInfo,
        nameloc: Loc,
        ty: Type,
        vk: ValueKind,
    ):
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
        self.value_kind = ValueKind.PRVALUE  # ok ordinary
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

    def children(self) -> List[StmtChild]:
        return [self.val]


class UnaryOperatorKind(enum.Enum):
    POSTINC = enum.auto()
    POSTDEC = enum.auto()
    PREINC = enum.auto()
    PREDEC = enum.auto()
    ADDROF = enum.auto()
    DEREF = enum.auto()
    PLUS = enum.auto()
    MINUS = enum.auto()
    NOT = enum.auto()
    LNOT = enum.auto()

    @classmethod
    def from_tok(cls, t: Tok):
        match t:
            case Tok.PLUSPLUS:
                return cls.PREINC
            case Tok.MINUSMINUS:
                return cls.PREDEC
            case Tok.AMP:
                return cls.ADDROF
            case Tok.STAR:
                return cls.DEREF
            case Tok.PLUS:
                return cls.PLUS
            case Tok.MINUS:
                return cls.MINUS
            case Tok.TILDE:
                return cls.NOT
            case Tok.EXCLAIM:
                return cls.LNOT

    def __str__(self) -> str:
        match self:
            case self.POSTINC:
                return "post ++"
            case self.POSTDEC:
                return "post --"
            case self.PREINC:
                return "++"
            case self.PREDEC:
                return "--"
            case self.ADDROF:
                return "&"
            case self.DEREF:
                return "*"
            case self.PLUS:
                return "+"
            case self.MINUS:
                return "-"
            case self.NOT:
                return "~"
            case self.LNOT:
                return "!"


@dataclass
class UnaryExpr(Expr):
    opc: UnaryOperatorKind
    # can_overflow: bool
    # has_fp_features: bool
    loc: Loc
    arg: Expr

    def __init__(
        self, arg: Expr, opc: UnaryOperatorKind, ty: Type, vk: ValueKind, l: Loc
    ):  # ctx, ok, can_overflow, fp_features
        self.opc = opc
        self.loc = l
        self.value_kind = vk
        self.ty = ty
        self.arg = arg
        # ok, can_overflow and fp_fearures ...

    def is_postfix(self) -> bool:
        return (
            self.opc == UnaryOperatorKind.POSTINC
            or self.opc == UnaryOperatorKind.POSTDEC
        )

    def get_range(self) -> LocRge:
        return (
            (self.arg.get_range()[0], self.loc)
            if self.is_postfix()
            else (self.loc, self.arg.get_range()[1])
        )

    def children(self) -> List[StmtChild]:
        return [self.arg]


class BinaryOperatorKind(enum.Enum):
    MUL = enum.auto()
    DIV = enum.auto()
    REM = enum.auto()
    ADD = enum.auto()
    SUB = enum.auto()
    SHL = enum.auto()
    SHR = enum.auto()
    AND = enum.auto()
    XOR = enum.auto()
    OR = enum.auto()
    LT = enum.auto()
    GT = enum.auto()
    LE = enum.auto()
    GE = enum.auto()
    EQ = enum.auto()
    NE = enum.auto()
    LAND = enum.auto()
    LOR = enum.auto()
    ASSIGN = enum.auto()
    MULASSIGN = enum.auto()
    DIVASSIGN = enum.auto()
    REMASSIGN = enum.auto()
    ADDASSIGN = enum.auto()
    SUBASSIGN = enum.auto()
    SHLASSIGN = enum.auto()
    SHRASSIGN = enum.auto()
    ANDASSIGN = enum.auto()
    XORASSIGN = enum.auto()
    ORASSIGN = enum.auto()

    def is_compound_assignment(self) -> bool:
        return self.value >= self.MULASSIGN.value and self.value <= self.ORASSIGN.value

    def to_non_compound(self):
        assert self.is_compound_assignment()
        return BinaryOperatorKind(self.value + (self.MUL.value - self.MULASSIGN.value))

    @classmethod
    def from_tok(cls, t: Tok):  #  -> Self
        match t:
            case Tok.STAR:
                return cls.MUL
            case Tok.SLASH:
                return cls.DIV
            case Tok.PERCENT:
                return cls.REM
            case Tok.PLUS:
                return cls.ADD
            case Tok.MINUS:
                return cls.SUB
            case Tok.LESSLESS:
                return cls.SHL
            case Tok.GREATERGREATER:
                return cls.SHR
            case Tok.LESS:
                return cls.LT
            case Tok.GREATER:
                return cls.GT
            case Tok.LESSEQUAL:
                return cls.LE
            case Tok.GREATEREQUAL:
                return cls.GE
            case Tok.EQUALEQUAL:
                return cls.EQ
            case Tok.EXCLAIMEQUAL:
                return cls.NE
            case Tok.AMP:
                return cls.AND
            case Tok.CARET:
                return cls.XOR
            case Tok.PIPE:
                return cls.OR
            case Tok.AMPAMP:
                return cls.LAND
            case Tok.PIPEPIPE:
                return cls.LOR
            case Tok.EQUAL:
                return cls.ASSIGN
            case Tok.STAREQUAL:
                return cls.MULASSIGN
            case Tok.SLASHEQUAL:
                return cls.DIVASSIGN
            case Tok.PERCENTEQUAL:
                return cls.REMASSIGN
            case Tok.PLUSEQUAL:
                return cls.ADDASSIGN
            case Tok.MINUSEQUAL:
                return cls.SUBASSIGN
            case Tok.LESSLESSEQUAL:
                return cls.SHLASSIGN
            case Tok.GREATERGREATEREQUAL:
                return cls.SHRASSIGN
            case Tok.AMPEQUAL:
                return cls.ANDASSIGN
            case Tok.CARETEQUAL:
                return cls.XORASSIGN
            case Tok.PIPEEQUAL:
                return cls.ORASSIGN
            case _:
                assert False, "Invalid token for bin op"

    def __str__(self) -> str:
        match self:
            case self.MUL:
                return "*"
            case self.DIV:
                return "/"
            case self.REM:
                return "%"
            case self.ADD:
                return "+"
            case self.SUB:
                return "-"
            case self.SHL:
                return "<<"
            case self.SHR:
                return ">>"
            case self.LT:
                return "<"
            case self.GT:
                return ">"
            case self.LE:
                return "<="
            case self.GE:
                return ">="
            case self.EQ:
                return "=="
            case self.NE:
                return "!="
            case self.AND:
                return "&"
            case self.XOR:
                return "^"
            case self.OR:
                return "|"
            case self.LAND:
                return "&&"
            case self.LOR:
                return "||"
            case self.ASSIGN:
                return "="
            case self.MULASSIGN:
                return "*="
            case self.DIVASSIGN:
                return "/="
            case self.REMASSIGN:
                return "%="
            case self.ADDASSIGN:
                return "+="
            case self.SUBASSIGN:
                return "-="
            case self.SHLASSIGN:
                return "<<="
            case self.SHRASSIGN:
                return ">>="
            case self.ANDASSIGN:
                return "&="
            case self.XORASSIGN:
                return "^="
            case self.ORASSIGN:
                return "|="


@dataclass
class BinaryExpr(Expr):
    opc: BinaryOperatorKind
    # has_fp_features: bool
    # excluded_overflow_pattern: bool
    op_loc: Loc
    lhs: Expr
    rhs: Expr

    def __init__(
        self,
        lhs: Expr,
        rhs: Expr,
        opc: BinaryOperatorKind,
        res_ty: Type,
        vk: ValueKind,
        op_loc: Loc,
    ):  # ctx, ok, fpfeatures
        self.ty = res_ty
        self.value_kind = vk  # ok
        self.opc = opc
        # assert(!is_compound_assignment_op())
        self.op_loc = op_loc
        self.lhs = lhs
        self.rhs = rhs
        # excluded_overflow_pattern = False, fpfeatures ...

    def get_range(self) -> LocRge:
        return (self.lhs.get_range()[0], self.rhs.get_range()[1])

    def children(self) -> List[StmtChild]:
        return [self.lhs, self.rhs]


@dataclass
class CompoundAssignExpr(BinaryExpr):
    def __init__(
        self,
        lhs: Expr,
        rhs: Expr,
        opc: BinaryOperatorKind,
        res_ty: Type,
        vk: ValueKind,
        op_loc: Loc,
    ):  # ctx, ok, fpfeatures
        self.ty = res_ty
        self.value_kind = vk  # ok
        self.opc = opc
        # assert(is_compound_assignment_op())
        self.op_loc = op_loc
        self.lhs = lhs
        self.rhs = rhs
        # excluded_overflow_pattern = False, fpfeatures ...


@dataclass
class CallExpr(Expr):
    rparen_loc: Loc
    fn: Expr
    args: List[Expr]

    def __init__(
        self, fn: Expr, args: List[Expr], ty: Type, vk: ValueKind, rparen_loc: Loc
    ):  # fpfeatures, min_num_args = 0, uses_adl = NotADL
        self.ty = ty
        self.value_kind = vk
        self.fn = fn
        self.args = args
        self.rparen_loc = rparen_loc

    def get_range(self) -> LocRge:
        return (self.fn.get_range()[0], self.rparen_loc)

    def children(self) -> List[StmtChild]:
        return [self.fn, *self.args]


@dataclass
class MethodCallExpr(CallExpr):
    def __init__(
        self, method: Expr, args: List[Expr], ty: Type, vk: ValueKind, rparen_loc: Loc
    ):
        super().__init__(method, args, ty, vk, rparen_loc)


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

    def children(self) -> List[StmtChild]:
        return [] if self.expr is None else [self.expr]


@dataclass
class MemberExpr(Expr):
    # TODO:
    base: Expr
    field_offset: int
    name: str
    is_arrow: bool
    oploc: Loc

    def __init__(
        self, base, is_arrow, oploc, name: str, ty: Type, vk: ValueKind, field_offset
    ):
        self.ty = ty
        self.value_kind = vk
        self.name = name
        self.base = base
        self.is_arrow = is_arrow
        self.oploc = oploc
        self.field_offset = field_offset

    def get_range(self) -> LocRge:
        return (self.base.get_range()[0], self.oploc)

    def children(self) -> List[StmtChild]:
        return [self.base]


@dataclass
class MethodExpr(Expr):
    self_object: Expr
    method_func: FnDecl
    is_arrow: bool
    oploc: Loc

    def __init__(self, self_object, is_arrow, oploc, method: FnDecl):
        self.ty = method.ty
        self.value_kind = ValueKind.PRVALUE
        self.oploc = oploc
        self.is_arrow = is_arrow
        self.method_func = method
        self.self_object = self_object

    def get_range(self) -> LocRge:
        return (self.self_object.get_range()[0], self.oploc)

    def children(self) -> List[StmtChild]:
        return [self.self_object]


@dataclass
class ArraySubscriptExpr(Expr):
    lhs: Expr
    rhs: Expr
    rbracket_loc: Loc

    def __init__(
        self, lhs: Expr, rhs: Expr, t: Type, vk: ValueKind, rbracket_loc: Loc
    ):  # ok
        self.ty = t
        self.value_kind = vk
        self.lhs = lhs
        self.rhs = rhs
        self.rbracket_loc = rbracket_loc

    def get_range(self) -> LocRge:
        return (self.lhs.get_range()[0], self.rbracket_loc)

    def children(self) -> List[StmtChild]:
        return [self.lhs, self.rhs]


@dataclass
class ConditionalExpr(Expr):
    cond: Expr
    question_loc: Loc
    lhs: Expr
    colon_loc: Loc
    rhs: Expr

    def __init__(
        self,
        cond: Expr,
        qloc: Loc,
        lhs: Expr,
        cloc: Loc,
        rhs: Expr,
        t: Type,
        vk: ValueKind,
    ):  # ok
        self.ty = t
        self.value_kind = vk  # ok
        self.question_loc = qloc
        self.colon_loc = cloc
        self.cond = cond
        self.lhs = lhs
        self.rhs = rhs

    def get_range(self) -> LocRge:
        return (self.cond.get_range()[0], self.rhs.get_range()[1])

    def children(self) -> List[StmtChild]:
        return [self.cond, self.lhs, self.rhs]


@dataclass
class RecoveryExpr(Expr):
    begin_loc: Loc
    end_loc: Loc
    sub_exprs: List[Expr]

    def __init__(self, t: Type, begin_loc: Loc, end_loc: Loc, sub_exprs: List[Expr]):
        self.ty = t  # more complex
        self.value_kind = ValueKind.PRVALUE  # more complex
        self.begin_loc = begin_loc
        self.end_loc = end_loc
        self.sub_exprs = sub_exprs

    def get_range(self) -> LocRge:
        return (self.begin_loc, self.end_loc)

    def children(self) -> List[StmtChild]:
        return list(self.sub_exprs)


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

    def children(self) -> List[StmtChild]:
        return list(self.args)


class CastKind(enum.Enum):
    NOOP = enum.auto()
    TO_VOID = enum.auto()
    LVALUE_TO_RVALUE = enum.auto()
    POINTER_TO_BOOLEAN = enum.auto()
    INTEGRAL_TO_BOOLEAN = enum.auto()
    INTEGRAL_CAST = enum.auto()
    ARRAY_TO_POINTER_DECAY = enum.auto()
    FUNCTION_TO_POINTER_DECAY = enum.auto()

    def __str__(self) -> str:
        match self:
            case self.NOOP:
                return "Noop"
            case self.TO_VOID:
                return "ToVoid"
            case self.POINTER_TO_BOOLEAN:
                return "PointerToBoolean"
            case self.INTEGRAL_TO_BOOLEAN:
                return "IntegralToBoolean"
            case self.INTEGRAL_CAST:
                return "IntegralCast"
            case self.LVALUE_TO_RVALUE:
                return "LValueToRValue"
            case self.ARRAY_TO_POINTER_DECAY:
                return "ArrayToPointerDecay"
            case self.FUNCTION_TO_POINTER_DECAY:
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

    def children(self) -> List[StmtChild]:
        return [self.op]


@dataclass
class ImplicitCastExpr(CastExpr):
    def __init__(self, ty: Type, kind: CastKind, op: Expr, vk: ValueKind):
        super().__init__(ty, vk, kind, op)


@dataclass
class VAArgExpr(Expr):
    s: Loc
    e: Loc

    def __init__(self, ty: Type, s: Loc, e: Loc):
        self.s = s
        self.e = e
        self.ty = ty
        self.value_kind = ValueKind.PRVALUE

    def get_range(self) -> LocRge:
        return (self.s, self.e)


def ignore_parens_single_step(e: Expr) -> Expr:
    if isinstance(e, ParenExpr):
        return e.val
    return e


def ignore_expr_nodes(e: Expr, *args):
    last_e = None
    while e != last_e:
        last_e = e
        for a in args:
            e = a(e)
    return e
