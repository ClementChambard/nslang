from dataclasses import dataclass
from typing import List, Self
from lex import Loc, LocRge
from . import Decl


@dataclass
class Stmt:
    pass


@dataclass
class DeclStmt(Stmt):
    decl: Decl
    start_loc: Loc
    end_loc: Loc

    def __init__(self, start: Loc, end: Loc, decl: Decl):
        self.decl = decl
        self.start_loc = start
        self.end_loc = end

    def get_range(self) -> LocRge:
        return (self.start_loc, self.end_loc)


@dataclass
class NullStmt(Stmt):
    # hasleadingemptymacro: bool
    semi_loc: Loc

    def __init__(self, semi_loc: Loc):
        self.semi_loc = semi_loc

    def get_range(self) -> LocRge:
        return (self.semi_loc, self.semi_loc)


@dataclass
class CompoundStmt(Stmt):
    # hasfpfeatures
    inner: List[Stmt]
    lbrace_loc: Loc
    rbrace_loc: Loc

    def __init__(self, stmts: List[Stmt], lb: Loc, rb: Loc): # hasfpfeatures
        # hasfpfeatures
        self.inner = stmts
        self.lbrace_loc = lb
        self.rbrace_loc = rb

    def get_range(self) -> LocRge:
        return (self.lbrace_loc, self.rbrace_loc)


@dataclass
class SwitchCase(Stmt):
    keyword_loc: Loc
    colon_loc: Loc
    next_switch_case: Self | None


@dataclass
class CaseStmt(SwitchCase):
    # CaseStmtIsGNURange: bool
    sub_stmt: Stmt
    case_val: int

    def __init__(self, dl: Loc, cl: Loc, sub_stmt: Stmt, case_val: int):
        self.keyword_loc = dl
        self.colon_loc = cl
        self.sub_stmt = sub_stmt
        self.next_switch_case = None
        self.case_val = case_val

    def get_range(self) -> LocRge:
        if self.sub_stmt is None:
            return (self.keyword_loc, self.colon_loc)
        substmt = self.sub_stmt
        while isinstance(substmt, CaseStmt):
            substmt = substmt.sub_stmt
        return (self.keyword_loc, substmt.get_range()[1])


@dataclass
class DefaultStmt(SwitchCase):
    sub_stmt: Stmt

    def __init__(self, dl: Loc, cl: Loc, sub_stmt: Stmt):
        self.next_switch_case = None
        self.keyword_loc = dl
        self.colon_loc = cl
        self.sub_stmt = sub_stmt

    def get_range(self) -> LocRge:
        return (self.keyword_loc, self.sub_stmt.get_range()[1])


# CPP: Label stmt, Attributed Stmt


@dataclass
class IfStmt(Stmt):
    # kind: Ordinary, Constexpr, ConstevalNonNegated, ConstevalNegated
    # has_else, has_var, has_init
    if_loc: Loc
    cond: "Expr"
    then_stmt: Stmt
    else_stmt: Stmt | None
    else_loc: Loc | None
    lparen_loc: Loc
    rparen_loc: Loc

    def __init__(self, il: Loc, cond: "Expr", lp: Loc, rp: Loc, then: Stmt, el: Loc | None = None, els: Stmt | None = None): # init, var, ctx, kind ...
        self.lparen_loc = lp
        self.rparen_loc = rp
        # has ...
        # kind
        self.cond = cond
        self.then_stmt = then
        self.else_stmt = els
        self.else_loc = el
        self.if_loc = il
        # init, var ...

    def get_range(self) -> LocRge:
        if self.else_stmt is not None:
            return (self.if_loc, self.else_stmt.get_range()[1])
        return (self.if_loc, self.then_stmt.get_range()[1])


@dataclass
class SwitchStmt(Stmt):
    # has_init has_var
    # all_enum_cases_covered: bool
    switch_loc: Loc
    first_case: SwitchCase | None
    lparen_loc: Loc
    rparen_loc: Loc
    cond: "Expr"
    body: Stmt

    def __init__(self, sl: Loc, cond: "Expr", lp: Loc, rp: Loc, body: Stmt):
        self.switch_loc = sl
        self.first_case = None
        self.lparen_loc = lp
        self.rparen_loc = rp
        self.cond = cond
        self.body = body

    def get_range(self) -> LocRge:
        return (self.switch_loc, self.body.get_range()[1])


@dataclass
class WhileStmt(Stmt):
    # has_var
    while_loc: Loc
    cond: "Expr"
    while_stmt: Stmt
    lparen_loc: Loc
    rparen_loc: Loc

    def __init__(self, wl: Loc, cond: "Expr", lp: Loc, rp: Loc, body: Stmt):
        self.lparen_loc = lp
        self.rparen_loc = rp
        self.cond = cond
        self.while_stmt = body
        self.while_loc = wl
        # init, var ...

    def get_range(self) -> LocRge:
        return (self.while_loc, self.while_stmt.get_range()[1])


@dataclass
class DoStmt(Stmt):
    do_loc: Loc
    body: Stmt
    expr: "Expr"
    while_loc: Loc
    rparen_loc: Loc

    def __init__(self, body: Stmt, cond: "Expr", dl: Loc, wl: Loc, rp: Loc):
        self.do_loc = dl
        self.body = body
        self.expr = cond
        self.while_loc = wl
        self.rparen_loc = rp

    def get_range(self) -> LocRge:
        return (self.do_loc, self.rparen_loc)


@dataclass
class ForStmt(Stmt):
    for_loc: Loc
    lparen_loc: Loc
    rparen_loc: Loc
    # TODO: init, condvar, cond, inc, body


@dataclass
class ContinueStmt(Stmt):
    continue_loc: Loc

    def __init__(self, cl: Loc):
        self.continue_loc = cl

    def get_range(self) -> LocRge:
        return (self.continue_loc, self.continue_loc)


@dataclass
class BreakStmt(Stmt):
    break_loc: Loc

    def __init__(self, bl: Loc):
        self.break_loc = bl

    def get_range(self) -> LocRge:
        return (self.break_loc, self.break_loc)


@dataclass
class ReturnStmt(Stmt):
    # hasnrvocandidate: bool
    return_loc: Loc
    ret_expr: "Expr | None"

    def __init__(self, rl: Loc, e: "Expr"): # const VarDecl *NRVOCandidate
        self.ret_expr = e
        # self.hasnrvocandidate = NRVOCandidate != nullptr
        # set candidate ...
        self.return_loc = rl

    def get_range(self) -> LocRge:
        if self.ret_expr is None:
            return (self.return_loc, self.return_loc)
        return (self.return_loc, self.ret_expr.get_range()[1])
