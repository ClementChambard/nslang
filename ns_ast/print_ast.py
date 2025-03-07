#!/usr/bin/env python3

from typing import Callable, List, Tuple

from .nodes import *
from lex import LOC_INVALID, Loc, OpenedFile

C_RESET = "\x1b[0m"
C_BOLD = "\x1b[1m"
C_ITALIC = "\x1b[3m"
C_RED = "\x1b[31m"
C_GREEN = "\x1b[32m"
C_ORANGE = "\x1b[33m"
C_BLUE = "\x1b[34m"
C_MAGENTA = "\x1b[35m"
C_CYAN = "\x1b[36m"
C_GRAY = "\x1b[2m"

type StrTreeNode = Tuple[str, List[StrTreeNode]]

def _loc_or_range_str(loc_or_range: Loc | Tuple[Loc, Loc]) -> str:
    def loc_str(loc: Loc) -> str:
        if (ploc := OpenedFile.get_loc(loc)) is None:
            return "<invalid>"
        else:
            f, l, c = ploc
            return f"{f}:{l}:{c}"  # TODO: actual
    if type(loc_or_range) is tuple:
        b = loc_str(loc_or_range[0])
        e = loc_str(loc_or_range[1])
        if b == e:
            return b
        return f"{b} {e}"
    else:
        return loc_str(loc_or_range)

def _node_str(name: str, loc_or_range: Loc | Tuple[Loc, Loc] = LOC_INVALID, color: str = C_GREEN + C_BOLD) -> str:
    loc_str = _loc_or_range_str(loc_or_range)
    return f"{color}{name}{C_RESET} <{C_ORANGE}{loc_str}{C_RESET}>"

def _ty_vk_str(ty: Type, vk: ValueKind) -> str:
    n_str = f" {C_GREEN}'{ty}'{C_RESET}"
    if vk == ValueKind.LVALUE:
        n_str += f" {C_BOLD}{C_CYAN}lvalue{C_RESET}"
    return n_str


def _expr_node_str(expr: Expr, name = None) -> str:
    if name is None:
        name = expr.__class__.__name__
    n_str = _node_str(name, expr.get_range(), C_MAGENTA + C_BOLD)
    n_str += _ty_vk_str(expr.ty, expr.value_kind)
    return n_str

def _op_str(expr: BinaryExpr | UnaryExpr) -> str:
    return f" {C_ITALIC}{C_GRAY}'{expr.opc.as_str()}'{C_RESET}"

def _builtinexpr_str(expr: BuiltinExpr) -> StrTreeNode:
    builtin_name = ""
    cap = True
    for c in expr.builtin_name:
        if c == "_":
            cap = True
            continue
        if cap:
            builtin_name += c.upper()
            cap = False
        else:
            builtin_name += c
    n_str = _expr_node_str(expr, name=builtin_name + "Expr")
    sub = [_expr_str(a) for a in expr.args]
    return (n_str, sub)

def _expr_str(expr: Expr) -> StrTreeNode:
    if isinstance(expr, BuiltinExpr):
        return _builtinexpr_str(expr)
    n_str = _expr_node_str(expr)
    if isinstance(expr, IntegerLiteral):
        return (n_str + f" {C_CYAN}{C_BOLD}{expr.value}{C_RESET}", [])
    if isinstance(expr, StringLiteral):
        return (n_str + f" {expr.value}", [])
    elif isinstance(expr, BoolLiteral):
        return (n_str + [" false", " true"][int(expr.value)], [])
    elif isinstance(expr, BinaryExpr):
        return (n_str + _op_str(expr), [_expr_str(expr.lhs), _expr_str(expr.rhs)])
    elif isinstance(expr, CompoundAssignExpr):
        return (n_str + _op_str(expr), [_expr_str(expr.lhs), _expr_str(expr.rhs)])
    elif isinstance(expr, UnaryExpr):
        return (n_str + _op_str(expr), [_expr_str(expr.arg)])
    elif isinstance(expr, ConditionalExpr):
        return (n_str, [_expr_str(expr.cond), _expr_str(expr.lhs), _expr_str(expr.rhs)])
    elif isinstance(expr, ParenExpr):
        return (n_str, [_expr_str(expr.val)])
    elif isinstance(expr, ArraySubscriptExpr):
        return (n_str, [_expr_str(expr.lhs), _expr_str(expr.rhs)])
    elif isinstance(expr, CallExpr):
        return (n_str, [_expr_str(expr.fn)] + [_expr_str(e) for e in expr.args])
    elif isinstance(expr, MemberExpr):
        return (n_str + [" .", " ->"][expr.is_arrow] + expr.name, [_expr_str(expr.base)])
    elif isinstance(expr, CastExpr):
        return (n_str + f" <{C_RED}{expr.kind}{C_RESET}>", [_expr_str(expr.op)])
    elif isinstance(expr, SizeofExpr):
        return (n_str + f" {C_CYAN}{C_BOLD}{expr.ty_of_sizeof}{C_RESET}", [_expr_str(expr.expr)] if expr.expr is not None else [])
    elif isinstance(expr, DeclRefExpr):
        ref_kind = "Var"
        add = ""
        if isinstance(expr.decl, FnDecl): ref_kind = "Function"
        if isinstance(expr.decl, EnumVariantDecl):
            ref_kind = "Enum"
            add = f" = {expr.decl.val}"
        n_str += f" {C_GREEN}{C_BOLD}{ref_kind} {C_CYAN}'{expr.decl.ty}'{C_RESET} {C_GREEN}'{expr.decl.name}'{C_RESET}{add}"
        return (n_str, [])
    else:
        return (n_str + f" {C_BOLD}{C_RED}NO PRINT FUNC DEFINED{C_RESET}", [])

def _stmt_node_str(stmt: Stmt, name = None) -> str:
    if name is None:
        name = stmt.__class__.__name__
    n_str = _node_str(name, stmt.get_range(), C_MAGENTA + C_BOLD)
    return n_str

def _stmt_str(stmt: Stmt) -> StrTreeNode:
    if isinstance(stmt, CompoundStmt):
        return (_stmt_node_str(stmt), [_stmt_str(s) for s in stmt.inner])
    elif isinstance(stmt, IfStmt):
        n_str = _stmt_node_str(stmt)
        sub = [_expr_str(stmt.cond), _stmt_str(stmt.then_stmt)]
        if stmt.else_stmt is not None:
            sub.append(_stmt_str(stmt.else_stmt))
        return (n_str, sub)
    elif isinstance(stmt, WhileStmt):
        return (_stmt_node_str(stmt), [_expr_str(stmt.cond), _stmt_str(stmt.while_stmt)])
    elif isinstance(stmt, DoStmt):
        return (_stmt_node_str(stmt), [_expr_str(stmt.expr), _stmt_str(stmt.body)])
    elif isinstance(stmt, Expr):
        return _expr_str(stmt)
    elif isinstance(stmt, DeclStmt):
        return _decl_str(stmt.decl)
    elif isinstance(stmt, NullStmt) or isinstance(stmt, BreakStmt) or isinstance(stmt, ContinueStmt):
        return (_stmt_node_str(stmt), [])
    elif isinstance(stmt, SwitchStmt):
        return (_stmt_node_str(stmt), [_expr_str(stmt.cond), _stmt_str(stmt.body)])
    elif isinstance(stmt, DefaultStmt):
        return (_stmt_node_str(stmt), [_stmt_str(stmt.sub_stmt)])
    elif isinstance(stmt, CaseStmt):
        return (_stmt_node_str(stmt) + f" case {stmt.case_val}", [_stmt_str(stmt.sub_stmt)] if stmt.sub_stmt is not None else [])
    elif isinstance(stmt, ReturnStmt):
        n_str = _stmt_node_str(stmt)
        sub = []
        if stmt.ret_expr is not None:
            n_str += f" {C_GREEN}'{stmt.ret_expr.ty}'{C_RESET}"
            sub = [_expr_str(stmt.ret_expr)]
        else:
            n_str += f" {C_GREEN}'void'{C_RESET}"
        return (n_str, sub)
    else:
        return (_stmt_node_str(stmt) + f" {C_BOLD}{C_RED}NO PRINT FUNC DEFINED{C_RESET}", [])


def _decl_node_str(decl: Decl, name = None) -> str:
    if name is None:
        name = decl.__class__.__name__
    return _node_str(name, decl.src_range)

def _named_decl_node_str(decl: Decl, name = None) -> str:
    n_str = _decl_node_str(decl, name)
    n_str += f" {C_CYAN}{C_BOLD}{decl.name}{C_RESET}"
    n_str += f" {C_GREEN}'{decl.ty}'{C_RESET}"
    if decl.is_lib:
        n_str += f" {C_GRAY}{C_BOLD}LIB{C_RESET}"
    return n_str

def _decl_str(decl: Decl) -> StrTreeNode:
    if isinstance(decl, FnDecl):
        return (_named_decl_node_str(decl), [_decl_str(p) for p in decl.param_decls] + [_stmt_str(decl.body)] if decl.body is not None else [])
    elif isinstance(decl, TypeDecl):
        if isinstance(decl.ty, AliasType):
            return (_named_decl_node_str(decl, "TypeAliasDecl"), [])
        if isinstance(decl.ty, StructType):
            return (_named_decl_node_str(decl, "StructDecl"), [_decl_str(p) for p in decl.ty.fields])
        if isinstance(decl.ty, EnumType):
            return (_named_decl_node_str(decl, "EnumDecl"), [_decl_str(p) for p in decl.ty.variants])
        return (_named_decl_node_str(decl), [])
    elif isinstance(decl, EnumVariantDecl):
        return (_named_decl_node_str(decl) + f" = {decl.val}", [])
    elif isinstance(decl, NamedDecl):
        return (_named_decl_node_str(decl), [])
        # TODO: var decl might have an initializer
    else:
        return (_decl_node_str(decl) + f" {C_BOLD}{C_RED}NO PRINT FUNC DEFINED{C_RESET}", [])


def _tu_str(tu: List[Decl]) -> StrTreeNode:
    loc_start = 0
    loc_end = 0
    if len(tu) > 0:
        loc_start = tu[0].get_range()[0]
        loc_end = tu[-1].get_range()[1]
    tu_str = _node_str("TranslationUnit", (loc_start, loc_end))
    tree = []
    for decl in tu:
        tree.append(_decl_str(decl))
    return (tu_str, tree)


def _indent_str(indent_list: List[int], last: bool) -> str:
    s = ""
    if len(indent_list) == 0:
        return s
    if len(indent_list) > 1:
        for i in indent_list[:-1]:
            if i == 0:
                s += "   "
            else:
                s += "│  "
    if last:
        s += "╰─ "
    else:
        s += "├─ "
    return s


def _print_string_tree_inner(
    tree: StrTreeNode, indent_list: List[int], last: bool, tree_color: str
):
    istr = _indent_str(indent_list, last)
    if last and len(indent_list) > 0:
        indent_list[-1] = 0
    print(f"{tree_color}{istr}{C_RESET}{tree[0]}")
    children = tree[1]
    if len(children) == 0:
        return
    indent_list.append(1)
    for c in children[:-1]:
        _print_string_tree_inner(c, indent_list, False, tree_color)
    _print_string_tree_inner(children[-1], indent_list, True, tree_color)
    indent_list.pop()


def _print_string_tree(tree: StrTreeNode, tree_color: str = C_BLUE):
    indent_list = []
    _print_string_tree_inner(tree, indent_list, True, tree_color)


def print_ast(translation_unit: List[Decl]):
    _print_string_tree(_tu_str(translation_unit))
