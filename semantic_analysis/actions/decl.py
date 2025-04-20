from typing import List, Tuple
from ns_ast.nodes.decl import ParamDecl, StructDecl, Decl, FnDecl
from ns_ast.nodes.stmt import CompoundStmt
from ns_ast.nodes.types import StructType, Type, FunctionType
from semantic_analysis.scope import Scope
from utils.diagnostic import diag, Diag
from lex import Loc
from . import state


def check_fn_decl_compat(
    old_decl: Decl,
    name: str,
    params: List[ParamDecl],
    return_type: Type | None,
    fn_loc: Loc,
    body: bool = False,
    is_vararg: bool = False,
) -> bool:
    if return_type is None:
        return_type = Type()
    if not isinstance(old_decl, FnDecl):
        diag(fn_loc, f"name '{name}' already used with different type", Diag.ERROR)
        return True
    assert isinstance(old_decl.ty, FunctionType)
    if body and old_decl.body is not None:
        diag(fn_loc, f"function '{name}' has already been defined", Diag.ERROR)
        return True
    if len(params) != len(old_decl.param_decls):
        diag(
            fn_loc,
            f"function '{name}' already declared with different parameters",
            Diag.ERROR,
        )
        return True
    for cur_param, prev_param in zip(params, old_decl.param_decls):
        if cur_param.ty != prev_param.ty:
            diag(
                fn_loc,
                f"function '{name}' already declared with different parameters",
                Diag.ERROR,
            )
            return True
    if is_vararg != old_decl.is_vararg:
        diag(
            fn_loc,
            f"function '{name}' already declared with different parameters",
            Diag.ERROR,
        )
        return True
    if return_type != old_decl.ty.return_type:
        diag(
            fn_loc,
            f"function '{name}' already declared with different return type",
            Diag.ERROR,
        )
        return True
    return False


def act_on_fn_decl(
    scope: Scope,
    name: str,
    params: List[ParamDecl],
    return_type: Type | None,
    fn_loc: Loc,
    semi_loc: Loc,
    is_vararg: bool = False,
) -> FnDecl | None:
    old_decl = scope.lookup_named_decl(name)
    is_lib = False
    if old_decl is not None:
        if check_fn_decl_compat(old_decl, name, params, return_type, fn_loc):
            return None
        scope.remove_decl(old_decl)
        is_lib = old_decl.is_lib
    decl = FnDecl((fn_loc, semi_loc), name, params, return_type, is_vararg)
    decl.is_lib = is_lib
    scope.add_decl(decl)
    return decl


def act_on_start_fn_definition(
    scope: Scope,
    name: str,
    params: List[ParamDecl],
    return_type: Type | None,
    fn_loc: Loc,
    is_vararg: bool = False,
) -> FnDecl | None:
    old_decl = scope.lookup_named_decl(name)
    is_lib = False
    if old_decl is not None:
        if check_fn_decl_compat(
            old_decl, name, params, return_type, fn_loc, True, is_vararg
        ):
            return None
        scope.remove_decl(old_decl)
        is_lib = old_decl.is_lib
    decl = FnDecl((fn_loc, 0), name, params, return_type, is_vararg)
    decl.is_lib = is_lib
    scope.add_decl(decl)
    state.CUR_FN_DECL = decl
    return decl


def act_on_end_fn_definition(decl: FnDecl, body: CompoundStmt) -> FnDecl:
    decl.set_body(body)
    state.CUR_FN_DECL = None
    return decl


def act_on_method_decl_name(
    scope: Scope, struct_name: str, loc: Loc, method_name: str, name_loc: Loc
) -> Tuple[str, "StructType | None"]:
    _ = name_loc  # UNUSED
    full_name = f"{struct_name}::{method_name}"
    maybe_struct_decl = scope.lookup_named_decl(struct_name)
    if maybe_struct_decl is None or not isinstance(maybe_struct_decl, StructDecl):
        diag(loc, f"'{struct_name}' does not name a type", Diag.ERROR)
        return full_name, None
    assert isinstance(maybe_struct_decl.ty, StructType)
    return full_name, maybe_struct_decl.ty


def act_on_start_of_translation_unit():
    pass


def act_on_end_of_translation_unit():
    pass
