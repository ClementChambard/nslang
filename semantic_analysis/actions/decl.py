from semantic_analysis import Scope
from ns_ast.nodes import FnDecl, Type
from utils.diagnostic import diag, Diag
from . import state

def check_fn_decl_compat(old_decl: FnDecl, name, params, return_type, fn_loc, body: bool = False, is_vararg = False) -> bool:
    if return_type is None:
        return_type = Type()
    if not isinstance(old_decl, FnDecl):
        diag(fn_loc, f"name '{name}' already used with different type", Diag.ERROR)
        return True
    if body and old_decl.body is not None:
        diag(fn_loc, f"function '{name}' has already been defined", Diag.ERROR)
        return True
    if len(params) != len(old_decl.param_decls):
        diag(fn_loc, f"function '{name}' already declared with different parameters", Diag.ERROR)
        return True
    for cur_param, prev_param in zip(params, old_decl.param_decls):
        if cur_param.ty != prev_param.ty:
            diag(fn_loc, f"function '{name}' already declared with different parameters", Diag.ERROR)
            return True
    if is_vararg != old_decl.is_vararg:
        diag(fn_loc, f"function '{name}' already declared with different parameters", Diag.ERROR)
        return True
    if return_type != old_decl.ty.return_type:
        diag(fn_loc, f"function '{name}' already declared with different return type", Diag.ERROR)
        return True
    return False

def act_on_fn_decl(scope, name, params, return_type, fn_loc, semi_loc, is_vararg = False):
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

def act_on_start_fn_definition(scope, name, params, return_type, fn_loc, is_vararg = False):
    old_decl = scope.lookup_named_decl(name)
    is_lib = False
    if old_decl is not None:
        if check_fn_decl_compat(old_decl, name, params, return_type, fn_loc, True, is_vararg):
            return None
        scope.remove_decl(old_decl)
        is_lib = old_decl.is_lib
    decl = FnDecl((fn_loc, 0), name, params, return_type, is_vararg)
    decl.is_lib = is_lib
    scope.add_decl(decl)
    state.CUR_FN_DECL = decl
    return decl

def act_on_end_fn_definition(decl, body):
    decl.set_body(body)
    state.CUR_FN_DECL = None
    return decl
