#include sema_scope.nsh
#include ast_nodes_decl.nsh
#include loc.nsh

struct sema;
struct StructType;
struct CompoundStmt;

lib fn sema::act_on_method_decl_name(scope: Scope*, struct_name: CStr, loc: Loc, method_name: CStr, name_loc: Loc, out_fn_name: CStr*, out_fn_scope: StructType**);
lib fn sema::act_on_start_of_translation_unit();
lib fn sema::act_on_end_of_translation_unit();

lib fn sema::act_on_fn_decl(scope: Scope*, name: CStr, params: ParamDecl**, params_count: i64, return_type: Type*, fn_loc: Loc, semi_loc: Loc, is_vararg: bool) -> FnDecl*;
lib fn sema::act_on_start_fn_definition(scope: Scope*, name: CStr, params: ParamDecl**, params_count: i64, return_type: Type*, fn_loc: Loc, is_vararg: bool) -> FnDecl*;
lib fn sema::act_on_end_fn_definition(decl: FnDecl*, body: CompoundStmt*) -> FnDecl*;

