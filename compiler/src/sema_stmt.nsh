#include ast_nodes_stmt.nsh
#include ast_nodes_expr.nsh
#include loc.nsh

struct Scope;
struct Decl;

struct sema;

lib fn sema::act_on_expr_stmt(fe: Expr*, discarded_value: bool) -> Expr*;
lib fn sema::act_on_expr_stmt_error() -> Stmt*;
lib fn sema::act_on_compound_stmt(lp_loc: Loc, rp_loc: Loc, elts: Stmt**, elt_cnt: i64) -> CompoundStmt*;
lib fn sema::act_on_decl_stmt(d: Decl*, start_loc: Loc, end_loc: Loc) -> DeclStmt*;
lib fn sema::act_on_if_stmt(if_loc: Loc, lp_loc: Loc, cond: Expr*, rp_loc: Loc, then_stmt: Stmt*, else_loc: Loc, else_stmt: Stmt*) -> IfStmt*;
lib fn sema::act_on_start_of_switch_stmt(switch_loc: Loc, lp_loc: Loc, cond: Expr*, rp_loc: Loc) -> SwitchStmt*;
lib fn sema::act_on_finish_switch_stmt(sl: Loc, stmt: SwitchStmt*, body: Stmt*) -> SwitchStmt*;
lib fn sema::act_on_while_stmt(while_loc: Loc, lparen_loc: Loc, cond: Expr*, rparen_loc: Loc, body: Stmt*) -> WhileStmt*;
lib fn sema::act_on_do_stmt(do_loc: Loc, body: Stmt*, while_loc: Loc, lp_loc: Loc, cond: Expr*, rp_loc: Loc) -> DoStmt*;
lib fn sema::act_on_default_stmt(default_loc: Loc, colon_loc: Loc, sub_stmt: Stmt*, scope: Scope*) -> DefaultStmt*;
lib fn sema::act_on_case_expr(case_loc: Loc, val: Expr*) -> Expr*;
lib fn sema::check_case_expression(e: Expr*) -> bool;
lib fn sema::act_on_case_stmt(case_loc: Loc, e: Expr*, colon_loc: Loc) -> CaseStmt*;
lib fn sema::act_on_case_stmt_body(s: Stmt*, sub_stmt: Stmt*);
lib fn sema::act_on_continue_stmt(cl: Loc, cur_scope: Scope*) -> ContinueStmt*;
lib fn sema::act_on_break_stmt(bl: Loc, cur_scope: Scope*) -> BreakStmt*;
lib fn sema::act_on_null_stmt(semi_loc: Loc) -> NullStmt*;
lib fn sema::act_on_return_stmt(return_loc: Loc, ret_val_expr: Expr*, scope: Scope*, allow_recovery: bool) -> ReturnStmt*;
lib fn sema::diagnose_unused_expr_result(s: Stmt*, diag_id: CStr);
