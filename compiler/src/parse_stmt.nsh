#include ast_nodes_stmt.nsh
#include sema_scope.nsh

enum ParsedStmtContext : i64 {
  PSC_ALLOW_DECLARATION_IN_C = 1,
  PSC_IN_STMT_EXPR = 4,
  PSC_SUB_STMT = 0,
  PSC_COMPOUND = 1,
};

lib fn parse_if_stmt(trailing_else_loc: Loc*) -> IfStmt*;
lib fn parse_case_stmt(stmt_ctx: ParsedStmtContext, missing_case: bool, expr: Expr*) -> Stmt*;
lib fn parse_default_stmt(stmt_ctx: ParsedStmtContext) -> DefaultStmt*;
lib fn parse_switch_stmt(trailing_else_loc: Loc*) -> SwitchStmt*;
lib fn parse_while_stmt(trailing_else_loc: Loc*) -> WhileStmt*;
lib fn parse_do_stmt() -> DoStmt*;
lib fn parse_continue_stmt() -> ContinueStmt*;
lib fn parse_break_stmt() -> BreakStmt*;
// lib fn parse_initializer();
lib fn parse_return_stmt() -> ReturnStmt*;
lib fn parse_expr_stmt(stmt_ctx: ParsedStmtContext) -> Stmt*;
lib fn parse_compound_stmt_body() -> CompoundStmt*;
lib fn parse_compound_stmt(scope_flags: ScopeFlags) -> CompoundStmt*;
lib fn parse_stmt(stmt_ctx: ParsedStmtContext, trailing_else_loc: Loc*) -> Stmt*;

lib fn is_declaration_statement() -> bool;
