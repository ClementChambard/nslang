#include ast_nodes_stmt.nsh
#include sema_scope.nsh
#include parser.nsh

lib fn Parser::parse_if_stmt(self: Parser*, trailing_else_loc: Loc*) -> IfStmt*;
lib fn Parser::parse_case_stmt(self: Parser*, missing_case: bool, expr: Expr*) -> Stmt*;
lib fn Parser::parse_default_stmt(self: Parser*) -> DefaultStmt*;
lib fn Parser::parse_switch_stmt(self: Parser*, trailing_else_loc: Loc*) -> SwitchStmt*;
lib fn Parser::parse_while_stmt(self: Parser*, trailing_else_loc: Loc*) -> WhileStmt*;
lib fn Parser::parse_do_stmt(self: Parser*) -> DoStmt*;
lib fn Parser::parse_continue_stmt(self: Parser*) -> ContinueStmt*;
lib fn Parser::parse_break_stmt(self: Parser*) -> BreakStmt*;
lib fn Parser::parse_return_stmt(self: Parser*) -> ReturnStmt*;
lib fn Parser::parse_expr_stmt(self: Parser*) -> Stmt*;
lib fn Parser::parse_compound_stmt_body(self: Parser*) -> CompoundStmt*;
lib fn Parser::parse_compound_stmt(self: Parser*, scope_flags: ScopeFlags) -> CompoundStmt*;
lib fn Parser::parse_stmt(self: Parser*, trailing_else_loc: Loc*) -> Stmt*;

lib fn Parser::is_declaration_statement(self: Parser*) -> bool;
