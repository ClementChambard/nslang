#include loc.nsh

struct Decl;
struct Expr;

enum StmtKind {
  STMTKIND_NONE,
  STMTKIND_DECL,
  STMTKIND_NULL,
  STMTKIND_COMPOUND,
  STMTKIND_SWITCHCASE,
  STMTKIND_CASE,
  STMTKIND_DEFAULT,
  STMTKIND_IF,
  STMTKIND_SWITCH,
  STMTKIND_WHILE,
  STMTKIND_DO,
  STMTKIND_CONTINUE,
  STMTKIND_BREAK,
  STMTKIND_RETURN,
  EXPRKIND_DECLREF,
  EXPRKIND_INTLIT,
  EXPRKIND_BOOLLIT,
  EXPRKIND_STRLIT,
  EXPRKIND_PAREN,
  EXPRKIND_UNARY,
  EXPRKIND_BINARY,
  EXPRKIND_COMPOUNDASSIGN,
  EXPRKIND_CALL,
  EXPRKIND_METHODCALL,
  EXPRKIND_SIZEOF,
  EXPRKIND_MEMBER,
  EXPRKIND_METHOD,
  EXPRKIND_ARRAYSUBSCRIPT,
  EXPRKIND_CONDITIONAL,
  EXPRKIND_RECOVERY,
  EXPRKIND_CAST,
  EXPRKIND_IMPLICITCAST,
  EXPRKIND_VAARG,
};

struct Stmt {
  kind: StmtKind;
};

lib fn Stmt::get_range(self: Stmt*, out: LocRge*);
lib fn Stmt::ast_free(self: Stmt*);

struct DeclStmt {
  super base: Stmt;
  decl: Decl*;
  start_loc: Loc;
  end_loc: Loc;
};

lib fn DeclStmt::new(start: Loc, end: Loc, decl: Decl*) -> DeclStmt*;

struct NullStmt {
  super base: Stmt;
  semi_loc: Loc;
};

lib fn NullStmt::new(semi_loc: Loc) -> NullStmt*;

struct CompoundStmt {
  super base: Stmt;
  children: Stmt**; // owns this pointer
  children_count: i64;
  lbrace_loc: Loc;
  rbrace_loc: Loc;
};

lib fn CompoundStmt::new(children: Stmt**, children_count: i64, lbrace_loc: Loc, rbrace_loc: Loc) -> CompoundStmt*;

struct SwitchCase {
  super stmt_base: Stmt;
  keyword_loc: Loc;
  colon_loc: Loc;
  next_switch_case: SwitchCase*;
};

struct CaseStmt {
  super base: SwitchCase;
  sub_stmt: Stmt*;
  case_val: i64;
};

lib fn CaseStmt::new(kwl: Loc, cl: Loc, sub_stmt: Stmt*, case_val: i64) -> CaseStmt*;

struct DefaultStmt {
  super base: SwitchCase;
  sub_stmt: Stmt*;
};

lib fn DefaultStmt::new(kwl: Loc, cl: Loc, sub_stmt: Stmt*) -> DefaultStmt*;

struct IfStmt {
  super base: Stmt;
  if_loc: Loc;
  cond: Expr*;
  then_stmt: Stmt*;
  else_stmt: Stmt*;
  else_loc: Loc;
  lparen_loc: Loc;
  rparen_loc: Loc;
};

lib fn IfStmt::new(il: Loc, cond: Expr*, lp: Loc, rp: Loc, then: Stmt*, el: Loc, els: Stmt*) -> IfStmt*;

struct SwitchStmt {
  super base: Stmt;
  switch_loc: Loc;
  first_case: SwitchCase*;
  lparen_loc: Loc;
  rparen_loc: Loc;
  cond: Expr*;
  body: Stmt*;
};

lib fn SwitchStmt::new(sl: Loc, cond: Expr*, lp: Loc, rp: Loc, body: Stmt*) -> SwitchStmt*;

struct WhileStmt {
  super base: Stmt;
  while_loc: Loc;
  cond: Expr*;
  body: Stmt*;
  lparen_loc: Loc;
  rparen_loc: Loc;
};

lib fn WhileStmt::new(wl: Loc, cond: Expr*, lp: Loc, rp: Loc, body: Stmt*) -> WhileStmt*;

struct DoStmt {
  super base: Stmt;
  do_loc: Loc;
  body: Stmt*;
  cond: Expr*;
  while_loc: Loc;
  rparen_loc: Loc;
};

lib fn DoStmt::new(body: Stmt*, cond: Expr*, dl: Loc, wl: Loc, rp: Loc) -> DoStmt*;

struct ForStmt {
  super base: Stmt;
  // TODO
};

struct ContinueStmt {
  super base: Stmt;
  continue_loc: Loc;
};

lib fn ContinueStmt::new(cl: Loc) -> ContinueStmt*;

struct BreakStmt {
  super base: Stmt;
  break_loc: Loc;
};

lib fn BreakStmt::new(cl: Loc) -> BreakStmt*;

struct ReturnStmt {
  super base: Stmt;
  return_loc: Loc;
  ret_expr: Expr*;
};

lib fn ReturnStmt::new(rl: Loc, e: Expr*) -> ReturnStmt*;

