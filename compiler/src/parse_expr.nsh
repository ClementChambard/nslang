#include loc.nsh
#include ast_nodes_expr.nsh
#include ast_nodes_unqualified_id.nsh


enum Prec {
  PREC_UNKNOWN,
  PREC_COMMA,
  PREC_ASSIGN,
  PREC_COND,
  PREC_OR,
  PREC_AND,
  PREC_BOR,
  PREC_XOR,
  PREC_BAND,
  PREC_EQ,
  PREC_COMP,
  PREC_SHIFT,
  PREC_PLUS,
  PREC_STAR,
};

lib fn prec_from_bin_op(token_kind: Tok) -> Prec;

lib fn parse_expression_list(exprs: Expr***, exprs_count: i64*) -> bool;
lib fn parse_postfix_expression_suffix(lhs: Expr*) -> Expr*;
lib fn parse_unqualified_id(out: UnqualifiedId*) -> bool;
lib fn parse_unit_expr() -> Expr*;
lib fn parse_assignment_expr() -> Expr*;
lib fn parse_rhs_of_binary_expr(lhs: Expr*, prec: Prec) -> Expr*;
lib fn parse_expr() -> Expr*;
lib fn parse_integer_constexpr(loc: Loc*) -> i64;
