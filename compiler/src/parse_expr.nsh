#include loc.nsh
#include ast_nodes_expr.nsh
#include ast_nodes_unqualified_id.nsh
#include parser.nsh

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

lib fn Parser::parse_expression_list(self: Parser*, exprs: Expr***, exprs_count: i64*) -> bool;
lib fn Parser::parse_postfix_expression_suffix(self: Parser*, lhs: Expr*) -> Expr*;
lib fn Parser::parse_unqualified_id(self: Parser*, out: UnqualifiedId*) -> bool;
lib fn Parser::parse_unit_expr(self: Parser*) -> Expr*;
lib fn Parser::parse_assignment_expr(self: Parser*) -> Expr*;
lib fn Parser::parse_rhs_of_binary_expr(self: Parser*, lhs: Expr*, prec: Prec) -> Expr*;
lib fn Parser::parse_expr(self: Parser*) -> Expr*;
lib fn Parser::parse_integer_constexpr(self: Parser*, loc: Loc*) -> i64;
