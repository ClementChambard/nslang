#ifndef PARSE_PARSER_HPP_INCLUDED
#define PARSE_PARSER_HPP_INCLUDED

#include "ast/nodes/decl.hpp"
#include "ast/nodes/stmt.hpp"
#include "lexer/lexer.hpp"
#include "parse/prec.hpp"
#include "semantic_analysis/scope.hpp"
#include "semantic_analysis/sema.hpp"
#include <cassert>
#include <memory>
#include <utility>

struct Parser {
  Lexer &lexer;
  Sema &sema;
  Token tok{};
  Loc prev_tok_location = LOC_INVALID;
  i32 paren_count = 0;
  i32 brace_count = 0;
  i32 square_count = 0;
  Scope *cur_scope;

  Parser(Lexer &lexer, Sema &sema) : lexer(lexer), sema(sema) {
    cur_scope = new Scope(nullptr, SF_NO);
    tok = lexer.lex();
  }
  ~Parser() { while (cur_scope) exit_scope(); }

  // clang-format off

  // basic parser functions
  void unconsume_token(Token consumed);
  Loc consume_token();
  bool try_consume_token(Tok expected, Loc *loc = nullptr);
  Loc consume_any_token();
  bool is_token_paren() { return tok.kind == tok::LPAREN || tok.kind == tok::RPAREN; }
  bool is_token_brace() { return tok.kind == tok::LBRACE || tok.kind == tok::RBRACE; }
  bool is_token_square() { return tok.kind == tok::LSQUARE || tok.kind == tok::RSQUARE; }
  bool is_token_special() { return is_token_brace() || is_token_square() || is_token_paren(); }
  Loc consume_paren();
  Loc consume_brace();
  Loc consume_square();
  bool expect_and_consume(Tok expected, char const *diag_id = "expected %s", char const *str = "");
  bool expect_and_consume_semi(char const *diag_id = "expected ';'", char const *token_used = "");
  bool skip_until(std::span<Tok> until_toks, bool at_semi = false, bool before_match = false);
  bool skip_until(Tok t, bool at_semi = false, bool before_match = false) {
    Tok until[] = {t};
    return skip_until(until, at_semi, before_match);
  }
  bool skip_until(Tok t, Tok t2, bool at_semi = false, bool before_match = false) {
    Tok until[] = {t, t2};
    return skip_until(until, at_semi, before_match);
  }
  void enter_scope(ScopeFlags scope_flags) { cur_scope = new Scope(cur_scope, scope_flags); }
  void exit_scope() { auto old_scope = cur_scope; cur_scope = old_scope->get_parent(); delete old_scope; }
  std::unique_ptr<TranslationUnitDecl> parse() { return parse_translation_unit(); }

  // decl parsing functions
  std::unique_ptr<TranslationUnitDecl> parse_translation_unit();
  std::unique_ptr<Decl> parse_top_level_decl();
  std::unique_ptr<Decl> parse_decl(bool in_func, Loc *decl_end = nullptr);
  std::unique_ptr<EnumDecl> parse_enum_decl();
  std::unique_ptr<ParamDecl> parse_param_decl();
  std::unique_ptr<VarDecl> parse_var_decl(bool in_func);
  std::unique_ptr<FunctionDecl> parse_fn_decl();
  std::unique_ptr<AliasDecl> parse_type_alias_decl();
  std::unique_ptr<StructDecl> parse_struct_decl();
  void parse_enum_variant_decl(EnumDecl *ed, i64 &cur_value);
  void parse_field_decl(StructDecl *my_struct);

  // type parsing functions
  Type *parse_type();
  bool is_start_of_type();
  bool following_is_type(int ctx);

  // stmt parsing functions
  std::unique_ptr<IfStmt> parse_if_stmt(Loc *trailing_else_loc = nullptr);
  std::unique_ptr<Stmt> parse_case_stmt(bool missing_case = false, ExprUPtr e = nullptr);
  std::unique_ptr<DefaultStmt> parse_default_stmt();
  std::unique_ptr<SwitchStmt> parse_switch_stmt(Loc *trailing_else_loc = nullptr);
  std::unique_ptr<WhileStmt> parse_while_stmt(Loc *trailing_else_loc = nullptr);
  std::unique_ptr<DoStmt> parse_do_stmt();
  std::unique_ptr<ContinueStmt> parse_continue_stmt();
  std::unique_ptr<BreakStmt> parse_break_stmt();
  std::unique_ptr<ReturnStmt> parse_return_stmt();
  std::unique_ptr<Stmt> parse_expr_stmt();
  std::unique_ptr<CompoundStmt> parse_compound_stmt_body();
  std::unique_ptr<CompoundStmt> parse_compound_stmt(ScopeFlags scope_flags = ScopeFlags(SF_DECL | SF_COMPOUND_STMT));
  std::unique_ptr<Stmt> parse_stmt(Loc *trailing_else_loc = nullptr);
  bool is_declaration_statement();

  // expr parsing functions
  bool parse_expression_list(std::vector<std::unique_ptr<Expr>> &out); 
  ExprUPtr parse_postfix_expression_suffix(ExprUPtr lhs);
  ExprUPtr parse_unit_expr();
  ExprUPtr parse_assignment_expr();
  ExprUPtr parse_rhs_of_binary_expr(ExprUPtr lhs, Prec prec);
  ExprUPtr parse_expr();
  std::pair<i64, ExprUPtr> parse_integer_constexpr();
};

bool is_common_typo(Tok expected, Tok actual);

#endif // PARSE_PARSER_HPP_INCLUDED
