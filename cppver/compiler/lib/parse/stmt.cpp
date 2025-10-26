#include "ast/nodes/stmt.hpp"
#include "diags/diagnostic.hpp"
#include "lexer/loc.hpp"
#include "lexer/tok.hpp"
#include "parser.hpp"
#include "semantic_analysis/scope.hpp"

std::unique_ptr<IfStmt> Parser::parse_if_stmt(Loc *trailing_else_loc) {
  assert(tok.kind == tok::KW_IF);
  auto if_loc = consume_token();

  if (tok.kind != tok::LPAREN) {
    Diag(diag::ERROR, tok.loc, "expected '(' after 'if'");
    skip_until(tok::SEMI);
    return nullptr;
  }

  enter_scope(ScopeFlags(SF_DECL | SF_CONTROL));

  auto lp_loc = consume_paren();
  auto start = tok.loc;
  auto cond = parse_expr();

  if (!cond && tok.kind != tok::RPAREN) {
    skip_until(tok::SEMI);
    if (tok.kind != tok::RPAREN) {
      exit_scope();
      return nullptr;
    }
  }
  if (!cond) {
    cond = sema.create_recovery_expr(
        start, tok.loc == start ? start : prev_tok_location, {});
  }

  auto rp_loc = tok.loc;
  expect_and_consume(tok::RPAREN);

  while (tok.kind == tok::RPAREN) {
    Diag(diag::ERROR, tok.loc,
         "extraneous ')' after condition, expected a statement");
    consume_paren();
  }

  bool is_braced = tok.kind == tok::LBRACE;

  if (is_braced)
    enter_scope(SF_DECL);

  auto then_stmt_loc = tok.loc;
  Loc inner_statement_trailing_else_loc = LOC_INVALID;
  auto then_stmt = parse_stmt(&inner_statement_trailing_else_loc);
  if (is_braced)
    exit_scope();

  Loc else_loc = LOC_INVALID;
  StmtUPtr else_stmt = nullptr;

  if (tok.kind == tok::KW_ELSE) {
    if (trailing_else_loc)
      *trailing_else_loc = tok.loc;
    else_loc = consume_token();
    is_braced = tok.kind == tok::LBRACE;
    if (is_braced)
      enter_scope(SF_DECL);
    else_stmt = parse_stmt();
    if (is_braced)
      exit_scope();
  } else if (inner_statement_trailing_else_loc != LOC_INVALID) {
    Diag(diag::WARNING, inner_statement_trailing_else_loc,
         "add explicit braces to avoid dangling else");
  }

  exit_scope();

  if (!then_stmt && !else_stmt)
    return nullptr;

  if (!then_stmt)
    then_stmt = sema.act_on_null_stmt(then_stmt_loc);

  return sema.act_on_if_stmt(if_loc, lp_loc, std::move(cond), rp_loc,
                             std::move(then_stmt), else_loc,
                             std::move(else_stmt));
}

std::unique_ptr<Stmt> Parser::parse_case_stmt(bool missing_case, ExprUPtr e) {
  assert(missing_case || tok.kind == tok::KW_CASE);
  StmtUPtr top_level_case = nullptr;
  CaseStmt *deepest_parsed_case_stmt = nullptr;
  Loc colon_loc = LOC_INVALID;
  while (missing_case || tok.kind == tok::KW_CASE) {
    Loc case_loc;
    if (!missing_case) {
      case_loc = consume_token();
    } else {
      assert(e);
      case_loc = e->get_start_loc();
    }
    colon_loc = LOC_INVALID;
    ExprUPtr lhs;
    if (missing_case) {
      lhs = std::move(e);
      missing_case = false;
    } else {
      lhs = parse_expr();
      if (!lhs) {
        if (!skip_until(tok::COLON, tok::RBRACE, true, true))
          return nullptr;
      }
    }
    assert(lhs);
    if (!try_consume_token(tok::COLON, &colon_loc)) {
      Diag(diag::ERROR, prev_tok_location, "expected ':' after 'case'");
      colon_loc = prev_tok_location;
    }
    auto case_stmt = sema.act_on_case_stmt(case_loc, std::move(lhs), colon_loc);
    if (!case_stmt) {
      if (!top_level_case)
        return parse_stmt();
    } else {
      if (top_level_case) {
        auto next_deepest = case_stmt.get();
        sema.act_on_case_stmt_body(deepest_parsed_case_stmt,
                                   std::move(case_stmt));
        deepest_parsed_case_stmt = next_deepest;
      } else {
        deepest_parsed_case_stmt = case_stmt.get();
        top_level_case = std::move(case_stmt);
      }
    }
  }
  StmtUPtr sub_stmt = nullptr;
  if (tok.kind == tok::RBRACE)
    sub_stmt = sema.act_on_null_stmt(colon_loc);
  else
    sub_stmt = parse_stmt();
  if (deepest_parsed_case_stmt) {
    sema.act_on_case_stmt_body(deepest_parsed_case_stmt, std::move(sub_stmt));
  }
  return top_level_case;
}

std::unique_ptr<DefaultStmt> Parser::parse_default_stmt() {
  assert(tok.kind == tok::KW_DEFAULT);
  auto default_loc = consume_token();
  Loc colon_loc = LOC_INVALID;
  if (try_consume_token(tok::COLON, &colon_loc)) {
  } else if (try_consume_token(tok::SEMI, &colon_loc)) {
    Diag(diag::ERROR, colon_loc, "expected ':' after 'default'");
    // fixit
  } else {
    Diag(diag::ERROR, prev_tok_location, "expected ':' after 'default'");
    // fixit
    colon_loc = prev_tok_location;
  }
  StmtUPtr sub_stmt = nullptr;
  if (tok.kind == tok::RBRACE)
    sub_stmt = sema.act_on_null_stmt(colon_loc);
  else
    sub_stmt = parse_stmt();
  return sema.act_on_default_stmt(default_loc, colon_loc, std::move(sub_stmt),
                                  cur_scope);
}

std::unique_ptr<SwitchStmt> Parser::parse_switch_stmt(Loc *trailing_else_loc) {
  assert(tok.kind == tok::KW_SWITCH);
  auto switch_loc = consume_token();

  if (tok.kind != tok::LPAREN) {
    Diag(diag::ERROR, tok.loc, "expected '(' after 'switch'");
    skip_until(tok::SEMI);
    return nullptr;
  }

  enter_scope(ScopeFlags(SF_SWITCH | SF_DECL | SF_CONTROL));

  auto lp_loc = consume_paren();
  auto start = tok.loc;
  auto cond = parse_expr();

  if (!cond && tok.kind != tok::RPAREN) {
    skip_until(tok::SEMI);
    if (tok.kind != tok::RPAREN) {
      exit_scope();
      return nullptr;
    }
  }

  if (!cond) {
    cond = sema.create_recovery_expr(
        start, start == tok.loc ? start : prev_tok_location, {});
  }

  auto rp_loc = tok.loc;
  expect_and_consume(tok::RPAREN);

  while (tok.kind == tok::RPAREN) {
    Diag(diag::ERROR, tok.loc,
         "extraneous ')' after condition, expected a statement");
    consume_paren();
  }

  auto switch_stmt = sema.act_on_start_of_switch_stmt(switch_loc, lp_loc,
                                                      std::move(cond), rp_loc);
  if (!switch_stmt) {
    if (tok.kind == tok::LBRACE) {
      consume_brace();
      skip_until(tok::RBRACE);
    } else {
      skip_until(tok::SEMI);
    }
    return nullptr;
  }

  cur_scope->add_flags(SF_BREAK);
  bool has_lbrace = tok.kind == tok::LBRACE;
  if (has_lbrace)
    enter_scope(SF_DECL);

  auto body = parse_stmt(trailing_else_loc);
  assert(body);

  if (has_lbrace)
    exit_scope();

  exit_scope();

  return sema.act_on_finish_switch_stmt(switch_loc, std::move(switch_stmt),
                                        std::move(body));
}

std::unique_ptr<WhileStmt> Parser::parse_while_stmt(Loc *trailing_else_loc) {
  assert(tok.kind == tok::KW_WHILE);
  auto while_loc = consume_token();
  if (tok.kind != tok::LPAREN) {
    Diag(diag::ERROR, tok.loc, "expected '(' after 'while'");
    skip_until(tok::SEMI);
    return nullptr;
  }

  enter_scope(ScopeFlags(SF_BREAK | SF_CONTINUE | SF_DECL | SF_CONTROL));

  auto lp_loc = consume_paren();
  auto start = tok.loc;
  auto cond = parse_expr();

  if (!cond && tok.kind != tok::RPAREN) {
    skip_until(tok::SEMI);
    if (tok.kind != tok::RPAREN) {
      exit_scope();
      return nullptr;
    }
  }

  if (!cond) {
    cond = sema.create_recovery_expr(
        start, start == tok.loc ? start : prev_tok_location, {});
  }

  auto rp_loc = tok.loc;
  expect_and_consume(tok::RPAREN);

  bool has_brace = tok.kind == tok::LBRACE;
  if (has_brace)
    enter_scope(SF_DECL);

  auto body = parse_stmt(trailing_else_loc);

  if (has_brace)
    exit_scope();
  exit_scope();

  if (!cond || !body)
    return nullptr;

  return sema.act_on_while_stmt(while_loc, lp_loc, std::move(cond), rp_loc,
                                std::move(body));
}

std::unique_ptr<ForStmt> Parser::parse_for_stmt(Loc *trailing_else_loc) {
  assert(tok.kind == tok::KW_FOR);
  auto for_loc = consume_token();
  if (tok.kind != tok::LPAREN) {
    Diag(diag::ERROR, tok.loc, "expected '(' after 'for'");
    skip_until(tok::SEMI);
    return nullptr;
  }

  enter_scope(ScopeFlags(SF_CONTROL | SF_BREAK | SF_CONTINUE | SF_DECL));

  auto lp_loc = consume_paren();
  StmtUPtr init_stmt{};

  if (tok.kind == tok::KW_LET) {
    auto decl_start = tok.loc;
    auto decl_end = LOC_INVALID;
    auto decl = parse_decl(true, &decl_end);
    init_stmt = sema.act_on_decl_stmt(std::move(decl), decl_start, decl_end);
  } else if (tok.kind == tok::SEMI) {
    consume_token();
  } else {
    Diag(diag::ERROR, tok.loc, "expected 'let' or ';' in 'for'");
    skip_until(tok::SEMI);
    return nullptr;
  }

  auto cond = tok.kind == tok::SEMI ? nullptr : parse_expr();

  if (tok.kind != tok::SEMI) {
    Diag(diag::ERROR, tok.loc, "expected ';' in 'for'");
    skip_until(tok::SEMI);
    return nullptr;
  }
  consume_token();

  auto latch = tok.kind == tok::RPAREN ? nullptr : parse_expr();

  auto rp_loc = tok.loc;
  expect_and_consume(tok::RPAREN);

  bool has_brace = tok.kind == tok::LBRACE;
  if (has_brace)
    enter_scope(SF_DECL);

  auto body = parse_stmt(trailing_else_loc);

  if (has_brace)
    exit_scope();
  exit_scope();

  if (!body)
    return nullptr;

  return sema.act_on_for_stmt(for_loc, lp_loc, std::move(init_stmt), std::move(cond), std::move(latch), rp_loc, std::move(body));
}

std::unique_ptr<DoStmt> Parser::parse_do_stmt() {
  assert(tok.kind == tok::KW_DO);
  auto do_loc = consume_token();

  enter_scope(ScopeFlags(SF_BREAK | SF_CONTINUE | SF_DECL));

  bool has_lbrace = tok.kind == tok::LBRACE;
  if (has_lbrace)
    enter_scope(SF_DECL);

  auto body = parse_stmt();

  if (has_lbrace)
    exit_scope();

  if (tok.kind != tok::KW_WHILE) {
    if (body) {
      Diag(diag::ERROR, tok.loc, "expected 'while' in do/while loop");
      Diag(diag::NOTE, do_loc, "to match this 'do'");
      skip_until(tok::SEMI, false, true);
    }
    return nullptr;
  }

  auto while_loc = consume_token();
  if (tok.kind != tok::LPAREN) {
    Diag(diag::ERROR, tok.loc, "expected '(' after do/while");
    skip_until(tok::SEMI);
    return nullptr;
  }

  auto lp_loc = consume_paren();
  auto start = tok.loc;
  auto cond = parse_expr();
  if (!cond) {
    if (tok.kind != tok::RPAREN && tok.kind != tok::RSQUARE &&
        tok.kind != tok::RBRACE) {
      skip_until(tok::SEMI);
    }
    cond = sema.create_recovery_expr(
        start, start == tok.loc ? start : prev_tok_location, {},
        sema.ctx.bool_ty);
  }
  auto rp_loc = consume_paren();
  exit_scope();
  if (!cond || !body)
    return nullptr;
  return sema.act_on_do_stmt(do_loc, std::move(body), while_loc, lp_loc,
                             std::move(cond), rp_loc);
}

std::unique_ptr<ContinueStmt> Parser::parse_continue_stmt() {
  assert(tok.kind == tok::KW_CONTINUE);
  auto continue_loc = consume_token();
  return sema.act_on_continue_stmt(continue_loc, cur_scope);
}

std::unique_ptr<BreakStmt> Parser::parse_break_stmt() {
  assert(tok.kind == tok::KW_BREAK);
  auto break_loc = consume_token();
  return sema.act_on_break_stmt(break_loc, cur_scope);
}

std::unique_ptr<ReturnStmt> Parser::parse_return_stmt() {
  assert(tok.kind == tok::KW_RETURN);
  auto return_loc = consume_token();
  ExprUPtr ret_expr = nullptr;
  if (tok.kind != tok::SEMI) {
    ret_expr = parse_expr();
    if (!ret_expr) {
      skip_until(tok::RBRACE, true, true);
      return nullptr;
    }
  }
  return sema.act_on_return_stmt(return_loc, std::move(ret_expr), cur_scope);
}

std::unique_ptr<Stmt> Parser::parse_expr_stmt() {
  auto old_token_loc = tok.loc;

  auto expr = parse_expr();
  if (!expr) {
    skip_until(tok::RBRACE, true, true);
    if (tok.kind == tok::SEMI)
      consume_token();
    return sema.act_on_expr_stmt_error();
  }

  if (tok.kind == tok::COLON && cur_scope->is_switch_scope()) {
    if ((expr = sema.act_on_case_expr(tok.loc, std::move(expr)))) {
      Diag(diag::ERROR, old_token_loc,
           "expected 'case' keyword before expression");
      return parse_case_stmt(true, std::move(expr));
    }
  }

  expect_and_consume_semi("expected ';' after expression");

  return sema.act_on_expr_stmt(std::move(expr), true);
}

std::unique_ptr<CompoundStmt> Parser::parse_compound_stmt_body() {
  if (tok.kind != tok::LBRACE)
    return nullptr;
  auto open_loc = consume_brace();
  std::vector<StmtUPtr> stmts;
  while (tok.kind != tok::RBRACE && tok.kind != tok::END_OF_FILE) {
    auto r = parse_stmt();
    if (r)
      stmts.push_back(std::move(r));
  }
  auto close_loc = consume_brace();
  return sema.act_on_compound_stmt(open_loc, close_loc, std::move(stmts));
}

std::unique_ptr<CompoundStmt>
Parser::parse_compound_stmt(ScopeFlags scope_flags) {
  assert(tok.kind == tok::LBRACE);
  enter_scope(scope_flags);
  auto out = parse_compound_stmt_body();
  exit_scope();
  return out;
}

std::unique_ptr<Stmt> Parser::parse_stmt(Loc *trailing_else_loc) {
  cstr semi_error = "";
  StmtUPtr res = nullptr;
  switch (tok.kind) {
  case tok::KW_CASE:
    return parse_case_stmt();
  case tok::KW_DEFAULT:
    return parse_default_stmt();
  case tok::LBRACE:
    return parse_compound_stmt();
  case tok::SEMI:
    return sema.act_on_null_stmt(consume_token());
  case tok::KW_IF:
    return parse_if_stmt(trailing_else_loc);
  case tok::KW_SWITCH:
    return parse_switch_stmt(trailing_else_loc);
  case tok::KW_WHILE:
    return parse_while_stmt(trailing_else_loc);
  case tok::KW_DO:
    res = parse_do_stmt();
    semi_error = "do/while";
    break;
  case tok::KW_FOR:
    return parse_for_stmt(trailing_else_loc);
  case tok::KW_CONTINUE:
    res = parse_continue_stmt();
    semi_error = "continue";
    break;
  case tok::KW_BREAK:
    res = parse_break_stmt();
    semi_error = "break";
    break;
  case tok::KW_RETURN:
    res = parse_return_stmt();
    semi_error = "return";
    break;
  default:
    if (is_declaration_statement()) {
      auto decl_start = tok.loc;
      auto decl_end = LOC_INVALID;
      auto decl = parse_decl(true, &decl_end);
      return sema.act_on_decl_stmt(std::move(decl), decl_start, decl_end);
    }
    if (tok.kind == tok::RBRACE) {
      Diag(diag::ERROR, tok.loc, "expected statement");
    }
    return parse_expr_stmt();
  }
  if (!try_consume_token(tok::SEMI) && res) {
    expect_and_consume(tok::SEMI, "expected ';' after %s statement",
                       semi_error);
    skip_until(tok::RBRACE, true, true);
  }
  return res;
}

bool Parser::is_declaration_statement() {
  switch (tok.kind) {
  case tok::KW_LET:
  case tok::KW_FN:
  case tok::KW_LIB:
  case tok::KW_TYPE:
  case tok::KW_STRUCT:
  case tok::KW_ENUM:
    return true;
  default:
    return false;
  }
}
