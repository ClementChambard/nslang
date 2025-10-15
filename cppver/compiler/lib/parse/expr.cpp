#include "ast/nodes/expr.hpp"
#include "diags/diagnostic.hpp"
#include "lexer/loc.hpp"
#include "lexer/tok.hpp"
#include "parse/prec.hpp"
#include "parser.hpp"

// Exprlist ::= SepList<Expr, ','>
bool Parser::parse_expression_list(std::vector<std::unique_ptr<Expr>> &out) {
  bool saw_error = false;
  while (true) {
    auto expr = parse_assignment_expr();
    if (!expr) {
      saw_error = true;
      skip_until(tok::COMMA, tok::RPAREN, false, true);
    } else {
      out.push_back(std::move(expr));
    }
    if (tok.kind != tok::COMMA)
      break;
    consume_token();
  }
  return saw_error;
}

ExprUPtr Parser::parse_postfix_expression_suffix(ExprUPtr lhs) {
  // TODO: improve error recovery instead of returning nullptr
  while (true) {
    switch (tok.kind) {
    case tok::PLUSPLUS:
    case tok::MINUSMINUS:
      if (lhs) {
        lhs = sema.act_on_postfix_unary_op(cur_scope, tok.loc, tok.kind,
                                           std::move(lhs));
      }
      consume_token();
      break;
    case tok::LSQUARE: {
      auto lloc = consume_square();
      auto arg = parse_expr();
      auto rloc = tok.loc;
      if (lhs && tok.kind == tok::RSQUARE) {
        lhs = sema.act_on_array_subscript_expr(cur_scope, std::move(lhs), lloc,
                                               std::move(arg), rloc);
      } else {
        lhs = nullptr;
      }
      consume_square();
      break;
    }
    case tok::LPAREN: {
      auto lp = consume_paren();
      std::vector<ExprUPtr> args;
      if (tok.kind != tok::RPAREN) {
        if (parse_expression_list(args))
          lhs = nullptr;
      }
      if (!lhs) {
        skip_until(tok::RPAREN, true);
      } else if (tok.kind != tok::RPAREN) {
        skip_until(tok::RPAREN, true);
        lhs = nullptr;
      } else {
        auto rp = consume_paren();
        lhs = sema.act_on_call_expr(cur_scope, std::move(lhs), lp, std::move(args), rp);
        // TODO: recovery expr
      }
      break;
    }
    case tok::ARROW:
    case tok::PERIOD: {
      auto opkind = tok.kind;
      auto oploc = consume_token();
      if (tok.kind != tok::IDENT) {
        Diag(diag::ERROR, tok.loc, "expected unqualified identifier");
        lhs = nullptr;
      } else {
        auto id = tok.ident();
        auto idloc = consume_token();
        lhs = sema.act_on_member_access_expr(cur_scope, std::move(lhs), oploc,
                                             opkind, idloc, id);
      }
      break;
    }
    default:
      return lhs;
    }
  }
}

ExprUPtr Parser::parse_unit_expr() {
  // TODO: improve error recovery instead of returning nullptr
  ExprUPtr res = nullptr;
  switch (tok.kind) {
  case tok::LPAREN: {
    auto open_loc = consume_paren();
    res = parse_expr();
    if (res && tok.kind == tok::RPAREN) {
      res = sema.act_on_paren_expr(open_loc, tok.loc, std::move(res));
    }
    if (!res)
      skip_until(tok::RPAREN, true);
    else
      expect_and_consume(tok::RPAREN);
    break;
  }
  case tok::NUM:
    res = sema.act_on_numeric_constant(tok);
    consume_token();
    break;
  case tok::KW_NULLPTR:
    res = sema.act_on_nullptr_literal(consume_token());
    break;
  case tok::KW_TRUE:
  case tok::KW_FALSE: {
    auto k = tok.kind;
    res = sema.act_on_bool_literal(consume_token(), k);
    break;
  }
  case tok::CHR:
    res = sema.act_on_character_constant(tok);
    consume_token();
    break;
  case tok::STR: {
    std::vector<Token> string_toks;
    while (tok.kind == tok::STR) {
      string_toks.push_back(tok);
      consume_any_token();
    }
    res = sema.act_on_string_literal(string_toks);
    break;
  }
  case tok::KW_VAARG: {
    auto start_loc = consume_token();
    expect_and_consume(tok::LESS);
    auto type = parse_type();
    auto end_loc = tok.loc;
    expect_and_consume(tok::GREATER);
    res = sema.act_on_vaarg_expr(type, start_loc, end_loc);
    break;
  }
  case tok::KW_VAARGS: {
    auto loc = consume_token();
    res = sema.act_on_vaargs_expr(loc);
    break;
  }
  case tok::PLUSPLUS:
  case tok::MINUSMINUS:
  case tok::PLUS:
  case tok::MINUS:
  case tok::AMP:
  case tok::STAR:
  case tok::TILDE:
  case tok::EXCLAIM: {
    auto opc = tok.kind;
    auto opl = consume_token();
    res = parse_unit_expr();
    if (res) {
      res = sema.act_on_unary_op(cur_scope, opl, opc, std::move(res));
    }
    break;
  }
  case tok::KW_CAST: {
    auto sl = consume_token();
    expect_and_consume(tok::LESS);
    auto type = parse_type();
    expect_and_consume(tok::GREATER);
    expect_and_consume(tok::LPAREN);
    auto expr = parse_expr();
    auto el = tok.loc;
    expect_and_consume(tok::RPAREN);
    if (!expr) {
      Diag(diag::BUG, sl, "expr invalid in cast !!!\n") << LocRge(sl, el);
      res = nullptr;
      break;
    }
    res = sema.act_on_explicit_cast(type, std::move(expr), sl, el);
    break;
  }
  case tok::KW_SIZEOF: {
    auto sl = consume_token();
    expect_and_consume(tok::LPAREN);
    ExprUPtr e;
    Type *type;
    if (is_start_of_type()) {
      type = parse_type();
    } else {
      e = parse_expr();
      type = e->type;
    }
    if (!type) {
      Diag(diag::ERROR, sl, "sizeof on invalid type");
      type = sema.ctx.i64_ty;
    }
    auto el = tok.loc;
    expect_and_consume(tok::RPAREN);
    res = sema.act_on_sizeof_expr(type, std::move(e), sl, el);
    break;
  }
  case tok::IDENT: {
    auto ii = tok.ident();
    auto iloc = consume_token();

    StructDecl *scope = nullptr;
    if (tok.kind == tok::COLONCOLON) {
      consume_token();
      assert(tok.kind == tok::IDENT && "TODO: error handling");
      auto scope_ii = ii;
      auto scope_loc = iloc;
      ii = tok.ident();
      iloc = consume_token();
      scope = sema.act_on_scoped_identifier(cur_scope, scope_ii, scope_loc);
    }

    // Token replacement;

    res = sema.act_on_id_expression(cur_scope, scope, ii, iloc);
    if (!res) {
      // unconsume_token(replacement);
      return parse_unit_expr();
    }
    break;
  }
  default:
    return nullptr;
  }
  return parse_postfix_expression_suffix(std::move(res));
}

ExprUPtr Parser::parse_assignment_expr() {
  auto lhs = parse_unit_expr();
  return parse_rhs_of_binary_expr(std::move(lhs), Prec::ASSIGN);
}

ExprUPtr Parser::parse_rhs_of_binary_expr(ExprUPtr lhs, Prec prec) {
  // TODO: improve error recovery instead of returning nullptr
  auto next_tok_prec = prec_from_tok(tok.kind);
  Loc colon_loc = LOC_INVALID;
  while (true) {
    if (next_tok_prec < prec)
      return lhs;
    auto op_token = tok;
    consume_token();
    if (op_token.kind == tok::COMMA) {
      lexer.enter_token(tok, true);
      tok = op_token;
      return lhs;
    }
    ExprUPtr ternary_middle = nullptr;
    if (next_tok_prec == Prec::COND) {
      ternary_middle = parse_expr();
      if (!ternary_middle) {
        lhs = nullptr;
      }
      if (!try_consume_token(tok::COLON, &colon_loc)) {
        Diag(diag::ERROR, tok.loc, "expected ':'");
        Diag(diag::NOTE, op_token.loc, "to match this '?'");
      }
    }
    ExprUPtr rhs = nullptr;
    if (next_tok_prec <= Prec::COND) {
      rhs = parse_assignment_expr();
    } else {
      rhs = parse_unit_expr();
    }
    if (!rhs)
      lhs = nullptr;
    auto this_prec = next_tok_prec;
    next_tok_prec = prec_from_tok(tok.kind);
    bool is_right_assoc = this_prec == Prec::COND || this_prec == Prec::ASSIGN;
    if (this_prec < next_tok_prec ||
        (this_prec == next_tok_prec && is_right_assoc)) {
      rhs = parse_rhs_of_binary_expr(
          std::move(rhs), Prec(i32(this_prec) + i32(is_right_assoc)));
      if (!rhs)
        lhs = nullptr;
      next_tok_prec = prec_from_tok(tok.kind);
    }
    if (lhs) {
      if (ternary_middle) {
        auto cond_op = sema.act_on_conditional_op(
            op_token.loc, colon_loc, std::move(lhs), std::move(ternary_middle),
            std::move(rhs));
        // TODO: error check
        lhs = std::move(cond_op);
      } else {
        auto bin_op = sema.act_on_bin_op(cur_scope, op_token.loc, op_token.kind,
                                         std::move(lhs), std::move(rhs));
        // TODO: error check
        lhs = std::move(bin_op);
      }
    }
  }
}

ExprUPtr Parser::parse_expr() {
  auto lhs = parse_assignment_expr();
  return parse_rhs_of_binary_expr(std::move(lhs), Prec::COMMA);
}

std::pair<i64, ExprUPtr> Parser::parse_integer_constexpr() {
  auto e = parse_expr();
  if (!e->type->is_integer_type()) {
    Diag(diag::ERROR, e->get_start_loc(), "expected integer constexpr")
        << e->get_range();
    return {0, std::move(e)};
  }
  return {sema.eval_integer_constexpr(e.get()), std::move(e)};
}
