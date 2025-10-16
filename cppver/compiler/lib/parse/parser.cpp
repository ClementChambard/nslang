
#include "parser.hpp"
#include "ast/nodes/type.hpp"
#include "diags/diagnostic.hpp"
#include "lexer/tok.hpp"
#include "prec.hpp"
#include <cassert>
#include <cstring>

Prec prec_from_tok(Tok kind) {
  switch (kind) {
  case tok::EQUAL:
  case tok::STAREQUAL:
  case tok::SLASHEQUAL:
  case tok::PERCENTEQUAL:
  case tok::PLUSEQUAL:
  case tok::MINUSEQUAL:
  case tok::LESSLESSEQUAL:
  case tok::GREATERGREATEREQUAL:
  case tok::AMPEQUAL:
  case tok::PIPEEQUAL:
  case tok::CARETEQUAL:
    return Prec::ASSIGN;
  case tok::QUESTION:
    return Prec::COND;
  case tok::PIPEPIPE:
    return Prec::OR;
  case tok::AMPAMP:
    return Prec::AND;
  case tok::PIPE:
    return Prec::BOR;
  case tok::CARET:
    return Prec::XOR;
  case tok::AMP:
    return Prec::BAND;
  case tok::EXCLAIMEQUAL:
  case tok::EQUALEQUAL:
    return Prec::EQ;
  case tok::GREATEREQUAL:
  case tok::GREATER:
  case tok::LESSEQUAL:
  case tok::LESS:
    return Prec::COMP;
  case tok::LESSLESS:
  case tok::GREATERGREATER:
    return Prec::SHIFT;
  case tok::PLUS:
  case tok::MINUS:
    return Prec::PLUS;
  case tok::PERCENT:
  case tok::SLASH:
  case tok::STAR:
    return Prec::STAR;
  default:
    return Prec::UNKNOWN;
  }
}

void Parser::unconsume_token(Token consumed) {
  lexer.enter_token(tok, true);
  tok = consumed;
  // auto next_tok = tok;
  // lexer.enter_token(consumed, true);
  // tok = lexer.lex();
  // lexer.enter_token(next_tok, true);
}

Loc Parser::consume_token() {
  assert(!is_token_special());
  prev_tok_location = tok.loc;
  tok = lexer.lex();
  return prev_tok_location;
}

bool Parser::try_consume_token(Tok expected, Loc *loc) {
  if (tok.kind != expected)
    return false;
  auto l = consume_token();
  if (loc)
    *loc = l;
  return true;
}

Loc Parser::consume_any_token() {
  if (is_token_paren())
    return consume_paren();
  if (is_token_square())
    return consume_square();
  if (is_token_brace())
    return consume_brace();
  return consume_token();
}

Loc Parser::consume_paren() {
  assert(is_token_paren());
  if (tok.kind == tok::LPAREN)
    paren_count++;
  else if (paren_count > 0)
    paren_count--;
  prev_tok_location = tok.loc;
  tok = lexer.lex();
  return prev_tok_location;
}

Loc Parser::consume_brace() {
  assert(is_token_brace());
  if (tok.kind == tok::LBRACE)
    brace_count++;
  else if (brace_count > 0)
    brace_count--;
  prev_tok_location = tok.loc;
  tok = lexer.lex();
  return prev_tok_location;
}

Loc Parser::consume_square() {
  assert(is_token_square());
  if (tok.kind == tok::LSQUARE)
    square_count++;
  else if (square_count > 0)
    square_count--;
  prev_tok_location = tok.loc;
  tok = lexer.lex();
  return prev_tok_location;
}

bool Parser::expect_and_consume(Tok expected, char const *diag_id,
                                char const *str) {
  if (tok.kind == expected) {
    consume_any_token();
    return false;
  }
  auto loc = tok.loc;
  if (is_common_typo(expected, tok.kind)) {
    // fixit with replacement...
    Diag(diag::ERROR, loc, "%s %s", diag_id, str);
    consume_any_token();
    return false;
  }
  // fixit
  if (strcmp(diag_id, "expected %s") == 0)
    Diag(diag::ERROR, loc, diag_id, tok::get_name(expected));
  else if (strcmp(diag_id, "expected %s after %s") == 0)
    Diag(diag::ERROR, loc, diag_id, tok::get_name(expected), str);
  else
    Diag(diag::ERROR, loc, diag_id, str);
  return true;
}

bool Parser::expect_and_consume_semi(char const *diag_id,
                                     char const *token_used) {

  if (try_consume_token(tok::SEMI))
    return false;
  // if ((tok.kind == tok::RPAREN || tok.kind == tok::RSQUARE) &&
  //     next_token().kind == tok::SEMI) {
  //   Diag(diag::ERROR, tok.loc, "extraneous '%s' before ';'",
  //        tok::get_name(tok.kind));
  //   consume_any_token();
  //   consume_token();
  //   return false;
  // }
  return expect_and_consume(tok::SEMI, diag_id, token_used);
}

bool Parser::skip_until(std::span<Tok> toks, bool at_semi, bool before_match) {
  bool is_first_token_skipped = true;
  while (true) {
    for (auto t : toks) {
      if (tok.kind == t) {
        if (!before_match)
          consume_any_token();
        return true;
      }
    }
    if (toks.size() == 1 && toks[0] == tok::END_OF_FILE && !at_semi) {
      while (tok.kind != tok::END_OF_FILE)
        consume_any_token();
      return true;
    }
    switch (tok.kind) {
    case tok::END_OF_FILE:
      return false;
    case tok::LPAREN:
      consume_paren();
      skip_until(tok::RPAREN);
      break;
    case tok::LSQUARE:
      consume_square();
      skip_until(tok::RSQUARE);
      break;
    case tok::LBRACE:
      consume_brace();
      skip_until(tok::RBRACE);
      break;
    case tok::QUESTION:
      consume_token();
      skip_until(tok::COLON, true, before_match);
      break;
    case tok::RPAREN:
      if (paren_count > 0 && !is_first_token_skipped)
        return false;
      consume_paren();
      break;
    case tok::RSQUARE:
      if (square_count > 0 && !is_first_token_skipped)
        return false;
      consume_square();
      break;
    case tok::RBRACE:
      if (brace_count > 0 && !is_first_token_skipped)
        return false;
      consume_brace();
      break;
    case tok::SEMI:
      if (at_semi)
        return false;
      consume_token();
      break;
    default:
      consume_any_token();
    }
    is_first_token_skipped = false;
  }
}

bool is_common_typo(Tok expected, Tok actual) {
  if (expected == tok::SEMI)
    return actual == tok::COLON || actual == tok::COMMA;
  return false;
}

// TYPE functions

Type *Parser::parse_type() {
  Type *cur_type = nullptr;
  while (true) {
    if (cur_type == nullptr) {
      if (tok::is_builtin_type(tok.kind)) {
        cur_type = sema.ctx.get_builtin_type_from_tok(tok.kind);
        consume_token();
        continue;
      }
      if (tok.kind == tok::IDENT) {
        auto type_name = tok.ident();
        auto type_loc = consume_token();
        auto lookup_decl = cur_scope->lookup_named_decl(type_name);
        TypeDecl *td = nullptr;
        if (lookup_decl == nullptr ||
            !(td = lookup_decl->dyn_cast<TypeDecl>())) {
          Diag(diag::ERROR, type_loc, "'%s' is not a type name",
               type_name->name.c_str());
          return nullptr;
        }
        cur_type = sema.ctx.get_declared_type(td);
        continue;
      }
      break;
    }
    if (tok.kind == tok::STAR) {
      cur_type = sema.ctx.get_pointer_type(cur_type);
      consume_token();
      continue;
    }
    if (tok.kind == tok::LSQUARE) {
      consume_square();
      auto [count, _] = parse_integer_constexpr();
      expect_and_consume(tok::RSQUARE);
      cur_type = sema.ctx.get_array_type(cur_type, count);
      continue;
    }
    // function type ?
    break;
  }
  return cur_type;
}

bool Parser::is_start_of_type() {
  if (tok::is_builtin_type(tok.kind))
    return true;
  if (tok.kind == tok::IDENT) {
    auto type_name = tok.ident();
    auto lookup_decl = cur_scope->lookup_named_decl(type_name);
    return lookup_decl && lookup_decl->dyn_cast<TypeDecl>();
  }
  return false;
}

bool Parser::following_is_type(int) { return false; }
