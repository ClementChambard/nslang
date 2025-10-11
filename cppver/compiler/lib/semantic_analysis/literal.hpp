#ifndef SEMA_LITERAL_HPP_INCLUDED
#define SEMA_LITERAL_HPP_INCLUDED

#include "lexer/token.hpp"
#include <span>
struct Type;

struct NumLiteralParser {
  i64 res;
  Type *ty;
  bool had_error;
  bool has_explicit_type;
  struct ASTContext *ctx;

  NumLiteralParser(ASTContext *ctx, Token tok);
};

struct CharLiteralParser {
  i64 res;
  Type *ty;
  bool had_error;
  struct ASTContext *ctx;

  CharLiteralParser(ASTContext *ctx, Token tok);
};

struct StringLiteralParser {
  i64 max_token_length;
  i64 size_bound;
  i64 char_byte_width;
  Tok kind;
  std::string result_buf;
  i64 result_off;
  bool eval_method;
  bool had_error;
  struct ASTContext *ctx;

  StringLiteralParser(ASTContext *ctx, std::span<const Token> string_toks,
                      bool eval_method);

  std::string const &get_string() { return result_buf; }
  i64 get_string_length() { return result_off; }
  i64 get_num_string_chars() { return get_string_length(); }
};

#endif // SEMA_LITERAL_HPP_INCLUDED
