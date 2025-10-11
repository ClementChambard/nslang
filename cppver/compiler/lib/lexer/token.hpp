#ifndef LEXER_TOKEN_HPP_INCLUDED
#define LEXER_TOKEN_HPP_INCLUDED

#include "ident.hpp"
#include "loc.hpp"
#include "tok.hpp"

#include <string_view>

struct Token {
  void *value;
  Loc loc;
  u32 len;
  Tok kind;

  Token() : value(nullptr), loc(LOC_INVALID), len(0), kind(tok::UNKNOWN) {}

  void dump() const;

  IdentInfo *ident() const;

  std::string_view value_str() const;
};

#endif // LEXER_TOKEN_HPP_INCLUDED
