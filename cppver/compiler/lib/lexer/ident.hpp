#ifndef LEXER_IDENT_HPP_INCLUDED
#define LEXER_IDENT_HPP_INCLUDED

#include "tok.hpp"
#include <string>

struct IdentInfo {
  std::string name;
  Tok token_kind;

  IdentInfo(std::string_view name, Tok kind = tok::IDENT)
      : name(name), token_kind(kind) {}

  bool needs_handling() const { return false; }

  static void create_ident_info_list();
  static IdentInfo *find(std::string const &name);
};

#endif // LEXER_IDENT_HPP_INCLUDED
