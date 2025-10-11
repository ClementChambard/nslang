#include "token.hpp"
#include "lexer/file.hpp"
#include "lexer/loc.hpp"
#include <cassert>
#include <cstdio>

void Token::dump() const {
  DecomposedLoc dloc = OpenedFile::get_loc(loc);
  printf("%s:%d:%d: %s", dloc.file_name, dloc.line, dloc.col,
         tok::get_name(kind));
  if (kind == tok::NUM || kind == tok::STR || kind == tok::CHR) {
    printf(" (%.*s)\n", len, static_cast<char *>(value));
  } else if (kind == tok::IDENT) {
    printf(" '%s'\n", ident()->name.c_str());
  } else {
    printf("\n");
  }
}

IdentInfo *Token::ident() const {
  assert(kind == tok::IDENT &&
         "ident method can only be called on identifiers");
  return static_cast<IdentInfo *>(value);
}

std::string_view Token::value_str() const {
  if (kind == tok::IDENT) {
    return ident()->name;
  }
  return std::string_view(static_cast<char *>(value), len);
}
