#ifndef PARSE_PREC_HPP_INCLUDED
#define PARSE_PREC_HPP_INCLUDED

#include "lexer/tok.hpp"
#include <defines.hpp>

enum class Prec : i8 {
  UNKNOWN,
  COMMA,
  ASSIGN,
  COND,
  OR,
  AND,
  BOR,
  XOR,
  BAND,
  EQ,
  COMP,
  SHIFT,
  PLUS,
  STAR,
};

Prec prec_from_tok(Tok kind);

#endif // PARSE_PREC_HPP_INCLUDED
