#ifndef LEXER_LOC_HPP_INCLUDED
#define LEXER_LOC_HPP_INCLUDED

#include <defines.hpp>

struct Loc {
  u32 offset = 0;
  constexpr Loc() = default;
  constexpr Loc(u32 offset) : offset(offset) {}

  constexpr operator u32() const { return offset; }
};

#define LOC_INVALID Loc()

struct LocRge {
  LocRge() = default;
  LocRge(Loc start, Loc end) : start(start), end(end) {}
  LocRge(Loc loc) : start(loc), end(loc) {}

  Loc start;
  Loc end;
};

struct DecomposedLoc {
  cstr file_name;
  u32 line;
  u32 col;
};

#endif // LEXER_LOC_HPP_INCLUDED
