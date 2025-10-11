#include "char_utils.hpp"

bool is_ident_continue(char c) {
  // clang-format off
  return (c <= 'z' && c >= 'a') ||
         (c <= 'Z' && c >= 'A') ||
         (c <= '9' && c >= '0') ||
          c == '_';
  // clang-format on
}
