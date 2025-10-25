#ifndef OPTS_HPP_INCLUDED
#define OPTS_HPP_INCLUDED

#include "defines.hpp"
#include <string>
#include <vector>

enum class Mode {
  PRINT_AST,
  PRINT_LLVM,
  PRINT_ASM,
  COMPILE_ONLY,
  COMPILE_AND_LINK,
};

struct Options {
  std::vector<std::string> include_path;
  std::vector<std::string> link_libraries;
  Mode mode;
  u8 opt_level = 0;
};

#endif // OPTS_HPP_INCLUDED
