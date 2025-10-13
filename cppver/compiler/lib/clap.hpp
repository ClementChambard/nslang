#ifndef CLAP_HPP_INCLUDED
#define CLAP_HPP_INCLUDED

#include "opts.hpp"
#include <optional>

struct CheckedClap {
  Options opts;
  std::vector<std::pair<std::string, std::string>> files_to_compile;
  std::vector<std::string> files_to_link;
  std::vector<std::string> files_to_remove;
  std::string program_name;
};

struct Clap {
  std::vector<std::string> linked_libs;
  std::vector<std::string> included_dirs;
  std::vector<std::string> supplied_filenames;
  std::optional<std::string> output_filename;
  bool print_ast = false;
  bool print_llvm = false;
  bool print_opt_llvm = false;
  bool print_asm = false;
  bool compile_only = false;
  bool no_stdlib = false;
  bool no_runtime = false;

  static Clap parse(int argc, char **argv);

  CheckedClap check() const;
};


#endif // CLAP_HPP_INCLUDED
