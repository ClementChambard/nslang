#ifndef LEXER_FILE_HPP_INCLUDED
#define LEXER_FILE_HPP_INCLUDED

#include "lexer/loc.hpp"
#include <defines.hpp>
#include <string>
#include <vector>

struct OpenedFile {
  std::string file_name;
  char *source;
  u32 source_len;
  u32 pos_offset;
  std::vector<u32> line_cache;

  explicit OpenedFile(std::string const &filename);
  OpenedFile(OpenedFile const &) = delete;
  OpenedFile &operator=(OpenedFile const &) = delete;
  ~OpenedFile();
  OpenedFile() = default;

  static OpenedFile *open(std::string const &filename);

  u32 line_offset(u32 line) const;

  static OpenedFile *find(std::string const &filename);

  static OpenedFile *find_by_loc(Loc loc);

  static DecomposedLoc get_loc(Loc loc);

  static void close_all_files();
};

#endif // LEXER_FILE_HPP_INCLUDED
