#include "file.hpp"
#include <fstream>
#include <memory>

static std::vector<std::unique_ptr<OpenedFile>> opened_files;

void make_line_cache(OpenedFile &f) {
  u32 pos = 0;
  f.line_cache.clear();
  while (pos < f.source_len) {
    if (f.source[pos] == '\r') {
      pos += 1;
      if (f.source[pos] == '\n')
        pos += 1;
      f.line_cache.push_back(pos);
    } else if (f.source[pos] == '\n') {
      pos += 1;
      if (f.source[pos] == '\r')
        pos += 1;
      f.line_cache.push_back(pos);
    } else {
      pos += 1;
    }
  }
}

OpenedFile *OpenedFile::open(const std::string &filename) {
  auto f = std::make_unique<OpenedFile>(filename);
  opened_files.push_back(std::move(f));
  return opened_files.back().get();
}

OpenedFile::OpenedFile(std::string const &filename) : file_name(filename) {
  // read file

  std::ifstream file(filename, std::ios::ate);
  source_len = file.tellg();
  file.seekg(std::ios::beg);
  source_len -= file.tellg();
  source = new char[source_len + 1];
  source[source_len] = 0;
  file.read(source, source_len);
  file.close();

  if (opened_files.size() == 0) {
    pos_offset = 1;
  } else {
    pos_offset = opened_files.back()->pos_offset + opened_files.back()->source_len;
  }
  make_line_cache(*this);
}

OpenedFile::~OpenedFile() { delete[] source; }

u32 OpenedFile::line_offset(u32 line) const {
  if (line == 1)
    return 0;
  return line_cache[line - 2];
}

OpenedFile *OpenedFile::find(std::string const &filename) {
  for (auto &f : opened_files) {
    if (f->file_name == filename)
      return f.get();
  }
  return nullptr;
}

OpenedFile *OpenedFile::find_by_loc(Loc loc) {
  if (loc == LOC_INVALID || opened_files.empty())
    return nullptr;
  u32 file_i = 0;
  while (file_i + 1 < opened_files.size() &&
         loc >= opened_files[file_i + 1]->pos_offset)
    file_i += 1;
  return opened_files[file_i].get();
}

DecomposedLoc OpenedFile::get_loc(Loc loc) {
  auto f = OpenedFile::find_by_loc(loc);
  if (!f) {
    return {nullptr, 0, 0};
  }
  u32 offset = loc - f->pos_offset;
  u32 line = 0;
  u32 col = offset;
  while (line < f->line_cache.size() && offset >= f->line_cache[line]) {
    line += 1;
  }
  if (line > 0) {
    col = offset - f->line_cache[line - 1];
  }
  return {f->file_name.c_str(), line + 1, col + 1};
}

void OpenedFile::close_all_files() {
  opened_files.clear();
}
