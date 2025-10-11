#include "lexer.hpp"
#include "diags/diagnostic.hpp"
#include "lexer/tok.hpp"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <filesystem>
#include <memory>

void Lexer::enter_source_file(std::string const &filename) {
  if (cur_lexer != nullptr) {
    lexer_stack.push_back(std::move(cur_lexer));
  }
  cur_lexer = std::make_unique<FileLexer>(filename, this);
}

void Lexer::end_source_file(Token &tok, u32 cur_pos) {
  if (cur_lexer == nullptr)
    return;
  if (lexer_stack.size() == 0) {
    cur_lexer->construct_token(tok, cur_pos, tok::END_OF_FILE);
    return;
  }
  cur_lexer = std::move(lexer_stack.back());
  lexer_stack.pop_back();
  tok = lex();
}

std::string_view include_directive_strip_filename(std::string_view filename) {
  while (std::isspace(filename.front())) {
    filename = filename.substr(1);
  }
  while (std::isspace(filename.back())) {
    filename = filename.substr(0, filename.length() - 1);
  }
  return filename;
}

std::string find_file_to_include(Lexer &l, std::string_view filename, Loc loc) {
  namespace fs = std::filesystem;
  fs::path path(filename);
  if (fs::exists(path) && !fs::is_directory(path)) {
    return std::string(filename);
  }
  if (path.is_absolute()) {
    Diag(diag::ERROR, loc, "file not found for absolute path '%.*s'",
         filename.length(), filename.data());
    return "/dev/null";
  }
  fs::path cur_filename(l.cur_lexer ? l.cur_lexer->f->file_name : "");
  if (cur_filename.has_parent_path()) {
    fs::path dir_path = cur_filename.parent_path();
    fs::path lookup_path = dir_path / path;
    if (fs::exists(lookup_path) && !fs::is_directory(lookup_path)) {
      return lookup_path.string();
    }
  }
  for (auto const &ip : l.include_paths) {
    fs::path dir_path(ip);
    fs::path lookup_path = dir_path / path;
    if (fs::exists(lookup_path) && !fs::is_directory(lookup_path)) {
      return lookup_path.string();
    }
  }
  Diag(diag::ERROR, loc, "file not found for path '%.*s'", filename.length(),
       filename.data());
  return "/dev/null";
}

void Lexer::handle_directive(Token &tok) {
  std::string dir_kind = cur_lexer->read_to_whitespace();
  if (dir_kind != "include") {
    Diag(diag::ERROR, tok.loc, "unsupported directive '%s'", dir_kind.c_str());
    cur_lexer->read_to_end_of_line();
    tok = lex();
    return;
  }
  std::string file_to_include = cur_lexer->read_to_end_of_line();
  std::string_view file_to_include_stripped =
      include_directive_strip_filename(file_to_include);
  file_to_include =
      find_file_to_include(*this, file_to_include_stripped, tok.loc);
  if (std::find(already_included_files.begin(), already_included_files.end(),
                file_to_include) == already_included_files.end()) {
    already_included_files.push_back(file_to_include);
    enter_source_file(file_to_include);
  }
  tok = lex();
}

void Lexer::handle_ident(Token &tok, IdentInfo *ident) {
  (void)tok, (void)ident;
}

Token Lexer::lex() {
  if (cached_tokens.size() != 0) {
    auto t = cached_tokens.front();
    cached_tokens.erase(cached_tokens.begin());
    return t;
  }
  assert(cur_lexer != nullptr && "Nothing to lex");
  return cur_lexer->lex();
}
