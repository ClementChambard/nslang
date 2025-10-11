#ifndef LEXER_LEXER_HPP_INCLUDED
#define LEXER_LEXER_HPP_INCLUDED

#include "file_lexer.hpp"
#include "lexer/ident.hpp"
#include "token.hpp"
#include <memory>
#include <vector>

struct Lexer {
  std::unique_ptr<FileLexer> cur_lexer;
  std::vector<std::unique_ptr<FileLexer>> lexer_stack;
  std::vector<Token> cached_tokens;
  std::vector<std::string> include_paths;
  std::vector<std::string> already_included_files;

  Lexer() = default;

  void add_include_path(std::string const &path) {
    include_paths.push_back(path);
  }

  void enter_token(Token tok, bool a) {
    (void)a;
    cached_tokens.push_back(tok);
  }

  void enter_source_file(std::string const &filename);
  void end_source_file(Token &tok, u32 cur_pos);
  void handle_directive(Token &tok);
  void handle_ident(Token &tok, IdentInfo *ident);

  Token lex();
};

#endif // LEXER_LEXER_HPP_INCLUDED
