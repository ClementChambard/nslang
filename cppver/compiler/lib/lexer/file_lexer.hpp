#ifndef LEXER_FILELEXER_HPP_INCLUDED
#define LEXER_FILELEXER_HPP_INCLUDED

#include "lexer/file.hpp"
#include "lexer/token.hpp"

struct Lexer;

struct FileLexer {
  Lexer *l;
  OpenedFile *f;
  u32 pos;

  FileLexer(std::string const &filename, Lexer *the_lexer = nullptr);

  void construct_token(Token &token, u32 end_pos, Tok kind);

  void handle_end_of_file(Token &token, u32 cur_pos);

  std::string read_to_whitespace();

  std::string read_to_end_of_line();

  void handle_directive(Token &token);

  Token lex() {
    Token token;
    lex_internal(token);
    return token;
  }

private:
  void lex_ident(Token &token, u32 cur_pos);
  void lex_number(Token &token, u32 cur_pos);
  void lex_char_literal(Token &token, u32 cur_pos);
  void lex_str_literal(Token &token, u32 cur_pos);

  u32 skip_to_end_of_line_comment(u32 cur_pos);
  u32 skip_to_end_of_multiline_comment(u32 cur_pos);

  void lex_internal(Token &token);
};

#endif // LEXER_FILELEXER_HPP_INCLUDED
