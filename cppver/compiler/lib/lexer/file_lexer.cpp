#include "file_lexer.hpp"
#include "diags/diagnostic.hpp"
#include "file.hpp"
#include "lexer.hpp"
#include "lexer/char_utils.hpp"
#include "lexer/ident.hpp"
#include "lexer/tok.hpp"

FileLexer::FileLexer(std::string const &filename, Lexer *the_lexer)
    : l(the_lexer), pos(0) {
  f = OpenedFile::find(filename);
  if (!f) {
    f = OpenedFile::open(filename);
  }
}

void FileLexer::construct_token(Token &token, u32 end_pos, Tok kind) {
  token.kind = kind;
  token.loc = f->pos_offset + pos;
  token.value = &f->source[pos];
  token.len = end_pos - pos;
  pos = end_pos;
}

void FileLexer::handle_end_of_file(Token &token, u32 cur_pos) {
  if (l)
    l->end_source_file(token, cur_pos);
  else
    construct_token(token, cur_pos, tok::END_OF_FILE);
}

std::string FileLexer::read_to_whitespace() {
  u32 cur_pos = pos;
  while (!std::isspace(f->source[cur_pos])) {
    cur_pos++;
  }
  std::string out(&f->source[pos], cur_pos - pos);
  pos = cur_pos;
  return out;
}

std::string FileLexer::read_to_end_of_line() {
  u32 cur_pos = pos;
  while (f->source[cur_pos] != '\n' && f->source[cur_pos] != '\r') {
    cur_pos++;
  }
  char last = f->source[cur_pos];
  cur_pos++;
  char next = f->source[cur_pos];
  if (last == '\n' && next == '\r')
    cur_pos++;
  if (last == '\r' && next == '\n')
    cur_pos++;
  std::string out(&f->source[pos], cur_pos - pos);
  pos = cur_pos;
  return out;
}

void FileLexer::handle_directive(Token &token) {
  if (l)
    l->handle_directive(token);
  else {
    construct_token(token, pos, tok::END_OF_FILE); // TODO: ??
  }
}

void FileLexer::lex_ident(Token &token, u32 cur_pos) {
  while (is_ident_continue(f->source[cur_pos]))
    cur_pos++;
  auto ident = IdentInfo::find(std::string(&f->source[pos], cur_pos - pos));
  if (l && ident->needs_handling())
    l->handle_ident(token, ident);
  construct_token(token, cur_pos, ident->token_kind);
  if (ident->token_kind == tok::IDENT)
    token.value = ident;
}

void FileLexer::lex_number(Token &token, u32 cur_pos) {
  while (true) {
    char c = f->source[cur_pos];
    if (c == 'e' &&
        (f->source[cur_pos + 1] == '+' || f->source[cur_pos + 1] == '-')) {
      cur_pos += 2;
    } else if (c == '\'' && is_ident_continue(f->source[cur_pos + 1])) {
      cur_pos += 2;
    } else if (is_ident_continue(c) || c == '.') {
      cur_pos += 1;
    } else {
      break;
    }
  }
  construct_token(token, cur_pos, tok::NUM);
}

void FileLexer::lex_char_literal(Token &token, u32 cur_pos) {
  char c = f->source[cur_pos];
  while (c != '\'') {
    if (c == '\\')
      cur_pos += 1;
    cur_pos += 1;
    c = f->source[cur_pos];
  }
  construct_token(token, cur_pos + 1, tok::CHR);
}

void FileLexer::lex_str_literal(Token &token, u32 cur_pos) {
  char c = f->source[cur_pos];
  while (c != '"') {
    if (c == '\\')
      cur_pos += 1;
    cur_pos += 1;
    c = f->source[cur_pos];
  }
  construct_token(token, cur_pos + 1, tok::STR);
}

u32 FileLexer::skip_to_end_of_line_comment(u32 cur_pos) {
  while (true) {
    char c = f->source[cur_pos];
    if (c == '\0' || c == '\n' || c == '\r')
      return cur_pos;
    cur_pos++;
  }
}

u32 FileLexer::skip_to_end_of_multiline_comment(u32 cur_pos) {
  while (true) {
    char c = f->source[cur_pos];
    if (c == '\0') {
      if (cur_pos != f->source_len) {
        Diag(diag::WARNING, f->pos_offset + cur_pos, "NULL in file.");
      } else {
        Diag(diag::ERROR, f->pos_offset + cur_pos - 1,
             "unterminated multiline comment.");
        return cur_pos - 1;
      }
    } else if (c == '*' && f->source[cur_pos + 1] == '/') {
      return cur_pos + 2;
    }
    cur_pos += 1;
  }
}

void FileLexer::lex_internal(Token &token) {
  while (std::isspace(f->source[pos]) && f->source[pos] != '\0')
    pos += 1;
  char c = f->source[pos];
  u32 cur_pos = pos + 1;
  // clang-format off
  switch(c) {
    case '\0':
      if (cur_pos == f->source_len + 1) {
        pos -= 1;
        return handle_end_of_file(token, cur_pos - 2);
      } else {
        Diag(diag::WARNING, f->pos_offset + pos, "NULL in file.");
        pos = cur_pos;
        return lex_internal(token);
      }
    case '#':
      token.loc = f->pos_offset + pos;
      pos += 1;
      return handle_directive(token);
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      return lex_number(token, cur_pos);
    case 'a': case 'b': case 'c': case 'd': case 'e': 
    case 'f': case 'g': case 'h': case 'i': case 'j': 
    case 'k': case 'l': case 'm': case 'n': case 'o': 
    case 'p': case 'q': case 'r': case 's': case 't': 
    case 'u': case 'v': case 'w': case 'x': case 'y': 
    case 'A': case 'B': case 'C': case 'D': case 'E': 
    case 'F': case 'G': case 'H': case 'I': case 'J': 
    case 'K': case 'L': case 'M': case 'N': case 'O': 
    case 'P': case 'Q': case 'R': case 'S': case 'T': 
    case 'U': case 'V': case 'W': case 'X': case 'Y': 
    case '_': case 'z': case 'Z':
      return lex_ident(token, cur_pos);
    case '(':
      return construct_token(token, cur_pos, tok::LPAREN);
    case ')':
      return construct_token(token, cur_pos, tok::RPAREN);
    case '{':
      return construct_token(token, cur_pos, tok::LBRACE);
    case '}':
      return construct_token(token, cur_pos, tok::RBRACE);
    case '[':
      return construct_token(token, cur_pos, tok::LSQUARE);
    case ']':
      return construct_token(token, cur_pos, tok::RSQUARE);
    case ';':
      return construct_token(token, cur_pos, tok::SEMI);
    case ':':
      c = f->source[cur_pos];
      if (c == ':') {
        return construct_token(token, cur_pos + 1, tok::COLONCOLON);
      } else {
        return construct_token(token, cur_pos, tok::COLON);
      }
    case ',':
      return construct_token(token, cur_pos, tok::COMMA);
    case '?':
      return construct_token(token, cur_pos, tok::QUESTION);
    case '~':
      return construct_token(token, cur_pos, tok::TILDE);
    case '<':
      c = f->source[cur_pos];
      if (c == '<') {
        c = f->source[cur_pos + 1];
        if (c == '=') {
          return construct_token(token, cur_pos + 2, tok::LESSLESSEQUAL);
        } else {
          return construct_token(token, cur_pos + 1, tok::LESSLESS);
        }
      } else if (c == '=') {
        return construct_token(token, cur_pos + 1, tok::LESSEQUAL);
      } else {
        return construct_token(token, cur_pos, tok::LESS);
      }
    case '>':
      c = f->source[cur_pos];
      if (c == '>') {
        c = f->source[cur_pos + 1];
        if (c == '=') {
          return construct_token(token, cur_pos + 2, tok::GREATERGREATEREQUAL);
        } else {
          return construct_token(token, cur_pos + 1, tok::GREATERGREATER);
        }
      } else if (c == '=') {
        return construct_token(token, cur_pos + 1, tok::GREATEREQUAL);
      } else {
        return construct_token(token, cur_pos, tok::GREATER);
      }
    case '+':
      c = f->source[cur_pos];
      if (c == '+') {
        return construct_token(token, cur_pos + 1, tok::PLUSPLUS);
      } else if (c == '=') {
        return construct_token(token, cur_pos + 1, tok::PLUSEQUAL);
      } else {
        return construct_token(token, cur_pos, tok::PLUS);
      }
    case '&':
      c = f->source[cur_pos];
      if (c == '&') {
        return construct_token(token, cur_pos + 1, tok::AMPAMP);
      } else if (c == '=') {
        return construct_token(token, cur_pos + 1, tok::AMPEQUAL);
      } else {
        return construct_token(token, cur_pos, tok::AMP);
      }
    case '|':
      c = f->source[cur_pos];
      if (c == '|') {
        return construct_token(token, cur_pos + 1, tok::PIPEPIPE);
      } else if (c == '=') {
        return construct_token(token, cur_pos + 1, tok::PIPEEQUAL);
      } else {
        return construct_token(token, cur_pos, tok::PIPE);
      }
    case '-':
      c = f->source[cur_pos];
      if (c == '-') {
        return construct_token(token, cur_pos + 1, tok::MINUSMINUS);
      } else if (c == '=') {
        return construct_token(token, cur_pos + 1, tok::MINUSEQUAL);
      } else if (c == '>') {
        return construct_token(token, cur_pos + 1, tok::ARROW);
      } else {
        return construct_token(token, cur_pos, tok::MINUS);
      }
    case '=':
      c = f->source[cur_pos];
      if (c == '=') {
        return construct_token(token, cur_pos + 1, tok::EQUALEQUAL);
      } else {
        return construct_token(token, cur_pos, tok::EQUAL);
      }
    case '*':
      c = f->source[cur_pos];
      if (c == '=') {
        return construct_token(token, cur_pos + 1, tok::STAREQUAL);
      } else {
        return construct_token(token, cur_pos, tok::STAR);
      }
    case '%':
      c = f->source[cur_pos];
      if (c == '=') {
        return construct_token(token, cur_pos + 1, tok::PERCENTEQUAL);
      } else {
        return construct_token(token, cur_pos, tok::PERCENT);
      }
    case '!':
      c = f->source[cur_pos];
      if (c == '=') {
        return construct_token(token, cur_pos + 1, tok::EXCLAIMEQUAL);
      } else {
        return construct_token(token, cur_pos, tok::EXCLAIM);
      }
    case '^':
      c = f->source[cur_pos];
      if (c == '=') {
        return construct_token(token, cur_pos + 1, tok::CARETEQUAL);
      } else {
        return construct_token(token, cur_pos, tok::CARET);
      }
    case '/':
      c = f->source[cur_pos];
      if (c == '/') {
        pos = skip_to_end_of_line_comment(cur_pos + 1);
        return lex_internal(token);
      } else if (c == '*') {
        pos = skip_to_end_of_multiline_comment(cur_pos + 1);
        return lex_internal(token);
      } else if (c == '=') {
        return construct_token(token, cur_pos + 1, tok::SLASHEQUAL);
      } else {
        return construct_token(token, cur_pos, tok::SLASH);
      }
    case '.':
      c = f->source[cur_pos];
      if (c >= '0' && c <= '9') {
        return lex_number(token, cur_pos + 1);
      } else if (c == '.' && f->source[cur_pos + 1] == '.') {
        return construct_token(token, cur_pos + 2, tok::ELLIPSIS);
      } else {
        return construct_token(token, cur_pos, tok::PERIOD);
      }
    case '\'':
      return lex_char_literal(token, cur_pos);
    case '"':
      return lex_str_literal(token, cur_pos);
    default:
      Diag(diag::ERROR, pos + f->pos_offset, "Unknown character '%c' in source file.", c);
      pos = cur_pos;
      return lex_internal(token);
      //clang-format on
  }
}
