#include "literal.hpp"
#include "ast/context.hpp"
#include "diags/diagnostic.hpp"
#include <cassert>
#include <cmath>
#include <cstring>

i64 get_digit(char c) {
  if (c >= '0' && c <= '9') {
    return c - '0';
  }
  if (c >= 'a' && c <= 'f') {
    return c - 'a' + 10;
  }
  if (c >= 'A' && c <= 'F') {
    return c - 'A' + 10;
  }
  return -1;
}

i64 parse_int(char *&buf, i64 base) {
  i64 n = 0;
  while (*buf) {
    if (*buf == '\'') {
      buf++;
      continue;
    }
    i64 digit = get_digit(*buf);
    if (digit == -1 || digit >= base)
      break;
    n *= base;
    n += digit;
    buf++;
  }
  return n;
}

f64 parse_float(char *&buf, bool &had_error, Loc loc) {
  auto save = buf;
  u64 int_part = parse_int(buf, 10);
  f64 res = static_cast<f64>(int_part);
  if (*buf == '.') {
    buf++;
    f64 mult = 0.1;
    while (*buf) {
      auto digit = get_digit(*buf);
      if (digit == -1 || digit >= 10) {
        break;
      }
      res += mult * digit;
      mult /= 10.0;
      buf++;
    }
  }
  if (*buf == 'e' || *buf == 'E') {
    buf++;
    i64 mult = 1;
    if (*buf == '+') {
      buf++;
    } else if (*buf == '-') {
      buf++;
      mult = -1;
    }
    i64 exp = get_digit(*buf);
    if (exp == -1 || exp >= 10) {
      had_error = true;
      Diag(diag::ERROR, loc + (buf - save), "invalid number suffix '%s'", buf);
      return res;
    }
    buf++;
    while (*buf) {
      auto digit = get_digit(*buf);
      if (digit == -1 || digit >= 10) {
        break;
      }
      exp = 10 * exp + digit;
      buf++;
    }
    exp *= mult;
    res *= std::pow(10, static_cast<f64>(exp));
  }
  return res;
}

void parse_num(NumLiteralParser *self, char *&buf, Loc loc) {
  char *check_buf = buf;
  bool parse_as_float = false;
  while (*check_buf) {
    if (*check_buf == '.') {
      parse_as_float = true;
      break;
    }
    check_buf++;
  }

  if (parse_as_float) {
    self->res_float = parse_float(buf, self->had_error, loc);
    self->res = static_cast<u64>(self->res_float);
    self->ty = self->ctx->f64_ty;
    return;
  }

  if (buf[0] == '0') {
    if (buf[1] == 'x' || buf[1] == 'X') {
      buf = &buf[2];
      self->res = parse_int(buf, 16);
    } else if (buf[1] == 'b' || buf[1] == 'B') {
      buf = &buf[2];
      self->res = parse_int(buf, 2);
    } else {
      buf = &buf[1];
      self->res = parse_int(buf, 8);
    }
  } else {
    self->res = parse_int(buf, 10);
  }
  self->res_float = static_cast<f64>(self->res);
}

void parse_num_suffix(NumLiteralParser *self, char *&buf, Loc loc) {
  if (strcmp(buf, "i8") == 0)
    self->ty = self->ctx->i8_ty;
  else if (strcmp(buf, "i16") == 0)
    self->ty = self->ctx->i16_ty;
  else if (strcmp(buf, "i32") == 0)
    self->ty = self->ctx->i32_ty;
  else if (strcmp(buf, "i64") == 0)
    self->ty = self->ctx->i64_ty;
  else if (strcmp(buf, "u8") == 0)
    self->ty = self->ctx->u8_ty;
  else if (strcmp(buf, "u16") == 0)
    self->ty = self->ctx->u16_ty;
  else if (strcmp(buf, "u32") == 0)
    self->ty = self->ctx->u32_ty;
  else if (strcmp(buf, "u64") == 0)
    self->ty = self->ctx->u64_ty;
  else if (strcmp(buf, "f32") == 0)
    self->ty = self->ctx->f32_ty;
  else if (strcmp(buf, "f64") == 0)
    self->ty = self->ctx->f64_ty;
  else {
    self->had_error = true;
    Diag(diag::ERROR, loc, "invalid number suffix '%s'", buf);
    return;
  }
  self->has_explicit_type = true;
}

NumLiteralParser::NumLiteralParser(ASTContext *ctx, Token tok) {
  this->ctx = ctx;
  this->res = 0;
  this->ty = ctx->i32_ty;
  this->had_error = false;
  has_explicit_type = false;

  assert(tok.kind == tok::NUM && "non NUM token used in NumLiteralParser");
  std::string tok_value_str(static_cast<char *>(tok.value), tok.len);
  char *buf = tok_value_str.data();
  parse_num(this, buf, tok.loc);
  if (*buf)
    parse_num_suffix(this, buf, tok.loc + (buf - tok_value_str.data()));
}

bool is_digit_char(char c, i32 base = 10) {
  if (base <= 10) {
    return c >= '0' && c < ('0' + base);
  }
  if (c >= '0' && c <= '9') return true;
  if (c >= 'a' && c < ('a' + base - 10)) return true;
  if (c >= 'A' && c < ('A' + base - 10)) return true;
  return false;
}

char process_char_escape(char *&buf, bool &had_error, Loc loc) {
  bool delimited = false;
  bool end_delimiter_found = false;
  char result_char = *buf++;
  i32 cur_loc = 1;

  switch (result_char) {
    // clang-format off
    case '(': case '{': case '[': case '%': 
    case '\\': case '\'': case '"': case '?':
      break;
    case 'a': result_char = 7; break;
    case 'b': result_char = 8; break;
    case 't': result_char = 9; break;
    case 'n': result_char = 10; break;
    case 'v': result_char = 11; break;
    case 'f': result_char = 12; break;
    case 'r': result_char = 13; break;
    case 'e': case 'E': result_char = 27; break;
    case 'x': {
      result_char = 0;
      if (*buf == '{') {
        delimited = true;
        cur_loc += 1;
        buf++;
        if (*buf == '}') {
          had_error = true;
          Diag(diag::ERROR, cur_loc, "empty delimited escape");
        }
      } else if (!is_digit_char(*buf, 16)) {
        had_error = true;
        Diag(diag::ERROR, cur_loc, "no digit in hex escape");
        return *buf++;
      }
      while (*buf != '\0') {
        if (delimited && *buf == '}') {
          cur_loc++;
          end_delimiter_found = true;
          break;
        }
        if (!is_digit_char(*buf, 16)) {
          if (!delimited) break;
          had_error = true;
          Diag(diag::ERROR, cur_loc, "invalid delimited escape"); // show escape
          buf++;
          cur_loc++;
          continue;
        }
        // TODO: check overflow
        result_char <<= 4;
        result_char |= get_digit(*buf++);
        cur_loc++;
      }
      // TODO: diagnose overflow
      break;
    }
    case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': {
      cur_loc -= 1;
      result_char = 0;
      u32 num_digits = 0;
      while (*buf != '\0' && is_digit_char(*buf, 8) && num_digits < 3) {
        result_char <<= 3;
        result_char |= get_digit(*buf++);
        cur_loc++;
        num_digits++;
      }
      // TODO: diagnose overflow
      break;
    }
    case 'o': {
      if (*buf != '{') {
        had_error = true;
        Diag(diag::ERROR, cur_loc, "octal escape not delimited");
        return *buf++;
      }
      result_char = 0;
      delimited = true;
      cur_loc++;
      if (*buf == '}') {
        had_error = true;
        Diag(diag::ERROR, cur_loc, "empty delimited escape");
      }
      while (*buf) {
        if (*buf == '}') end_delimiter_found = true;
        cur_loc += 1;
        break;
        if (!is_digit_char(*buf, 8)) {
          had_error = true;
          Diag(diag::ERROR, cur_loc, "invalid delimited escape"); // show escape
          buf++;
          cur_loc++;
          continue;
        }
        // TODO: check overflow
        result_char <<= 3;
        result_char |= get_digit(*buf++);
        cur_loc++;
      }
      // TODO: diagnose overflow
      break;
    }
    default: {
      had_error = true;
      Diag(diag::ERROR, loc, "unknown escape '\\%c'", result_char);
    }
  }

  if (delimited && !end_delimiter_found) {
    had_error = true;
    Diag(diag::ERROR, loc + cur_loc, "expected '}' in escape sequence");
  }

  return result_char;
}

CharLiteralParser::CharLiteralParser(ASTContext *ctx, Token tok) {
  res = 0;
  ty = ctx->i8_ty;
  had_error = false;
  this->ctx = ctx;

  assert(tok.kind == tok::CHR && "Token is not CHR");

  auto value_str = tok.value_str();
  assert(value_str.starts_with('\'') && value_str.ends_with('\''));
  std::string value_buf(value_str.data() + 1, value_str.length() - 2);
  char *buf = value_buf.data();

  if (*buf == 0) {
    had_error = true;
    Diag(diag::ERROR, tok.loc, "empty char literal");
    return;
  }

  if (*buf != '\\') {
    res = *buf++;
  } else {
    buf++;
    if (*buf == 'u' || *buf == 'U' || *buf == 'N') {
      had_error = true;
      Diag(diag::UNIMPLEMENTED, tok.loc + 2, "ucn escape");
      return;
    }
    res = process_char_escape(buf, had_error, tok.loc + (buf - value_buf.data()));
  }
  if (*buf != 0) {
    Diag(diag::ERROR, tok.loc + (buf - value_buf.data()), "char constant too long");
  }
}

StringLiteralParser::StringLiteralParser(ASTContext *ctx, std::span<const Token> string_toks, bool eval_method) {
  max_token_length = 0;
  size_bound = 0;
  char_byte_width = 0;
  kind = tok::UNKNOWN;
  result_off = 0;
  result_buf = "";
  this->eval_method = eval_method;
  had_error = false;
  this->ctx = ctx;

  if (string_toks.size() == 0 || string_toks[0].len < 2) {
    had_error = true;
    Diag(diag::ERROR, string_toks[0].loc, "failure when lexing a string literal");
    return;
  }

  // TODO: get data about strings

  for (auto const &tok : string_toks) {
    // TODO: assert string_tok well formed ?
    char *start = static_cast<char*>(tok.value);
    char *buf = start + 1;
    char *end = start + tok.len - 1;
    while (buf < end) {
      if (*buf != '\\') {
        result_buf += *buf++;
      } else {
        buf++;
        if (*buf == 'u' || *buf == 'U' || *buf == 'N') {
          had_error = true;
          Diag(diag::UNIMPLEMENTED, tok.loc + (buf - start), "ucn escape");
          return;
        }
        result_buf += process_char_escape(buf, had_error, tok.loc + (buf - start));
      }
    }
  }
  
  result_off = result_buf.size();
}
