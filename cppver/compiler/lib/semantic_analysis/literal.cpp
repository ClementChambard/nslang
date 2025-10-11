#include "literal.hpp"
#include "ast/context.hpp"
#include "diags/diagnostic.hpp"
#include <cassert>
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
    i64 digit = get_digit(*buf);
    if (digit == -1 || digit >= base)
      break;
    n *= base;
    n += digit;
    buf++;
  }
  return n;
}

void parse_num(NumLiteralParser *self, char *&buf) {
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
    // TODO: float that start with 0
  } else {
    // TODO: float
    self->res = parse_int(buf, 10);
  }
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
  parse_num(this, buf);
  if (*buf)
    parse_num_suffix(this, buf, tok.loc + (buf - tok_value_str.data()));
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
    //        result_char = 0
    //        if buf[cur_loc:] != "" and buf[cur_loc] == "{":
    //            delimited = True
    //            cur_loc += 1
    //            if buf[cur_loc] == "}":
    //                *had_error = True
    //                diag(loc, "diag::err_delimited_escape_empty", Diag.ERROR)
    //        elif buf[cur_loc:] == "" or buf[cur_loc] not in HEX_DIGITS:
    //            *had_error = True
    //            diag(loc, "diag::err_hex_escape_no_digits x", Diag.ERROR)
    //            return buf[1:]
    //        overflow = False
    //        while buf[cur_loc:] != "":
    //            if delimited and buf[cur_loc] == "}":
    //                cur_loc += 1
    //                end_delimiter_found = True
    //                break
    //            if buf[cur_loc] not in HEX_DIGITS:
    //                if not delimited:
    //                    break
    //                *had_error = True
    //                diag(
    //                    loc,
    //                    "diag::err_delimited_escape_invalid <<
    //                    StringRef(cur_loc, 1)", Diag.ERROR,
    //                )
    //                continue
    //            if result_char & 0xF0000000:
    //                overflow = True
    //            result_char <<= 4
    //            result_char |= hex_digit_value(buf[cur_loc])
    //            cur_loc += 1
    //        # Check overflow depending on char width
    //        if result_char >> 8 != 0:  # use char_width variable
    //            overflow = True
    //            result_char &= ~0 >> (32 - 8)  # use char_width variable
    //        if not *had_error and overflow:
    //            *had_error = True
    //            diag(loc, "diag::err_escape_too_large 0", Diag.ERROR)
      break;
    }
    case '0': case '1': case '2': case '3': case '4': case '5': case '6': case '7': {
    //        cur_loc -= 1
    //        result_char = 0
    //        num_digits = 0
    //        while (
    //            buf[cur_loc:] != ""
    //            and num_digits < 3
    //            and buf[cur_loc] in OCTAL_DIGITS
    //        ):
    //            result_char <<= 3
    //            result_char |= ord(buf[cur_loc]) - ord("0")
    //            cur_loc += 1
    //            num_digits += 1
    //        # Check overflow depending on char width
    //        if result_char >> 8 != 0:  # use char_width variable
    //            *had_error = True
    //            result_char &= ~0 >> (32 - 8)  # use char_width variable
    //            diag(loc, "diag::err_escape_too_large 1", Diag.ERROR)
      break;
    }
    case 'o': {
    //        overflow = False
    //        if buf[cur_loc:] == "" or buf[cur_loc] != "{":
    //            *had_error = True
    //            diag(loc, "diag::err_hex_escape_no_digits x", Diag.ERROR)
    //            return buf[1:]
    //        result_char = 0
    //        delimited = True
    //        cur_loc += 1
    //        if buf[cur_loc] == "}":
    //            *had_error = True
    //            diag(loc, "diag::err_delimited_escape_empty", Diag.ERROR)
    //        while buf[cur_loc:] != "":
    //            if buf[cur_loc] == "}":
    //                end_delimiter_found = True
    //                cur_loc += 1
    //                break
    //            if buf[cur_loc] not in OCTAL_DIGITS:
    //                *had_error = True
    //                diag(
    //                    loc,
    //                    "diag::err_delimited_escape_invalid <<
    //                    StringRef(cur_loc, 1)", Diag.ERROR,
    //                )
    //                cur_loc += 1
    //                continue
    //            if result_char & 0xE0000000:
    //                overflow = True
    //            result_char <<= 3
    //            result_char |= ord(buf[cur_loc]) - ord("0")
    //            cur_loc += 1
    //        # Check overflow depending on char width
    //        if not *had_error and overflow or (result_char >> 8) != 0:
    //            *had_error = True
    //            result_char &= ~0 >> (32 - 8)  # use char_width variable
    //            diag(loc, "diag::err_escape_too_large 1", Diag.ERROR)
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

  // if (EvalMethod == StringLiteralEvalMethod::Unevaluated &&
  // !IsEscapeValidInUnevaluatedStringLiteral(Escape)) { Diag(Diags, Features,
  // Loc, ThisTokBegin, EscapeBegin, ThisTokBuf,
  // diag::err_unevaluated_string_invalid_escape_sequence) <<
  // StringRef(EscapeBegin, ThisTokBuf - EscapeBegin); HadError = true; }

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
