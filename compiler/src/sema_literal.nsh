#include tok.nsh
#include token.nsh

struct Type;

struct NumLiteralParser {
  res: i64;
  ty: Type*;
  had_error: bool;
};

lib fn NumLiteralParser::init(self: NumLiteralParser*, tok: Token*);

struct CharLiteralParser {
  res: i64;
  ty: Type*;
  had_error: bool;
};

lib fn CharLiteralParser::init(self: CharLiteralParser*, tok: Token*);

struct StringLiteralParser {
  max_token_length: i64;
  size_bound: i64;
  char_byte_width: i64;
  kind: Tok;
  result_buf: i8*;
  result_off: i64;
  eval_method: bool;
  had_error: bool;
};

lib fn StringLiteralParser::init(self: StringLiteralParser*, string_toks: Token**, string_toks_count: i64, eval_method: bool);

lib fn StringLiteralParser::get_string(self: StringLiteralParser*) -> i8*;
lib fn StringLiteralParser::get_string_length(self: StringLiteralParser*) -> i64;
lib fn StringLiteralParser::get_num_string_chars(self: StringLiteralParser*) -> i64;
