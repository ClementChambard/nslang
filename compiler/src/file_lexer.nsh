#include opened_file.nsh
#include token.nsh
#include str.nsh

// struct Lexer;

struct FileLexer {
    f: OpenedFile*;
    pos: i64;
    the_lexer: void*; // Lexer*;
};

lib fn file_lexer_create(filename: i8*, the_lexer: void*) -> FileLexer*;

lib fn file_lexer_construct_token(self: FileLexer*, token: Token*, end_pos: i64, ty: Tok);

lib fn file_lexer_handle_end_of_file(self: FileLexer*, token: Token*, cur_pos: i64);

lib fn file_lexer_read_to_whitespace(self: FileLexer*, out: Str*); // TODO: simple structs (and complex structs) return types

lib fn file_lexer_read_to_end_of_line(self: FileLexer*, out: Str*);

lib fn file_lexer_handle_directive(self: FileLexer*, token: Token*);

lib fn file_lexer_lex_ident(self: FileLexer*, token: Token*, cur_pos: i64);

lib fn file_lexer_lex_number(self: FileLexer*, token: Token*, cur_pos: i64);

lib fn file_lexer_skip_to_end_of_line_comment(self: FileLexer*, cur_pos: i64) -> i64;

lib fn file_lexer_skip_to_end_of_multiline_comment(self: FileLexer*, cur_pos: i64) -> i64;

lib fn file_lexer_lex_char_literal(self: FileLexer*, token: Token*, cur_pos: i64);

lib fn file_lexer_lex_str_literal(self: FileLexer*, token: Token*, cur_pos: i64);

lib fn file_lexer_lex_internal(self: FileLexer*, token: Token*);

lib fn file_lexer_lex(self: FileLexer*) -> Token*;
