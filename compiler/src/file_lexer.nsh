#include opened_file.nsh
#include token.nsh
#include str.nsh

// TODO: Forward declaration of structures

// struct Lexer;

struct FileLexer {
    f: OpenedFile*;
    pos: i64;
    the_lexer: void*; // Lexer*;
};

lib fn FileLexer::create(filename: i8*, the_lexer: void*) -> FileLexer*;

lib fn FileLexer::construct_token(self: FileLexer*, token: Token*, end_pos: i64, ty: Tok);

lib fn FileLexer::handle_end_of_file(self: FileLexer*, token: Token*, cur_pos: i64);

lib fn FileLexer::read_to_whitespace(self: FileLexer*, out: Str*); // TODO: simple structs (and complex structs) return types

lib fn FileLexer::read_to_end_of_line(self: FileLexer*, out: Str*);

lib fn FileLexer::handle_directive(self: FileLexer*, token: Token*);

lib fn FileLexer::lex_ident(self: FileLexer*, token: Token*, cur_pos: i64);

lib fn FileLexer::lex_number(self: FileLexer*, token: Token*, cur_pos: i64);

lib fn FileLexer::skip_to_end_of_line_comment(self: FileLexer*, cur_pos: i64) -> i64;

lib fn FileLexer::skip_to_end_of_multiline_comment(self: FileLexer*, cur_pos: i64) -> i64;

lib fn FileLexer::lex_char_literal(self: FileLexer*, token: Token*, cur_pos: i64);

lib fn FileLexer::lex_str_literal(self: FileLexer*, token: Token*, cur_pos: i64);

lib fn FileLexer::lex_internal(self: FileLexer*, token: Token*);

lib fn FileLexer::lex(self: FileLexer*) -> Token*;
