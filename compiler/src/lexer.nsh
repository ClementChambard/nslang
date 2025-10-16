#include file_lexer.nsh
#include darray.nsh
#include token.nsh
#include ident_info.nsh

struct Lexer {
    cur_lexer: FileLexer*;
    lexer_stack: DArray;
    cached_tokens: DArray;
    include_paths: DArray;
    already_included_files: DArray;
};

lib fn Lexer::init(self: Lexer*) init;

lib fn Lexer::delete(self: Lexer*);

lib fn Lexer::add_include_path(self: Lexer*, path: i8*);

lib fn Lexer::enter_token(self: Lexer*, token: Token*);

lib fn Lexer::enter_source_file(self: Lexer*, filename: i8*);

lib fn Lexer::end_source_file(self: Lexer*, token: Token*);

lib fn Lexer::handle_directive(self: Lexer*, token: Token*);

lib fn Lexer::handle_ident(self: Lexer*, token: Token*, ident_info: IdentInfo*);

lib fn Lexer::lex(self: Lexer*) -> Token*;
