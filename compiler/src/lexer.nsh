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

lib fn lexer_init(self: Lexer*);

lib fn lexer_delete(self: Lexer*);

lib fn lexer_add_include_path(self: Lexer*, path: i8*);

lib fn lexer_enter_token(self: Lexer*, token: Token*);

lib fn lexer_enter_source_file(self: Lexer*, filename: i8*);

lib fn lexer_end_source_file(self: Lexer*, token: Token*);

lib fn lexer_handle_directive(self: Lexer*, token: Token*);

lib fn lexer_handle_ident(self: Lexer*, token: Token*, ident_info: IdentInfo*);

lib fn lexer_lex(self: Lexer*) -> Token*;
