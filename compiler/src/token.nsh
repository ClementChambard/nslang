#include loc.nsh
#include tok.nsh

struct Token {
    ty: Tok;
    value: void*; // IdentInfo* or i8*
    len: i64;
    loc: Loc;
};

lib fn token_dump(self: Token*);
lib fn token_copy_from(self: Token*, other: Token*);
lib fn token_get_end_loc(self: Token*) -> Loc;
