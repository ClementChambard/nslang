#include loc.nsh
#include tok.nsh

struct Token {
    ty: Tok;
    value: void*; // IdentInfo* or i8*
    len: i64;
    loc: Loc;
};

lib fn Token::dump(self: Token*);
lib fn Token::copy_from(self: Token*, other: Token*);
lib fn Token::get_end_loc(self: Token*) -> Loc;
