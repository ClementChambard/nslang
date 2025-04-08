#include tok.nsh

struct IdentInfo {
    ty: Tok;
    val: i8*;
    len: i64;
};

lib fn ident_info_add(ty: Tok, val: i8*, len: i64) -> IdentInfo*;

lib fn ident_info_add_keywords();

lib fn ident_info_needs_handling(self: IdentInfo *) -> bool;

lib fn ident_info_find(name: i8*, len: i64) -> IdentInfo*;

lib fn ident_info_remove_all();
