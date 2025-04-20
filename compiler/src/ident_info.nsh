#include tok.nsh

struct IdentInfo {
    ty: Tok;
    val: i8*;
    len: i64;
};

lib fn IdentInfo::add(ty: Tok, val: i8*, len: i64) -> IdentInfo*;

lib fn IdentInfo::add_keywords();

lib fn IdentInfo::needs_handling(self: IdentInfo *) -> bool;

lib fn IdentInfo::find(name: i8*, len: i64) -> IdentInfo*;

lib fn IdentInfo::remove_all();
