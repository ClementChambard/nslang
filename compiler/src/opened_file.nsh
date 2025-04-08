#include loc.nsh
#include str.nsh

struct OpenedFile {
    filename: i8*;
    source: Str;
    pos_offset: i64;
    line_cache: i64*;
    line_cache_len: i64;
};


lib fn opened_file_line_offset(self: OpenedFile*, line: i64) -> i64;

lib fn opened_file_make_line_cache(self: OpenedFile*);

lib fn opened_file_open(filename: i8*) -> OpenedFile*;

lib fn opened_file_close_all();

lib fn opened_file_find(filename: i8*) -> OpenedFile*;

lib fn opened_file_find_by_loc(loc: Loc) -> OpenedFile*;

lib fn opened_file_get_loc(out: CompleteLoc*, loc: Loc);
