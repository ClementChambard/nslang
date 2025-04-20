#include loc.nsh
#include str.nsh

struct OpenedFile {
    filename: i8*;
    source: Str;
    pos_offset: i64;
    line_cache: i64*;
    line_cache_len: i64;
};


lib fn OpenedFile::line_offset(self: OpenedFile*, line: i64) -> i64;

lib fn OpenedFile::make_line_cache(self: OpenedFile*);

lib fn OpenedFile::open(filename: i8*) -> OpenedFile*;

lib fn OpenedFile::close_all();

lib fn OpenedFile::find(filename: i8*) -> OpenedFile*;

lib fn OpenedFile::find_by_loc(loc: Loc) -> OpenedFile*;

lib fn OpenedFile::get_loc(out: CompleteLoc*, loc: Loc);
