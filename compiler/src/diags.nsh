#include loc.nsh
#include darray.nsh

enum : i64 {
    DIAG_WARNING,
    DIAG_ERROR,
    DIAG_NOTE,
    DIAG_UNIMPLEMENTED,
};

struct Diag {
    level: i64;
    loc: Loc;
    msg: i8*;
    ranges: DArray;
    hints: DArray;
};

lib fn fatal_error(msg: i8*);

lib fn diag_init(d: Diag*, loc: Loc, msg: i8*, level: i64);

lib fn diag_destroy(d: Diag*);

lib fn diag_add_range(d: Diag*, rge: LocRge*);

lib fn diag_emit(d: Diag*);

lib fn diag(loc: Loc, msg: i8*, level: i64); // equivalent to diag_init diag_emit diag_destroy (cant add range)

lib fn reset_errors();

lib fn compilation_had_errors() -> bool;
