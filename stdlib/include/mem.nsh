lib fn brk(addr: void*) -> i64;

lib fn sbrk(size: i64) -> void*;

lib fn memcpy(dst: void*, src: void*, size: i64) -> void*;

lib fn memset(dst: void*, byte: i8, size: i64) -> void*;

lib fn malloc(size: i64) -> void*;

lib fn free(ptr: void*);

lib fn realloc(ptr: void*, size: i64) -> void*;

lib fn calloc(nelem: i64, elsize: i64) -> void*;
