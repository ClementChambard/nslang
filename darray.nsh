struct DArray {
    data: void*;
    el_size: i64;
    size: i64;
    capacity: i64;
};

lib fn darray_init(this: DArray*, el_size: i64);

lib fn darray_free(this: DArray*);

lib fn darray_get_ptr(this: DArray*, i: i64) -> void*;

lib fn darray_get_cpy(this: DArray*, i: i64, buf: void*);

lib fn darray_push(this: DArray*, el: void*);

lib fn darray_pop(this: DArray*);
