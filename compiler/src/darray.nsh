struct DArray {
    data: void**;
    len: i64;
    capacity: i64;
};

lib fn darray_init(self: DArray*);

lib fn darray_destroy(self: DArray*);

lib fn darray_push(self: DArray*, value: void*);

lib fn darray_pop(self: DArray*) -> void*;

lib fn darray_pop_front(self: DArray*) -> void*;

lib fn darray_erase(self: DArray*, pos: i64);
