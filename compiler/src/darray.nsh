struct DArray {
    super data: void**;
    len: i64;
    capacity: i64;
};

lib fn DArray::init(self: DArray*) init;

lib fn DArray::destroy(self: DArray*);

lib fn DArray::push(self: DArray*, value: void*);

lib fn DArray::pop(self: DArray*) -> void*;

lib fn DArray::pop_front(self: DArray*) -> void*;

lib fn DArray::erase(self: DArray*, pos: i64);

lib fn DArray::erase_it(self: DArray*, pos: void**);

lib fn DArray::begin(self: DArray*) -> void**;

lib fn DArray::end(self: DArray*) -> void**;

lib fn range_find(begin: void**, end: void**, value: void*) -> void**;
