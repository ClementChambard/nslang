//////////////////////////////////////////
/// Memory Standard Library for NSLang ///
//////////////////////////////////////////

/// function brk
///   Change location of end of data segment.
///   It is recommended to use 'malloc' for memory allocation.
///   @param addr the new location of end of data segment
///   @returns the new location of end of data segment, or -ERRNO on error
lib fn brk(addr: void*) -> i64;

/// function sbrk
///   Increase location of end of data segment.
///   It is recommended to use 'malloc' for memory allocation.
///   @param size number of bytes to increase the location of end of data segment
///   @returns the old location of end of data segment, or nullptr on error
lib fn sbrk(size: i64) -> void*;

/// function memcpy
///   Copies memory from one buffer to another.
///   @param[out] dst the buffer in which to copy the memory
///   @param[in] src the buffer from which to copy the memory
///   @param size the number of bytes to copy
///   @returns 'dst'
lib fn memcpy(dst: void*, src: void*, size: i64) -> void*;

/// function memset
///   Sets all bytes of a buffer to some value.
///   @param[out] dst the buffer in which to set the memory
///   @param byte the byte to fill the buffer with
///   @param size the number of bytes to set
///   @returns 'dst'
lib fn memset(dst: void*, byte: i8, size: i64) -> void*;

/// function memcmp
///   Compares two memory buffers.
///   @param[in] mem1 the first buffer
///   @param[in] mem2 the second buffer
///   @param size the number of bytes to compare
///   @returns the difference between the first non matching bytes, or 0 if every bytes match.
lib fn memcmp(mem1: void*, mem2: void*, size: i64) -> i64;

/// function malloc
///   Allocates a new block of memory.
///   @param size the size of the block to allocate
///   @returns the address of the block
lib fn malloc(size: i64) -> void*;

/// function free
///   Indicates that a block of memory is free to be reused by the memory allocator
///   @param ptr the address of the block
lib fn free(ptr: void*);

/// function realloc
///   Changes the size of a memory block
///   @param ptr the address of the block to resize
///   @param size the new size of the block
///   @returns the address of the new resized block @warning this address can differ from 'ptr'
lib fn realloc(ptr: void*, size: i64) -> void*;

/// function calloc
///   Allocates and initializes a memory block to store some elements.
///   @param nelem the number of elements
///   @param elsize the size of an element
///   @returns the address of the block @note the memory will be zero-initialized
lib fn calloc(nelem: i64, elsize: i64) -> void*;
