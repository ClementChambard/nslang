
enum : i64 {
    STDIN = 0,
    STDOUT = 1,
    STDERR = 2,
};

lib fn read(fd: i64, buf: void*, count: i64) -> i64;

lib fn write(fd: i64, buf: void*, count: i64) -> i64;

lib fn println();

lib fn print(str: i8*);

lib fn print_num(num: i64);

lib fn print_hex(num: i64);
