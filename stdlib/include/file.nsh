enum : i64 {
    O_ACCMODE   = 0x000003,
    O_RDONLY    = 0x000000,
    O_WRONLY    = 0x000001,
    O_RDWR      = 0x000002,
    O_CREAT     = 0x000040,
    O_EXCL      = 0x000080,
    O_NOCTTY    = 0x000100,
    O_TRUNC     = 0x000200,
    O_APPEND    = 0x000400,
    O_NONBLOCK  = 0x000800,
    O_NDELAY    = 0x000800,
    O_SYNC      = 0x101000,
    O_FSYNC     = 0x101000,
    O_DSYNC     = 0x001000,
    O_ASYNC     = 0x002000,
    O_DIRECT    = 0x004000,
    O_LARGEFILE = 0x008000,
    O_DIRECTORY = 0x010000,
    O_NOFOLLOW  = 0x020000,
    O_NOATIME   = 0x040000,
    O_CLOEXEC   = 0x080000,
    O_PATH      = 0x200000,
    O_TMPFILE   = 0x410000,
};


lib fn open(filename: i8*, flags: i64, mode: i64) -> i64;

lib fn close(fd: i64) -> i64;
