//////////////////////////////////////
/// IO Standard Library for NSLang ///
//////////////////////////////////////

/// constants
///   Standard file descriptors.
enum : i64 {
    STDIN = 0,   //< FD for the standard input.
    STDOUT = 1,  //< FD for the standard output.
    STDERR = 2,  //< FD for the standard error output.
};


/// function read
///   Reads from a file.
///   @param fd the file descriptor
///   @param[out] buf the buffer in which to read (it must be allocated with size 'count')
///   @param count the number of bytes to read
///   @returns the number of bytes read, or -ERRNO on error
lib fn read(fd: i64, buf: void*, count: i64) -> i64;

/// function write
///   Writes to a file.
///   @param fd the file descriptor
///   @param[in] buf the buffer to write to the file
///   @param count the number of bytes to write
///   @returns the number of bytes written, or -ERRNO on error
lib fn write(fd: i64, buf: void*, count: i64) -> i64;

/// function fprint
///   Prints formatted data to a file.
///   @param fd the file descriptor
///   @param fmt the format string
///   @vararg the format parameters
lib fn fprint(fd: i64, fmt: i8*, ...);

/// function print
///   Prints formatted data to the standard output.
///   @param fmt the format string
///   @vararg the format parameters
lib fn print(fmt: i8*, ...);

// va functions
lib fn fprint_va(fd: i64, fmt: i8*, ...*);
lib fn print_va(fmt: i8*, ...*);



// command line arguments
struct Args {
  super data: i8**;
  count: i64;
};

lib fn Args::get(self: Args *) init;
