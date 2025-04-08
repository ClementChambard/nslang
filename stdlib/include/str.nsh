//////////////////////////////////////////
/// String Standard Library for NSLang ///
//////////////////////////////////////////

/// type char
///   Character type.
type char = i8;

/// function isspace
///   Determines if a character is whitespace
///   @param c the character
///   @returns true if the character is whitespace
lib fn isspace(c: char) -> bool;


/// type CStr
///   C-String type. Must be null terminated.
type CStr = char*;


/// function strcpm
///   Compares two C-Strings.
///   @param[in] str_1 the first string
///   @param[in] str_2 the second string
///   @returns the difference between the first two non-matching characters
lib fn strcmp(str_1: CStr, str_2: CStr) -> i64;

/// function strlen
///   Calculates the length of a C-String.
///   @param[in] str the string to evaluate
///   @returns the length of the string in bytes (not counting the terminating '\0')
lib fn strlen(str: CStr) -> i64;

/// function strdup
///   Duplicates a C-String.
///   @param[in] str the string to duplicate
///   @return: a new memory region containing a duplicate of the string
lib fn strdup(str: CStr) -> CStr;

/// function strcat
///   Concatenates two C-Strings.
///   @param[in] str_1 the first string
///   @param[in] str_2 the second string
///   @return: a new memory region containing str_1 ++ str_2
lib fn strcat(str_1: CStr, str_2: CStr) -> CStr;

//////////////////////////////////////////////////////////////////////


/// type Str
///   String reference type.
struct Str {
    data: char*;   //< contents of the string. May not be null terminated.
    len: i64;      //< length of the string in bytes.
};


/// method str_strip
///   Removes whitespace at the start and end of the string.
///   @object self
lib fn str_strip(self: Str*);

/// constructor str_from_cstr
///   Creates a string reference from a C-String.
///   @object self
///   @param[in] cstr the string to reference
lib fn str_from_cstr(self: Str*, cstr: CStr);

//////////////////////////////////////////////////////////////////////


/// type String
///   Owned string type.
struct String {
    inner: Str;     //< contents of the string
    capacity: i64;  //< capacity of the allocated inner memory
};


/// constructor string_init
///   Initializes a String object.
///   @object self
lib fn string_init(self: String*);

/// constructor string_from_str
///   Inititializes a String object with the contents of a string reference
///   @object self
///   @param[in] cstr the string reference to use
lib fn string_from_str(self: String*, str: Str*);

/// constructor string_from_cstr
///   Inititializes a String object with the contents of a C-String
///   @object self
///   @param[in] cstr the C-String to use
lib fn string_from_cstr(self: String*, cstr: CStr);

/// constructor string_from_raw_parts
///   Inititializes a String object with raw data
///   @object self
///   @param[in] data the string data
///   @param len the string length
///   @param capacity the string capacity
lib fn string_from_raw_parts(self: String*, data: char*, len: i64, capacity: i64);

/// destructor string_destroy
///   Destroys a String object.
///   @object self
lib fn string_destroy(self: String*);

/// method string_reserve
///   Reserves some capacity in a string.
///   @object self
///   @param count the number of bytes the string will be able to contain
lib fn string_reserve(self: String*, count: i64);

/// method string_shrink_to_fit
///   Reduces the capacity of the string to fit the length
///   @object self
lib fn string_shrink_to_fit(self: String*);

/// method string_truncate
///   Shortens the string to the specifier length (no effect if 'length' > string.len)
///   @object self
///   @param length the new length
lib fn string_truncate(self: String*, count: i64);

/// method string_pop
///   Pops the last character of the string
///   @object self
///   @returns the popped character
lib fn string_pop(self: String*) -> char;

/// method string_remove
///   Remove a character from the string
///   @object self
///   @param idx the index of the character to remove
///   @returns the removed character
lib fn string_remove(self: String*, idx: i64) -> char;

/// method string_push
///   Adds a character at the end of a string.
///   @object self
///   @param c the character to push
lib fn string_push(self: String*, c: char);

/// method string_push_str
///   Appends a string at the end of a string.
///   @object self
///   @param s the string to append
lib fn string_push_str(self: String*, s: Str*);

/// method string_push_cstr
///   Appends a C-String at the end of a string.
///   @object self
///   @param s the C-String to append
lib fn string_push_cstr(self: String*, s: CStr);

/// method string_push_format
///   Appends formatted content at the end of a string.
///   @object self
///   @param fmt the format string
///   @vararg the format parameters
lib fn string_push_format(self: String*, fmt: CStr, ...);

/// constructor string_push_format
///   Creates a string from formatted data.
///   @object self
///   @param fmt the format string
///   @vararg the format parameters
lib fn string_format(self: String*, fmt: CStr, ...);

/// method string_insert
///   Inserts a character in the string
///   @object self
///   @param idx the location to insert
///   @param c the character to insert
lib fn string_insert(self: String*, idx: i64, c: char);

/// method string_insert_str
///   Inserts a string in the string
///   @object self
///   @param idx the location to insert
///   @param str the string to insert
lib fn string_insert_str(self: String*, idx: i64, str: Str*);

/// method string_insert_cstr
///   Inserts a C-String in the string
///   @object self
///   @param idx the location to insert
///   @param cstr the C-String to insert
lib fn string_insert_cstr(self: String*, idx: i64, str: CStr);

/// method string_split_off
///   Splits a string into two at an offset
///   @object self
///   @param idx the index at which to split the string
///   @param[out] out the output string
lib fn string_split_off(self: String*, idx: i64, out: String*);

/// method string_clear
///   Clears the contents of a string. Does not change the capacity.
///   @object self
lib fn string_clear(self: String*);

/// constructor string_clone_from
///   Copies a string into another
///   @object self
///   @param[in] clone_from the string to copy
lib fn string_clone_from(self: String*, clone_from: String*);
