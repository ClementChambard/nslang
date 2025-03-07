"fn" @keyword
"let" @keyword
"lib" @keyword
"enum" @keyword
"type" @keyword
"struct" @keyword
"if" @keyword
"else" @keyword
"while" @keyword
"do" @keyword
"break" @keyword
"continue" @keyword
"return" @keyword
"sizeof" @keyword
"true" @literal
"false" @literal
"nullptr" @literal
"i8" @type
"i16" @type
"i32" @type
"i64" @type
"u8" @type
"u16" @type
"u32" @type
"u64" @type
"void" @type
"bool" @type
"__builtin_syscall" @builtin
(fn_decl name: (ident) @function)
(param_decl name: (ident) @variable.parameter)
(var_decl name: (ident) @variable)
(include_pp file: (file_name) @string)
(type name: (ident) @type)
(num) @number
(str) @string
