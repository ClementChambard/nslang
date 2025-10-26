[
  "fn"
  "let"
  "lib"
] @keyword

[
  "enum"
  "type"
  "struct"
] @keyword.type

[
  "if"
  "else"
  "switch"
  "case"
  "default"
] @keyword.conditional

[
  "while"
  "do" 
  "for"
  "break"
  "continue"
] @keyword.repeat

"return" @keyword.return

"sizeof" @keyword.operator

"true" @boolean
"false" @boolean
"nullptr" @constant.builtin
"vaarg" @constant.builtin
"vaargs" @constant.builtin

"#include" @keyword.import

[
  ";"
  ":"
  ","
  "."
  "::"
] @punctuation.delimiter

"..." @punctuation.special

[
  "("
  ")"
  "["
  "]"
  "{"
  "}"
] @punctuation.bracket

[
  "="
  "-"
  "*"
  "/"
  "+"
  "%"
  "~"
  "|"
  "&"
  "^"
  "<<"
  ">>"
  "->"
  "<"
  "<="
  ">="
  ">"
  "=="
  "!="
  "!"
  "&&"
  "||"
  "-="
  "+="
  "*="
  "/="
  "%="
  "|="
  "&="
  "^="
  ">>="
  "<<="
  "--"
  "++"
] @operator

(str) @string
(chr) @character

[
  "i8"
  "i16"
  "i32"
  "i64"
  "u8"
  "u16"
  "u32"
  "u64"
  "f32"
  "f64"
  "void"
  "bool" 
] @type.builtin

"cast" @function.builtin

"super" @keyword.modifier
"init" @keyword.modifier

(fn_decl name: (ident) @function)
(fn_decl struct_scope: (ident) @type)

(param_decl name: (ident) @variable.parameter)

(var_decl name: (ident) @variable)

(include_pp file: (file_name) @string)

(type name: (ident) @type)

(member_expr field: (ident) @variable.member)
(call_expr func: (ident) @function.call)
(call_expr func: (member_expr field: (ident) @function.method.call))
(method_ident name: (ident) @function.method.call)
(method_ident type: (ident) @type)

(enum_variant_decl name: (ident) @constant)
(field_decl name: (ident) @variable.member)

(enum_decl name: (ident) @type.definition)
(type_decl name: (ident) @type.definition)
(struct_decl name: (ident) @type.definition)

(num) @number

(comment) @comment @spell
