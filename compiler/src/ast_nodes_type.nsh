#include str.nsh
#include darray.nsh
#include token.nsh

lib fn init_type_data();
lib fn cleanup_type_data();

enum TypeKind : i64 {
  TYPEKIND_NONE,
  TYPEKIND_BUILTIN,
  TYPEKIND_ENUM,
  TYPEKIND_STRUCT,
  TYPEKIND_POINTER,
  TYPEKIND_ARRAY,
  TYPEKIND_FUNCTION,
};

enum BuiltinTypeKind : i64 {
  BTK_I8,
  BTK_I16,
  BTK_I32,
  BTK_I64,
  BTK_U8,
  BTK_U16,
  BTK_U32,
  BTK_U64,
  BTK_BOOL,
  BTK_COUNT,
};

struct PointerType;
struct FnDecl;

struct Type {
  kind: TypeKind;
};

lib fn Type::get_size(self: Type*) -> i64;
lib fn Type::get_align(self: Type*) -> i64;
lib fn Type::is_scalar_type(self: Type*) -> bool;
lib fn Type::is_arithmetic_type(self: Type*) -> bool;
lib fn Type::is_integer_type(self: Type*) -> bool;
lib fn Type::is_integral_or_enumeration_type(self: Type*) -> bool;
lib fn Type::get_unqualified(self: Type*) -> Type*;
lib fn Type::str_append(self: Type*, append_to: String*);
lib fn Type::is_void(self: Type*) -> bool;


struct EnumType {
  super base: Type;
  name: CStr;
  aliased_type: Type*;
};

lib fn EnumType::get_aliased_type(self: EnumType*) -> Type*;

struct StructType {
  super base: Type;
  name: CStr;
  fields: DArray;
  methods: DArray;
  first_field_is_super: bool;
  is_incomplete: bool;
};

lib fn StructType::field_type(self: StructType*, name: CStr) -> Type*;
lib fn StructType::super_type(self: StructType*) -> Type*;
lib fn StructType::deepest_super_field(self: StructType*, name: CStr*) -> Type*;
lib fn StructType::add_method(self: StructType*, name: CStr, decl: FnDecl*);

struct BuiltinType {
  super base: Type;
  builtin_kind: BuiltinTypeKind;
};

lib fn BuiltinType::get_from_tok(token: Token*) -> BuiltinType*;
lib fn BuiltinType::max_value(self: BuiltinType*) -> u64;
lib fn BuiltinType::get_bit_width(self: BuiltinType*) -> i64;
lib fn BuiltinType::is_signed(self: BuiltinType*) -> bool;

struct PointerType {
  super base: Type;
  subtype: Type*;
};

struct ArrayType {
  super base: Type;
  subtype: Type*;
  count: i64;
};

struct FunctionType {
  super base: Type;
  return_type: Type*;
  param_types: Type**;
  param_count: i64;
  is_variadic: bool;
};

lib fn Type::get_void() -> Type*;
lib fn Type::get_builtin(kind: BuiltinTypeKind) -> BuiltinType*;
lib fn Type::get_enum(name: CStr, subtype: Type*) -> EnumType*;
lib fn Type::get_struct(name: CStr) -> StructType*;
lib fn Type::get_pointer(subtype: Type*) -> PointerType*;
lib fn Type::get_array(subtype: Type*, size: i64) -> ArrayType*;
lib fn Type::get_function(return_type: Type*, param_types: Type**, param_count: i64, is_variadic: bool) -> FunctionType*;
// TODO: should param_types's ownership be transferred ?
