#ifndef AST_NODES_TYPE_HPP_INCLUDED
#define AST_NODES_TYPE_HPP_INCLUDED

#include "defines.hpp"
#include "lexer/tok.hpp"
#include <span>
#include <vector>

struct NamedDecl;
struct TypeDecl;
struct EnumDecl;
struct StructDecl;
struct ASTContext;

struct StructType;
struct EnumType;
struct BuiltinType;
struct AliasType;
struct FunctionType;
struct PointerType;
struct ArrayType;

struct Type {
  enum Kind {
    BUILTIN_TYPE,
    ALIAS_TYPE,
    ENUM_TYPE,
    STRUCT_TYPE,
    POINTER_TYPE,
    ARRAY_TYPE,
    FUNCTION_TYPE,
  };

  struct BuiltinTypeBits {
    u32 _ : 8;
    u32 kind : 8;
  };

  struct FunctionTypeBits {
    enum ValistKind {
      NONE,
      VARIADIC,
      VALIST_IN_ARGS,
    };
    u32 _ : 8;
    u32 num_params : 8;
    ValistKind variadic : 2;
  };

  union {
    u32 kind : 8;
    BuiltinTypeBits builtin;
    FunctionTypeBits function;
  };

  Type(Kind k) : kind(k) {}
  virtual ~Type() = default;

  bool is_incomplete_type(NamedDecl **def = nullptr) const;

  bool is_incomplete_or_object_type() const { return !is_function_type(); }
  bool is_object_type() const { return !is_function_type() && !is_void_type(); }
  bool is_builtin_type() const { return isa<BuiltinType>(); }
  bool is_enumeral_type() const { return isa<EnumType>(); }
  bool is_structure_type() const { return isa<StructType>(); }
  bool is_function_type() const { return isa<FunctionType>(); }
  bool is_pointer_type() const { return isa<PointerType>(); }
  bool is_array_type() const { return isa<ArrayType>(); }
  bool is_alias_type() const { return isa<AliasType>(); }
  bool is_integral_or_enumeration_type() const;
  bool is_nullptr_type() const;
  bool is_integer_type() const;
  bool is_boolean_type() const;
  bool is_specific_builtin_type(unsigned k) const;
  bool is_scalar_type() const;
  bool is_void_type() const;
  bool is_object_pointer_type() const;
  bool is_fundamental_type() const;
  bool is_compound_type() const;
  bool is_function_pointer_type() const;
  bool is_scoped_enumeral_type() const;
  bool is_integral_or_unscoped_enumeration_type() const;
  bool is_integral_type() const;
  bool is_unscoped_enumeration_type() const;
  bool is_void_pointer_type() const;
  bool is_valist_pointer_type() const;
  bool is_arithmetic_type() const;
  bool is_signed_integer_type() const;
  bool is_signed_integer_or_enumeration_type() const;
  bool is_unsigned_integer_type() const;
  bool is_unsigned_integer_or_enumeration_type() const;
  bool is_literal_type() const;
  bool is_real_floating_type() const;
  bool is_floating_type() const;
  bool is_float32_type() const;
  bool is_float64_type() const;
  bool is_real_type() const;
  Type *get_pointee_type() const;
  struct EnumDecl *get_as_enum_decl() const;
  struct StructDecl *get_as_struct_decl() const;
  struct AliasDecl *get_as_alias_decl() const;
  const Type *get_base_element_type() const;
  // ScalarTypeKind getScalarTypeKind() const;
  // bool canDecayToPointerType() const;
  // const RecordType *getAsStructureType() const;
  // RecordDecl *getAsRecordDecl() const;
  template <typename T> const T *dyn_cast() const {
    return const_cast<Type *>(this)->dyn_cast<T>();
  }
  template <typename T> T *dyn_cast() {
    if (isa<T>())
      return reinterpret_cast<T *>(this);
    return nullptr;
  }
  template <typename T> bool isa() const { return T::is_class(Kind(kind)); }

  void dump() const;

  static bool is_class(Kind) { return true; }
};

struct BuiltinType : public Type {
  enum Kind {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    BOOL,
    F32,
    F64,
    NULLPTR,
    VOID,
    VALIST,
  };

  BuiltinType(BuiltinType::Kind k) : Type(BUILTIN_TYPE) { builtin.kind = k; }

  BuiltinType::Kind get_kind() const { return Kind(builtin.kind); }

  cstr get_name() const;

  bool is_integer() const { return get_kind() >= I8 && get_kind() <= BOOL; }

  bool is_signed_integer() const {
    return get_kind() >= I8 && get_kind() <= I64;
  }

  bool is_unsigned_integer() const {
    return get_kind() >= U8 && get_kind() <= BOOL;
  }

  bool is_floating_point() const {
    return get_kind() >= F32 && get_kind() <= F64;
  }

  static bool is_class(Type::Kind k) { return k == BUILTIN_TYPE; }
};

struct AliasType : public Type {
  TypeDecl *decl = nullptr;

  AliasType(TypeDecl *decl) : Type(ALIAS_TYPE), decl(decl) {}

  static bool is_class(Type::Kind k) { return k == ALIAS_TYPE; }
};

struct DeclaredType : public Type {
  TypeDecl *decl;

  DeclaredType(Kind k, TypeDecl *decl) : Type(k), decl(decl) {}

  TypeDecl const *get_decl() const { return decl; }

  static bool is_class(Kind k) { return k == ENUM_TYPE || k == STRUCT_TYPE; }
};

struct EnumType : public DeclaredType {
  EnumType(EnumDecl *decl);

  EnumDecl const *get_decl() const {
    return const_cast<EnumType *>(this)->get_decl();
  }
  EnumDecl *get_decl();

  static bool is_class(Kind k) { return k == ENUM_TYPE; }
};

struct StructType : public DeclaredType {
  StructType(StructDecl *decl);

  StructDecl const *get_decl() const {
    return const_cast<StructType *>(this)->get_decl();
  }
  StructDecl *get_decl();

  static bool is_class(Kind k) { return k == STRUCT_TYPE; }
};

struct PointerType : public Type {
  Type *pointee_type;

  PointerType(Type *pointee_type)
      : Type(POINTER_TYPE), pointee_type(pointee_type) {}

  Type *get_pointee_type() const { return pointee_type; }

  static bool is_class(Kind k) { return k == POINTER_TYPE; }
};

struct ArrayType : public Type {
  Type *element_type;
  u64 element_count;

  ArrayType(Type *element_type, u64 element_count)
      : Type(ARRAY_TYPE), element_type(element_type),
        element_count(element_count) {}

  static bool is_class(Kind k) { return k == ARRAY_TYPE; }
};

struct FunctionType : public Type {
  Type *result_type;
  std::vector<Type *> param_types;

  FunctionType(Type *result_type, std::span<Type *> param_types,
               FunctionTypeBits::ValistKind variadic)
      : Type(FUNCTION_TYPE), result_type(result_type),
        param_types(param_types.begin(), param_types.end()) {
    function.num_params = param_types.size();
    function.variadic = variadic;
  }

  static bool is_class(Kind k) { return k == FUNCTION_TYPE; }
};

inline bool Type::is_void_type() const {
  return is_specific_builtin_type(BuiltinType::VOID);
}

inline bool Type::is_boolean_type() const {
  return is_specific_builtin_type(BuiltinType::BOOL);
}

inline bool Type::is_fundamental_type() const {
  return is_void_type() || is_nullptr_type() ||
         (is_arithmetic_type() && !is_enumeral_type());
}

inline bool Type::is_compound_type() const {
  return is_array_type() || is_function_type() || is_pointer_type() ||
         is_structure_type() || is_enumeral_type();
}

inline bool Type::is_nullptr_type() const {
  return is_specific_builtin_type(BuiltinType::NULLPTR);
}

inline bool Type::is_float32_type() const {
  return is_specific_builtin_type(BuiltinType::F32);
}

inline bool Type::is_float64_type() const {
  return is_specific_builtin_type(BuiltinType::F64);
}

#endif // AST_NODES_TYPE_HPP_INCLUDED
