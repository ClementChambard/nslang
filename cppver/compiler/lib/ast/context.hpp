#ifndef AST_CONTEXT_HPP_INCLUDED
#define AST_CONTEXT_HPP_INCLUDED

#include "lexer/tok.hpp"
#include "nodes/decl.hpp"
#include "nodes/type.hpp"
#include <map>

struct ASTContext {
  std::vector<Type *> types;
  Type *i8_ty;
  Type *i16_ty;
  Type *i32_ty;
  Type *i64_ty;
  Type *u8_ty;
  Type *u16_ty;
  Type *u32_ty;
  Type *u64_ty;
  Type *bool_ty;
  Type *f32_ty;
  Type *f64_ty;
  Type *void_ty;
  Type *nullptr_ty;
  Type *valist_ty;
  std::vector<PointerType *> pointer_types; // TODO: better datastructure ?
  std::vector<ArrayType *> array_types;
  std::vector<FunctionType *> function_types;
  std::map<Decl *, Type *> declared_types;

  ASTContext();
  ~ASTContext();

  Type *get_builtin_type_from_tok(Tok tok);
  PointerType *get_pointer_type(Type *base_type);
  ArrayType *get_array_type(Type *base_type, u64 size);
  FunctionType *get_function_type(Type *result_type, std::span<Type *> args,
                                  bool is_variadic);
  Type *get_declared_type(Decl *declaration);
  void set_declared_type(Type *ty, Decl *decl);

  u32 get_type_size(Type *ty);
  u32 get_type_align(Type *ty);

  bool is_promotable_integer_type(Type *t) const;
  Type *get_promoted_integer_type(Type *t);
  bool is_same_type(Type *ty1, Type *ty2);
  Type *get_common_type(Type *ty1, Type *ty2);
  Type *desugar_type(Type *ty1);

  i32 get_integer_type_order(Type *lhs, Type *rhs);
  u32 get_integer_rank(const Type *t);
  Type *get_corresponding_unsigned_type(Type *t) const;
};

#endif // AST_CONTEXT_HPP_INCLUDED
