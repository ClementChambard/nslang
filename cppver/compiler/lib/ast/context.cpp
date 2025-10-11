
#include "context.hpp"
#include "ast/nodes/type.hpp"
#include <cassert>

u32 ASTContext::get_type_size(Type *t) {
  // TODO: actual values that depend on arch
  if (t->is_specific_builtin_type(BuiltinType::I8))
    return 1;
  if (t->is_specific_builtin_type(BuiltinType::U8))
    return 1;
  if (t->is_specific_builtin_type(BuiltinType::BOOL))
    return 1;
  if (t->is_specific_builtin_type(BuiltinType::VOID))
    return 1;
  if (t->is_specific_builtin_type(BuiltinType::I16))
    return 2;
  if (t->is_specific_builtin_type(BuiltinType::U16))
    return 2;
  if (t->is_specific_builtin_type(BuiltinType::I32))
    return 4;
  if (t->is_specific_builtin_type(BuiltinType::U32))
    return 4;
  if (t->is_specific_builtin_type(BuiltinType::I64))
    return 8;
  if (t->is_specific_builtin_type(BuiltinType::U64))
    return 8;
  if (t->is_specific_builtin_type(BuiltinType::NULLPTR))
    return 8;
  if (t->is_pointer_type())
    return 8;
  if (auto *at = t->dyn_cast<ArrayType>()) {
    return get_type_size(at->element_type) * at->element_count;
  }
  if (auto *et = t->dyn_cast<EnumType>()) {
    if (auto *ty = et->decl->dyn_cast<EnumDecl>()->int_ty)
      return get_type_size(ty);
    return 4;
  }
  if (auto *at = t->dyn_cast<AliasType>()) {
    return get_type_size(at->decl->dyn_cast<AliasDecl>()->underlying_type);
  }
  if (!t->isa<StructType>())
    return 1;
  auto *d = t->dyn_cast<StructType>()->decl->dyn_cast<StructDecl>();
  i64 s = 0;
  for (auto &f : d->fields) {
    auto a = get_type_align(f->type);
    if (s % a != 0) {
      s += (a - (s % a));
    }
    s += get_type_size(f->type);
  }
  return s;
}

u32 ASTContext::get_type_align(Type *t) {
  t = desugar_type(t);
  if (auto *a = t->dyn_cast<ArrayType>()) return get_type_align(a->element_type);
  if (!t->isa<StructType>())
    return get_type_size(t);
  auto *d = t->dyn_cast<StructType>()->decl->dyn_cast<StructDecl>();
  i64 align = 1;
  for (auto &f : d->fields) {
    auto a = get_type_align(f->type);
    if (a > align)
      align = a;
  }
  return align;
}

static void prepare_builtin_type(ASTContext *ctx, Type *&where,
                                 BuiltinType::Kind kind) {
  Type *t = new BuiltinType(kind);
  where = t;
  ctx->types.push_back(t);
}

ASTContext::ASTContext() {
  prepare_builtin_type(this, i8_ty, BuiltinType::I8);
  prepare_builtin_type(this, i16_ty, BuiltinType::I16);
  prepare_builtin_type(this, i32_ty, BuiltinType::I32);
  prepare_builtin_type(this, i64_ty, BuiltinType::I64);
  prepare_builtin_type(this, u8_ty, BuiltinType::U8);
  prepare_builtin_type(this, u16_ty, BuiltinType::U16);
  prepare_builtin_type(this, u32_ty, BuiltinType::U32);
  prepare_builtin_type(this, u64_ty, BuiltinType::U64);
  prepare_builtin_type(this, bool_ty, BuiltinType::BOOL);
  prepare_builtin_type(this, void_ty, BuiltinType::VOID);
  prepare_builtin_type(this, nullptr_ty, BuiltinType::NULLPTR);
}

ASTContext::~ASTContext() {
  for (auto *t : types)
    delete t;
}

PointerType *ASTContext::get_pointer_type(Type *base_type) {
  for (auto ty : pointer_types) {
    if (ty->pointee_type != base_type)
      continue;
    return ty;
  }
  auto ty = new PointerType(base_type);
  pointer_types.push_back(ty);
  types.push_back(ty);
  return ty;
}

ArrayType *ASTContext::get_array_type(Type *base_type, u64 size) {
  for (auto ty : array_types) {
    if (ty->element_type != base_type)
      continue;
    if (ty->element_count != size)
      continue;
    return ty;
  }
  auto ty = new ArrayType(base_type, size);
  array_types.push_back(ty);
  types.push_back(ty);
  return ty;
}

FunctionType *ASTContext::get_function_type(Type *result_type,
                                            std::span<Type *> args,
                                            bool is_variadic) {
  for (auto ty : function_types) {
    if (ty->result_type != result_type)
      continue;
    if (ty->function.is_variadic != is_variadic)
      continue;
    if (ty->param_types.size() != args.size())
      continue;
    bool wrong_arg = false;
    for (u32 i = 0; i < args.size(); i++) {
      if (ty->param_types[i] != args[i]) {
        wrong_arg = true;
        break;
      }
    }
    if (wrong_arg)
      continue;
    return ty;
  }
  auto ty = new FunctionType(result_type, args, is_variadic);
  function_types.push_back(ty);
  types.push_back(ty);
  return ty;
}

Type *ASTContext::get_declared_type(Decl *declaration) {
  if (auto it = declared_types.find(declaration); it != declared_types.end()) {
    assert(it->first == declaration);
    return it->second;
  } else {
    Type *ty;
    if (auto *alias = declaration->dyn_cast<AliasDecl>()) {
      ty = new AliasType(alias);
      alias->type_for_decl = ty;
    } else if (auto *enumd = declaration->dyn_cast<EnumDecl>()) {
      ty = new EnumType(enumd);
      enumd->type_for_decl = ty;
    } else if (auto *structd = declaration->dyn_cast<StructDecl>()) {
      ty = new StructType(structd);
      structd->type_for_decl = ty;
    } else {
      assert(false && "wrong kind of declaration");
    }
    types.push_back(ty);
    declared_types[declaration] = ty;
    return ty;
  }
}

void ASTContext::set_declared_type(Type *ty, Decl *decl) {
  declared_types[decl] = ty;
  if (auto *td = decl->dyn_cast<TypeDecl>()) {
    td->type_for_decl = ty;
  }
}

Type *ASTContext::get_builtin_type_from_tok(Tok tok) {
  switch (tok) {
  case tok::KW_I8:
    return i8_ty;
  case tok::KW_I16:
    return i16_ty;
  case tok::KW_I32:
    return i32_ty;
  case tok::KW_I64:
    return i64_ty;
  case tok::KW_U8:
    return u8_ty;
  case tok::KW_U16:
    return u16_ty;
  case tok::KW_U32:
    return u32_ty;
  case tok::KW_U64:
    return u64_ty;
  case tok::KW_VOID:
    return void_ty;
  case tok::KW_BOOL:
    return bool_ty;
  default:
    return nullptr;
  }
}

u32 ASTContext::get_integer_rank(const Type *t) {
  if (auto ad = t->get_as_alias_decl()) {
    return get_integer_rank(ad->underlying_type);
  }
  switch (t->dyn_cast<BuiltinType>()->get_kind()) {
  case BuiltinType::BOOL:
    return 1 + get_type_size(bool_ty);
  case BuiltinType::U8:
  case BuiltinType::I8:
    return 2 + get_type_size(i8_ty);
  case BuiltinType::U16:
  case BuiltinType::I16:
    return 3 + get_type_size(i16_ty);
  case BuiltinType::U32:
  case BuiltinType::I32:
    return 4 + get_type_size(i32_ty);
  case BuiltinType::U64:
  case BuiltinType::I64:
    return 5 + get_type_size(i64_ty);
  default:
    assert(false && "not an integer");
  }
}

i32 ASTContext::get_integer_type_order(Type *lhs, Type *rhs) {
  if (const auto *ed = lhs->get_as_enum_decl())
    lhs = ed->int_ty ? ed->int_ty : i32_ty;
  if (const auto *ed = rhs->get_as_enum_decl())
    rhs = ed->int_ty ? ed->int_ty : i32_ty;
  if (lhs == rhs)
    return 0;
  bool lhs_unsigned = lhs->is_unsigned_integer_type();
  bool rhs_unsigned = rhs->is_unsigned_integer_type();
  u32 lhs_rank = get_integer_rank(lhs);
  u32 rhs_rank = get_integer_rank(rhs);
  if (lhs_unsigned == rhs_unsigned) {
    if (lhs_rank == rhs_rank)
      return 0;
    return lhs_rank > rhs_rank ? 1 : -1;
  }
  if (lhs_unsigned) {
    if (lhs_rank >= rhs_rank)
      return 1;
    return -1;
  }
  if (rhs_rank >= lhs_rank)
    return -1;
  return 1;
}

Type *ASTContext::get_corresponding_unsigned_type(Type *t) const {
  if (auto ad = t->get_as_alias_decl()) {
    return get_corresponding_unsigned_type(ad->underlying_type);
  }
  if (auto const *ed = t->get_as_enum_decl())
    t = ed->int_ty ? ed->int_ty : i32_ty;
  switch (t->dyn_cast<BuiltinType>()->get_kind()) {
  case BuiltinType::I8:
    return u8_ty;
  case BuiltinType::I16:
    return u16_ty;
  case BuiltinType::I32:
    return u32_ty;
  case BuiltinType::I64:
    return u64_ty;
  default:
    return t;
  }
}
