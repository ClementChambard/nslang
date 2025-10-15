#include "type.hpp"
#include "decl.hpp"
#include <algorithm>

EnumType::EnumType(EnumDecl *decl) : DeclaredType(ENUM_TYPE, decl) {}
EnumDecl *EnumType::get_decl() { return static_cast<EnumDecl *>(decl); }

StructType::StructType(StructDecl *decl) : DeclaredType(STRUCT_TYPE, decl) {}
StructDecl *StructType::get_decl() { return static_cast<StructDecl *>(decl); }

bool is_enum_decl_complete(EnumDecl const *ed) { return ed->is_complete; }

bool is_enum_decl_scoped(EnumDecl const *ed) { return ed->is_scoped; }

bool Type::is_signed_integer_type() const {
  if (auto *bt = dyn_cast<BuiltinType>()) {
    return bt->is_signed_integer();
  }
  if (auto *et = dyn_cast<EnumType>()) {
    auto *d = et->get_decl();
    return !d->is_scoped && d->is_complete &&
           (!d->int_ty || d->int_ty->is_signed_integer_type());
  }
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->is_signed_integer_type();
  return false;
}

bool Type::is_signed_integer_or_enumeration_type() const {
  if (auto *bt = dyn_cast<BuiltinType>()) {
    return bt->is_signed_integer();
  }
  if (auto *et = dyn_cast<EnumType>()) {
    auto *d = et->get_decl();
    return d->is_complete &&
           (!d->int_ty || d->int_ty->is_signed_integer_type());
  }
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->is_signed_integer_or_enumeration_type();
  return false;
}

bool Type::is_unsigned_integer_type() const {
  if (auto *bt = dyn_cast<BuiltinType>()) {
    return bt->is_unsigned_integer();
  }
  if (auto *et = dyn_cast<EnumType>()) {
    auto *d = et->get_decl();
    return !d->is_scoped && d->is_complete && d->int_ty &&
           d->int_ty->is_unsigned_integer_type();
  }
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->is_unsigned_integer_type();
  return false;
}

bool Type::is_unsigned_integer_or_enumeration_type() const {
  if (auto *bt = dyn_cast<BuiltinType>()) {
    return bt->is_unsigned_integer();
  }
  if (auto *et = dyn_cast<EnumType>()) {
    auto *d = et->get_decl();
    return d->is_complete && d->int_ty && d->int_ty->is_unsigned_integer_type();
  }
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->is_unsigned_integer_or_enumeration_type();
  return false;
}

Type *Type::get_pointee_type() const {
  if (auto *pt = dyn_cast<PointerType>())
    return pt->pointee_type;
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->get_pointee_type();
  return nullptr;
}

AliasDecl *Type::get_as_alias_decl() const {
  if (auto *at = dyn_cast<AliasType>()) {
    return at->decl->dyn_cast<AliasDecl>();
  }
  return nullptr;
}

EnumDecl *Type::get_as_enum_decl() const {
  if (auto *et = dyn_cast<EnumType>())
    return et->decl->dyn_cast<EnumDecl>();
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->get_as_enum_decl();
  return nullptr;
}

StructDecl *Type::get_as_struct_decl() const {
  if (auto *et = dyn_cast<StructType>())
    return et->decl->dyn_cast<StructDecl>();
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->get_as_struct_decl();
  return nullptr;
}

const Type *Type::get_base_element_type() const {
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->get_base_element_type();
  const Type *type = this;
  while (auto *at = type->dyn_cast<ArrayType>()) {
    type = at->element_type;
  }
  return type;
}

bool Type::is_unscoped_enumeration_type() const {
  if (auto *et = dyn_cast<EnumType>()) {
    return !is_enum_decl_scoped(et->get_decl());
  }
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->is_unscoped_enumeration_type();
  return false;
}

bool Type::is_arithmetic_type() const {
  if (auto *bt = dyn_cast<BuiltinType>()) {
    return bt->get_kind() >= BuiltinType::I8 &&
           bt->get_kind() <= BuiltinType::BOOL;
  }
  if (auto *et = dyn_cast<EnumType>()) {
    return !is_enum_decl_scoped(et->get_decl()) &&
           is_enum_decl_complete(et->get_decl());
  }
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->is_arithmetic_type();
  return false;
}

bool Type::is_literal_type() const {
  if (is_void_type())
    return true;
  const Type *base_ty = get_base_element_type();
  if (base_ty->is_incomplete_type())
    return false;
  if (base_ty->is_scalar_type())
    return true;
  if (base_ty->is_structure_type())
    return true;
  return false;
}

bool Type::is_integral_type() const {
  if (auto *bt = dyn_cast<BuiltinType>()) {
    return bt->is_integer();
  }
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->is_integral_type();
  return false;
}

bool Type::is_integral_or_unscoped_enumeration_type() const {
  return is_integral_type() || is_unscoped_enumeration_type();
}

bool Type::is_scoped_enumeral_type() const {
  if (auto *et = dyn_cast<EnumType>()) {
    return is_enum_decl_scoped(et->get_decl());
  }
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->is_scoped_enumeral_type();
  return false;
}

bool Type::is_void_pointer_type() const {
  if (auto *pt = dyn_cast<PointerType>()) {
    return pt->get_pointee_type()->is_void_type();
  }
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->is_void_pointer_type();
  return false;
}

bool Type::is_valist_pointer_type() const {
  if (auto *pt = dyn_cast<PointerType>()) {
    return pt->get_pointee_type()->is_specific_builtin_type(BuiltinType::VALIST);
  }
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->is_valist_pointer_type();
  return false;
}

bool Type::is_incomplete_type(NamedDecl **def) const {
  if (def)
    *def = nullptr;
  switch (kind) {
  case BUILTIN_TYPE:
    return is_void_type();
  case ENUM_TYPE: {
    auto *et = dyn_cast<EnumType>();
    auto *enumd = const_cast<EnumDecl *>(et->get_decl());
    if (def)
      *def = static_cast<NamedDecl *>(enumd);
    return !enumd->is_complete;
  }
  case STRUCT_TYPE: {
    auto *st = dyn_cast<StructType>();
    auto *structd = const_cast<StructDecl *>(st->get_decl());
    if (def)
      *def = static_cast<NamedDecl *>(structd);
    return !structd->is_complete;
  }
  case ARRAY_TYPE:
    return dyn_cast<ArrayType>()->element_type->is_incomplete_type(def);
  case ALIAS_TYPE:
    return get_as_alias_decl()->underlying_type->is_void_pointer_type();
  default:
    return false;
  }
}

bool Type::is_specific_builtin_type(unsigned k) const {
  if (auto *bt = dyn_cast<BuiltinType>()) {
    return bt->get_kind() == BuiltinType::Kind(k);
  }
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->is_specific_builtin_type(k);
  return false;
}

bool is_enum_decl_complete(EnumDecl const *);
bool is_enum_decl_scoped(EnumDecl const *);

bool Type::is_integer_type() const {
  if (auto *bt = dyn_cast<BuiltinType>()) {
    return bt->is_integer();
  }
  if (auto *et = dyn_cast<EnumType>()) {
    return is_enum_decl_complete(et->get_decl()) &&
           !is_enum_decl_scoped(et->get_decl());
  }
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->is_integer_type();
  return false;
}

bool Type::is_integral_or_enumeration_type() const {
  if (auto *bt = dyn_cast<BuiltinType>()) {
    return bt->is_integer();
  }
  if (auto *et = dyn_cast<EnumType>()) {
    return is_enum_decl_complete(et->get_decl());
  }
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->is_integral_or_enumeration_type();
  return false;
}

bool Type::is_scalar_type() const {
  if (auto *bt = dyn_cast<BuiltinType>()) {
    return bt->is_integer() || bt->get_kind() == BuiltinType::NULLPTR;
  }
  if (auto *et = dyn_cast<EnumType>()) {
    return is_enum_decl_complete(et->get_decl());
  }
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->is_scalar_type();
  return isa<PointerType>();
}

bool Type::is_object_pointer_type() const {
  if (auto *t = dyn_cast<PointerType>())
    return !t->get_pointee_type()->is_function_type();
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->is_object_pointer_type();
  return false;
}

bool Type::is_function_pointer_type() const {
  if (auto *t = dyn_cast<PointerType>())
    return t->get_pointee_type()->is_function_type();
  if (auto *ad = get_as_alias_decl())
    return ad->underlying_type->is_function_pointer_type();
  return false;
}

cstr BuiltinType::get_name() const {
  switch (builtin.kind) {
  case I8:
    return "i8";
  case I16:
    return "i16";
  case I32:
    return "i32";
  case I64:
    return "i64";
  case U8:
    return "u8";
  case U16:
    return "u16";
  case U32:
    return "u32";
  case U64:
    return "u64";
  case BOOL:
    return "bool";
  case NULLPTR:
    return "nullptr_t";
  case VOID:
    return "void";
  case VALIST:
    return "valist_t";
  }
  return "";
}

// FUNCTION_TYPE,

void Type::dump() const {
  if (auto *bt = dyn_cast<BuiltinType>()) {
    printf("%s", bt->get_name());
  } else if (auto *pt = dyn_cast<PointerType>()) {
    pt->pointee_type->dump();
    if (!pt->pointee_type->is_pointer_type())
      printf(" ");
    printf("*");
  } else if (auto *pt = dyn_cast<ArrayType>()) {
    pt->element_type->dump();
    printf("[%lld]", pt->element_count);
  } else if (auto *at = dyn_cast<AliasType>()) {
    printf("type %s", at->decl->get_name().c_str());
  } else if (auto *at = dyn_cast<EnumType>()) {
    printf("enum %s", at->decl->get_name().c_str());
  } else if (auto *at = dyn_cast<StructType>()) {
    printf("struct %s", at->decl->get_name().c_str());
  } else if (auto *ft = dyn_cast<FunctionType>()) {
    ft->result_type->dump();
    printf("(");
    u32 i = 0;
    for (i = 0; i < ft->param_types.size(); i++) {
      if (i != 0) {
        printf(", ");
      }
      ft->param_types[i]->dump();
    }
    if (ft->function.variadic) {
      if (i)
        printf(", ");
      printf("...");
      if (ft->function.variadic == FunctionTypeBits::VALIST_IN_ARGS)
        printf("*");
    }
    printf(")");
  }
}
