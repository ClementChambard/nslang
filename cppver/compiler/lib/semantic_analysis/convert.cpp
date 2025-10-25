#include "convert.hpp"
#include "ast/context.hpp"
#include "ast/nodes/expr.hpp"
#include "ast/nodes/stmt.hpp"
#include "ast/nodes/type.hpp"
#include "diags/diagnostic.hpp"
#include "sema.hpp"
#include <cassert>

ConversionSequence Sema::try_implicit_conversion(Expr *f, Type *to_type,
                                                 bool is_explicit) {
  ConversionSequence cs;
  if (is_standard_conversion(this, f, to_type, &cs, is_explicit)) {
    cs.state = ConversionSequence::OK;
    return cs;
  }
  // non standard conversions ...
  return cs;
}

ConversionSequence Sema::try_contextually_convert_to_bool(Expr *f) {
  // if f is of type nullptr_t,
  // ImplicitConversionSequence::getNullptrToBool(From->getType(),
  // S.Context.BoolTy, From->isGLValue());
  if (!f->type) {
    Diag(diag::BUG, f->get_start_loc(), "??? no type ?") << f->get_range();
    return ConversionSequence();
  }
  return try_implicit_conversion(f, ctx.bool_ty);
}

ExprUPtr Sema::perform_contextually_convert_to_bool(ExprUPtr f) {
  auto cs = try_contextually_convert_to_bool(f.get());
  if (cs.state != ConversionSequence::OK)
    return nullptr;
  return perform_implicit_conversion(std::move(f), ctx.bool_ty, &cs);
}

ConversionRank get_ck_rank(ConversionKind k) {
  const ConversionRank lut[] = {
      CR_EXACT_MATCH, CR_EXACT_MATCH, CR_EXACT_MATCH, CR_EXACT_MATCH,
      CR_EXACT_MATCH, CR_EXACT_MATCH, CR_PROMOTION,   CR_PROMOTION,
      CR_CONVERSION,  CR_CONVERSION,  CR_CONVERSION,  CR_CONVERSION,
      CR_CONVERSION,  CR_CONVERSION,  CR_CONVERSION,  CR_CONVERSION,
      CR_EXACT_MATCH, CR_EXACT_MATCH, CR_CONVERSION,
  };
  return lut[k];
}

ConversionRank ConversionSequence::get_rank() const {
  auto r = CR_EXACT_MATCH;
  if (auto r_ = get_ck_rank(first))
    r = r_;
  if (auto r_ = get_ck_rank(second))
    r = r_;
  return r;
}

bool is_allowed_explicit_cast(Sema *sema, Type *from_type, Type *to_type, ConversionKind &kind) {
  (void)sema;
  if (from_type->is_integral_or_enumeration_type() && to_type->is_integral_or_enumeration_type()) {
    kind = CK_INTEGRAL_CONVERSION;
    return true;
  }
  return false;
}


bool is_integral_promotion(Sema *sema, Type *from_type, Type *to_type);

bool is_floating_point_promotion(Type *from_type, Type *to_type) {
  BuiltinType *from_bt;
  BuiltinType *to_bt;
  if (!(from_bt = from_type->dyn_cast<BuiltinType>())) return false;
  if (!(to_bt = to_type->dyn_cast<BuiltinType>())) return false;
  return from_bt->get_kind() == BuiltinType::F32 && to_bt->get_kind() == BuiltinType::F64;
}

bool is_pointer_conversion(Sema *sema, Expr *f, Type *from_type, Type *to_type,
                           Type *&out_type, bool is_explicit);

bool is_standard_conversion(Sema *sema, Expr *from_e, Type *to_type,
                            ConversionSequence *cs, bool is_explicit) {
  auto from_type = sema->ctx.desugar_type(from_e->type);
  to_type = sema->ctx.desugar_type(to_type);
  cs->set_as_identity();
  cs->from_type = from_type;

  if (from_type->isa<StructType>() || to_type->isa<StructType>())
    return false;

  bool arg_is_lvalue = from_e->vk != ValueKind::PRVALUE;
  if (arg_is_lvalue &&
      !(from_type->isa<FunctionType>() || from_type->isa<ArrayType>())) {
    cs->first = CK_LVALUE_TO_RVALUE;
  } else if (from_type->isa<ArrayType>()) {
    cs->first = CK_ARRAY_TO_POINTER;
    from_type = sema->ctx.get_pointer_type(
        from_type->dyn_cast<ArrayType>()->element_type);
    // TODO: special case: string literal to char* -> second=identity, return
    // true
  } else if (from_type->isa<FunctionType>() && arg_is_lvalue) {
    cs->first = CK_FUNCTION_TO_POINTER;
    from_type = sema->ctx.get_pointer_type(from_type);
  } else {
    cs->first = CK_IDENTITY;
  }
  cs->to_types[0] = from_type;

  if (from_type == to_type) {
    cs->second = CK_IDENTITY;
  } else if (is_integral_promotion(sema, from_type, to_type)) {
    cs->second = CK_INTEGRAL_PROMOTION;
    from_type = to_type;
  } else if (is_floating_point_promotion(from_type, to_type)) {
    cs->second = CK_FLOATING_PROMOTION;
    from_type = to_type;
  } else if (to_type->is_boolean_type() && (from_type->is_arithmetic_type() ||
                                            from_type->is_pointer_type())) {
    cs->second = CK_BOOLEAN_CONVERSION;
    from_type = to_type;
  } else if (from_type->is_integral_or_unscoped_enumeration_type() &&
             to_type->is_integral_type()) {
    cs->second = CK_INTEGRAL_CONVERSION;
    from_type = to_type;
  } else if (from_type->is_real_floating_type() && to_type->is_real_floating_type()) {
    cs->second = CK_FLOATING_CONVERSION;
    from_type = to_type;
  } else if ((from_type->is_real_floating_type() && to_type->is_integral_type()) ||
      (from_type->is_integral_or_unscoped_enumeration_type() && to_type->is_real_floating_type())) {
    cs->second = CK_FLOATING_INTEGRAL;
    from_type = to_type;
  } else if (is_pointer_conversion(sema, from_e, from_type, to_type, from_type,
                                   is_explicit)) {
    cs->second = CK_POINTER_CONVERSION;
    from_type = to_type;
  } else if (false) { // TODO: CK_FIXED_POINT_CONVERSION
  } else if (is_explicit && is_allowed_explicit_cast(sema, from_type, to_type, cs->second)) {
    from_type = to_type;
  } else {
    cs->second = CK_IDENTITY;
  }

  cs->to_types[1] = from_type;
  return from_type == to_type;
}

bool ASTContext::is_promotable_integer_type(Type *t) const {
  if (const auto *bt = t->dyn_cast<BuiltinType>())
    switch (bt->builtin.kind) {
    case BuiltinType::BOOL:
    case BuiltinType::U8:
    case BuiltinType::I8:
    case BuiltinType::U16:
    case BuiltinType::I16:
      return true;
    default:
      return false;
    }

  if (const auto *ed = t->get_as_enum_decl()) {
    if ((ed->int_ty && !is_promotable_integer_type(ed->int_ty)) ||
        ed->is_scoped)
      return false;
    return true;
  }
  return false;
}

static Type *desugar_once(ASTContext *ctx, Type *ty) {
  if (auto t = ty->dyn_cast<AliasType>()) {
    ty = t->decl->dyn_cast<AliasDecl>()->underlying_type;
  } else if (auto t = ty->dyn_cast<PointerType>()) {
    ty = ctx->get_pointer_type(desugar_once(ctx, t->pointee_type));
  } else if (auto t = ty->dyn_cast<ArrayType>()) {
    ty = ctx->get_array_type(desugar_once(ctx, t->element_type), t->element_count);
  }
  return ty;
}

Type *ASTContext::desugar_type(Type *ty) {
  if (auto t = ty->dyn_cast<AliasType>()) {
    ty = desugar_type(t->decl->dyn_cast<AliasDecl>()->underlying_type);
  } else if (auto t = ty->dyn_cast<PointerType>()) {
    ty = get_pointer_type(desugar_type(t->pointee_type));
  } else if (auto t = ty->dyn_cast<ArrayType>()) {
    ty = get_array_type(desugar_type(t->element_type), t->element_count);
  }
  return ty;
}

bool ASTContext::is_same_type(Type *ty1, Type *ty2) {
  return (desugar_type(ty1) == desugar_type(ty2));
}

Type *ASTContext::get_common_type(Type *ty1, Type *ty2) {
  assert(is_same_type(ty1, ty2));
  if (ty1 == ty2) return ty1;
  if (!ty1->isa<AliasType>()) return ty1;
  if (!ty2->isa<AliasType>()) return ty2;
  std::vector<Type*> sugar_stack_ty1;
  while (true) {
    auto nt = desugar_once(this, ty1);
    if (nt != ty1) {
      sugar_stack_ty1.push_back(ty1);
    }
    ty1 = nt;
  }
  std::vector<Type*> sugar_stack_ty2;
  while (true) {
    auto nt = desugar_once(this, ty2);
    if (nt != ty2) {
      sugar_stack_ty2.push_back(ty2);
    }
    ty2 = nt;
  }
  Type *out;
  while (sugar_stack_ty1.size() > 0 && sugar_stack_ty2.size() > 0) {
    auto ty1_sugar = sugar_stack_ty1.back();
    auto ty2_sugar = sugar_stack_ty2.back();
    if (ty1_sugar != ty2_sugar) break;
    out = ty1_sugar;
    sugar_stack_ty1.pop_back();
    sugar_stack_ty2.pop_back();
  }
  return out;
}

Type * ASTContext::get_promoted_integer_type(Type *promotable) {
  assert(promotable);
  assert(is_promotable_integer_type(promotable));
  if (auto const *ed = promotable->get_as_enum_decl()) {
    return ed->int_ty ? get_promoted_integer_type(ed->int_ty) : i32_ty;
  }
  if (promotable->is_signed_integer_type()) return i32_ty;
  u64 s = get_type_size(promotable);
  return (s < 4) ? i32_ty : u32_ty;
}

bool is_integral_promotion(Sema *sema, Type *from_type, Type *to_type) {
  auto *to = to_type->dyn_cast<BuiltinType>();
  if (!to)
    return false;

  if (sema->ctx.is_promotable_integer_type(from_type) &&
      !from_type->is_boolean_type() && !from_type->is_enumeral_type()) {
    if (from_type->is_signed_integer_type() ||
        sema->ctx.get_type_size(from_type) < sema->ctx.get_type_size(to_type)) {
      return to->builtin.kind == BuiltinType::I32;
    }
    return to->builtin.kind == BuiltinType::U32;
  }

  if (auto *ed = from_type->get_as_enum_decl()) {
    if (ed->is_scoped)
      return false;

    if (ed->int_ty) {
      return ed->int_ty == to_type ||
             is_integral_promotion(sema, ed->int_ty, to_type);
    }

    if (to_type->is_integer_type() && !from_type->is_incomplete_type()) {
      return to_type == sema->ctx.i32_ty;
    }

    return false;
  }

  if (from_type->is_boolean_type() && to->builtin.kind == BuiltinType::I32)
    return true;

  return false;
}

static bool is_null_pointer_constant(Expr *expr) {
  if (auto *e = expr->dyn_cast<ParenExpr>())
    return is_null_pointer_constant(e->val.get());
  if (auto *e = expr->dyn_cast<ImplicitCastExpr>())
    return is_null_pointer_constant(e->op.get());
  if (auto *_ = expr->dyn_cast<NullptrLiteral>())
    return true;
  if (auto *e = expr->dyn_cast<IntegerLiteral>())
    return e->value == 0;
  if (auto *e = expr->dyn_cast<FloatingLiteral>())
    return e->value == 0.0;
  return false;
}

bool is_pointer_conversion_allowed(Sema *sema, Type *from_pointee,
                                   Type *to_pointee, bool is_explicit) {
  (void)sema;

  // TODO: better explicit rules
  if (is_explicit)
    return true;

  // TODO: do we allow implicit void* to any* ?
  if (from_pointee->is_void_type())
    return true;

  // struct supertype
  if (auto *sd = from_pointee->get_as_struct_decl()) {
    while (sd && sd->has_super) {
      auto super_type = sd->fields[0]->type;
      if (super_type == to_pointee)
        return true;
      sd = super_type->get_as_struct_decl();
    }
  }

  return false;
}

bool is_pointer_conversion(Sema *sema, Expr *f, Type *from_type, Type *to_type,
                           Type *&out_type, bool is_explicit) {
  if (is_explicit) {
    // TODO: better rules ?
    // int <-> ptr
    if (from_type->is_integral_type() && to_type->is_pointer_type()) {
      out_type = to_type;
      return true;
    }
    if (from_type->is_pointer_type() && to_type->is_integral_type()) {
      out_type = to_type;
      return true;
    }
  }

  if (to_type->is_nullptr_type() && is_null_pointer_constant(f)) {
    out_type = to_type;
    return true;
  }

  auto *to_type_ptr = to_type->dyn_cast<PointerType>();
  if (!to_type_ptr)
    return false;

  if (is_null_pointer_constant(f)) {
    out_type = to_type;
    return true;
  }

  auto *from_type_ptr = from_type->dyn_cast<PointerType>();
  if (!from_type_ptr)
    return false;

  auto *to_pointee_type = to_type_ptr->pointee_type;
  auto *from_pointee_type = from_type_ptr->pointee_type;
  if (to_pointee_type == from_pointee_type)
    return false;

  // allow implicit cast to void*
  if (to_pointee_type->is_void_type()) {
    out_type = sema->ctx.get_pointer_type(sema->ctx.void_ty);
    return true;
  }

  if (is_pointer_conversion_allowed(sema, from_pointee_type, to_pointee_type,
                                    is_explicit)) {
    out_type = to_type;
    return true;
  }

  return false;
}
