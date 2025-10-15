#include "ast/nodes/decl.hpp"
#include "expr.hpp"
#include <cassert>

using Cl = Expr::Classification;

static Cl::Kinds classify_internal(const Expr *e);

static Cl::Kinds classify_decl(const Decl *d) {
  bool islvalue = d->isa<ValueDecl>() && !d->isa<EnumVariantDecl>();
  return islvalue ? Cl::CL_LValue : Cl::CL_PRValue;
}

static Cl::Kinds classify_member_expr(const MemberExpr *e) {
  if (e->isa<MethodExpr>()) {
    return Cl::CL_PRValue;
  }
  if (e->is_arrow) return Cl::CL_LValue;
  return classify_internal(e->base.get());
}

static Cl::Kinds classify_conditional(const Expr *true_expr, const Expr *false_expr) {
  if (true_expr->type->is_void_type() || false_expr->type->is_void_type()) {
    return Cl::CL_PRValue;
  }
  Cl::Kinds lcl = classify_internal(true_expr), rcl = classify_internal(false_expr);
  return lcl == rcl ? lcl : Cl::CL_PRValue;
}

static Cl::ModifiableType is_modifiable(const Expr *e, Cl::Kinds kind, Loc &loc);

Cl Expr::classify_impl(Loc *loc) const {
  Cl::Kinds kind = classify_internal(this);
  // C99 6.3.2.1: An lvalue is an expression with an object type or an
  //   incomplete type other than void.

  Cl::ModifiableType modifiable = Cl::CM_Untested;
  if (loc)
    modifiable = is_modifiable(this, kind, *loc);
  return Classification(kind, modifiable);
}

/// Classify an expression which creates a temporary, based on its type.
static Cl::Kinds classify_temporary(Type *T) {
  if (T->is_structure_type())
    return Cl::CL_ClassTemporary;
  if (T->is_array_type())
    return Cl::CL_ArrayTemporary;
  return Cl::CL_PRValue;
}

static Cl::Kinds classify_expr_value_kind(const Expr *e, ValueKind kind) {
  switch (kind) {
  case ValueKind::PRVALUE:
    return classify_temporary(e->type);
  case ValueKind::LVALUE:
    return Cl::CL_LValue;
  case ValueKind::XVALUE:
    return Cl::CL_XValue;
  }
  assert(false && "unreachable");
}

static Cl::Kinds classify_internal(const Expr *e) {
  switch (e->kind) {
  case Expr::STRING_LITERAL:
    return e->is_lvalue() ? Cl::CL_LValue : Cl::CL_PRValue;
  case Expr::BOOL_LITERAL:
  case Expr::SIZEOF_EXPR:
  case Expr::NULLPTR_LITERAL:
  case Expr::INTEGER_LITERAL:
  case Expr::CHAR_LITERAL:
  case Expr::VAARGS_EXPR:
    return Cl::CL_PRValue;
  case Expr::ARRAY_SUBSCRIPT_EXPR: {
    Expr *base = static_cast<const ArraySubscriptExpr*>(e)->lhs->ignore_imp_casts();
    if (base->type->is_array_type())
      return classify_internal(base);
    return Cl::CL_LValue;
  }
  case Expr::DECLREF_EXPR:
    return classify_decl(static_cast<const DeclRefExpr*>(e)->decl);
  case Expr::MEMBER_EXPR:
  case Expr::METHOD_EXPR:
    return classify_member_expr(static_cast<const MemberExpr*>(e));
  case Expr::UNARY_EXPR:
    switch (static_cast<const UnaryExpr*>(e)->opc) {
    case UnaryExpr::DEREF:
      return Cl::CL_LValue;
    case UnaryExpr::PREINC:
    case UnaryExpr::PREDEC:
      return Cl::CL_LValue;
    default:
      return Cl::CL_PRValue;
    }
  case Expr::RECOVERY_EXPR:
  case Expr::IMPLICIT_CAST_EXPR:
    return classify_expr_value_kind(e, e->vk);
  case Expr::PAREN_EXPR:
    return classify_internal( static_cast<const ParenExpr*>(e)->val.get());
  case Expr::BINARY_EXPR:
  case Expr::COMPOUND_ASSIGN_EXPR:
    return static_cast<const BinaryExpr *>(e)->is_assignment_op() ? Cl::CL_LValue : Cl::CL_PRValue;
  case Expr::CALL_EXPR:
  case Expr::METHOD_CALL_EXPR:
  case Expr::EXPLICIT_CAST_EXPR:
  case Expr::VAARG_EXPR:
    return classify_temporary(e->type);
  case Expr::CONDITIONAL_EXPR: {
    const auto *co = static_cast<const ConditionalExpr *>(e);
    return classify_conditional(co->true_expr.get(), co->false_expr.get());
  }
  default: assert(false && "unreachable");
  }
}

static Cl::ModifiableType is_modifiable(const Expr *e, Cl::Kinds kind, Loc &loc) {
  if (kind == Cl::CL_PRValue) {
    if (const auto *CE = e->ignore_parens()->dyn_cast<ExplicitCastExpr>()) {
      if (CE->op->ignore_paren_imp_casts()->is_lvalue())  {
        loc = CE->get_start_loc();
        return Cl::CM_LValueCast;
      }
    }
  }
  if (kind != Cl::CL_LValue)
    return Cl::CM_RValue;
  if (e->type->is_function_type())
    return Cl::CM_Function;
  if (e->type->is_array_type())
    return Cl::CM_ArrayType;
  if (e->type->is_incomplete_type())
    return Cl::CM_IncompleteType;
  return Cl::CM_Modifiable;
}

Expr::LValueClassification Expr::classify_lvalue() const {
  Classification VC = classify();
  switch (VC.kind) {
  case Cl::CL_LValue:
    return LV_Valid;
  case Cl::CL_XValue:
    return LV_InvalidExpression;
  case Cl::CL_Function:
    return LV_NotObjectType;
  case Cl::CL_Void:
    return LV_InvalidExpression;
  case Cl::CL_AddressableVoid:
    return LV_IncompleteVoidType;
  case Cl::CL_DuplicateVectorComponents:
    return LV_DuplicateVectorComponents;
  case Cl::CL_MemberFunction:
    return LV_MemberFunction;
  case Cl::CL_SubObjCPropertySetting:
    return LV_SubObjCPropertySetting;
  case Cl::CL_ClassTemporary:
    return LV_ClassTemporary;
  case Cl::CL_ArrayTemporary:
    return LV_ArrayTemporary;
  case Cl::CL_ObjCMessageRValue:
    return LV_InvalidMessageExpression;
  case Cl::CL_PRValue:
    return LV_InvalidExpression;
  }
  assert(false && "unreachable");
}

Expr::isModifiableLvalueResult Expr::is_modifiable_lvalue(Loc *loc) const {
  Loc dummy;
  Classification VC = classify_modifiable(loc ? *loc : dummy);
  switch (VC.kind) {
  case Cl::CL_LValue:
    break;
  case Cl::CL_XValue:
    return MLV_InvalidExpression;
  case Cl::CL_Function:
    return MLV_NotObjectType;
  case Cl::CL_Void:
    return MLV_InvalidExpression;
  case Cl::CL_AddressableVoid:
    return MLV_IncompleteVoidType;
  case Cl::CL_DuplicateVectorComponents:
    return MLV_DuplicateVectorComponents;
  case Cl::CL_MemberFunction:
    return MLV_MemberFunction;
  case Cl::CL_SubObjCPropertySetting:
    return MLV_SubObjCPropertySetting;
  case Cl::CL_ClassTemporary:
    return MLV_ClassTemporary;
  case Cl::CL_ArrayTemporary:
    return MLV_ArrayTemporary;
  case Cl::CL_ObjCMessageRValue:
    return MLV_InvalidMessageExpression;
  case Cl::CL_PRValue:
    return VC.modifiable == Cl::CM_LValueCast ? MLV_LValueCast : MLV_InvalidExpression;
  }
  assert(VC.kind == Cl::CL_LValue && "Unhandled kind");
  switch (VC.modifiable) {
  case Cl::CM_Untested:
    assert(false && "Did not test modifiability");
  case Cl::CM_Modifiable:
    return MLV_Valid;
  case Cl::CM_RValue:
    assert(false && "CM_RValue and CL_LValue don't match");
  case Cl::CM_Function:
    return MLV_NotObjectType;
  case Cl::CM_LValueCast:
    assert(false && "CM_LValueCast and CL_LValue don't match");
  case Cl::CM_NoSetterProperty:
    return MLV_NoSetterProperty;
  case Cl::CM_ConstQualified:
    return MLV_ConstQualified;
  case Cl::CM_ConstQualifiedField:
    return MLV_ConstQualifiedField;
  case Cl::CM_ConstAddrSpace:
    return MLV_ConstAddrSpace;
  case Cl::CM_ArrayType:
    return MLV_ArrayType;
  case Cl::CM_IncompleteType:
    return MLV_IncompleteType;
  }
  assert(false && "Unhandled modifiable type");
}
