#include "sema.hpp"
#include "ast/nodes/expr.hpp"
#include <cassert>

CastExpr::CastKind Sema::scalar_type_to_boolean_cast_kind(Type *scalar_ty) {
  if (const auto *bt = scalar_ty->dyn_cast<BuiltinType>()) {
    if (bt->get_kind() == BuiltinType::BOOL)
      return CastExpr::NOOP;
    if (bt->get_kind() == BuiltinType::NULLPTR)
      return CastExpr::POINTER_TO_BOOLEAN;
    if (bt->is_integer())
      return CastExpr::INTEGRAL_TO_BOOLEAN;
    // float
    // fixed
  } else if (scalar_ty->isa<PointerType>()) {
    return CastExpr::POINTER_TO_BOOLEAN;
  } else if (scalar_ty->isa<EnumType>()) {
    assert(scalar_ty->get_as_enum_decl()->is_complete);
    return CastExpr::INTEGRAL_TO_BOOLEAN;
  }
  // complex
  assert(false && "other types");
}
