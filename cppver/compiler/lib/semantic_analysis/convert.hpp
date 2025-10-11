#ifndef SEMA_CONVERT_HPP_INCLUDED
#define SEMA_CONVERT_HPP_INCLUDED

#include "ast/nodes/expr.hpp"
#include "ast/nodes/type.hpp"

enum ConversionKind {
  CK_IDENTITY,
  CK_LVALUE_TO_RVALUE,
  CK_ARRAY_TO_POINTER,
  CK_FUNCTION_TO_POINTER,
  CK_FUNCTION_CONVERSION,
  CK_QUALIFICATION,
  CK_INTEGRAL_PROMOTION,
  CK_FLOATING_PROMOTION,
  CK_INTEGRAL_CONVERSION,
  CK_FLOATING_CONVERSION,
  CK_FLOATING_INTEGRAL,
  CK_POINTER_CONVERSION,
  CK_POINTER_MEMBER,
  CK_BOOLEAN_CONVERSION,
  CK_COMPATIBLE_CONVERSION,
  CK_DERIVED_TO_BASE,
  CK_ZERO_EVENT_CONVERSION,
  CK_ZERO_QUEUE_CONVERSION,
  CK_INCOMPATIBLE_POINTER_CONVERSION,
  CK_FIXED_POINT_CONVERSION,
  CK_NUM_CONVERSION_KINDS,
};

enum ConversionRank {
  CR_EXACT_MATCH,
  CR_PROMOTION,
  CR_CONVERSION,
};

ConversionRank get_ck_rank(ConversionKind k);

struct ConversionSequence {
  enum State { OK, AMBIGUOUS, ERR };
  State state = ERR;
  ConversionKind first = CK_IDENTITY;
  ConversionKind second = CK_IDENTITY;
  Type *from_type = nullptr;
  Type *to_types[2] = {nullptr, nullptr};

  void set_all_to_types(Type *t) { to_types[0] = to_types[1] = t; }
  void set_as_identity() { first = second = CK_IDENTITY; }
  bool is_identity() const { return second == CK_IDENTITY; }
  ConversionRank get_rank() const;
};

bool is_standard_conversion(struct Sema *sema, Expr *from_e, Type *to_type, ConversionSequence *cs, bool is_explicit);


#endif // SEMA_CONVERT_HPP_INCLUDED
