#include "ast/nodes/expr.hpp"
#include "ast/nodes/decl.hpp"
#include "ast/nodes/stmt.hpp"
#include "ast/nodes/type.hpp"
#include "convert.hpp"
#include "diags/diagnostic.hpp"
#include "lexer/ident.hpp"
#include "lexer/loc.hpp"
#include "lexer/tok.hpp"
#include "sema.hpp"
#include "semantic_analysis/literal.hpp"
#include <cassert>
#include <chrono>
#include <cstdio>
#include <memory>

UPtr<RecoveryExpr> Sema::create_recovery_expr(Loc begin, Loc end,
                                              std::vector<ExprUPtr> &&sub_exprs,
                                              Type *ty) {
  // if not context.recovery_ast: return None
  // if is_sfinae_context(): return None
  // if t.is_null() or t->is_undeduced_type() or not context.recovery_ast_type:
  // t = context.dependent_ty
  return std::make_unique<RecoveryExpr>(
      std::move(sub_exprs), LocRge(begin, end), ty, ValueKind::PRVALUE);
}

UPtr<ParenExpr> Sema::act_on_paren_expr(Loc lp, Loc rp, ExprUPtr e) {
  assert(e);
  return std::make_unique<ParenExpr>(std::move(e), lp, rp);
}

void Sema::check_completed_expr(Expr *e, Loc check_loc, bool is_constexpr) {
  (void)e, (void)check_loc, (void)is_constexpr;
  // llvm::SaveAndRestore ConstantContext(isConstantEvaluatedOverride,
  // IsConstexpr || isa<ConstantExpr>(E)); CheckImplicitConversions(E,
  // CheckLoc); CheckUnsequencedOperations(E); if not is_constexpr:
  // check_for_int_overflow(e) DiagnoseMisalignedMembers();
}

ExprUPtr Sema::maybe_create_expr_with_cleanups(ExprUPtr sub_expr) {
  // assert sub_expr is not None, "subexpression can't be null!"
  // CleanupVarDeclMarking();
  // unsigned FirstCleanup = ExprEvalContexts.back().NumCleanupObjects;
  // assert(ExprCleanupObjects.size() >= FirstCleanup);
  // assert(Cleanup.exprNeedsCleanups() || ExprCleanupObjects.size() ==
  // FirstCleanup); if (!Cleanup.exprNeedsCleanups()) return SubExpr; auto
  // Cleanups = llvm::ArrayRef(ExprCleanupObjects.begin() + FirstCleanup,
  // ExprCleanupObjects.size() - FirstCleanup); auto *E =
  // ExprWithCleanups::Create(Context, SubExpr,
  // Cleanup.cleanupsHaveSideEffects(), Cleanups);
  // DiscardCleanupsInEvaluationContext();
  // return E;
  return sub_expr;
}

ExprUPtr Sema::act_on_finish_full_expr(ExprUPtr fe, Loc cc,
                                       bool discarded_value,
                                       bool is_constexpr) {
  if (!fe)
    return nullptr;

  if (discarded_value) {
    fe = ignored_value_conversions(std::move(fe));
    if (!fe)
      return nullptr;
    diagnose_unused_expr_result(fe.get(), "diag::warn_unused_expr");
  }

  check_completed_expr(fe.get(), cc, is_constexpr);

  return maybe_create_expr_with_cleanups(std::move(fe));
}

ExprUPtr Sema::ignored_value_conversions(ExprUPtr e) {
  // maybe_decrement_count(e, RefsMinusAssignments);
  return e;
}

ExprUPtr Sema::act_on_explicit_cast(Type *ty, ExprUPtr e, Loc sl, Loc el) {
  (void)sl, (void)el;
  // TODO: use ExplicitCastExpr ...
  auto cs = try_implicit_conversion(e.get(), ty, true);
  return perform_implicit_conversion(std::move(e), ty, &cs);
}

UPtr<VAArgExpr> Sema::act_on_vaarg_expr(Type *ty, Loc sl, Loc el) {
  if (!cur_fn_decl ||
      !cur_fn_decl->type->dyn_cast<FunctionType>()->function.variadic) {
    Diag(diag::ERROR, sl, "cannot use vararg expr in non vararg function");
    return nullptr;
  }
  return std::make_unique<VAArgExpr>(LocRge{sl, el}, ty);
}

UPtr<VAArgsExpr> Sema::act_on_vaargs_expr(Loc loc) {
  if (!cur_fn_decl ||
      !cur_fn_decl->type->dyn_cast<FunctionType>()->function.variadic) {
    Diag(diag::ERROR, loc, "cannot use varargs expr in non vararg function");
    return nullptr;
  }
  return std::make_unique<VAArgsExpr>(loc, ctx.get_pointer_type(ctx.valist_ty));
}

UPtr<SizeofExpr> Sema::act_on_sizeof_expr(Type *ty, ExprUPtr expr, Loc sl,
                                          Loc el) {
  return std::make_unique<SizeofExpr>(ty, std::move(expr), sl, el, ctx.u64_ty);
}

StructDecl *Sema::act_on_scoped_identifier(Scope *scope, IdentInfo *ii,
                                           Loc loc) {
  auto maybe_struct_decl = scope->lookup_named_decl(ii);
  StructDecl *sd = nullptr;
  if (!maybe_struct_decl || !(sd = maybe_struct_decl->dyn_cast<StructDecl>())) {
    Diag(diag::ERROR, loc, "'%s' does not name a struct type",
         ii->name.c_str());
  }
  return sd;
}

Type *Sema::invalid_operands(Loc loc, ExprUPtr &lhs, ExprUPtr &rhs) {
  (void)lhs, (void)rhs;
  Diag(diag::ERROR, loc, "invalid operands to binary expression (%s and %s)",
       "TODO", "TODO");
  lhs->type->dump();
  printf(" -- ");
  rhs->type->dump();
  printf("\n");
  return nullptr;
}

ExprUPtr Sema::default_function_array_conversion(ExprUPtr e, bool diagnose) {
  (void)diagnose;
  auto type = e->type;
  assert(type);
  if (auto *_ = type->dyn_cast<FunctionType>()) {
    // TODO: cast to function ptr
    return imp_cast_expr_to_type(std::move(e), ctx.get_pointer_type(type),
                                 CastExpr::FUNCTION_TO_POINTER_DECAY);
  } else if (ArrayType *ar_ty; (ar_ty = type->dyn_cast<ArrayType>()) &&
                               e->vk == ValueKind::LVALUE) {
    return imp_cast_expr_to_type(std::move(e),
                                 ctx.get_pointer_type(ar_ty->element_type),
                                 CastExpr::ARRAY_TO_POINTER_DECAY);
  }
  return e;
}

ExprUPtr Sema::default_lvalue_conversion(ExprUPtr e) {
  if (e->vk == ValueKind::PRVALUE)
    return e;
  auto type = e->type;
  if (type->is_function_type() || type->is_array_type() || type->is_void_type())
    return e;
  if (type->is_structure_type())
    return e;
  if (type->is_nullptr_type())
    return std::make_unique<ImplicitCastExpr>(
        std::move(e), CastExpr::NULL_TO_POINTER, type, ValueKind::PRVALUE);
  return std::make_unique<ImplicitCastExpr>(
      std::move(e), CastExpr::LVALUE_TO_RVALUE, type, ValueKind::PRVALUE);
}

ExprUPtr Sema::default_function_array_lvalue_conversion(ExprUPtr e,
                                                        bool diagnose) {
  e = default_function_array_conversion(std::move(e), diagnose);
  if (!e)
    return e;
  return default_lvalue_conversion(std::move(e));
}

ExprUPtr Sema::usual_unary_conversions(ExprUPtr expr) {
  expr = default_function_array_lvalue_conversion(std::move(expr));
  if (!expr)
    return nullptr;
  // usual_unary_fp_conversions
  auto type = expr->type;
  if (type->is_integral_or_unscoped_enumeration_type()) {
    if (ctx.is_promotable_integer_type(type)) {
      auto pt = ctx.get_promoted_integer_type(type);
      return imp_cast_expr_to_type(std::move(expr), pt,
                                   CastExpr::INTEGRAL_CAST);
    }
  }
  return expr;
}

static bool unsupported_type_conversion(const Sema &S, Type *lhs_type,
                                        Type *rhs_type) {
  (void)S, (void)lhs_type, (void)rhs_type;
  // TODO:
  return false;
}

namespace {
using PerformCastFn = ExprUPtr(Sema &s, ExprUPtr op, Type *to_type);
ExprUPtr do_integral_cast(Sema &s, ExprUPtr op, Type *to_type) {
  return s.imp_cast_expr_to_type(std::move(op), to_type,
                                 CastExpr::INTEGRAL_CAST);
}
} // namespace

template <PerformCastFn doLHSCast, PerformCastFn doRHSCast>
static Type *handle_integer_conversion(Sema &s, ExprUPtr &lhs, ExprUPtr &rhs,
                                       Type *lhs_type, Type *rhs_type,
                                       bool is_comp_assign) {
  // The rules for this case are in C99 6.3.1.8
  i32 order = s.ctx.get_integer_type_order(lhs_type, rhs_type);
  bool lhs_signed = lhs_type->is_signed_integer_or_enumeration_type();
  bool rhs_signed = rhs_type->is_signed_integer_or_enumeration_type();
  if (lhs_signed == rhs_signed) {
    if (order >= 0) {
      rhs = (*doRHSCast)(s, std::move(rhs), lhs_type);
      return lhs_type;
    } else if (!is_comp_assign)
      lhs = (*doLHSCast)(s, std::move(lhs), rhs_type);
    return rhs_type;
  } else if (order != (lhs_signed ? 1 : -1)) {
    if (rhs_signed) {
      rhs = (*doRHSCast)(s, std::move(rhs), lhs_type);
      return lhs_type;
    } else if (!is_comp_assign)
      lhs = (*doLHSCast)(s, std::move(lhs), rhs_type);
    return rhs_type;
  } else if (s.ctx.get_type_size(lhs_type) != s.ctx.get_type_size(rhs_type)) {
    if (lhs_signed) {
      rhs = (*doRHSCast)(s, std::move(rhs), lhs_type);
      return lhs_type;
    } else if (!is_comp_assign)
      lhs = (*doLHSCast)(s, std::move(lhs), rhs_type);
    return rhs_type;
  } else {
    // The signed type is higher-ranked than the unsigned type,
    // but isn't actually any bigger (like unsigned int and long
    // on most 32-bit systems).  Use the unsigned type corresponding
    // to the signed type.
    Type *result =
        s.ctx.get_corresponding_unsigned_type(lhs_signed ? lhs_type : rhs_type);
    rhs = (*doRHSCast)(s, std::move(rhs), result);
    if (!is_comp_assign)
      lhs = (*doLHSCast)(s, std::move(lhs), result);
    return result;
  }
}

Type *Sema::usual_arithmetic_conversions(ExprUPtr &lhs, ExprUPtr &rhs,
                                         bool is_comp_assign) {
  if (!is_comp_assign) {
    lhs = usual_unary_conversions(std::move(lhs));
    if (!lhs)
      return nullptr;
  }
  rhs = usual_unary_conversions(std::move(rhs));
  if (!rhs)
    return nullptr;
  auto lhs_type = lhs->type;
  auto rhs_type = rhs->type;
  if (ctx.is_same_type(lhs_type, rhs_type)) {
    return ctx.get_common_type(lhs_type, rhs_type);
  }
  if (!lhs_type->is_arithmetic_type() || !rhs_type->is_arithmetic_type())
    return nullptr;
  Type *lhs_unpromoted_type = lhs_type;
  if (ctx.is_promotable_integer_type(lhs_type))
    lhs_type = ctx.get_promoted_integer_type(lhs_type);
  if (lhs_type != lhs_unpromoted_type && !is_comp_assign)
    lhs = imp_cast_expr_to_type(std::move(lhs), lhs_type,
                                CastExpr::INTEGRAL_CAST);
  if (ctx.is_same_type(lhs_type, rhs_type)) {
    return ctx.get_common_type(lhs_type, rhs_type);
  }
  if (unsupported_type_conversion(*this, lhs_type, rhs_type))
    return nullptr;
  // complex types
  // float types
  // complex int types
  // fixed types
  return handle_integer_conversion<do_integral_cast, do_integral_cast>(
      *this, lhs, rhs, lhs_type, rhs_type, is_comp_assign);
}

static bool check_arithmetic_op_pointer_operand(Sema &s, Loc loc,
                                                Expr *operand) {
  Type *res_type = operand->type;
  if (!res_type->is_pointer_type()) return true;
  Type *pointee_type = res_type->get_pointee_type();
  if (pointee_type->is_void_type()) {
    Diag(diag::ERROR, loc, "arithmetic on void pointer") << operand->get_range();
    return false;
  }
  if (pointee_type->is_function_type()) {
    Diag(diag::ERROR, loc, "arithmetic on function pointer") << operand->get_range();
    return false;
  }

  if (pointee_type->is_incomplete_type()) {
    Diag(diag::ERROR, loc, "arithmetic on incomplete type") << operand->get_range();
    return false;
  }

  return true;
}

static bool check_arithmetic_bin_op_pointer_operands(Loc loc,
                                                     Expr *lhs, Expr *rhs) {
  bool is_lhs_ptr = lhs->type->is_pointer_type();
  bool is_rhs_ptr = rhs->type->is_pointer_type();
  if (!is_lhs_ptr && !is_rhs_ptr) return true;

  Type *lhs_pointee_ty, *rhs_pointee_ty;
  if (is_lhs_ptr) lhs_pointee_ty = lhs->type->get_pointee_type();
  if (is_rhs_ptr) rhs_pointee_ty = rhs->type->get_pointee_type();

  bool is_lhs_voidptr = is_lhs_ptr && lhs_pointee_ty->is_void_type();
  bool is_rhs_voidptr = is_rhs_ptr && rhs_pointee_ty->is_void_type();
  if (is_lhs_voidptr || is_rhs_voidptr) {
    Diag(diag::ERROR, loc, "arithmetic on void pointer");
    return false;
  }

  bool is_lhs_funptr = is_lhs_ptr && lhs_pointee_ty->is_function_type();
  bool is_rhs_funptr = is_rhs_ptr && rhs_pointee_ty->is_function_type();
  if (is_lhs_funptr || is_rhs_funptr) {
    Diag(diag::ERROR, loc, "arithmetic on function pointer");
    return false;
  }

  if (is_lhs_funptr && lhs_pointee_ty->is_incomplete_type()) {
    Diag(diag::ERROR, loc, "arithmetic on incomplete type") << lhs->get_range();
    return false;
  }

  if (is_rhs_funptr && rhs_pointee_ty->is_incomplete_type()) {
    Diag(diag::ERROR, loc, "arithmetic on incomplete type") << rhs->get_range();
    return false;
  }

  return true;
}

void Sema::check_array_access(
    Expr *base_expr, Expr *index_expr // , const ArraySubscriptExpr *ASE, bool
                                      // AllowOnePastEnd, bool IndexNegated):
) {
  // TODO:
  (void)base_expr, (void)index_expr;
  // if (isConstantEvaluatedContext()) return;
  // IndexExpr = IndexExpr->IgnoreParenImpCasts();
  // if (IndexExpr->isValueDependent()) return;
  //
  // const Type *EffectiveType =
  // BaseExpr->getType()->getPointeeOrArrayElementType(); BaseExpr =
  // BaseExpr->IgnoreParenCasts(); const ConstantArrayType *ArrayTy =
  // Context.getAsConstantArrayType(BaseExpr->getType());
  // LangOptions::StrictFlexArraysLevelKind StrictFlexArraysLevel =
  // getLangOpts().getStrictFlexArraysLevel(); const Type *BaseType = ArrayTy ==
  // nullptr ? nullptr : ArrayTy->getElementType().getTypePtr(); bool
  // IsUnboundedArray = BaseType == nullptr ||
  // BaseExpr->isFlexibleArrayMemberLike(Context, StrictFlexArraysLevel,
  // /*IgnoreTemplateOrMacroSubstitution=*/true); if
  // (EffectiveType->isDependentType() || (!IsUnboundedArray &&
  // BaseType->isDependentType())) return; Expr::EvalResult Result; if
  // (!IndexExpr->EvaluateAsInt(Result, Context, Expr::SE_AllowSideEffects))
  // return; llvm::APSInt index = Result.Val.getInt(); if (IndexNegated) {
  // index.setIsUnsigned(false); index = -index; } if (IsUnboundedArray) {
  //   if (EffectiveType->isFunctionType()) return;
  //   if (index.isUnsigned() || !index.isNegative()) {
  //     const auto &ASTC = getASTContext();
  //     unsigned AddrBits =
  //     ASTC.getTargetInfo().getPointerWidth(EffectiveType->getCanonicalTypeInternal().getAddressSpace());
  //     if (index.getBitWidth() < AddrBits) index = index.zext(AddrBits);
  //     std::optional<CharUnits> ElemCharUnits =
  //     ASTC.getTypeSizeInCharsIfKnown(EffectiveType); if (!ElemCharUnits ||
  //     ElemCharUnits->isZero()) return; llvm::APInt
  //     ElemBytes(index.getBitWidth(), ElemCharUnits->getQuantity()); if
  //     (index.getActiveBits() <= AddrBits) {
  //       bool Overflow;
  //       llvm::APInt Product(index);
  //       Product += 1;
  //       Product = Product.umul_ov(ElemBytes, Overflow);
  //       if (!Overflow && Product.getActiveBits() <= AddrBits)
  //         return;
  //     }
  //     llvm::APInt MaxElems = llvm::APInt::getMaxValue(AddrBits);
  //     MaxElems = MaxElems.zext(std::max(AddrBits + 1,
  //     ElemBytes.getBitWidth())); MaxElems += 1; ElemBytes =
  //     ElemBytes.zextOrTrunc(MaxElems.getBitWidth()); MaxElems =
  //     MaxElems.udiv(ElemBytes); unsigned DiagID = ASE ?
  //     diag::warn_array_index_exceeds_max_addressable_bounds :
  //     diag::warn_ptr_arith_exceeds_max_addressable_bounds;
  //     DiagRuntimeBehavior(BaseExpr->getBeginLoc(), BaseExpr, PDiag(DiagID) <<
  //     toString(index, 10, true) << AddrBits <<
  //     (unsigned)ASTC.toBits(*ElemCharUnits) << toString(ElemBytes, 10, false)
  //     << toString(MaxElems, 10, false) <<
  //     (unsigned)MaxElems.getLimitedValue(~0U) <<
  //     IndexExpr->getSourceRange()); const NamedDecl *ND = nullptr; while
  //     (const auto *ASE = dyn_cast<ArraySubscriptExpr>(BaseExpr)) BaseExpr =
  //     ASE->getBase()->IgnoreParenCasts(); if (const auto *DRE =
  //     dyn_cast<DeclRefExpr>(BaseExpr)) ND = DRE->getDecl(); if (const auto
  //     *ME = dyn_cast<MemberExpr>(BaseExpr)) ND = ME->getMemberDecl(); if (ND)
  //     DiagRuntimeBehavior(ND->getBeginLoc(), BaseExpr,
  //     PDiag(diag::note_array_declared_here) << ND);
  //   }
  //   return;
  // }
  //
  // if (index.isUnsigned() || !index.isNegative()) {
  //   if (BaseType->isIncompleteType()) return;
  //   llvm::APInt size = ArrayTy->getSize();
  //   if (BaseType != EffectiveType) {
  //     uint64_t ptrarith_typesize = Context.getTypeSize(EffectiveType);
  //     uint64_t array_typesize = Context.getTypeSize(BaseType);
  //     if (!ptrarith_typesize) ptrarith_typesize = Context.getCharWidth();
  //     if (ptrarith_typesize != array_typesize) {
  //       uint64_t ratio = array_typesize / ptrarith_typesize;
  //       if (ptrarith_typesize * ratio == array_typesize) size *=
  //       llvm::APInt(size.getBitWidth(), ratio);
  //     }
  //   }
  //   if (size.getBitWidth() > index.getBitWidth()) index =
  //   index.zext(size.getBitWidth()); else if (size.getBitWidth() <
  //   index.getBitWidth()) size = size.zext(index.getBitWidth()); if
  //   (AllowOnePastEnd ? index.ule(size) : index.ult(size)) return; if (ASE) {
  //     SourceLocation RBracketLoc =
  //     SourceMgr.getSpellingLoc(ASE->getRBracketLoc()); if
  //     (SourceMgr.isInSystemHeader(RBracketLoc)) {
  //       SourceLocation IndexLoc =
  //       SourceMgr.getSpellingLoc(IndexExpr->getBeginLoc()); if
  //       (SourceMgr.isWrittenInSameFile(RBracketLoc, IndexLoc)) return;
  //     }
  //   }
  //   unsigned DiagID = ASE ? diag::warn_array_index_exceeds_bounds :
  //   diag::warn_ptr_arith_exceeds_bounds; unsigned CastMsg = (!ASE || BaseType
  //   == EffectiveType) ? 0 : 1; QualType CastMsgTy = ASE ?
  //   ASE->getLHS()->getType() : QualType();
  //   DiagRuntimeBehavior(BaseExpr->getBeginLoc(), BaseExpr, PDiag(DiagID) <<
  //   toString(index, 10, true) << ArrayTy->desugar() << CastMsg << CastMsgTy
  //   << IndexExpr->getSourceRange());
  // } else {
  //   unsigned DiagID = diag::warn_array_index_precedes_bounds;
  //   if (!ASE) {
  //     DiagID = diag::warn_ptr_arith_precedes_bounds;
  //     if (index.isNegative()) index = -index;
  //   }
  //   DiagRuntimeBehavior(BaseExpr->getBeginLoc(), BaseExpr, PDiag(DiagID) <<
  //   toString(index, 10, true) << IndexExpr->getSourceRange());
  // }
  //
  // const NamedDecl *ND = nullptr;
  // while (const auto *ASE = dyn_cast<ArraySubscriptExpr>(BaseExpr)) BaseExpr =
  // ASE->getBase()->IgnoreParenCasts(); if (const auto *DRE =
  // dyn_cast<DeclRefExpr>(BaseExpr)) ND = DRE->getDecl(); if (const auto *ME =
  // dyn_cast<MemberExpr>(BaseExpr)) ND = ME->getMemberDecl(); if (ND)
  // DiagRuntimeBehavior(ND->getBeginLoc(), BaseExpr,
  // PDiag(diag::note_array_declared_here) << ND);
}

Type *Sema::check_addition_operands(ExprUPtr &lhs, ExprUPtr &rhs, Loc loc,
                                    Type **comp_lhs_ty) {
  auto comp_type =
      usual_arithmetic_conversions(lhs, rhs, comp_lhs_ty != nullptr);
  if (!lhs || !rhs)
    return nullptr;
  if (comp_type && comp_type->is_arithmetic_type()) {
    if (comp_lhs_ty)
      *comp_lhs_ty = comp_type;
    return comp_type;
  }
  auto pexp = lhs.get();
  auto iexp = rhs.get();
  if (!pexp->type->is_pointer_type()) {
    std::swap(pexp, iexp);
    if (!pexp->type->is_pointer_type()) {
      return invalid_operands(loc, lhs, rhs);
    }
  }
  if (!iexp->type->is_integer_type())
    return invalid_operands(loc, lhs, rhs);
  // diagnose nullptr ?
  if (!check_arithmetic_op_pointer_operand(*this, loc, pexp))
    return nullptr;
  check_array_access(pexp, iexp);
  if (comp_lhs_ty) {
    auto lhs_ty = lhs->type;
    if (ctx.is_promotable_integer_type(lhs_ty)) {
      lhs_ty = ctx.get_promoted_integer_type(lhs_ty);
    }
    *comp_lhs_ty = lhs_ty;
  }
  return pexp->type;
}

Type *Sema::check_subtraction_operands(ExprUPtr &lhs, ExprUPtr &rhs, Loc loc,
                                       Type **comp_lhs_ty) {
  auto comp_type =
      usual_arithmetic_conversions(lhs, rhs, comp_lhs_ty != nullptr);
  if (!lhs || !rhs)
    return nullptr;
  if (comp_type && comp_type->is_arithmetic_type()) {
    if (comp_lhs_ty)
      *comp_lhs_ty = comp_type;
    return comp_type;
  }
  if (!lhs->type->is_pointer_type()) {
    return invalid_operands(loc, lhs, rhs);
  }
  //
  if (rhs->type->is_integer_type()) {
    if (!check_arithmetic_op_pointer_operand(*this, loc, lhs.get()))
      return nullptr;
    check_array_access(lhs.get(), rhs.get()); // nullptr, true, true
    if (comp_lhs_ty)
      *comp_lhs_ty = lhs->type;
    return lhs->type;
  }
  if (auto *rhs_pty = rhs->type->dyn_cast<PointerType>()) {
    auto rpointee = rhs_pty->get_pointee_type();
    auto lpointee = lhs->type->get_pointee_type();
    if (!ctx.is_same_type(lpointee, rpointee)) {
      Diag(diag::WARNING, loc, "comparison between pointers of different types");
    }
    if (!check_arithmetic_bin_op_pointer_operands(loc, lhs.get(),
                                                  rhs.get()))
      return nullptr;
    if (comp_lhs_ty)
      *comp_lhs_ty = lhs->type;
    return ctx.i64_ty; // pointer_diff_type
  }
  return invalid_operands(loc, lhs, rhs);
}

Type *Sema::check_multiply_divide_operands(ExprUPtr &lhs, ExprUPtr &rhs,
                                           Loc loc, BinaryExpr::OpKind opc) {
  bool is_comp_assign =
      opc == BinaryExpr::MULASSIGN || opc == BinaryExpr::DIVASSIGN;
  bool is_div = opc == BinaryExpr::DIVASSIGN || opc == BinaryExpr::DIV;
  auto comp_type = usual_arithmetic_conversions(lhs, rhs, is_comp_assign);
  if (!lhs || !rhs)
    return nullptr;
  if (!comp_type || !comp_type->is_arithmetic_type()) {
    return invalid_operands(loc, lhs, rhs);
  }
  if (is_div) {
    // DetectPrecisionLossInComplexDivision(*this, RHS.get()->getType(), Loc);
    // DiagnoseBadDivideOrRemainderValues(*this, LHS, RHS, Loc, IsDiv);
    // DiagnoseDivisionSizeofPointerOrArray(*this, LHS.get(), RHS.get(), Loc);
  }
  return comp_type;
}

Type *Sema::check_remainder_operands(ExprUPtr &lhs, ExprUPtr &rhs, Loc loc,
                                     bool is_comp_assign) {
  // if (LHS.get()->getType()->isDoubleType() ||
  // RHS.get()->getType()->isDoubleType()) return InvalidOperands(Loc, LHS,
  // RHS);
  auto comp_type = usual_arithmetic_conversions(lhs, rhs, is_comp_assign);
  if (!lhs || !rhs)
    return nullptr;
  if (!comp_type || !comp_type->is_integer_type()) {
    return invalid_operands(loc, lhs, rhs);
  }
  // DiagnoseBadDivideOrRemainderValues(*this, LHS, RHS, Loc, false /* IsDiv
  // */);
  return comp_type;
}

Type *Sema::check_shift_operands(ExprUPtr &lhs, ExprUPtr &rhs, Loc loc,
                                 BinaryExpr::OpKind opc, bool is_comp_assign) {
  Type *lhs_ty;
  if (!is_comp_assign) {
    lhs = usual_unary_conversions(std::move(lhs));
    if (!lhs)
      return nullptr;
    lhs_ty = lhs->type;
  } else {
    // TODO: get ty after conversion, but without converting ...
    lhs_ty = lhs->type;
  }
  rhs = usual_unary_conversions(std::move(rhs));
  if (!rhs)
    return nullptr;
  Type *rhs_ty = rhs->type;

  if ((!lhs_ty->is_integer_type() /*or fixed*/ && !lhs_ty->is_integer_type()) ||
      !rhs_ty->is_integer_type()) {
    return invalid_operands(loc, lhs, rhs);
  }
  (void)opc;
  // DiagnoseBadShiftValues(*this, LHS, RHS, Loc, Opc, LHSType);
  return lhs_ty;
}

Type *Sema::check_bitwise_operands(ExprUPtr &lhs, ExprUPtr &rhs, Loc loc,
                                   BinaryExpr::OpKind opc) {
  bool is_comp_assign = opc == BinaryExpr::ANDASSIGN ||
                        opc == BinaryExpr::ORASSIGN ||
                        opc == BinaryExpr::XORASSIGN;
  // if (opc == BinaryExpr::AND) diagnoseLogicalNotOnLHSofCheck(*this, LHS, RHS,
  // Loc, Opc); if (LHS.get()->getType()->hasFloatingRepresentation() ||
  // RHS.get()->getType()->hasFloatingRepresentation()) return
  // InvalidOperands(Loc, LHS, RHS);
  auto comp_type = usual_arithmetic_conversions(lhs, rhs, is_comp_assign);
  if (!lhs || !rhs)
    return nullptr; // revert to before conversion ?
  // if (opc == BinaryExpr::XOR) diagnoseXorMisusedAsPow(*this, LHS, RHS, Loc);
  if (comp_type && comp_type->is_integral_or_unscoped_enumeration_type())
    return comp_type;
  return invalid_operands(loc, lhs, rhs);
}

Type *Sema::check_logical_operands(ExprUPtr &lhs, ExprUPtr &rhs, Loc loc,
                                   BinaryExpr::OpKind opc) {
  (void)opc;
  // TODO: enum constant => some diagnostics

  // TODO: don't move lhs ?
  auto lhsres = perform_contextually_convert_to_bool(std::move(lhs));
  if (!lhsres)
    return invalid_operands(loc, lhs, rhs);
  lhs = std::move(lhsres);

  // TODO: don't move rhs ?
  auto rhsres = perform_contextually_convert_to_bool(std::move(rhs));
  if (!rhsres)
    return invalid_operands(loc, lhs, rhs);
  rhs = std::move(rhsres);

  return ctx.bool_ty;
}

static bool check_for_modifiable_lvalue(Expr *e, Loc loc, Sema &s) {
  (void)s;
  // Loc orig_loc = loc;
  auto is_lv = e->is_modifiable_lvalue(&loc);
  if (is_lv == Expr::MLV_Valid)
    return false;
  // TODO: appropriate diagnostic
  Diag(diag::ERROR, loc, "NON MODIFIABLE LVALUE!");
  return true;
}

Type *Sema::check_assignment_operands(Expr *lhs_expr, ExprUPtr &rhs, Loc loc,
                                      Type *compound_type,
                                      BinaryExpr::OpKind opc) {
  if (check_for_modifiable_lvalue(lhs_expr, loc, *this))
    return nullptr;
  auto lhs_type = lhs_expr->type;
  auto rhs_type = compound_type ? compound_type : rhs->type;
  bool conv_ty;
  (void)conv_ty, (void)rhs_type, (void)opc;
  if (!compound_type) {
    if (!lhs_type->is_structure_type()) {
      auto cs = try_implicit_conversion(rhs.get(), lhs_type);
      rhs = perform_implicit_conversion(std::move(rhs), lhs_type, &cs);
      conv_ty = true;
    } else {
      conv_ty = false;
    }
    if (!rhs)
      return nullptr;
  } else {
    // TODO: actual...

    auto cs = try_implicit_conversion(rhs.get(), lhs_type);
    rhs = perform_implicit_conversion(std::move(rhs), lhs_type, &cs);
    conv_ty = true; 
    if (!rhs) return nullptr;

    // rhs_expr = {loc, rhs_type, prvalue};
    // rhs_ptr = &rhs_expr;

    // QualType RHSType = rhs->type;
    // QualType OrigLHSType = lhs_type;

    // // Common case: no conversion required.
    // if (LHSType == RHSType) {
    //   return AssignConvertType::Compatible;
    // }

    // // Diagnose attempts to convert between __ibm128, __float128 and long
    // double
    // // where such conversions currently can't be handled.
    // if (unsupportedTypeConversion(*this, LHSType, RHSType))
    //   return AssignConvertType::Incompatible;

    // // Arithmetic conversions.
    // if (LHSType->isArithmeticType() && RHSType->isArithmeticType() &&
    // !(LHSType->isEnumeralType())) {
    //   return AssignConvertType::Compatible;
    // }

    // // Conversions to normal pointers.
    // if (const PointerType *LHSPointer = dyn_cast<PointerType>(LHSType)) {
    //   // U* -> T*
    //   if (isa<PointerType>(RHSType)) {
    //     LangAS AddrSpaceL = LHSPointer->getPointeeType().getAddressSpace();
    //     LangAS AddrSpaceR = RHSType->getPointeeType().getAddressSpace();
    //     return checkPointerTypesForAssignment(*this, LHSType, RHSType,
    //                                           loc);
    //   }

    //   // int -> T*
    //   if (RHSType->isIntegerType()) {
    //     return AssignConvertType::IntToPointer;
    //   }

    //   return AssignConvertType::Incompatible;
    // }

    // // Conversions from pointers that are not covered by the above.
    // if (isa<PointerType>(RHSType)) {
    //   // T* -> _Bool
    //   if (LHSType == Context.BoolTy) {
    //     return AssignConvertType::Compatible;
    //   }

    //   // T* -> int
    //   if (LHSType->isIntegerType()) {
    //     return AssignConvertType::PointerToInt;
    //   }

    //   return AssignConvertType::Incompatible;
    // }

    // return AssignConvertType::Incompatible;
  }
  // if (DiagnoseAssignmentResult(ConvTy, Loc, LHSType, RHSType, RHS.get(),
  // AssignmentAction::Assigning)) return QualType();
  // CheckForNullPointerDereference(*this, LHSExpr);
  // AssignedEntity AE{LHSExpr};
  // checkAssignmentLifetime(*this, AE, RHS.get());
  return lhs_type;
}

static Type *check_arithmetic_or_enumeral_compare(Sema &s, ExprUPtr &lhs,
                                                  ExprUPtr &rhs, Loc loc,
                                                  BinaryExpr::OpKind opc) {
  (void)opc;
  auto type = s.usual_arithmetic_conversions(lhs, rhs, false);
  if (!lhs || !rhs)
    return nullptr;
  if (!type)
    return s.invalid_operands(loc, lhs, rhs);
  // complex
  // float
  return s.ctx.bool_ty;
}

Type *Sema::check_compare_operands(ExprUPtr &lhs, ExprUPtr &rhs, Loc loc,
                                   BinaryExpr::OpKind opc) {
  lhs = default_function_array_lvalue_conversion(std::move(lhs));
  if (!lhs)
    return nullptr;
  rhs = default_function_array_lvalue_conversion(std::move(rhs));
  if (!rhs)
    return nullptr;
  // diagnoseLogicalNotOnLHSofCheck(*this, LHS, RHS, Loc, Opc);
  // diagnoseTautologicalComparison(*this, Loc, LHS.get(), RHS.get(), Opc);

  if ((lhs->type->is_arithmetic_type() || lhs->type->is_enumeral_type()) &&
      (rhs->type->is_arithmetic_type() || rhs->type->is_enumeral_type()))
    return check_arithmetic_or_enumeral_compare(*this, lhs, rhs, loc, opc);

  if (lhs->type->is_pointer_type() && rhs->type->is_pointer_type()) {
    // maybe diagnose if incompatible pointers
    return ctx.bool_ty;
  }

  if (lhs->type->is_pointer_type() && rhs->type->is_integer_type()) {
    Diag(diag::WARNING, loc, "comparing integer to pointer") << lhs->get_range() << rhs->get_range();
    rhs = imp_cast_expr_to_type(std::move(rhs), lhs->type, CastExpr::INTEGRAL_TO_POINTER);
    if (!rhs) return nullptr;
    return ctx.bool_ty;
  }

  if (rhs->type->is_pointer_type() && lhs->type->is_integer_type()) {
    Diag(diag::WARNING, loc, "comparing integer to pointer") << lhs->get_range() << rhs->get_range();
    lhs = imp_cast_expr_to_type(std::move(lhs), rhs->type, CastExpr::INTEGRAL_TO_POINTER);
    if (!lhs) return nullptr;
    return ctx.bool_ty;
  }

  return ctx.bool_ty;
}

ExprUPtr Sema::imp_cast_expr_to_type(ExprUPtr e, Type *ty,
                                     CastExpr::CastKind kind, ValueKind vk) {
  // diagnoseNullableToNonnullConversion(Ty, E->getType(), E->getBeginLoc());
  // diagnoseZeroToNullptrConversion(Kind, E);
  if (e->type == ty)
    return e;
  // if (kind == CastExpr::ARRAY_TO_POINTER_DECAY) {
  //   if (e->vk == ValueKind::PRVALUE) {
  //     ExprResult Materialized = CreateMaterializeTemporaryExpr(E->getType(),
  //     E, false); if (Materialized.isInvalid()) return ExprError(); E =
  //     Materialized.get();
  //   }
  // }
  if (ImplicitCastExpr *imp_cast = e->dyn_cast<ImplicitCastExpr>()) {
    if (imp_cast->cast_kind == kind) {
      imp_cast->type = ty;
      imp_cast->vk = vk;
      return e;
    }
  }
  return std::make_unique<ImplicitCastExpr>(std::move(e), kind, ty, vk);
}

UPtr<BinaryExpr> Sema::build_bin_op(Scope *s, Loc op_loc,
                                    BinaryExpr::OpKind opc, ExprUPtr lhs,
                                    ExprUPtr rhs) {
  (void)s;
  Type *result_ty = nullptr;
  Type *comp_lhs_ty = nullptr;
  Type *comp_result_ty = nullptr;
  ValueKind vk = ValueKind::PRVALUE;
  if (!lhs || !rhs)
    return nullptr;

  switch (opc) {
  case BinaryExpr::ASSIGN:
    result_ty = check_assignment_operands(lhs.get(), rhs, op_loc, nullptr, opc);
    vk = lhs->vk;
    // if (result_ty) {
    //   DiagnoseSelfAssignment(*this, LHS.get(), RHS.get(), OpLoc, true);
    //   DiagnoseSelfMove(LHS.get(), RHS.get(), OpLoc);
    // }
    // RecordModifiableNonNullParam(*this, LHS.get());
    break;
  case BinaryExpr::MUL:
  case BinaryExpr::DIV:
    result_ty = check_multiply_divide_operands(lhs, rhs, op_loc, opc);
    break;
  case BinaryExpr::REM:
    result_ty = check_remainder_operands(lhs, rhs, op_loc, false);
    break;
  case BinaryExpr::ADD:
    result_ty = check_addition_operands(lhs, rhs, op_loc);
    break;
  case BinaryExpr::SUB:
    result_ty = check_subtraction_operands(lhs, rhs, op_loc);
    break;
  case BinaryExpr::SHL:
  case BinaryExpr::SHR:
    result_ty = check_shift_operands(lhs, rhs, op_loc, opc, false);
    break;
  case BinaryExpr::LE:
  case BinaryExpr::LT:
  case BinaryExpr::GE:
  case BinaryExpr::GT:
  case BinaryExpr::EQ:
  case BinaryExpr::NE:
    result_ty = check_compare_operands(lhs, rhs, op_loc, opc);
    break;
  case BinaryExpr::AND:
  case BinaryExpr::XOR:
  case BinaryExpr::OR:
    result_ty = check_bitwise_operands(lhs, rhs, op_loc, opc);
    break;
  case BinaryExpr::LAND:
  case BinaryExpr::LOR:
    result_ty = check_logical_operands(lhs, rhs, op_loc, opc);
    break;
  case BinaryExpr::MULASSIGN:
  case BinaryExpr::DIVASSIGN:
    comp_result_ty = check_multiply_divide_operands(lhs, rhs, op_loc, opc);
    comp_lhs_ty = comp_result_ty;
    if (comp_result_ty && lhs && rhs)
      result_ty = check_assignment_operands(lhs.get(), rhs, op_loc,
                                            comp_result_ty, opc);
    break;
  case BinaryExpr::REMASSIGN:
    comp_result_ty = check_remainder_operands(lhs, rhs, op_loc, true);
    comp_lhs_ty = comp_result_ty;
    if (comp_result_ty && lhs && rhs)
      result_ty = check_assignment_operands(lhs.get(), rhs, op_loc,
                                            comp_result_ty, opc);
    break;
  case BinaryExpr::ADDASSIGN:
    comp_result_ty = check_addition_operands(lhs, rhs, op_loc, &comp_lhs_ty);
    if (comp_result_ty && lhs && rhs)
      result_ty = check_assignment_operands(lhs.get(), rhs, op_loc,
                                            comp_result_ty, opc);
    break;
  case BinaryExpr::SUBASSIGN:
    comp_result_ty = check_subtraction_operands(lhs, rhs, op_loc, &comp_lhs_ty);
    if (comp_result_ty && lhs && rhs)
      result_ty = check_assignment_operands(lhs.get(), rhs, op_loc,
                                            comp_result_ty, opc);
    break;
  case BinaryExpr::SHLASSIGN:
  case BinaryExpr::SHRASSIGN:
    comp_result_ty = check_shift_operands(lhs, rhs, op_loc, opc, true);
    comp_lhs_ty = comp_result_ty;
    if (comp_result_ty && lhs && rhs)
      result_ty = check_assignment_operands(lhs.get(), rhs, op_loc,
                                            comp_result_ty, opc);
    break;
  case BinaryExpr::ANDASSIGN:
  case BinaryExpr::ORASSIGN:
    // DiagnoseSelfAssignment(*this, LHS.get(), RHS.get(), OpLoc, true);
    [[fallthrough]];
  case BinaryExpr::XORASSIGN:
    comp_result_ty = check_bitwise_operands(lhs, rhs, op_loc, opc);
    comp_lhs_ty = comp_result_ty;
    if (comp_result_ty && lhs && rhs)
      result_ty = check_assignment_operands(lhs.get(), rhs, op_loc,
                                            comp_result_ty, opc);
    break;
  }
  if (!result_ty || !lhs || !rhs)
    return nullptr;
  // check_array_access(lhs.get());
  // check_array_access(rhs.get());
  if (!comp_result_ty) {
    return std::make_unique<BinaryExpr>(std::move(lhs), std::move(rhs), opc,
                                        op_loc, result_ty, vk);
  }
  vk = ValueKind::LVALUE;
  // fixed
  return std::make_unique<CompoundAssignExpr>(std::move(lhs), std::move(rhs),
                                              opc, op_loc, result_ty, vk);
}

UPtr<BinaryExpr> Sema::act_on_bin_op(Scope *s, Loc tok_loc, Tok kind,
                                     ExprUPtr lhs, ExprUPtr rhs) {
  auto opc = BinaryExpr::opc_from_tok(kind);
  assert(lhs && rhs);
  // DiagnoseBinOpPrecedence(*this, Opc, TokLoc, LHSExpr, RHSExpr);
  return build_bin_op(s, tok_loc, opc, std::move(lhs), std::move(rhs));
}

Type *Sema::check_address_of_operand(ExprUPtr &orig_op, Loc oploc) {
  Expr *op = orig_op.get()->ignore_parens();
  Expr::LValueClassification lval = op->classify_lvalue();
  if (lval == Expr::LV_ClassTemporary || lval == Expr::LV_ArrayTemporary) {
    Diag(diag::ERROR, oploc, "taking address of temporary value");
    return nullptr;
  } else if (lval != Expr::LV_Valid && lval != Expr::LV_IncompleteVoidType) {
    if (!op->type->is_function_type()) {
      Diag(diag::ERROR, oploc,
           "err_typecheck_invalid_lvalue_addrof << op->getType()") << op->get_range();
      return nullptr;
    }
  }
  return ctx.get_pointer_type(op->type);
}

Type *Sema::check_indirection_operand(ExprUPtr &op, ValueKind &vk, Loc oploc) {
  op = usual_unary_conversions(std::move(op));
  if (!op)
    return nullptr;
  Type *opty = ctx.desugar_type(op->type);
  Type *result = nullptr;
  if (auto pt = opty->dyn_cast<PointerType>())
    result = pt->pointee_type;
  if (!result) {
    Diag(diag::ERROR, oploc, "indirection requires pointer")
        << op->get_range(); // < type
    return nullptr;
  }
  if (result->is_void_type()) {
    Diag(diag::ERROR, oploc, "indirection through void pointer")
        << op->get_range();
  }
  vk = ValueKind::LVALUE;
  return result;
}

Type *Sema::check_increment_decrement_operand(ExprUPtr &op, ValueKind &vk,
                                              Loc oploc, bool is_inc,
                                              bool is_prefix) {
  auto res_type = op->type;
  if (res_type->is_boolean_type()) {
    if (!is_inc) {
      Diag(diag::ERROR, oploc, "decrement bool") << op->get_range();
      return nullptr;
    }
  } else if (res_type->is_enumeral_type()) {
    Diag(diag::ERROR, oploc, "increment decrement enum") << op->get_range();
    return nullptr;
  } else if (res_type->is_integer_type()) { // or float
                                            // OK
  } else if (res_type->is_pointer_type()) {
    if (!check_arithmetic_op_pointer_operand(*this, oploc, op.get()))
      return nullptr;
  } else if (false) { // complex
  } else {
    Diag(diag::ERROR, oploc, "illegal increment decrement") << op->get_range();
    return nullptr;
  }
  if (check_for_modifiable_lvalue(op.get(), oploc, *this))
    return nullptr;
  vk = is_prefix ? ValueKind::LVALUE : ValueKind::PRVALUE;
  return res_type;
}

static bool is_scoped_enumeration_type(Type *t) {
  if (const EnumDecl *ed = t->get_as_enum_decl())
    return ed->is_scoped;
  return false;
}

UPtr<UnaryExpr> Sema::build_unary_op(Scope *scope, Loc oploc,
                                     UnaryExpr::OpKind opc, ExprUPtr arg) {
  (void)scope;
  ValueKind vk = ValueKind::PRVALUE;
  Type *result_type = nullptr;
  ExprUPtr input = std::move(arg);

  switch (opc) {
  case UnaryExpr::PREINC:
  case UnaryExpr::PREDEC:
  case UnaryExpr::POSTINC:
  case UnaryExpr::POSTDEC:
    result_type = check_increment_decrement_operand(
        input, vk, oploc, opc == UnaryExpr::PREINC || opc == UnaryExpr::POSTINC,
        opc == UnaryExpr::PREINC || opc == UnaryExpr::PREDEC);
    break;
  case UnaryExpr::ADDROF:
    result_type = check_address_of_operand(input, oploc);
    // checkAddressOfNoDeref(InputExpr);
    // RecordModifiableNonNullParam(*this, InputExpr);
    break;
  case UnaryExpr::DEREF: {
    input = default_function_array_lvalue_conversion(std::move(input));
    if (!input)
      return nullptr;
    result_type = check_indirection_operand(input, vk, oploc);
    break;
  }
  case UnaryExpr::PLUS:
  case UnaryExpr::MINUS:
    input = usual_unary_conversions(std::move(input));
    if (!input)
      return nullptr;
    result_type = input->type;
    if (result_type->is_arithmetic_type())
      break;
    else if (opc == UnaryExpr::PLUS && result_type->is_pointer_type())
      break;
    Diag(diag::ERROR, oploc, "unary expr with invalid type")
        << input->get_range();
    return nullptr;
  case UnaryExpr::NOT:
    input = usual_unary_conversions(std::move(input));
    if (!input)
      return nullptr;
    result_type = input->type;
    // diagnose complex type
    if (result_type->is_integer_type()) {
      break;
    } else {
      Diag(diag::ERROR, oploc, "unary expr with invalid type")
          << input->get_range();
      return nullptr;
    }
  case UnaryExpr::LNOT:
    input = default_function_array_lvalue_conversion(std::move(input));
    if (!input)
      return nullptr;
    result_type = input->type;

    if (result_type->is_scalar_type() &&
        !is_scoped_enumeration_type(result_type)) {
      input =
          imp_cast_expr_to_type(std::move(input), ctx.bool_ty,
                                scalar_type_to_boolean_cast_kind(result_type));
      result_type = ctx.bool_ty;
      break;
    } else {
      Diag(diag::ERROR, oploc, "unary expr with invalid type")
          << input->get_range();
      return nullptr;
    }
  }
  if (!result_type || !input)
    return nullptr;
  // if (opc != UnaryExpr::ADDROF && opc != UnaryExpr::DEREF)
  // check_array_access(input.get());
  return std::make_unique<UnaryExpr>(std::move(input), opc, oploc, result_type,
                                     vk);
}

UPtr<UnaryExpr> Sema::act_on_unary_op(Scope *scope, Loc oploc, Tok op,
                                      ExprUPtr arg) {
  auto opc = UnaryExpr::opc_from_tok(op);
  return build_unary_op(scope, oploc, opc, std::move(arg));
}

UPtr<UnaryExpr> Sema::act_on_postfix_unary_op(Scope *scope, Loc oploc, Tok op,
                                              ExprUPtr arg) {
  UnaryExpr::OpKind opc;
  if (op == tok::PLUSPLUS)
    opc = UnaryExpr::POSTINC;
  else if (op == tok::MINUSMINUS)
    opc = UnaryExpr::POSTDEC;
  else
    assert(false && "unreachable");
  return build_unary_op(scope, oploc, opc, std::move(arg));
}

ExprUPtr Sema::act_on_numeric_constant(Token tok) {
  NumLiteralParser literal(&ctx, tok);

  if (literal.had_error)
    return nullptr;

  // TODO: float, fixed ...

  if (!literal.ty->is_integral_type())
    return nullptr;
  // TODO:
  // - diagnose bit width
  // - choose type if !literal.has_explicit_type

  return std::make_unique<IntegerLiteral>(literal.res, tok.loc, literal.ty);
}

UPtr<CharLiteral> Sema::act_on_character_constant(Token tok) {
  CharLiteralParser literal(&ctx, tok);
  if (literal.had_error)
    return nullptr;
  // TODO: choose type
  Type *ty = ctx.i8_ty;

  return std::make_unique<CharLiteral>(literal.res, tok.loc, ty);
}

UPtr<BoolLiteral> Sema::act_on_bool_literal(Loc loc, Tok kind) {
  assert(kind == tok::KW_TRUE || kind == tok::KW_FALSE);
  return std::make_unique<BoolLiteral>(kind == tok::KW_TRUE, loc, ctx.bool_ty);
}

UPtr<NullptrLiteral> Sema::act_on_nullptr_literal(Loc loc) {
  return std::make_unique<NullptrLiteral>(loc, ctx.nullptr_ty);
}

UPtr<StringLiteral>
Sema::act_on_string_literal(std::vector<Token> const &string_toks) {
  StringLiteralParser literal(&ctx, string_toks, false);
  if (literal.had_error)
    return nullptr;

  std::vector<Loc> string_tok_locs;
  for (auto const &tok : string_toks)
    string_tok_locs.push_back(tok.loc);
  // TODO: choose type
  Type *char_ty = ctx.i8_ty;
  Type *str_ty =
      ctx.get_array_type(char_ty, literal.get_num_string_chars() + 1);

  return std::make_unique<StringLiteral>(literal.result_buf, string_tok_locs,
                                         str_ty);
}

ExprUPtr Sema::check_boolean_condition(ExprUPtr cond_expr, bool is_constexpr) {
  auto e = perform_contextually_convert_to_bool(std::move(cond_expr));
  if (!is_constexpr || !e)
    return e;
  e = act_on_finish_full_expr(std::move(e), e->get_start_loc(), false);
  if (!e)
    return e;
  // llvm::APSInt Cond; E = VerifyIntegerConstantExpression(E.get(), &Cond,
  // diag::err_constexpr_if_condition_expression_is_not_constant);
  return e;
}

CastExpr::CastKind Sema::prepare_scalar_cast(ExprUPtr &src, Type *dest_ty) {
  auto src_ty = src->type;
  if (ctx.is_same_type(src_ty, dest_ty))
    return CastExpr::NOOP;

  if (src_ty->is_nullptr_type() || src_ty->is_pointer_type()) {
    if (dest_ty->is_pointer_type() || dest_ty->is_nullptr_type()) {
      // should be bitcast for different pointee type
      return CastExpr::NOOP;
    }
    if (dest_ty->is_boolean_type()) {
      return CastExpr::POINTER_TO_BOOLEAN;
    }
    if (dest_ty->is_integer_type()) {
      return CastExpr::POINTER_TO_INTEGRAL;
    }
  } else if (src_ty->is_boolean_type() || src_ty->is_integer_type()) {
    if (dest_ty->is_pointer_type() || dest_ty->is_nullptr_type()) {
      // check for NULL_TO_POINTER
      return CastExpr::INTEGRAL_TO_POINTER;
    }
    if (dest_ty->is_boolean_type()) {
      return CastExpr::INTEGRAL_TO_BOOLEAN;
    }
    if (dest_ty->is_integer_type()) {
      return CastExpr::INTEGRAL_CAST;
    }
  }
  // float, complex, fixed
  assert(false && "other types");
}

Type *Sema::check_conditional_operands(ExprUPtr &cond, ExprUPtr &lhs,
                                       ExprUPtr &rhs, ValueKind &vk,
                                       Loc question_loc) {
  vk = ValueKind::PRVALUE;

  cond = check_boolean_condition(std::move(cond));
  if (!cond)
    return nullptr;

  auto lty = lhs->type;
  auto rty = rhs->type;
  bool lvoid = lty->is_void_type();
  bool rvoid = rty->is_void_type();
  if (lvoid || rvoid) {
    if (lvoid && rvoid)
      return ctx.get_common_type(lty, rty);
    Diag(diag::ERROR, question_loc, "conditional void nonvoid (TODO MESSAGE)");
    return nullptr;
  }

  if (!ctx.is_same_type(lty, rty) &&
      (lty->is_structure_type() || rty->is_structure_type())) {
    Diag(diag::ERROR, question_loc, "conditional invalid");
    return nullptr;
  }

  auto lvk = lhs->vk;
  auto rvk = rhs->vk;
  if (!ctx.is_same_type(lty, rty) && lvk == rvk && lvk != ValueKind::PRVALUE) {
    // TODO... change some types
  }

  bool same = ctx.is_same_type(lty, rty);
  if (same && lvk == rvk && lvk != ValueKind::PRVALUE) {
    vk = lhs->vk;
    return ctx.get_common_type(lty, rty);
  }

  if (!same && (lty->is_structure_type() || rty->is_structure_type())) {
    return nullptr;
  }

  lhs = default_function_array_lvalue_conversion(std::move(lhs));
  rhs = default_function_array_lvalue_conversion(std::move(rhs));
  if (!lhs || !rhs)
    return nullptr;
  lty = lhs->type;
  rty = rhs->type;

  if (ctx.is_same_type(lty, rty)) {
    return ctx.get_common_type(lty, rty);
  }

  if (lty->is_arithmetic_type() && rty->is_arithmetic_type()) {
    Type *res_ty = usual_arithmetic_conversions(lhs, rhs, false);
    if (!lhs || !rhs)
      return nullptr;
    if (!res_ty) {
      Diag(diag::ERROR, question_loc,
           "err typecheck cond incompatible operands");
      return nullptr;
    }
    lhs = imp_cast_expr_to_type(std::move(lhs), res_ty,
                                prepare_scalar_cast(lhs, res_ty));
    rhs = imp_cast_expr_to_type(std::move(rhs), res_ty,
                                prepare_scalar_cast(rhs, res_ty));
    return res_ty;
  }
  Diag(diag::ERROR, question_loc, "err typecheck cond incompatible operands");
  return nullptr;
}

UPtr<ConditionalExpr> Sema::act_on_conditional_op(Loc question_loc,
                                                  Loc colon_loc,
                                                  ExprUPtr cond_expr,
                                                  ExprUPtr lhs, ExprUPtr rhs) {
  auto vk = ValueKind::PRVALUE;

  auto result =
      check_conditional_operands(cond_expr, lhs, rhs, vk, question_loc);
  if (!result || !cond_expr || !lhs || !rhs)
    return nullptr;

  // DiagnoseConditionalPrecedence(*this, QuestionLoc, Cond.get(), LHS.get(),
  // RHS.get()); CheckBoolLikeConversion(Cond.get(), QuestionLoc);

  return std::make_unique<ConditionalExpr>(std::move(cond_expr), std::move(lhs),
                                           std::move(rhs), question_loc,
                                           colon_loc, result, vk);
}

// bool Sema::CheckPointerConversion(Expr *From, QualType ToType,
//                                   CastKind &Kind,
//                                   CXXCastPath& BasePath) {
//   QualType FromType = From->getType();
//
//   Kind = CK_BitCast;
//
//   if (const PointerType *ToPtrType = ToType->getAs<PointerType>()) {
//     if (const PointerType *FromPtrType = FromType->getAs<PointerType>()) {
//       QualType FromPointeeType = FromPtrType->getPointeeType(),
//                ToPointeeType   = ToPtrType->getPointeeType();
//
//       if (FromPointeeType->isRecordType() && ToPointeeType->isRecordType() &&
//           !Context.hasSameUnqualifiedType(FromPointeeType, ToPointeeType)) {
//         // We must have a derived-to-base conversion. Check an
//         // ambiguous or inaccessible conversion.
//         unsigned InaccessibleID = 0;
//         unsigned AmbiguousID = 0;
//         if (CheckDerivedToBaseConversion(FromPointeeType, ToPointeeType,
//         InaccessibleID, AmbiguousID, From->getExprLoc(),
//         From->getSourceRange(), DeclarationName(), &BasePath, CStyle))
//           return true;
//
//         // The conversion was successful.
//         Kind = CK_DerivedToBase;
//       }
//
//     }
//   }
//   // We shouldn't fall into this case unless it's valid for other
//   // reasons.
//   if (From->isNullPointerConstant(Context, Expr::NPC_ValueDependentIsNull))
//     Kind = CK_NullToPointer;
//
//   return false;
// }

ExprUPtr Sema::perform_implicit_conversion(ExprUPtr from, Type *to_type,
                                           ConversionSequence *cs) {
  if (cs->state != ConversionSequence::OK) {
    Diag(diag::ERROR, from->get_start_loc(), "Conversion error") << from->get_range();
    from->type->dump();
    printf(" (%p) -> ", from->type);
    to_type->dump();
    printf(" (%p)  is_same=%d\n", to_type, ctx.is_same_type(from->type, to_type));
    return nullptr;
  }
  assert(cs->state == ConversionSequence::OK); // TODO: diagnose
  // bool CStyle = (CCK == CheckedConversionKind::CStyleCast || CCK ==
  // CheckedConversionKind::FunctionalCast);

  Type *from_type = from->type;

  switch (cs->first) {
  case CK_IDENTITY:
    break;
  case CK_LVALUE_TO_RVALUE:
    from = default_lvalue_conversion(std::move(from));
    from_type = from->type;
    break;
  case CK_ARRAY_TO_POINTER:
    from_type =
        ctx.get_pointer_type(from_type->dyn_cast<ArrayType>()->element_type);
    from = imp_cast_expr_to_type(std::move(from), from_type,
                                 CastExpr::ARRAY_TO_POINTER_DECAY,
                                 ValueKind::PRVALUE);
    break;
  case CK_FUNCTION_TO_POINTER:
    from_type = ctx.get_pointer_type(from_type);
    from = imp_cast_expr_to_type(std::move(from), from_type,
                                 CastExpr::FUNCTION_TO_POINTER_DECAY,
                                 ValueKind::PRVALUE);
    break;
  default:
    assert(false && "unreachable");
  }

  switch (cs->second) {
  case CK_IDENTITY:
    break;
  case CK_INTEGRAL_PROMOTION:
  case CK_INTEGRAL_CONVERSION:
    if (to_type->is_boolean_type()) {
      from = imp_cast_expr_to_type(std::move(from), to_type,
                                   CastExpr::INTEGRAL_TO_BOOLEAN,
                                   ValueKind::PRVALUE);
    } else {
      from = imp_cast_expr_to_type(std::move(from), to_type,
                                   CastExpr::INTEGRAL_CAST, ValueKind::PRVALUE);
    }
    break;
  case CK_FLOATING_CONVERSION:
  case CK_FLOATING_PROMOTION:
    // from = imp_cast_expr_to_type(std::move(from), to_type,
    // CastExpr::FLOATING_CAST, ValueKind::PRVALUE);
    break;
  case CK_FLOATING_INTEGRAL:
    // if (to_type->is_real_floating_point()) {
    //   from = imp_cast_expr_to_type(std::move(from), to_type,
    //   CastExpr::INTEGRAL_TO_FLOATING, ValueKind::PRVALUE);
    // } else {
    //   from = imp_cast_expr_to_type(std::move(from), to_type,
    //   CastExpr::FLOATING_TO_INTEGRAL, ValueKind::PRVALUE);
    // }
    break;
  case CK_FIXED_POINT_CONVERSION:
    // TODO:
    break;
  case CK_COMPATIBLE_CONVERSION:
    from = imp_cast_expr_to_type(std::move(from), to_type, CastExpr::NOOP,
                                 from->vk);
    break;
  case CK_POINTER_CONVERSION:
    if (from_type->is_pointer_type() && to_type->is_integral_or_enumeration_type()) {
      from = imp_cast_expr_to_type(std::move(from), to_type, CastExpr::POINTER_TO_INTEGRAL,
                                  ValueKind::PRVALUE);
    } else if (from_type->is_integral_or_enumeration_type()) {
      from = imp_cast_expr_to_type(std::move(from), to_type, CastExpr::INTEGRAL_TO_POINTER,
                                  ValueKind::PRVALUE);
    } else {
      from = imp_cast_expr_to_type(std::move(from), to_type, CastExpr::NOOP,
                                  ValueKind::PRVALUE);
    }
    break;
  case CK_BOOLEAN_CONVERSION:
    from = imp_cast_expr_to_type(std::move(from), to_type,
                                 scalar_type_to_boolean_cast_kind(from_type),
                                 ValueKind::PRVALUE);
    break;
  case CK_DERIVED_TO_BASE:
    // TODO:
    break;
  default:
    assert(false && "unreachable");
  }
  // if (!isCast(CCK)) diagnoseNullableToNonnullConversion(ToType,
  // InitialFromType, From->getBeginLoc());
  return from;
}

ExprUPtr Sema::act_on_call_expr(Scope *scope, ExprUPtr fn, Loc lp,
                                std::vector<ExprUPtr> &&arg_exprs, Loc rp) {
  auto call =
      build_call_expr(scope, std::move(fn), lp, std::move(arg_exprs), rp);
  if (!call)
    return nullptr;
  // if (const auto *CE = dyn_cast<CallExpr>(Call.get()))
  // DiagnosedUnqualifiedCallsToStdFunctions(*this, CE);
  return call;
}

ExprUPtr Sema::build_call_expr(Scope *scope, ExprUPtr fn, Loc lp,
                               std::vector<ExprUPtr> &&arg_exprs, Loc rp) {
  (void)scope;
  Expr *naked_fn = fn->ignore_parens();
  const NamedDecl *ndecl = nullptr;
  if (auto *dre = naked_fn->dyn_cast<DeclRefExpr>()) {
    ndecl = dre->decl;
  } else if (auto *me = naked_fn->dyn_cast<MethodExpr>()) {
    ndecl = me->method_func;
  }

  if (fn->type->is_function_type()) {
    fn = imp_cast_expr_to_type(std::move(fn), ctx.get_pointer_type(fn->type),
                               CastExpr::FUNCTION_TO_POINTER_DECAY);
  } else {
    fn = default_lvalue_conversion(std::move(fn));
  }
  if (!fn)
    return nullptr;

  const FunctionType *func_ty = nullptr;
  if (const PointerType *pt = fn->type->dyn_cast<PointerType>()) {
    func_ty = pt->pointee_type->dyn_cast<FunctionType>();
    if (!func_ty) {
      Diag(diag::ERROR, lp, "diag::err_typecheck_call_not_function << fn->type")
          << fn->get_range();
      return nullptr;
    }
  } else {
    Diag(diag::ERROR, lp, "diag::err_typecheck_call_not_function << fn->type")
        << fn->get_range();
    return nullptr;
  }

  if (func_ty->result_type->is_structure_type()) { // or incomplete enum type ?
    // TODO: more complex result type checking
    Diag(diag::UNIMPLEMENTED, fn->get_start_loc(), "function returns a struct...");
    return nullptr;
  }

  UPtr<CallExpr> the_call;
  bool method = false;
  if (naked_fn->isa<MethodExpr>()) {
    the_call = std::make_unique<MethodCallExpr>(
        std::move(fn), std::move(arg_exprs), rp, func_ty->result_type,
        ValueKind::PRVALUE);
    method = true;
  } else {
    the_call =
        std::make_unique<CallExpr>(std::move(fn), std::move(arg_exprs), rp,
                                   func_ty->result_type, ValueKind::PRVALUE);
  }

  FunctionDecl const *fdecl = ndecl->dyn_cast<FunctionDecl>();
  assert(fdecl);
  bool is_variadic = func_ty->function.variadic == Type::FunctionTypeBits::VARIADIC;

  if (the_call->args.size() + method < fdecl->params.size()) {
    if (fdecl->params.size() == 1 && fdecl->params[0]->name)
      Diag(diag::ERROR, rp,
           !is_variadic ? "too few arguments to %s call, single argument '%s' "
                          "was not specified"
                        : "too few arguments to %s call, at least argument "
                          "'%s' must be specified",
           method ? "method" : "function", fdecl->params[0]->get_name().c_str())
          << the_call->fn->get_range();
    else
      Diag(diag::ERROR, rp,
           !is_variadic
               ? "too few arguments to %s call, expected %lld, have %lld"
               : "too few arguments to %s call, expected at least %lld, have "
                 "%lld",
           method ? "method" : "function", fdecl->params.size(),
           the_call->args.size())
          << the_call->fn->get_range();
    Diag(diag::NOTE, fdecl->get_loc(), "%s declared here",
         fdecl->get_name().c_str())
        << fdecl->get_param_range();
    return nullptr;
  }

  if (the_call->args.size() + method > fdecl->params.size() && !is_variadic) {
    if (fdecl->params.size() == 1 && fdecl->params[0]->name)
      Diag(diag::ERROR, the_call->args[1]->get_start_loc(),
           "too many arguments to %s call, expected single argument %s, have "
           "%lld arguments",
           method ? "method" : "function", fdecl->params[0]->get_name().c_str(),
           the_call->args.size())
          << the_call->fn->get_range()
          << LocRge(the_call->args[0]->get_start_loc(),
                    the_call->args.back()->get_end_loc());
    else
      Diag(diag::ERROR, the_call->args[fdecl->params.size()]->get_start_loc(),
           "too many arguments to %s call, expected %lld, have %lld",
           method ? "method" : "function", fdecl->params.size(),
           the_call->args.size())
          << the_call->fn->get_range()
          << LocRge(the_call->args[fdecl->params.size()]->get_start_loc(),
                    the_call->args.back()->get_end_loc());
    Diag(diag::NOTE, fdecl->get_loc(), "%s declared here",
         fdecl->get_name().c_str())
        << fdecl->get_param_range();
    return nullptr;
  }

  u32 argix = 0;
  for (u32 i = method ? 1 : 0; i < fdecl->params.size(); i++) {
    auto cs = try_implicit_conversion(the_call->args[argix].get(),
                                      fdecl->params[i]->type);
    the_call->args[argix] = perform_implicit_conversion(
        std::move(the_call->args[argix]), fdecl->params[i]->type, &cs);
    // TODO: diagnose errors ?
    if (!the_call->args[argix])
      return nullptr;
    // CheckArrayAccess(the_call->args[argix].get());
    argix++;
  }

  if (is_variadic) {
    while (argix < the_call->args.size()) {
      // TODO: check if arg type is valid in variadic
      the_call->args[argix] =
          default_argument_promotion(std::move(the_call->args[argix]));
      if (!the_call->args[argix])
        return nullptr;
      // CheckArrayAccess(the_call->args[argix].get());
      argix++;
    }
  }

  return the_call;
}

ExprUPtr Sema::default_argument_promotion(ExprUPtr e) {
  e = usual_unary_conversions(std::move(e));
  if (!e)
    return nullptr;

  // float => double
  // temporary lvalues

  if (e->type->is_nullptr_type())
    e = imp_cast_expr_to_type(std::move(e), ctx.get_pointer_type(ctx.void_ty),
                              CastExpr::NULL_TO_POINTER);

  return e;
}

////// FROM HERE: TODO ////////////

FieldDecl const *Sema::lookup_field_in_struct(StructDecl const *s,
                                              IdentInfo const *name) {
  for (auto const &f : s->fields) {
    if (f->name == name)
      return f.get();
  }
  if (!s->has_super)
    return nullptr;
  if ((s = s->fields[0]->type->get_as_struct_decl())) {
    return lookup_field_in_struct(s, name);
  }
  return nullptr;
}

FunctionDecl const *Sema::lookup_method_in_struct(StructDecl const *struct_type,
                                                  IdentInfo const *name) {
  for (auto const &m : struct_type->methods) {
    if (m->name == name) {
      // first param must be pointer to struct_type for it to be a method
      if (m->params.size() == 0)
        continue;
      if (m->params[0]->type !=
          ctx.get_pointer_type(struct_type->type_for_decl))
        continue;
      return m;
    }
  }
  if (!struct_type->has_super)
    return nullptr;
  if ((struct_type = struct_type->fields[0]->type->get_as_struct_decl())) {
    return lookup_method_in_struct(struct_type, name);
  }
  return nullptr;
}

UPtr<Expr> Sema::build_member_reference_expr(ExprUPtr base, Type *base_type,
                                             Loc oploc, bool is_arrow,
                                             Loc name_loc, IdentInfo *name) {
  // TODO: better recovery ?
  StructDecl *struct_type = nullptr;
  if (is_arrow) {
    if (!base_type->is_pointer_type() ||
        !(struct_type = base_type->dyn_cast<PointerType>()
                            ->pointee_type->get_as_struct_decl())) {
      Diag(diag::ERROR, oploc, "base expr of '->' is not a pointer to a struct")
          << base->get_range();
      return nullptr;
    }
    base = default_lvalue_conversion(std::move(base)); // XXX < ?
  } else {
    if (!(struct_type = base_type->get_as_struct_decl())) {
      Diag(diag::ERROR, oploc, "base expr of '.' is not a struct")
          << base->get_range();
      return nullptr;
    }
  }
  assert(struct_type);

  if (auto *field = lookup_field_in_struct(struct_type, name)) {
    return std::make_unique<MemberExpr>(std::move(base), name, name_loc, field,
                                        is_arrow, field->type,
                                        ValueKind::LVALUE);
  }
  auto method = lookup_method_in_struct(struct_type, name);
  if (!method) {
    Diag(diag::ERROR, name_loc, "unknown field or method '%s' for struct '%s'",
         name->name.c_str(), struct_type->get_name().c_str());
    return nullptr;
  }
  return std::make_unique<MethodExpr>(std::move(base), name, method, name_loc,
                                      is_arrow, method->type,
                                      ValueKind::LVALUE);
}

UPtr<Expr> Sema::act_on_member_access_expr(Scope *scope, ExprUPtr base,
                                           Loc oploc, Tok opkind, Loc name_loc,
                                           IdentInfo *name) {
  (void)scope;
  bool is_arrow = opkind == tok::ARROW;
  return build_member_reference_expr(std::move(base), base->type, oploc,
                                     is_arrow, name_loc, name);
}

UPtr<Expr> Sema::create_builtin_array_subscript_expr(ExprUPtr base, Loc lloc,
                                                     ExprUPtr idx, Loc rloc) {
  auto vk = ValueKind::LVALUE;
  // TODO check XVALUE for array types
  base = default_lvalue_conversion(std::move(base));
  if (!base)
    return nullptr;
  idx = default_function_array_lvalue_conversion(std::move(idx));
  if (!idx)
    return nullptr;
  Type *lhs_ty = ctx.desugar_type(base->type);
  Type *rhs_ty = ctx.desugar_type(idx->type);
  Type *result_type = nullptr;
  Expr *base_expr = base.get();
  Expr *idx_expr = idx.get();
  // lhs_expr = base
  // rhs_expr = idx
  if (lhs_ty->is_pointer_type()) {
    result_type = lhs_ty->dyn_cast<PointerType>()->pointee_type;
  } else if (rhs_ty->is_pointer_type()) {
    std::swap(base_expr, idx_expr);
    result_type = rhs_ty->dyn_cast<PointerType>()->pointee_type;
  } else if (lhs_ty->is_array_type()) {
    base = imp_cast_expr_to_type(
        std::move(base),
        ctx.get_pointer_type(lhs_ty->dyn_cast<ArrayType>()->element_type),
        CastExpr::ARRAY_TO_POINTER_DECAY);
    assert(base);
    lhs_ty = ctx.desugar_type(base->type);
    result_type = lhs_ty->dyn_cast<PointerType>()->pointee_type;
  } else if (rhs_ty->is_array_type()) {
    idx = imp_cast_expr_to_type(
        std::move(idx),
        ctx.get_pointer_type(rhs_ty->dyn_cast<ArrayType>()->element_type),
        CastExpr::ARRAY_TO_POINTER_DECAY);
    assert(idx);
    rhs_ty = ctx.desugar_type(idx->type);
    std::swap(base_expr, idx_expr);
    result_type = rhs_ty->dyn_cast<PointerType>()->pointee_type;
  } else if (auto *sd = lhs_ty->get_as_struct_decl()) {
    if (!sd->has_super) {
      Diag(diag::ERROR, lloc, "subscripted value is not an array or a pointer");
      return nullptr;
    }
    FieldDecl const *deepest_super_field = sd->fields[0].get();
    while ((sd = deepest_super_field->type->get_as_struct_decl())) {
      if (!sd->has_super)
        break;
      deepest_super_field = sd->fields[0].get();
    }
    if (deepest_super_field->type->is_pointer_type()) {
      base =
          act_on_member_access_expr(nullptr, std::move(base), lloc, tok::PERIOD,
                                    lloc, deepest_super_field->name);
      base = default_lvalue_conversion(std::move(base));
      if (!base)
        return nullptr;
      result_type =
          deepest_super_field->type->dyn_cast<PointerType>()->pointee_type;
    } else {
      Diag(diag::ERROR, lloc, "subscripted value is not an array or a pointer");
      return nullptr;
    }
  } else {
    Diag(diag::ERROR, lloc, "subscripted value is not an array or a pointer");
    return nullptr;
  }
  if (!idx_expr->type->is_integer_type()) {
    Diag(diag::ERROR, idx_expr->get_start_loc(), "diag::err_typecheck_subscript_not_integer") << idx_expr->get_range();
    return nullptr;
  }
  //// Warn if index is char constexpr
  //// error if result_type =is function type
  // if (RequireCompleteSizedType(LLoc, ResultType,
  // diag::err_subscript_incomplete_or_sizeless_type, BaseExpr)) return
  // ExprError(); if
  // (lhs_exp.ignore_paren_imp_casts().ty.is_variably_modified_type() &&
  // function_scopes.size() > 1) {
  //   if (auto *TT =
  //   LHSExp->IgnoreParenImpCasts()->getType()->getAs<TypedefType>()) {
  //     for (auto I = FunctionScopes.rbegin(), E =
  //     std::prev(FunctionScopes.rend()); I != E; ++I) {
  //       auto *CSI = dyn_cast<CapturingScopeInfo>(*I);
  //       if (CSI == nullptr) break;
  //       DeclContext *DC = nullptr;
  //       if (auto *LSI = dyn_cast<LambdaScopeInfo>(CSI))
  //         DC = LSI->CallOperator;
  //       else if (auto *CRSI = dyn_cast<CapturedRegionScopeInfo>(CSI))
  //         DC = CRSI->TheCapturedDecl;
  //       else if (auto *BSI = dyn_cast<BlockScopeInfo>(CSI))
  //         DC = BSI->TheDecl;
  //       if (DC) {
  //         if (DC->containsDecl(TT->getDecl()))
  //           break;
  //         captureVariablyModifiedType(
  //             Context, LHSExp->IgnoreParenImpCasts()->getType(), CSI);
  //       }
  //     }
  //   }
  // }
  return std::make_unique<ArraySubscriptExpr>(std::move(base), std::move(idx),
                                              rloc, result_type, vk);
}

UPtr<Expr> Sema::act_on_array_subscript_expr(Scope *scope, ExprUPtr base,
                                             Loc lb, ExprUPtr arg, Loc rb) {
  (void)scope;
  return create_builtin_array_subscript_expr(std::move(base), lb,
                                             std::move(arg), rb);
}

UPtr<DeclRefExpr> Sema::build_decl_ref_expr(ValueDecl *d, Type *ty,
                                            ValueKind vk, IdentInfo *ii,
                                            Loc loc, StructDecl *struct_scope) {
  (void)struct_scope;
  return std::make_unique<DeclRefExpr>(d, ii, loc, ty, vk);
}

ExprUPtr Sema::act_on_id_expression(Scope *scope, StructDecl *struct_scope,
                                    IdentInfo *ident, Loc loc) {
  NamedDecl *decl;
  if (struct_scope) {
    for (auto const &m : struct_scope->methods)
      if (m->name == ident) {
        decl = m;
        break;
      }
  } else {
    decl = scope->lookup_named_decl(ident);
  }
  if (!decl) {
    Diag(diag::ERROR, loc, "Undeclared identifier '%s'", ident->name.c_str());
    return nullptr;
  }

  if (!decl->isa<ValueDecl>()) {
    Diag(diag::ERROR, loc, "'%s' does not refer to a value",
         ident->name.c_str());
    return nullptr;
  }

  auto vd = decl->dyn_cast<ValueDecl>();
  auto ty = vd->type;
  if (!ty)
    return nullptr;
  auto vk = ValueKind::LVALUE;

  if (decl->isa<EnumVariantDecl>()) {
    vk = ValueKind::PRVALUE;
  }

  return build_decl_ref_expr(vd, ty, vk, ident, loc, struct_scope);
}

#include "constexpr.hpp"

i64 Sema::eval_integer_constexpr(Expr *e) { 
  auto [i, b] =  ::eval_integer_constexpr(ctx, e);
  if (!b) {
    Diag(diag::ERROR, e->get_start_loc(), "expected integer constant expression") << e->get_range();
    return 0;
  }
  return i;
}
