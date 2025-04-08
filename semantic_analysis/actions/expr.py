from typing import Tuple, Callable
from ns_ast.nodes import *
from semantic_analysis import TYPES, StringLiteralParser, NumLiteralParser, CharLiteralParser
from utils.diagnostic import diag, Diag
from .overload import ImplicitConversionSequence, perform_contextually_convert_to_bool, try_implicit_conversion
from . import state

def get_expr_range(e: Expr | None) -> Tuple[int, int]:
    if e is None: return (LOC_INVALID, LOC_INVALID)
    return e.get_range()

def correct_delayed_typos_in_expr_inner(er: Expr, init_decl, recover_uncorrected_typos: bool, filter_: Callable[[Expr], Expr],) -> Expr:
    # TODO:
    return er

def correct_delayed_typos_in_expr(er: Expr | None, init_decl=None, recover_uncorrected_typos: bool = False, filter_: Callable[[Expr], Expr] = lambda e: e,) -> Expr | None:
    if er is None:
        return None
    return correct_delayed_typos_in_expr_inner(
        er, init_decl, recover_uncorrected_typos, filter_
    )

def invalid_operands(loc: Loc, lhs: Expr, rhs: Expr):
    # OriginalOperand OrigLHS(LHS.get()), OrigRHS(RHS.get());
    diag(loc, f"invalid operands to binary expression ({lhs.ty} and {rhs.ty})", Diag.ERROR, [lhs.get_range(), rhs.get_range()])
    # if (OrigLHS.Conversion) Diag(OrigLHS.Conversion->getLocation(), diag::note_typecheck_invalid_operands_converted) << 0 << LHS.get()->getType();
    # if (OrigRHS.Conversion) Diag(OrigRHS.Conversion->getLocation(), diag::note_typecheck_invalid_operands_converted) << 1 << RHS.get()->getType();
    return lhs, rhs, None

def default_function_array_conversion(e: Expr, diagnose: bool = True) -> Expr:
    ty = e.ty
    assert ty is not None, "default_function_array_conversion - missing type"
    if isinstance(ty, FunctionType):
        # TODO: cast to function ptr
        # if (auto *DRE = dyn_cast<DeclRefExpr>(E->IgnoreParenCasts())) if (auto *FD = dyn_cast<FunctionDecl>(DRE->getDecl())) if (!checkAddressOfFunctionIsAvailable(FD, Diagnose, E->getExprLoc())) return ExprError();
        # E = ImpCastExprToType(E, Context.getPointerType(Ty), CK_FunctionToPointerDecay).get();
        pass
    elif isinstance(ty, ArrayType):
        e = imp_cast_expr_to_type(e, PointerType(ty.subtype), CastKind.ARRAY_TO_POINTER_DECAY)
    return e

def default_lvalue_conversion(e: Expr) -> Expr:
    if e.value_kind == ValueKind.PRVALUE:
        return e
    t = e.ty
    if isinstance(t, FunctionType) or isinstance(t, ArrayType) or t == Type():
        return e;
    # if (T->isRecordType()) return E;
    # CheckForNullPointerDereference(*this, E);
    ck = CastKind.LVALUE_TO_RVALUE # T->isNullPtrType() ? CK_NullToPointer : CK_LValueToRValue;
    return ImplicitCastExpr(t, ck, e, ValueKind.PRVALUE)

def default_function_array_lvalue_conversion(e: Expr, diagnose: bool = True) -> Expr:
    res = default_function_array_conversion(e, diagnose)
    if res is None:
        return None
    return default_lvalue_conversion(res)

def usual_unary_conversions(e: Expr) -> Expr:
    e = default_function_array_lvalue_conversion(e)
    if e is None:
        return None
    ty = e.ty
    assert ty is not None, "Missing type"

    # if (Ty->isIntegralOrUnscopedEnumerationType()) {
    #   QualType PTy = Context.isPromotableBitField(E);
    #   if (!PTy.isNull()) {
    #     E = ImpCastExprToType(E, PTy, CK_IntegralCast).get();
    #     return E;
    #   }
    #   if (Context.isPromotableIntegerType(Ty)) {
    #     QualType PT = Context.getPromotedIntegerType(Ty);
    #     E = ImpCastExprToType(E, PT, CK_IntegralCast).get();
    #     return E;
    #   }
    # }

    return e


def usual_arithmetic_conversions(lhs: Expr, rhs: Expr, loc: Loc, is_comp_assign: bool):
    # lhs, rhs = check_enum_arithmetic_conversions(lhs, rhs, loc, is_comp_assign)
    if not is_comp_assign:
        lhs = usual_unary_conversions(lhs)
        if lhs is None:
            return lhs, rhs, None
    rhs = usual_unary_conversions(rhs)
    if rhs is None:
        return lhs, rhs, None

    lhs_type = lhs.ty.get_unqualified()
    rhs_type = rhs.ty.get_unqualified()

    if lhs_type == rhs_type:    # if (Context.hasSameType(LHSType, RHSType)) return Context.getCommonSugaredType(LHSType, RHSType);
        return lhs, rhs, lhs_type
    # TODO: different types
    if not lhs_type.is_arithmetic_type() or not rhs_type.is_arithmetic_type():
        return lhs, rhs, None
    lhs_unpromoted_type = lhs_type
    if lhs_type == TYPES["bool"] or lhs_type == TYPES["i8"]: # if (Context.isPromotableIntegerType(LHSType)) LHSType = Context.getPromotedIntegerType(LHSType);
        lhs_type = TYPES["i64"]
    # QualType LHSBitfieldPromoteTy = Context.isPromotableBitField(LHS.get());
    # if (!LHSBitfieldPromoteTy.isNull()) LHSType = LHSBitfieldPromoteTy;
    if lhs_type != lhs_unpromoted_type and not is_comp_assign:
        lhs = imp_cast_expr_to_type(lhs, lhs_type, CastKind.INTEGRAL_CAST)
    if lhs_type == rhs_type: # if (Context.hasSameType(LHSType, RHSType)) return Context.getCommonSugaredType(LHSType, RHSType);
        return lhs, rhs, lhs_type
    # if (unsupportedTypeConversion(*this, LHSType, RHSType)) return QualType();
    # if (LHSType->isRealFloatingType() || RHSType->isRealFloatingType()) return handleFloatConversion(*this, LHS, RHS, LHSType, RHSType, ACK == ACK_CompAssign);
    # return handleIntegerConversion<doIntegralCast, doIntegralCast> (*this, LHS, RHS, LHSType, RHSType, ACK == ACK_CompAssign);
    # TODO: actual
    rhs = imp_cast_expr_to_type(rhs, lhs_type, CastKind.INTEGRAL_CAST)
    return lhs, rhs, lhs_type

def check_arithmetic_op_pointer_operand(loc: Loc, operand: Expr) -> bool:
    res_type = operand.ty
    if not isinstance(res_type, PointerType):
        return True

    pointee_ty = res_type.subtype
    # if (PointeeTy->isVoidType()) { diagnoseArithmeticOnVoidPointer(S, Loc, Operand); return False }
    # if (PointeeTy->isFunctionType()) { diagnoseArithmeticOnFunctionPointer(S, Loc, Operand); return False; }
    # if (checkArithmeticIncompletePointerType(S, Loc, Operand)) return false;
    return True

def check_array_access(base_expr: Expr, index_expr: Expr): #, const ArraySubscriptExpr *ASE, bool AllowOnePastEnd, bool IndexNegated):
    pass
    # if (isConstantEvaluatedContext()) return;
    # IndexExpr = IndexExpr->IgnoreParenImpCasts();
    # if (IndexExpr->isValueDependent()) return;
    #
    # const Type *EffectiveType = BaseExpr->getType()->getPointeeOrArrayElementType();
    # BaseExpr = BaseExpr->IgnoreParenCasts();
    # const ConstantArrayType *ArrayTy = Context.getAsConstantArrayType(BaseExpr->getType());
    # LangOptions::StrictFlexArraysLevelKind StrictFlexArraysLevel = getLangOpts().getStrictFlexArraysLevel();
    # const Type *BaseType = ArrayTy == nullptr ? nullptr : ArrayTy->getElementType().getTypePtr();
    # bool IsUnboundedArray = BaseType == nullptr || BaseExpr->isFlexibleArrayMemberLike(Context, StrictFlexArraysLevel, /*IgnoreTemplateOrMacroSubstitution=*/true);
    # if (EffectiveType->isDependentType() || (!IsUnboundedArray && BaseType->isDependentType())) return;
    # Expr::EvalResult Result;
    # if (!IndexExpr->EvaluateAsInt(Result, Context, Expr::SE_AllowSideEffects)) return;
    # llvm::APSInt index = Result.Val.getInt();
    # if (IndexNegated) { index.setIsUnsigned(false); index = -index; }
    # if (IsUnboundedArray) {
    #   if (EffectiveType->isFunctionType()) return;
    #   if (index.isUnsigned() || !index.isNegative()) {
    #     const auto &ASTC = getASTContext();
    #     unsigned AddrBits = ASTC.getTargetInfo().getPointerWidth(EffectiveType->getCanonicalTypeInternal().getAddressSpace());
    #     if (index.getBitWidth() < AddrBits) index = index.zext(AddrBits);
    #     std::optional<CharUnits> ElemCharUnits = ASTC.getTypeSizeInCharsIfKnown(EffectiveType);
    #     if (!ElemCharUnits || ElemCharUnits->isZero()) return;
    #     llvm::APInt ElemBytes(index.getBitWidth(), ElemCharUnits->getQuantity());
    #     if (index.getActiveBits() <= AddrBits) {
    #       bool Overflow;
    #       llvm::APInt Product(index);
    #       Product += 1;
    #       Product = Product.umul_ov(ElemBytes, Overflow);
    #       if (!Overflow && Product.getActiveBits() <= AddrBits)
    #         return;
    #     }
    #     llvm::APInt MaxElems = llvm::APInt::getMaxValue(AddrBits);
    #     MaxElems = MaxElems.zext(std::max(AddrBits + 1, ElemBytes.getBitWidth()));
    #     MaxElems += 1;
    #     ElemBytes = ElemBytes.zextOrTrunc(MaxElems.getBitWidth());
    #     MaxElems = MaxElems.udiv(ElemBytes);
    #     unsigned DiagID = ASE ? diag::warn_array_index_exceeds_max_addressable_bounds : diag::warn_ptr_arith_exceeds_max_addressable_bounds;
    #     DiagRuntimeBehavior(BaseExpr->getBeginLoc(), BaseExpr, PDiag(DiagID) << toString(index, 10, true) << AddrBits << (unsigned)ASTC.toBits(*ElemCharUnits) << toString(ElemBytes, 10, false) << toString(MaxElems, 10, false) << (unsigned)MaxElems.getLimitedValue(~0U) << IndexExpr->getSourceRange());
    #     const NamedDecl *ND = nullptr;
    #     while (const auto *ASE = dyn_cast<ArraySubscriptExpr>(BaseExpr)) BaseExpr = ASE->getBase()->IgnoreParenCasts();
    #     if (const auto *DRE = dyn_cast<DeclRefExpr>(BaseExpr)) ND = DRE->getDecl();
    #     if (const auto *ME = dyn_cast<MemberExpr>(BaseExpr)) ND = ME->getMemberDecl();
    #     if (ND) DiagRuntimeBehavior(ND->getBeginLoc(), BaseExpr, PDiag(diag::note_array_declared_here) << ND);
    #   }
    #   return;
    # }
    #
    # if (index.isUnsigned() || !index.isNegative()) {
    #   if (BaseType->isIncompleteType()) return;
    #   llvm::APInt size = ArrayTy->getSize();
    #   if (BaseType != EffectiveType) {
    #     uint64_t ptrarith_typesize = Context.getTypeSize(EffectiveType);
    #     uint64_t array_typesize = Context.getTypeSize(BaseType);
    #     if (!ptrarith_typesize) ptrarith_typesize = Context.getCharWidth();
    #     if (ptrarith_typesize != array_typesize) {
    #       uint64_t ratio = array_typesize / ptrarith_typesize;
    #       if (ptrarith_typesize * ratio == array_typesize) size *= llvm::APInt(size.getBitWidth(), ratio);
    #     }
    #   }
    #   if (size.getBitWidth() > index.getBitWidth()) index = index.zext(size.getBitWidth());
    #   else if (size.getBitWidth() < index.getBitWidth()) size = size.zext(index.getBitWidth());
    #   if (AllowOnePastEnd ? index.ule(size) : index.ult(size)) return;
    #   if (ASE) {
    #     SourceLocation RBracketLoc = SourceMgr.getSpellingLoc(ASE->getRBracketLoc());
    #     if (SourceMgr.isInSystemHeader(RBracketLoc)) {
    #       SourceLocation IndexLoc = SourceMgr.getSpellingLoc(IndexExpr->getBeginLoc());
    #       if (SourceMgr.isWrittenInSameFile(RBracketLoc, IndexLoc)) return;
    #     }
    #   }
    #   unsigned DiagID = ASE ? diag::warn_array_index_exceeds_bounds : diag::warn_ptr_arith_exceeds_bounds;
    #   unsigned CastMsg = (!ASE || BaseType == EffectiveType) ? 0 : 1;
    #   QualType CastMsgTy = ASE ? ASE->getLHS()->getType() : QualType();
    #   DiagRuntimeBehavior(BaseExpr->getBeginLoc(), BaseExpr, PDiag(DiagID) << toString(index, 10, true) << ArrayTy->desugar() << CastMsg << CastMsgTy << IndexExpr->getSourceRange());
    # } else {
    #   unsigned DiagID = diag::warn_array_index_precedes_bounds;
    #   if (!ASE) {
    #     DiagID = diag::warn_ptr_arith_precedes_bounds;
    #     if (index.isNegative()) index = -index;
    #   }
    #   DiagRuntimeBehavior(BaseExpr->getBeginLoc(), BaseExpr, PDiag(DiagID) << toString(index, 10, true) << IndexExpr->getSourceRange());
    # }
    #
    # const NamedDecl *ND = nullptr;
    # while (const auto *ASE = dyn_cast<ArraySubscriptExpr>(BaseExpr)) BaseExpr = ASE->getBase()->IgnoreParenCasts();
    # if (const auto *DRE = dyn_cast<DeclRefExpr>(BaseExpr)) ND = DRE->getDecl();
    # if (const auto *ME = dyn_cast<MemberExpr>(BaseExpr)) ND = ME->getMemberDecl();
    # if (ND) DiagRuntimeBehavior(ND->getBeginLoc(), BaseExpr, PDiag(diag::note_array_declared_here) << ND);

def check_addition_operands(lhs: Expr, rhs: Expr, tok_loc: Loc, opc: BinaryOperatorKind, comp_lhs_ty = None):
    lhs, rhs, comp_type = usual_arithmetic_conversions(lhs, rhs, tok_loc, comp_lhs_ty is not None);
    if lhs is None or rhs is None:
        return lhs, rhs, None
    # if opc == BinaryOperatorKind.ADD:
    #   diagnoseStringPlusInt(*this, Loc, LHS.get(), RHS.get());
    #   diagnoseStringPlusChar(*this, Loc, LHS.get(), RHS.get());
    if comp_type is not None and comp_type.is_arithmetic_type():
        # if (CompLHSTy) *CompLHSTy = comp_type;
        return lhs, rhs, comp_type
    pexp = lhs
    iexp = rhs
    if not isinstance(pexp.ty, PointerType):
        pexp, iexp = iexp, pexp
        if not isinstance(pexp.ty, PointerType):
            return invalid_operands(tok_loc, lhs, rhs);
    if not iexp.ty.is_integer_type():
        return invalid_operands(tok_loc, lhs, rhs);
    # if (PExp->IgnoreParenCasts()->isNullPointerConstant(Context, Expr::NPC_ValueDependentIsNotNull)) {
    #   Expr::EvalResult KnownVal;
    #   if ((!IExp->isValueDependent() && (!IExp->EvaluateAsInt(KnownVal, Context) || KnownVal.Val.getInt() != 0))) {
    #     bool IsGNUIdiom = BinaryOperator::isNullPointerArithmeticExtension(Context, BinaryOperatorKind.ADD, pexp, iexp);
    #     diagnoseArithmeticOnNullPointer(*this, Loc, pexp, IsGNUIdiom);
    #   }
    # }
    if not check_arithmetic_op_pointer_operand(tok_loc, pexp):
        return lhs, rhs, None
    check_array_access(pexp, iexp);
    # if (CompLHSTy) { QualType LHSTy = Context.isPromotableBitField(LHS.get()); if (LHSTy.isNull()) { LHSTy = LHS.get()->getType(); if (Context.isPromotableIntegerType(LHSTy)) LHSTy = Context.getPromotedIntegerType(LHSTy); } *CompLHSTy = LHSTy; }
    return lhs, rhs, pexp.ty

def check_subtraction_operands(lhs: Expr, rhs: Expr, loc: Loc, comp_lhs_ty = None):
    lhs, rhs, comp_type = usual_arithmetic_conversions(lhs, rhs, loc, comp_lhs_ty is not None);
    if lhs is None or rhs is None:
        return lhs, rhs, None
    if comp_type is not None and comp_type.is_arithmetic_type():
        # if (CompLHSTy) *CompLHSTy = comp_type;
        return lhs, rhs, comp_type

    if isinstance(lhs.ty, PointerType):
        # TODO:
        return lhs, rhs, lhs.ty

    return invalid_operands(loc, lhs, rhs)

def check_multiply_divide_operands(lhs: Expr, rhs: Expr, loc: Loc, is_comp_assign: bool, is_div: bool):
    lhs_ty = lhs.ty
    rhs_ty = rhs.ty
    lhs, rhs, comp_type = usual_arithmetic_conversions(lhs, rhs, loc, is_comp_assign)
    if lhs is None or rhs is None:
        return lhs, rhs, None
    if comp_type is None or not comp_type.is_arithmetic_type():
        return invalid_operands(loc, lhs, rhs)
    # if is_div:
    #     diagnose_bad_divide_or_remainder_values(lhs, rhs, loc, is_div)
    #     diagnose_division_size_of_pointer_or_array(lhs, rhs, loc)
    return lhs, rhs, comp_type

def check_remainder_operands(lhs: Expr, rhs: Expr, loc: Loc, is_comp_assign: bool = False):
    lhs, rhs, comp_type = usual_arithmetic_conversions(lhs, rhs, loc, is_comp_assign)
    if lhs is None or rhs is None:
        return lhs, rhs, None
    if comp_type is None or not comp_type.is_integer_type():
        return invalid_operands(loc, lhs, rhs)
    # diagnose_bad_divide_or_remainder_values(lhs, rhs, loc, False)
    return lhs, rhs, comp_type;

def check_shift_operands(lhs: Expr, rhs: Expr, loc: Loc, opc: BinaryOperatorKind, is_comp_assign: bool = False):
    old_lhs = lhs
    lhs = usual_unary_conversions(lhs)
    if lhs is None:
        return lhs, rhs, None
    lhs_type = lhs.ty
    if is_comp_assign:
        lhs = old_lhs;
    rhs = usual_unary_conversions(rhs)
    if rhs is None:
        return lhs, rhs, None
    rhs_type = rhs.ty
    # if (not lhs_type.is_fixed_point_or_integer_type() and not lhs_type.has_integer_representation()) or not rhs_type.has_integer_representation():
    #     return invalid_operands(loc, lhs, rhs)
    # if is_scoped_enumeration_type(lhs_type) or is_scoped_enumeration_type(rhs_type):
    #     return invalid_operands(loc, lhs, rhs)
    # diagnose_bad_shift_values(lhs, rhs, loc, opc, lhs_type);
    return lhs, rhs, lhs_type

def check_bitwise_operands(lhs: Expr, rhs: Expr, loc: Loc, opc: BinaryOperatorKind):
    is_comp_assign = opc == BinaryOperatorKind.ANDASSIGN or opc == BinaryOperatorKind.ORASSIGN or opc == BinaryOperatorKind.XORASSIGN
    # if opc == BinaryOperatorKind.AND: diagnose_logical_not_on_lhs_of_check(lhs, rhs, loc, opc)
    # if lhs.ty.has_floating_representation() or rhs.ty.has_floating_representation(): return invalid_operands(loc, lhs, rhs)
    lhs_result = lhs
    rhs_result = rhs
    lhs_result, rhs_result, comp_type = usual_arithmetic_conversions(lhs_result, rhs_result, loc, is_comp_assign)
    if lhs_result is None or rhs_result is None:
        return lhs, rhs, None
    lhs = lhs_result
    rhs = rhs_result
    # if opc == BinaryOperatorKind.XOR: diagnose_xor_misused_as_pow(lhs, rhs, loc)
    if comp_type is not None: # and comp_type.is_integral_or_unscoped_enumeration_type():
        return lhs, rhs, comp_type
    return invalid_operands(loc, lhs, rhs)

def check_logical_operands(lhs: Expr, rhs: Expr, loc: Loc, opc: BinaryOperatorKind):
    # bool EnumConstantInBoolContext = false;
    # for (const ExprResult &HS : {LHS, RHS}) {
    #     if (const auto *DREHS = dyn_cast<DeclRefExpr>(HS.get())) {
    #     const auto *ECDHS = dyn_cast<EnumConstantDecl>(DREHS->getDecl());
    #     if (ECDHS && ECDHS->getInitVal() != 0 && ECDHS->getInitVal() != 1)
    #         EnumConstantInBoolContext = true;
    #     }
    # }
    #
    # if (EnumConstantInBoolContext) Diag(Loc, diag::warn_enum_constant_in_bool_context);
    #
    # QualType LHSTy = LHS.get()->getType();
    # QualType RHSTy = RHS.get()->getType();
    # const auto *LHSATy = dyn_cast<ArrayType>(LHSTy);
    # const auto *RHSATy = dyn_cast<ArrayType>(RHSTy);
    #
    # if (!EnumConstantInBoolContext && LHS.get()->getType()->isIntegerType() && !LHS.get()->getType()->isBooleanType() && RHS.get()->getType()->isIntegerType()) {
    #     Expr::EvalResult EVResult;
    #     if (RHS.get()->EvaluateAsInt(EVResult, Context)) {
    #         llvm::APSInt Result = EVResult.Val.getInt();
    #         if (!RHS.get()->getType()->isBooleanType() || (Result != 0 && Result != 1)) {
    #             Diag(Loc, diag::warn_logical_instead_of_bitwise) << RHS.get()->getSourceRange() << (Opc == BO_LAnd ? "&&" : "||");
    #             Diag(Loc, diag::note_logical_instead_of_bitwise_change_operator) << (Opc == BO_LAnd ? "&" : "|") << FixItHint::CreateReplacement(SourceRange(Loc, getLocForEndOfToken(Loc)), Opc == BO_LAnd ? "&" : "|");
    #             if (Opc == BO_LAnd)
    #                 Diag(Loc, diag::note_logical_instead_of_bitwise_remove_constant) << FixItHint::CreateRemoval(SourceRange(getLocForEndOfToken(LHS.get()->getEndLoc()), RHS.get()->getEndLoc()));
    #         }
    #     }
    # }

    lhs_res = perform_contextually_convert_to_bool(lhs)
    if lhs_res is None:
        return invalid_operands(loc, lhs, rhs)
    lhs = lhs_res;

    rhs_res = perform_contextually_convert_to_bool(rhs)
    if rhs_res is None:
        return invalid_operands(loc, lhs, rhs)
    rhs = rhs_res;

    return lhs, rhs, TYPES["bool"]

def check_for_modifiable_lvalue(e: Expr, loc: Loc) -> bool:
    # TODO:
    if isinstance(e, DeclRefExpr):
        return False
    elif isinstance(e, ArraySubscriptExpr):
        return False
    elif isinstance(e, UnaryExpr) and e.opc == UnaryOperatorKind.DEREF:
        return False
    elif isinstance(e, MemberExpr):
        return False
    else:
        diag(loc, "not modifiable lvalue (TODO)", Diag.ERROR)
        return True

def check_assignment_operands(lhs: Expr, rhs: Expr, op_loc: Loc, compound_type: Type, opc: BinaryOperatorKind):
    if check_for_modifiable_lvalue(lhs, op_loc):
        return lhs, rhs, None

    lhs_type = lhs.ty
    rhs_type = compound_type if compound_type is not None else rhs.ty

    conv_ty = None
    if compound_type is None:
        # TODO:
        rhs = default_function_array_lvalue_conversion(rhs)
        # rhs_check = rhs
        # check_identity_field_assignment(lhs_expr, rhs_check, op_loc, *this);
        # conv_ty = check_single_assignment_constraints(lhs_type, rhs);
        # if rhs is None:
        #     return lhs, rhs, None
        # warn a =+ b
    else:
        # TODO:
        # conv_ty = check_assignment_constraints(loc, lhs_type, rhs_type)
        pass
    # if (DiagnoseAssignmentResult(ConvTy, op_loc, LHSType, RHSType, RHS.get(), AA_Assigning)) return QualType();
    # CheckForNullPointerDereference(*this, LHSExpr);
    # AssignedEntity AE{LHSExpr};
    # check_expr_lifetime(*this, AE, RHS.get());
    return lhs, rhs, lhs_type

def check_compare_operands(lhs: Expr, rhs: Expr, loc: Loc, opc: BinaryOperatorKind):
    is_relational = opc not in [BinaryOperatorKind.EQ, BinaryOperatorKind.NE]
    is_ordered = is_relational
  # auto IsAnyPointerType = [](ExprResult E) {
  #   QualType Ty = E.get()->getType();
  #   return Ty->isPointerType() || Ty->isMemberPointerType();
  # };
    lhs = default_function_array_lvalue_conversion(lhs)
    if lhs is None:
        return lhs, rhs, None
    rhs = default_function_array_lvalue_conversion(rhs)
    if rhs is None:
        return lhs, rhs, None

    return lhs, rhs, TYPES["bool"]
  #
  # checkArithmeticNull(*this, LHS, RHS, Loc, /*IsCompare=*/true);
  #
  # diagnoseLogicalNotOnLHSofCheck(*this, LHS, RHS, Loc, Opc);
  # diagnoseTautologicalComparison(*this, Loc, LHS.get(), RHS.get(), Opc);
  #
  # QualType LHSType = LHS.get()->getType();
  # QualType RHSType = RHS.get()->getType();
  # if ((LHSType->isArithmeticType() || LHSType->isEnumeralType()) && (RHSType->isArithmeticType() || RHSType->isEnumeralType()))
  #   return checkArithmeticOrEnumeralCompare(*this, LHS, RHS, Loc, Opc);
  #
  # const Expr::NullPointerConstantKind LHSNullKind = LHS.get()->isNullPointerConstant(Context, Expr::NPC_ValueDependentIsNull);
  # const Expr::NullPointerConstantKind RHSNullKind = RHS.get()->isNullPointerConstant(Context, Expr::NPC_ValueDependentIsNull);
  # bool LHSIsNull = LHSNullKind != Expr::NPCK_NotNull;
  # bool RHSIsNull = RHSNullKind != Expr::NPCK_NotNull;
  #
  # auto computeResultTy = [&]() {
  #   if (Opc != BO_Cmp)
  #     return Context.getLogicalOperationType();
  #   assert(Context.hasSameType(LHS.get()->getType(), RHS.get()->getType()));
  #
  #   QualType CompositeTy = LHS.get()->getType();
  #   assert(!CompositeTy->isReferenceType());
  #
  #   std::optional<ComparisonCategoryType> CCT =
  #       getComparisonCategoryForBuiltinCmp(CompositeTy);
  #   if (!CCT)
  #     return InvalidOperands(Loc, LHS, RHS);
  #
  #   if (CompositeTy->isPointerType() && LHSIsNull != RHSIsNull) {
  #     Diag(Loc, diag::err_typecheck_three_way_comparison_of_pointer_and_zero)
  #         << (LHSIsNull ? LHS.get()->getSourceRange() : RHS.get()->getSourceRange());
  #     return QualType();
  #   }
  #
  #   return CheckComparisonCategoryType(
  #       *CCT, Loc, ComparisonCategoryUsage::OperatorInExpression);
  # };
  #
  # if (!IsOrdered && LHSIsNull != RHSIsNull) {
  #   bool IsEquality = Opc == BO_EQ;
  #   if (RHSIsNull)
  #     DiagnoseAlwaysNonNullPointer(LHS.get(), RHSNullKind, IsEquality, RHS.get()->getSourceRange());
  #   else
  #     DiagnoseAlwaysNonNullPointer(RHS.get(), LHSNullKind, IsEquality, LHS.get()->getSourceRange());
  # }
  #
  # if (IsOrdered && LHSType->isFunctionPointerType() &&
  #     RHSType->isFunctionPointerType()) {
  #   bool IsError = Opc == BO_Cmp;
  #   auto DiagID = IsError ? diag::err_typecheck_ordered_comparison_of_function_pointers : diag::warn_typecheck_ordered_comparison_of_function_pointers
  #
  #   Diag(Loc, DiagID) << LHSType << RHSType << LHS.get()->getSourceRange() << RHS.get()->getSourceRange();
  #   if (IsError)
  #     return QualType();
  # }
  #
  # if !((LHSType->isIntegerType() && !LHSIsNull) || (RHSType->isIntegerType() && !RHSIsNull)):
  #   if (!IsOrdered &&
  #       ((LHSType->isFunctionPointerType() && RHSType->isVoidPointerType()) ||
  #        (RHSType->isFunctionPointerType() && LHSType->isVoidPointerType()))) {
  #     diagnoseFunctionPointerToVoidComparison(*this, Loc, LHS, RHS, /*isError*/ (bool)isSFINAEContext());
  #
  #     if (isSFINAEContext())
  #       return QualType();
  #
  #     RHS = ImpCastExprToType(RHS.get(), LHSType, CK_BitCast);
  #     return computeResultTy();
  #   }
  #
  #   if ((int)LHSType->isPointerType() + (int)RHSType->isPointerType() >=
  #           (IsOrdered ? 2 : 1) &&
  #       (!LangOpts.ObjCAutoRefCount || !(LHSType->isObjCObjectPointerType() ||
  #                                        RHSType->isObjCObjectPointerType()))) {
  #     if (convertPointersToCompositeType(*this, Loc, LHS, RHS))
  #       return QualType();
  #     return computeResultTy();
  #   }
  #
  # if (!IsOrdered && LHSIsNull && RHSIsNull) {
  #   if (LHSType->isNullPtrType()) {
  #     RHS = ImpCastExprToType(RHS.get(), LHSType, CK_NullToPointer);
  #     return computeResultTy();
  #   }
  #   if (RHSType->isNullPtrType()) {
  #     LHS = ImpCastExprToType(LHS.get(), RHSType, CK_NullToPointer);
  #     return computeResultTy();
  #   }
  # }
  #
  # if (!IsOrdered && RHSType->isNullPtrType() &&
  #     (LHSType->isObjCObjectPointerType() || LHSType->isBlockPointerType())) {
  #   RHS = ImpCastExprToType(RHS.get(), LHSType, CK_NullToPointer);
  #   return computeResultTy();
  # }
  # if (!IsOrdered && LHSType->isNullPtrType() &&
  #     (RHSType->isObjCObjectPointerType() || RHSType->isBlockPointerType())) {
  #   LHS = ImpCastExprToType(LHS.get(), RHSType, CK_NullToPointer);
  #   return computeResultTy();
  # }
  #
  # if (IsRelational &&
  #     ((LHSType->isNullPtrType() && RHSType->isPointerType()) ||
  #      (RHSType->isNullPtrType() && LHSType->isPointerType()))) {
  #   DeclContext *DC = CurContext;
  #   if (isa<FunctionDecl>(DC))
  #     DC = DC->getParent();
  #   if (auto *CTSD = dyn_cast<ClassTemplateSpecializationDecl>(DC)) {
  #     if (CTSD->isInStdNamespace() &&
  #         llvm::StringSwitch<bool>(CTSD->getName())
  #             .Cases("less", "less_equal", "greater", "greater_equal", true)
  #             .Default(false)) {
  #       if (RHSType->isNullPtrType())
  #         RHS = ImpCastExprToType(RHS.get(), LHSType, CK_NullToPointer);
  #       else
  #         LHS = ImpCastExprToType(LHS.get(), RHSType, CK_NullToPointer);
  #       return computeResultTy();
  #     }
  #   }
  # }
  #
  # if (!IsOrdered && (LHSType->isMemberPointerType() || RHSType->isMemberPointerType())) {
  #   if (convertPointersToCompositeType(*this, Loc, LHS, RHS))
  #     return QualType();
  #   else
  #     return computeResultTy();
  # }
  #
  # if (!IsOrdered && LHSType->isBlockPointerType() && RHSType->isBlockPointerType()) {
  #   QualType lpointee = LHSType->castAs<BlockPointerType>()->getPointeeType();
  #   QualType rpointee = RHSType->castAs<BlockPointerType>()->getPointeeType();
  #
  #   if (!LHSIsNull && !RHSIsNull && !Context.typesAreCompatible(lpointee, rpointee)) {
  #     Diag(Loc, diag::err_typecheck_comparison_of_distinct_blocks)
  #       << LHSType << RHSType << LHS.get()->getSourceRange() << RHS.get()->getSourceRange();
  #   }
  #   RHS = ImpCastExprToType(RHS.get(), LHSType, CK_BitCast);
  #   return computeResultTy();
  # }
  #
  # if (!IsOrdered && ((LHSType->isBlockPointerType() && RHSType->isPointerType()) || (LHSType->isPointerType() && RHSType->isBlockPointerType()))) {
  #   if (!LHSIsNull && !RHSIsNull) {
  #     if (!((RHSType->isPointerType() && RHSType->castAs<PointerType>()
  #            ->getPointeeType()->isVoidType())
  #           || (LHSType->isPointerType() && LHSType->castAs<PointerType>()
  #               ->getPointeeType()->isVoidType())))
  #       Diag(Loc, diag::err_typecheck_comparison_of_distinct_blocks)
  #         << LHSType << RHSType << LHS.get()->getSourceRange()
  #         << RHS.get()->getSourceRange();
  #   }
  #   if (LHSIsNull && !RHSIsNull)
  #     LHS = ImpCastExprToType(LHS.get(), RHSType, RHSType->isPointerType() ? CK_BitCast : CK_AnyPointerToBlockPointerCast);
  #   else
  #     RHS = ImpCastExprToType(RHS.get(), LHSType, LHSType->isPointerType() ? CK_BitCast : CK_AnyPointerToBlockPointerCast);
  #   return computeResultTy();
  # }
  #
  # if ((LHSType->isAnyPointerType() && RHSType->isIntegerType()) ||
  #     (LHSType->isIntegerType() && RHSType->isAnyPointerType())) {
  #   unsigned DiagID = 0;
  #   bool isError = false;
  #   if (LangOpts.DebuggerSupport) {
  #     // Under a debugger, allow the comparison of pointers to integers,
  #     // since users tend to want to compare addresses.
  #   } else if ((LHSIsNull && LHSType->isIntegerType()) ||
  #              (RHSIsNull && RHSType->isIntegerType())) {
  #     if (IsRelational) {
  #       isError = getLangOpts().CPlusPlus;
  #       DiagID =
  #         isError ? diag::err_typecheck_ordered_comparison_of_pointer_and_zero
  #                 : diag::ext_typecheck_ordered_comparison_of_pointer_and_zero;
  #     }
  #   } else if (getLangOpts().CPlusPlus) {
  #     DiagID = diag::err_typecheck_comparison_of_pointer_integer;
  #     isError = true;
  #   } else if (IsRelational)
  #     DiagID = diag::ext_typecheck_ordered_comparison_of_pointer_integer;
  #   else
  #     DiagID = diag::ext_typecheck_comparison_of_pointer_integer;
  #
  #   if (DiagID) {
  #     Diag(Loc, DiagID)
  #       << LHSType << RHSType << LHS.get()->getSourceRange()
  #       << RHS.get()->getSourceRange();
  #     if (isError)
  #       return QualType();
  #   }
  #
  #   if (LHSType->isIntegerType())
  #     LHS = ImpCastExprToType(LHS.get(), RHSType,
  #                       LHSIsNull ? CK_NullToPointer : CK_IntegralToPointer);
  #   else
  #     RHS = ImpCastExprToType(RHS.get(), LHSType,
  #                       RHSIsNull ? CK_NullToPointer : CK_IntegralToPointer);
  #   return computeResultTy();
  # }
  #
  # if (!IsRelational && RHSIsNull && LHSType->isBlockPointerType() && RHSType->isIntegerType()) {
  #   RHS = ImpCastExprToType(RHS.get(), LHSType, CK_NullToPointer);
  #   return computeResultTy();
  # }
  # if (!IsRelational && LHSIsNull && LHSType->isIntegerType() && RHSType->isBlockPointerType()) {
  #   LHS = ImpCastExprToType(LHS.get(), RHSType, CK_NullToPointer);
  #   return computeResultTy();
  # }
  #
  # return InvalidOperands(Loc, LHS, RHS);

def scalar_type_to_boolean_cast_kind(scalar_ty: Type) -> CastKind:
    if scalar_ty == TYPES["bool"]:
        return CastKind.NOOP
    elif scalar_ty.is_integer_type():
        return CastKind.INTEGRAL_TO_BOOLEAN
    elif isinstance(scalar_ty, PointerType):
        return CastKind.POINTER_TO_BOOLEAN
    else:
        assert False

def imp_cast_expr_to_type(e: Expr, ty: Type, kind: CastKind, vk: ValueKind = ValueKind.PRVALUE): # const CXXCastPath *BasePath, CheckedConversionKind CCK
    # diagnose_nullable_to_nonnull_conversion(ty, e.ty, e.get_range()[0])
    # diagnose_zero_to_nullptr_conversion(kind, e);
    # if (Context.hasAnyFunctionEffects() && !isCast(CCK) && kind != CK_NullToPointer && kind != CK_NullToMemberPointer)
    #   diagnoseFunctionEffectConversion(Ty, E->getType(), E->getBeginLoc());
    expr_ty = e.ty #get_canonical_type
    type_ty = ty#get_canonical_type

    if expr_ty == type_ty:
        return e

    if kind == CastKind.ARRAY_TO_POINTER_DECAY:
        if e.value_kind == ValueKind.PRVALUE:
            e = create_materialize_temporary_expr(e.ty, e, false)
            if e is None:
                return None

    if isinstance(e, ImplicitCastExpr):
        if e.kind == kind: # && (!BasePath || BasePath->empty()):
            e.ty = ty
            e.value_kind = vk
            return e

    return ImplicitCastExpr(ty, kind, e, vk) # BasePath, CurFPFeatureOverrides()

def build_bin_op(scope, tok_loc: Loc, opc: BinaryOperatorKind, lhs: Expr, rhs: Expr) -> BinaryExpr:
    vk = ValueKind.PRVALUE
    result_ty = None
    # check both type support
    comp_result_ty = None
    match opc:
        case BinaryOperatorKind.ASSIGN:
            lhs, rhs, result_ty = check_assignment_operands(lhs, rhs, tok_loc, None, opc)
            # vk = lhs.value_kind ### ???
            # if (!ResultTy.isNull()) { Diagnosessignment(*this, LHS.get(), RHS.get(), OpLoc, true); DiagnoseSelfMove(LHS.get(), RHS.get(), OpLoc); }
            # RecordModifiableNonNullParam(*this, LHS.get());
        case BinaryOperatorKind.MUL | BinaryOperatorKind.DIV:
            lhs, rhs, result_ty = check_multiply_divide_operands(lhs, rhs, tok_loc, False, opc == BinaryOperatorKind.DIV)
        case BinaryOperatorKind.REM:
            lhs, rhs, result_ty = check_remainder_operands(lhs, rhs, tok_loc)
        case BinaryOperatorKind.ADD:
            lhs, rhs, result_ty = check_addition_operands(lhs, rhs, tok_loc, opc)
        case BinaryOperatorKind.SUB:
            lhs, rhs, result_ty = check_subtraction_operands(lhs, rhs, tok_loc)
        case BinaryOperatorKind.SHL | BinaryOperatorKind.SHR:
            lhs, rhs, result_ty = check_shift_operands(lhs, rhs, tok_loc, opc)
        case BinaryOperatorKind.LE | BinaryOperatorKind.LT | BinaryOperatorKind.GE | BinaryOperatorKind.GT:
            lhs, rhs, result_ty = check_compare_operands(lhs, rhs, tok_loc, opc)
            # if isinstance(lhs, BinExpr) and lhs.is_comparison_op: diag(tok_loc, warn_consecutive_comparison)
        case BinaryOperatorKind.EQ | BinaryOperatorKind.NE:
            lhs, rhs, result_ty = check_compare_operands(lhs, rhs, tok_loc, opc)
        case BinaryOperatorKind.AND | BinaryOperatorKind.XOR | BinaryOperatorKind.OR:
            lhs, rhs, result_ty = check_bitwise_operands(lhs, rhs, tok_loc, opc)
        case BinaryOperatorKind.LAND | BinaryOperatorKind.LOR:
            lhs, rhs, result_ty = check_logical_operands(lhs, rhs, tok_loc, opc)
        case BinaryOperatorKind.MULASSIGN | BinaryOperatorKind.DIVASSIGN:
            lhs, rhs, comp_result_ty = check_multiply_divide_operands(lhs, rhs, tok_loc, True, opc == BinaryOperatorKind.DIV)
            if comp_result_ty is not None and lhs is not None and rhs is not None:
                lhs, rhs, result_ty = check_assignment_operands(lhs, rhs, tok_loc, comp_result_ty, opc)
        case BinaryOperatorKind.REMASSIGN:
            lhs, rhs, comp_result_ty = check_remainder_operands(lhs, rhs, tok_loc, True)
            if comp_result_ty is not None and lhs is not None and rhs is not None:
                lhs, rhs, result_ty = check_assignment_operands(lhs, rhs, tok_loc, comp_result_ty, opc)
        case BinaryOperatorKind.ADDASSIGN:
            lhs, rhs, comp_result_ty = check_addition_operands(lhs, rhs, tok_loc, opc, True)
            if comp_result_ty is not None and lhs is not None and rhs is not None:
                lhs, rhs, result_ty = check_assignment_operands(lhs, rhs, tok_loc, comp_result_ty, opc)
        case BinaryOperatorKind.SUBASSIGN:
            lhs, rhs, comp_result_ty = check_subtraction_operands(lhs, rhs, tok_loc, True)
            if comp_result_ty is not None and lhs is not None and rhs is not None:
                lhs, rhs, result_ty = check_assignment_operands(lhs, rhs, tok_loc, comp_result_ty, opc)
        case BinaryOperatorKind.SHLASSIGN | BinaryOperatorKind.SHRASSIGN:
            lhs, rhs, comp_result_ty = check_shift_operands(lhs, rhs, tok_loc, opc, True)
            if comp_result_ty is not None and lhs is not None and rhs is not None:
                lhs, rhs, result_ty = check_assignment_operands(lhs, rhs, tok_loc, comp_result_ty, opc)
        case BinaryOperatorKind.ANDASSIGN | BinaryOperatorKind.XORASSIGN | BinaryOperatorKind.ORASSIGN:
            # if or | and, diagnose_self_assignment
            lhs, rhs, comp_result_ty = check_bitwise_operands(lhs, rhs, tok_loc, opc)
            if comp_result_ty is not None and lhs is not None and rhs is not None:
                lhs, rhs, result_ty = check_assignment_operands(lhs, rhs, tok_loc, comp_result_ty, opc)
        case _:
            assert False, "Unhandled operator"
    if result_ty is None or lhs is None or rhs is None:
        return None
    if comp_result_ty is None:
        return BinaryExpr(lhs, rhs, opc, result_ty, vk, tok_loc)
    # vk = ValueKind.LVALUE ### ???
    return CompoundAssignExpr(lhs, rhs, opc, lhs.ty, vk, tok_loc)

def check_address_of_operand(e: Expr, op_loc: Loc):
    # TODO:
    assert e.value_kind == ValueKind.LVALUE
    return PointerType(e.ty)

def check_indirection_operand(e: Expr, op_loc: Loc, is_after_amp: bool = False) -> Type:
    op = usual_unary_conversions(e)
    if op is None:
        return None
    op_ty = op.ty
    result = None
    # if (isa<CXXReinterpretCastExpr>(Op)) { QualType OpOrigType = Op->IgnoreParenCasts()->getType(); S.CheckCompatibleReinterpretCast(OpOrigType, OpTy, /*IsDereference*/true, Op->getSourceRange()); }
    if isinstance(op_ty, PointerType):
        result = op_ty.subtype
    else:
        diag(op_loc, "diag::err_typecheck_indirection_requires_pointer) << OpTy << Op->getSourceRange();", Diag.ERROR)
        return None

    # Diagnose void* deref
    return result

def build_unary_op(scope, op_loc: Loc, opc: UnaryOperatorKind, arg: Expr, is_after_amp: bool = False) -> UnaryExpr:
    # TODO: checks and possible conversion (change type for '*' and '&')
    Input = arg
    vk = ValueKind.PRVALUE
    result_type = None
    can_overflow = False
    convert_half_vec = False

    match opc:
        case UnaryOperatorKind.PREINC | UnaryOperatorKind.PREDEC | UnaryOperatorKind.POSTINC | UnaryOperatorKind.POSTDEC:
            result_type = check_increment_decrement_operand(Input, vk, op_loc, opc == UnaryOperatorKind.PREINC or opc == UnaryOperatorKind.POSTINC, opc == UnaryOperatorKind.PREINC or opc == UnaryOperatorKind.PREDEC)
            can_overflow = is_overflowing_integer_type(context, result_type)
        case UnaryOperatorKind.ADDROF:
            result_type = check_address_of_operand(Input, op_loc)
            # check_address_of_no_deref(arg)
            # record_modifiable_non_null_param(arg)
        case UnaryOperatorKind.DEREF:
            Input = default_function_array_lvalue_conversion(Input)
            if Input is None:
                return None
            result_type = check_indirection_operand(Input, op_loc, is_after_amp)
            vk = ValueKind.LVALUE
        case UnaryOperatorKind.PLUS | UnaryOperatorKind.MINUS:
            can_overflow = opc == UnaryOperatorKind.MINUS # and is_overflowing_integer_type(context, Input.ty)
            Input = usual_unary_conversions(Input)
            if Input is None:
                return None
            result_type = Input.ty
            if result_type.is_arithmetic_type():
                pass
            elif opc == UnaryOperatorKind.PLUS and isinstance(result, PointerType):
                pass
            else:
                diag(op_loc, "diag::err_typecheck_unary_expr) << resultType << Input.get()->getSourceRange()", Diag.ERROR)
                return None
        case UnaryOperatorKind.NOT:
            Input = usual_unary_conversions(Input)
            if Input is None:
                return None
            result_type = Input.ty
            if not result_type.is_integer_type():
                diag(op_loc, "diag::err_typecheck_unary_expr) << resultType << Input.get()->getSourceRange());", Diag.ERROR)
                return None
        case UnaryOperatorKind.LNOT:
            Input = default_function_array_lvalue_conversion(Input)
            if Input is None:
                return None
            result_type = Input.ty
            if result_type.is_scalar_type(): #  and not isScopedEnumerationType(resultType):
                Input = imp_cast_expr_to_type(Input, TYPES["bool"], scalar_type_to_boolean_cast_kind(result_type))
            else:
                diag(op_loc, "diag::err_typecheck_unary_expr) << resultType << Input.get()->getSourceRange());", Diag.ERROR)
                return None
            result_type = TYPES["bool"]
    if result_type is None or Input is None:
        return None

    # if opc == UnaryOperatorKind.ADDROF or opc == UnaryOperatorKind.DEREF:
    #     check_array_access(Input)

    uo = UnaryExpr(Input, opc, result_type, vk, op_loc) # ok, can_overflow, cur_fp_feature_overrides()
    # if opc == UnaryOperatorKind.DEREF and not isinstance(uo.ty, ArrayType): ExprEvalContexts.back().PossibleDerefs.insert(UO);
    return uo

def act_on_bin_op(scope, tok_loc: Loc, kind: Tok, lhs: Expr, rhs: Expr) -> BinaryExpr:
    opc = BinaryOperatorKind.from_tok(kind)
    assert lhs is not None and rhs is not None, "missing operand to binary op"
    # diagnose_bin_op_precedence(opc, tok_loc, lhs_expr, rhs_expr)
    return build_bin_op(scope, tok_loc, opc, lhs, rhs)

def act_on_unary_op(scope, op_loc: Loc, op: Tok, arg: Expr, is_after_amp: bool = False) -> UnaryExpr:
    opc = UnaryOperatorKind.from_tok(op)
    return build_unary_op(scope, op_loc, opc, arg, is_after_amp)

def act_on_postfix_unary_op(scope, op_loc: Loc, op: Tok, arg: Expr) -> UnaryExpr:
    opc = 0
    if op == Tok.PLUSPLUS:
        opc = UnaryOperatorKind.POSTINC
    elif op == Tok.MINUSMINUS:
        opc = UnaryOperatorKind.POSTDEC
    else:
        assert False, "Unknown postfix unary operator"

    return build_unary_op(scope, op_loc, opc, arg)

def lookup_field_in_struct(struct_type: StructType, name: str):
    assert isinstance(struct_type, StructType)
    offset = 0
    for n, ty in struct_type.fields.items():
        al = ty.get_align()
        if (a := (offset % al)) != 0:
            offset += al - a
        if n == name:
            return (n, ty), offset
        offset += ty.get_size()
    return None, 0

def build_member_reference_expr(base: Expr, base_type: Type, oploc: Loc, is_arrow: bool, ss, first_qualifier_in_scope, name_info, scope, name: UnqualifiedId) -> Expr:
    # TODO:
    struct_type = None
    if is_arrow:
        if not (isinstance(base_type, PointerType) and isinstance(base_type.subtype, StructType)):
            diag(base.get_range()[0], "base expr of '->' is not a pointer to a struct", Diag.ERROR, [base.get_range()])
            assert False
        base = default_lvalue_conversion(base)
        struct_type = base_type.subtype
    else:
        if not isinstance(base_type, StructType):
            diag(base.get_range()[0], "base expr of '.' is not a struct", Diag.ERROR, [base.get_range()])
            assert False
        struct_type = base_type
    field, offset = lookup_field_in_struct(struct_type, name_info)
    if field is None:
        diag(name.start_location, f"unknown field '{name.value.val}' for '{struct_type}'", Diag.ERROR)
        assert False
    return MemberExpr(base, is_arrow, oploc, name_info, field[1], ValueKind.LVALUE, offset)

def act_on_member_access_expr(scope, base: Expr, oploc: Loc, opkind: Tok, ss, name: UnqualifiedId) -> Expr:
    # DeclarationNameInfo name_info
    name_info = name.value.val # set_name

    is_arrow = opkind == Tok.ARROW

    first_qualifier_in_scope = None #(!SS.isSet() ? nullptr : FindFirstQualifierInScope(S, SS.getScopeRep()));

    # result = maybe_convert_paren_list_expr_to_paren_expr(scope, base)
    # if result is None: return None
    # base = result

    res = build_member_reference_expr(base, base.ty, oploc, is_arrow, ss, first_qualifier_in_scope, name_info, scope, name);

    # if res is not None and isinstance(res, MemberExpr):
    #     check_member_access_of_no_deref(res)

    return res;

def create_builtin_array_subscript_expr(base: Expr, lloc: Loc, idx: Expr, rloc: Loc) -> Expr:
    lhs_expr = base
    rhs_expr = idx
    vk = ValueKind.LVALUE # ok = ORDINARY
    for op in [lhs_expr, rhs_expr]:
        # op = op.ignore_implicit()
        if isinstance(op.ty, ArrayType) and op.value_kind != ValueKind.LVALUE:
            vk = ValueKind.XValue

    lhs_expr = default_lvalue_conversion(lhs_expr)
    if lhs_expr is None:
        return None
    rhs_expr = default_function_array_lvalue_conversion(rhs_expr)
    if rhs_expr is None:
        return None
    lhs_ty, rhs_ty = lhs_expr.ty, rhs_expr.ty
    base_expr, index_expr = None, None
    result_type = None
    if isinstance(lhs_ty, PointerType):
        base_expr, index_expr = lhs_expr, rhs_expr
        result_type = lhs_ty.subtype
    elif isinstance(rhs_ty, PointerType):
        base_expr, index_expr = rhs_expr, lhs_expr
        result_type = rhs_ty.subtype
    elif isinstance(lhs_ty, ArrayType):
        # diag::ext_subscript_non_lvalue
        lhs_expr = imp_cast_expr_to_type(lhs_expr, PointerType(lhs_ty.subtype), CastKind.ARRAY_TO_POINTER_DECAY)
        lhs_ty = lhs_expr.ty
        base_expr, index_expr = rhs_expr, lhs_expr
        result_type = lhs_ty.subtype
    elif isinstance(rhs_ty, ArrayType):
        # diag::ext_subscript_non_lvalue
        rhs_expr = imp_cast_expr_to_type(rhs_expr, PointerType(rhs_ty.subtype), CastKind.ARRAY_TO_POINTER_DECAY)
        rhs_ty = rhs_expr.ty
        base_expr, index_expr = rhs_expr, lhs_expr
        result_type = rhs_ty.subtype
    else:
        diag(lloc, "subscripted value is not an array or a pointer", Diag.ERROR)
        return None
    # if not index_expr.ty.is_integer_type():
    #   return None # diag::err_typecheck_subscript_not_integer
    ## Warn if index is char constexpr
    ## error if result_type =is function type diag::err_subscript_function_type

    # if (RequireCompleteSizedType(LLoc, ResultType, diag::err_subscript_incomplete_or_sizeless_type, BaseExpr))
    #   return ExprError();

    # if (lhs_exp.ignore_paren_imp_casts().ty.is_variably_modified_type() && function_scopes.size() > 1) {
    #   if (auto *TT = LHSExp->IgnoreParenImpCasts()->getType()->getAs<TypedefType>()) {
    #     for (auto I = FunctionScopes.rbegin(), E = std::prev(FunctionScopes.rend()); I != E; ++I) {
    #       auto *CSI = dyn_cast<CapturingScopeInfo>(*I);
    #       if (CSI == nullptr) break;
    #       DeclContext *DC = nullptr;
    #       if (auto *LSI = dyn_cast<LambdaScopeInfo>(CSI))
    #         DC = LSI->CallOperator;
    #       else if (auto *CRSI = dyn_cast<CapturedRegionScopeInfo>(CSI))
    #         DC = CRSI->TheCapturedDecl;
    #       else if (auto *BSI = dyn_cast<BlockScopeInfo>(CSI))
    #         DC = BSI->TheDecl;
    #       if (DC) {
    #         if (DC->containsDecl(TT->getDecl()))
    #           break;
    #         captureVariablyModifiedType(
    #             Context, LHSExp->IgnoreParenImpCasts()->getType(), CSI);
    #       }
    #     }
    #   }
    # }

    return ArraySubscriptExpr(lhs_expr, rhs_expr, result_type, vk, rloc) # , ok


def gather_arguments_for_call(call_loc: Loc, fdecl: FnDecl, proto: FunctionType, args: List[Expr], all_args: List[Expr]) -> bool:
    num_params = len(proto.param_types)
    for i in range(len(args)):
        arg = args[i]
        if i < num_params:
            proto_arg_type = proto.param_types[i]
            param = fdecl.param_decls[i]
            if i >= len(args):
                return True
            # InitializedEntity Entity = InitializedEntity::InitializeParameter(Context, Param, proto_arg_type)
            # ExprResult ArgE = PerformCopyInitialization(Entity, SourceLocation(), Arg, false, false);
            # if (ArgE.isInvalid()) return true;
            # Arg = ArgE.getAs<Expr>();
            # CheckArrayAccess(Arg);
            # CheckStaticArrayArgument(CallLoc, Param, Arg);

            # TODO: actual
            ics = try_implicit_conversion(arg, proto_arg_type)
            arg = perform_implicit_conversion(arg, proto_arg_type, ics)
        else:
            arg = default_function_array_lvalue_conversion(arg)

        all_args.append(arg)

    return False

def convert_arguments_for_call(call: CallExpr, fn: Expr, fdecl: FnDecl, proto: FunctionType, args: List[Expr], rparen_loc: Loc, is_exec_config: bool) -> bool:
    # bool AddressOf = isParenthetizedAndQualifiedAddressOfExpr(Fn);
    num_params = len(proto.param_types)

    if len(args) < num_params:
        expected = ["expected", "expected at least"][fdecl.is_vararg]
        diag(rparen_loc, f"too few arguments to function call, {expected} {num_params}, have {len(args)}", Diag.ERROR, [fn.get_range()])
        diag(fdecl.get_range()[0], f"{fdecl.name} declared here", Diag.NOTE, [fdecl.get_params_range()])
        return True

    if len(args) > num_params and not fdecl.is_vararg:
        diag(args[num_params].get_range()[0], f"too many arguments to function call, expected {num_params}, have {len(args)}", Diag.ERROR, [fn.get_range()])
        diag(fdecl.get_range()[0], f"{fdecl.name} declared here", Diag.NOTE, [fdecl.get_params_range()])
        call.args = call.args[:num_params]
        return True

    all_args = []

    if gather_arguments_for_call(call.get_range()[0], fdecl, proto, args, all_args):
        return True

    for i, a in enumerate(all_args):
        call.args[i] = a

    return False

def build_resolved_call_expr(fn: Expr, ndecl: NamedDecl, lparen_loc: Loc, args: List[Expr], rparen_loc: Loc, is_exec_config: bool, uses_adl: int = 0) -> Expr:
    fnty = ndecl.ty
    assert isinstance(fnty, FunctionType)

    the_call = CallExpr(fn, args, fnty.return_type, ValueKind.PRVALUE, rparen_loc)

    if convert_arguments_for_call(the_call, fn, ndecl, fnty, args, rparen_loc, is_exec_config):
        return None

    return the_call

def build_call_expr(scope, fn: Expr, lparen_loc: Loc, arg_exprs: List[Expr], rparen_loc: Loc, is_exec_config: bool, allow_recovery: bool) -> Expr:
    # result = maybe_convert_paren_list_expr_to_paren_expr(scope, fn);
    # if result is None: return None
    # fn = result
    ## Fn isa PseudoDestructorExpr => if args not empty: diag::err_pseudo_dtor_call_with_args => CallExpr::Create VoidTy PRValue
    ## Fn isa RecordType => build_call_to_object_of_class_type(scope, fn, lparen_loc, arg_exprs, rparen_loc)
    ## Fn isa BoundMemberType => build_call_to_member_function(scpoe, fn, lparen_loc, arg_exprs, rparen_loc, is_exec_config, allow_recovery)
    ## Fn isa OverloadTy => TODO:

    naked_fn = fn.ignore_parens()

    calling_ndecl_indirectly = False;
    ndecl = None
    if isinstance(naked_fn, UnaryExpr):
        if naked_fn.opc == UnaryOperatorKind.ADDROF:
            calling_ndecl_indirectly = True;
            naked_fn = naked_fn.arg.ignore_parens()

    if isinstance(naked_fn, DeclRefExpr):
        ndecl = naked_fn.decl
        # diag(fn.get_range()[0], "Can only call functions", Diag.ERROR)
        # return None
        # if isinstance(ndecl, FnDecl) and ndecl.get_builtin_id():
        #     fdecl = rewrite_builtin_function_decl(context, fdecl, arg_exprs)
        #     if fdecl is not None:
        #         ndecl = fdecl
        #         fn = DeclRefExpr(context, fdecl.get_qualifier_loc(), 0, fdecl, False, 0, fdecl.ty, fn.value_kind, fdecl, None, dre.isnonodruse())
    else:
        diag(fn.get_range()[0], "Can only call functions", Diag.ERROR)
        return None
    # elif isinstance(naked_fn, MemberExpr):
    #     ndecl = naked_fn.get_member_decl()

    # if isinstance(ndecl, FnDecl):
    #     if calling_ndecl_indirectly and not check_address_of_function_is_available(ndecl, True, fn->get_range()[0]):
    #         return None
    #     check_direct_call_validity(fn, ndecl, arg_exprs);

    return build_resolved_call_expr(fn, ndecl, lparen_loc, arg_exprs, rparen_loc, is_exec_config);

def act_on_call_expr(scope, fn: Expr, lparen_loc: Loc, arg_exprs: List[Expr], rparen_loc: Loc) -> Expr:
    call = build_call_expr(scope, fn, lparen_loc, arg_exprs, rparen_loc, False, True)
    if call is None:
        return call

    # if (const auto *ULE = dyn_cast<UnresolvedLookupExpr>(Fn); ULE && ULE->hasExplicitTemplateArgs() && ULE->decls_begin() == ULE->decls_end()) {
    #     Diag(Fn->getExprLoc(), getLangOpts().CPlusPlus20 ? diag::warn_cxx17_compat_adl_only_template_id : diag::ext_adl_only_template_id) << ULE->getName();
    # }
    #
    # if (const auto *CE = dyn_cast<CallExpr>(Call.get())) {
    #     DiagnosedUnqualifiedCallsToStdFunctions(*this, CE);
    # }
    #
    # if (auto *DRE = dyn_cast<DeclRefExpr>(Fn->IgnoreParens()); DRE && Call.get()->isValueDependent()) {
    #     currentEvaluationContext().ReferenceToConsteval.erase(DRE);
    # }
    return call;

def act_on_array_subscript_expr(scope, base: Expr, lbloc: Loc, arg_exprs: List[Expr], rbloc: Loc) -> Expr:
    # if (((base->getType()->isRecordType() || (ArgExprs.size() != 1 || isa<PackExpansionExpr>(ArgExprs[0]) || ArgExprs[0]->getType()->isRecordType())))) {
    #   return CreateOverloadedArraySubscriptExpr(lbLoc, rbLoc, base, ArgExprs);
    # }

    assert len(arg_exprs) == 1, "Unsupported multiple arg subscript"

    res = create_builtin_array_subscript_expr(base, lbloc, arg_exprs[0], rbloc);

    # if res is not None and isinstance(res, ArraySubscriptExpr):
    #     check_subscript_access_of_no_deref(res)

    return res;

def perform_implicit_conversion(f: Expr, to_type: Type, ics: ImplicitConversionSequence): # Action, CCK
    # TODO: doit
    if ics.conversion_kind != 0:
        diag(f.get_range()[0], "Invalid conversion", Diag.ERROR, [f.get_range()])
        print(f, to_type)
    assert ics.conversion_kind == 0 # TODO:
    scs = ics.val
    from_type = f.ty
    initial_from_type = from_type
    from .overload import ImplicitConversionKind
    match scs.first:
        case ImplicitConversionKind.IDENTITY:
            pass
        case ImplicitConversionKind.LVALUE_TO_RVALUE:
            f = default_lvalue_conversion(f)
            if f is None:
                return None
            from_type = f.ty
        case ImplicitConversionKind.ARRAY_TO_POINTER:
            from_type = PointerType(from_type.subtype)
            f = imp_cast_expr_to_type(f, from_type, CastKind.ARRAY_TO_POINTER_DECAY, ValueKind.PRVALUE)
        case ImplicitConversionKind.FUNCTION_TO_POINTER:
            from_type = from_type.get_pointer_type()
            f = imp_cast_expr_to_type(f, from_type, CastKind.FUNCTION_TO_POINTER_DECAY)
        case _:
            assert False, "unreachable"
    match scs.second:
        case ImplicitConversionKind.IDENTITY:
            pass
        case ImplicitConversionKind.INTEGRAL_PROMOTION | ImplicitConversionKind.INTEGRAL_CONVERSION:
            el_ty = to_type
            step_ty = to_type
            if to_type == TYPES["bool"]:
                # assert(FromType->castAs<EnumType>()->getDecl()->isFixed() && SCS.Second == ICK_Integral_Promotion && "only enums with fixed underlying type can promote to bool");
                f = imp_cast_expr_to_type(f, to_type, CastKind.INTEGRAL_TO_BOOLEAN, ValueKind.PRVALUE)
            else:
                f = imp_cast_expr_to_type(f, to_type, CastKind.INTEGRAL_CAST, ValueKind.PRVALUE)
        # TODO: fixed point / floating point
        case ImplicitConversionKind.COMPATIBLE_CONVERSION:
            f = imp_cast_expr_to_type(f, to_type, CastKind.NOOP, f.value_kind)
        case ImplicitConversionKind.POINTER_CONVERSION:
            # TODO:
            f = imp_cast_expr_to_type(f, to_type, CastKind.NOOP, f.value_kind)
            #     QualType FromPteeType = From->getType()->getPointeeType();
            #     QualType ToPteeType = ToType->getPointeeType();
            #     QualType NewToType = ToType;
            #     if (!FromPteeType.isNull() && !ToPteeType.isNull() && FromPteeType.getAddressSpace() != ToPteeType.getAddressSpace()) {
            #       NewToType = Context.removeAddrSpaceQualType(ToPteeType);
            #       NewToType = Context.getAddrSpaceQualType(NewToType, FromPteeType.getAddressSpace());
            #       if (ToType->isObjCObjectPointerType()) NewToType = Context.getObjCObjectPointerType(NewToType);
            #       else if (ToType->isBlockPointerType()) NewToType = Context.getBlockPointerType(NewToType);
            #       else NewToType = Context.getPointerType(NewToType);
            #     }
            #     CastKind Kind;
            #     CXXCastPath BasePath;
            #     if (CheckPointerConversion(From, NewToType, Kind, BasePath, false)) return ExprError();
            #     if (Kind == CK_BlockPointerToObjCPointerCast) {
            #       ExprResult E = From;
            #       (void)ObjC().PrepareCastToObjCObjectPointer(E);
            #       From = E.get();
            #     }
            #     if (getLangOpts().allowsNonTrivialObjCLifetimeQualifiers()) ObjC().CheckObjCConversion(SourceRange(), NewToType, From, CCK);
            #     From = ImpCastExprToType(From, NewToType, Kind, VK_PRValue, &BasePath, CCK) .get();
        # TODO: pointer_member, derived_to_base
        case ImplicitConversionKind.BOOLEAN_CONVERSION:
            # if (From->getType()->isHalfType()) { From = ImpCastExprToType(From, Context.FloatTy, CK_FloatingCast).get(); FromType = Context.FloatTy; }
            f = imp_cast_expr_to_type(f, to_type, scalar_type_to_boolean_cast_kind(from_type), ValueKind.PRVALUE)
        case _:
            assert False, "unreachable"

    match scs.third:
        case ImplicitConversionKind.IDENTITY:
            pass
        case ImplicitConversionKind.FUNCTION_CONVERSION:
            assert False, "TODO: FUNCTION_CONVERSION"
        case ImplicitConversionKind.QUALIFICATION:
            assert False, "TODO: QUALIFICATION"
        case _:
            assert False, "unreachable"

    # if (!isCast(CCK)) diagnoseNullableToNonnullConversion(ToType, InitialFromType, From->getBeginLoc());
    return f

def check_boolean_condition(loc: Loc, cond_expr: Expr, is_constexpr: bool = False, fst_checks: bool = True) -> Expr | None:
    e = perform_contextually_convert_to_bool(cond_expr)
    if not is_constexpr or e is None:
        return e

    assert False, "Not implemented"
    # llvm::APSInt Cond;
    # E = VerifyIntegerConstantExpression(E.get(), &Cond, diag::err_constexpr_if_condition_expression_is_not_constant);
    # return E;


def check_conditional_operands(cond: Expr, lhs: Expr, rhs: Expr, vk: ValueKind, question_loc: Loc) -> Tuple[Type, Expr, Expr, Expr, ValueKind]:
    vk = ValueKind.PRVALUE # ok = ORDINARY
    cond_res = check_boolean_condition(0, cond);
    if cond_res is None: return None, cond, lhs, rhs, vk
    cond = cond_res;
    # TODO: Check value kind and type of lhs and rhs. For now, assume its the exact same
    assert lhs.ty == rhs.ty
    return lhs.ty, cond, lhs, rhs, lhs.value_kind

def act_on_conditional_op(question_loc: int, colon_loc: int, cond_expr: Expr, lhs: Expr, rhs: Expr) -> ConditionalExpr | None:
    lhs_ty = lhs.ty
    rhs_ty = rhs.ty
    vk = ValueKind.PRVALUE
    # ok = ORDINARY
    result, cond, lhs, rhs, vk = check_conditional_operands(cond_expr, lhs, rhs, vk, question_loc) #, ok
    if result is None or cond is None or lhs is None or rhs is None: return None
    ## emit warning when a conditional operator and binary operator are mixed in a way that suggests
    ## the programmer assumed the conditional operator has higher precedence,
    ## for example: "int x = a + someBinaryCondition ? 1 : 2".
    # diagnose_conditional_precedence(question_loc, cond, lhs, rhs)
    # result = compute_conditional_nullability(result, lhs_ty, rhs_ty, context)
    return ConditionalExpr(cond, question_loc, lhs, colon_loc, rhs, result, vk) #, ok

def act_on_numeric_constant(tok: Token, udl_scope=None) -> IntegerLiteral:
    parser = NumLiteralParser(tok)
    if parser.had_error:
        diag(tok.loc, "Error parsing numeric constant", Diag.ERROR)
    if parser.ty.is_integer_type():
        return IntegerLiteral(parser.res, parser.ty, tok.loc)
    assert False, "float not implemented"

def act_on_character_constant(tok: Token, udl_scope=None) -> IntegerLiteral:
    parser = CharLiteralParser(tok)
    if parser.had_error:
        diag(tok.loc, "Error parsing char constant", Diag.ERROR)
    if parser.ty.is_integer_type():
        return IntegerLiteral(parser.res, parser.ty, tok.loc)
    assert False, "incorrect type for char constant"

def act_on_bool_literal(op_loc: Loc, kind: Tok) -> BoolLiteral:
    assert kind == Tok.KW_TRUE or kind == Tok.KW_FALSE, "Unknown boolean value"
    return BoolLiteral(kind == Tok.KW_TRUE, TYPES["bool"], op_loc)

def act_on_string_literal(string_toks: List[Token]):
    assert len(string_toks) > 0, "Must have at least one string!"

    expanded_toks = []

    literal = StringLiteralParser(string_toks)

    if literal.had_error:
        return None

    string_tok_locs = [t.loc for t in string_toks]

    char_ty = TYPES["i8"]
    kind = "ordinary"
    # TODO: multiple char_ty and kind

    str_ty = ArrayType(char_ty, literal.get_num_string_chars())

    return StringLiteral(literal.get_string(), string_tok_locs, kind, str_ty)

def build_decl_ref_expr_nns(d: ValueDecl, ty: Type, vk: ValueKind, name: str, ii: IdentInfo, nameloc: Loc, nns) -> Expr:
    # bool RefersToCapturedVariable = isa<VarDecl, BindingDecl>(D) && NeedToCaptureVariable(D, NameInfo.getLoc());

    e = DeclRefExpr(nns, d, name, ii, nameloc, ty, vk)
                    #, getNonOdrUseReasonInCurrentContext(D)) ctx, nns, refers..., template ...
    # MarkDeclRefReferenced(E);

    # if (const auto *FPT = Ty->getAs<FunctionProtoType>())
    #   if (isUnresolvedExceptionSpec(FPT->getExceptionSpecType()))
    #     if (const auto *NewFPT = ResolveExceptionSpec(NameInfo.getLoc(), FPT)) E->setType(Context.getQualifiedType(NewFPT, Ty.getQualifiers()));

    # const auto *FD = dyn_cast<FieldDecl>(D);
    # if (const auto *IFD = dyn_cast<IndirectFieldDecl>(D)) FD = IFD->getAnonField();
    # if (FD) {
    #   UnusedPrivateFields.remove(FD);
    #   if (FD->isBitField()) E->setObjectKind(OK_BitField);
    # }

    # if (const auto *BD = dyn_cast<BindingDecl>(D)) if (const auto *BE = BD->getBinding()) E->setObjectKind(BE->getObjectKind());

    return e;

def build_decl_ref_expr(d: ValueDecl, ty: Type, vk: ValueKind, name: str, ii: IdentInfo, nameloc: Loc, ss) -> Expr:
    # get nns from ss
    return build_decl_ref_expr_nns(d, ty, vk, name, ii, nameloc, None)

def build_declaration_name_expr(ss, name: str, ii: IdentInfo, nameloc: Loc, d: NamedDecl, accept_invalid_decl = False) -> Expr:
    assert d is not None
    loc = nameloc
    # if check_decl_in_expr(loc, d, accept_invalid_decl):
    #     return create_recovery_expr(nameloc, nameloc, [])

    if not isinstance(d, ValueDecl):
        diag(nameloc, f"{name} does not refer to a value", Diag.ERROR)
        return None

    # if diagnose_use_of_decl(d, loc):
    #     return None

    # vd = ValueDecl(d)

    # if vd.is_invalid_decl() and not accept_invalid_decl:
    #     return None

    # if (auto *IndirectField = dyn_cast<IndirectFieldDecl>(VD); IndirectField && !IndirectField->isCXXClassMember()) return BuildAnonymousStructUnionMemberReference(SS, NameInfo.getLoc(), IndirectField);

    ty = d.ty
    if ty is None:
        return None
    vk = ValueKind.PRVALUE

    if isinstance(d, VarDecl) or isinstance(d, ParamDecl):
        vk = ValueKind.LVALUE
    if isinstance(d, EnumVariantDecl):
        vk = ValueKind.PRVALUE
        ty = ty.get_aliased_type() if ty.aliased_type is not None else ty
    # TODO: get correct vkind and type depending on decl kind

    e = build_decl_ref_expr(d, ty, vk, name, ii, nameloc, ss)
    # if (VD->isInvalidDecl() && E) return CreateRecoveryExpr(E->getBeginLoc(), E->getEndLoc(), {E});
    return e;

def act_on_id_expression(s, ss, i: UnqualifiedId, has_trailing_lparen: bool, is_address_of_operand: bool, is_inline_asm_identifier, keyword_replacement: Token) -> Expr:
    assert not (is_address_of_operand and has_trailing_lparen), "cannot be direct & operand and have a trailing lparen"
    # if (SS.isInvalid()) return ExprError();

    name = i.value.val
    ii = i.value
    nameloc = i.start_location

    lookup_decl = s.lookup_named_decl(name)

    if lookup_decl is None:
        diag(nameloc, f"Undeclared identifier '{name}'", Diag.ERROR)
        # fill keyword_replacement
        return None

    # if (isPotentialImplicitMemberAccess(SS, R, IsAddressOfOperand))
    #   return BuildPossibleImplicitMemberExpr(SS, 0, R, nullptr, S);

    return build_declaration_name_expr(ss, name, ii, nameloc, lookup_decl);

def act_on_paren_expr(l: Loc, r: Loc, e: Expr | None) -> ParenExpr:
    assert e is not None, "missing expr"
    # float stuff ?
    return ParenExpr(l, r, e)

def create_recovery_expr(
     begin: int, end: int, sub_exprs: List[Expr], t=Type()
) -> RecoveryExpr:
    # if not context.recovery_ast: return None
    # if is_sfinae_context(): return None
    # if t.is_null() or t->is_undeduced_type() or not context.recovery_ast_type: t = context.dependent_ty
    return RecoveryExpr(t, begin, end, sub_exprs)

def act_on_builtin_syscall_expr(builtin_tok, rparen_loc, args):
    out_args = []
    for a in args:
        out_args.append(default_function_array_lvalue_conversion(a))
    return BuiltinExpr(builtin_tok.value.val, builtin_tok.loc, out_args, rparen_loc)

def act_on_builtin_expr(builtin_tok, rparen_loc, args):
    match builtin_tok.ty:
        case Tok.BUILTIN_SYSCALL:
            return act_on_builtin_syscall_expr(builtin_tok, rparen_loc, args)
        case _:
            assert False, "unhandled builtin tok kind"


def check_completed_expr(e: Expr, check_loc: Loc, is_constexpr: bool):
    # llvm::SaveAndRestore ConstantContext(isConstantEvaluatedOverride, IsConstexpr || isa<ConstantExpr>(E));
    # CheckImplicitConversions(E, CheckLoc);
    # CheckUnsequencedOperations(E);
    # if not is_constexpr: check_for_int_overflow(e)
    # DiagnoseMisalignedMembers();
    pass

def maybe_create_expr_with_cleanups(sub_expr: Expr) -> Expr:
    assert sub_expr is not None, "subexpression can't be null!"
    # CleanupVarDeclMarking();
    # unsigned FirstCleanup = ExprEvalContexts.back().NumCleanupObjects;
    # assert(ExprCleanupObjects.size() >= FirstCleanup);
    # assert(Cleanup.exprNeedsCleanups() || ExprCleanupObjects.size() == FirstCleanup);
    # if (!Cleanup.exprNeedsCleanups()) return SubExpr;
    # auto Cleanups = llvm::ArrayRef(ExprCleanupObjects.begin() + FirstCleanup, ExprCleanupObjects.size() - FirstCleanup);
    # auto *E = ExprWithCleanups::Create(Context, SubExpr, Cleanup.cleanupsHaveSideEffects(), Cleanups);
    # DiscardCleanupsInEvaluationContext();
    # return E;
    return sub_expr

def act_on_finish_full_expr(fe: Expr, cc: Loc, discarded_value: bool, is_constexpr: bool = False):
    if fe is None:
        return None
    # if (DiagnoseUnexpandedParameterPack(fe)) return ExprError();

    if discarded_value:
        fe = ignored_value_conversions(fe)
        if fe is None:
            return None
        from .stmt import diagnose_unused_expr_result
        diagnose_unused_expr_result(fe, "diag::warn_unused_expr")

    # TODO: do it correctly in the correct place...
    if fe.value_kind != ValueKind.PRVALUE:
        fe = default_lvalue_conversion(fe)

    fe = correct_delayed_typos_in_expr(fe, None, True)
    if fe is None:
        return None

    check_completed_expr(fe, cc, is_constexpr)

    # LambdaScopeInfo *const CurrentLSI = getCurLambda(/*IgnoreCapturedRegions=*/true);
    # DeclContext *DC = CurContext;
    # while (isa_and_nonnull<CapturedDecl>(DC)) DC = DC->getParent();
    # const bool IsInLambdaDeclContext = isLambdaCallOperator(DC);
    # if (IsInLambdaDeclContext && CurrentLSI && CurrentLSI->hasPotentialCaptures() && !fe.isInvalid()) CheckIfAnyEnclosingLambdasMustCaptureAnyPotentialCaptures(FE, CurrentLSI, *this);
    return maybe_create_expr_with_cleanups(fe);

def ignored_value_conversions(e: Expr) -> Expr:
    # maybe_decrement_count(e, RefsMinusAssignments);
    return e


def act_on_nullptr_literal(loc: Loc) -> Expr:
    return IntegerLiteral(0, PointerType(Type()), loc)


def act_on_explicit_cast(ty: Type, e: Expr, sl: Loc, el: Loc) -> CastExpr:
    ics = try_implicit_conversion(e, ty, True)
    return perform_implicit_conversion(e, ty, ics)


def act_on_vaarg_expr(ty: Type, sl: Loc, el: Loc) -> VAArgExpr:
    fn = state.CUR_FN_DECL
    assert fn is not None
    if not fn.is_vararg:
        diag(sl, "cannot use vararg expr in non vararg function", Diag.ERROR)
        return None
    return VAArgExpr(ty, sl, el)


def act_on_sizeof_expr(ty: Type, expr: Expr | None, sl: Loc, el: Loc) -> SizeofExpr:
    return SizeofExpr(ty, expr, sl, el)
