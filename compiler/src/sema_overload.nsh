struct sema;

struct Type;
struct Expr;

enum ImplicitConversionKind {
  ICK_IDENTITY,
  ICK_LVALUE_TO_RVALUE,
  ICK_ARRAY_TO_POINTER,
  ICK_FUNCTION_TO_POINTER,
  ICK_FUNCTION_CONVERSION,
  ICK_QUALIFICATION,
  ICK_INTEGRAL_PROMOTION,
  ICK_FLOATING_PROMOTION,
  ICK_INTEGRAL_CONVERSION,
  ICK_FLOATING_CONVERSION,
  ICK_FLOATING_INTEGRAL,
  ICK_POINTER_CONVERSION,
  ICK_POINTER_MEMBER,
  ICK_BOOLEAN_CONVERSION,
  ICK_COMPATIBLE_CONVERSION,
  ICK_DERIVED_TO_BASE,
  ICK_ZERO_EVENT_CONVERSION,
  ICK_ZERO_QUEUE_CONVERSION,
  ICK_INCOMPATIBLE_POINTER_CONVERSION,
  ICK_FIXED_POINT_CONVERSION,
  ICK_NUM_CONVERSION_KINDS,
};

enum ImplicitConversionRank {
  ICR_EXACT_MATCH,
  ICR_PROMOTION,
  ICR_CONVERSION,
  ICR_C_CONVERSION_EXTENSION,
};

enum NarrowingKind {
  NK_NOT_NARROWING,
  NK_TYPE_NARROWING,
  NK_CONSTANT_NARROWING,
  NK_VARIABLE_NARROWING,
  NK_DEPENDENT_NARROWING,
};

lib fn ick_get_rank(self: ImplicitConversionKind) -> ImplicitConversionRank;


// @dataclass
// class StandardConversionSequence:
//     first: ImplicitConversionKind
//     second: ImplicitConversionKind
//     dimension: ImplicitConversionKind
//     third: ImplicitConversionKind
//     DeprecatedStringLiteralToCharPtr: bool
//     ReferenceBinding: bool
//     DirectBinding: bool
//     IsLvalueReference: bool
//     BindsToFunctionLvalue: bool
//     BindsToRvalue: bool
//     BindsImplicitObjectArgumentWithoutRefQualifier: bool
//     from_type_ptr: Type
//     to_type_ptrs: List[Type]  # 3
//     copy_constructor: None
//     # DeclAccessPair FoundCopyConstructor;
// 
//     def __init__(self):
//         self.first = ImplicitConversionKind.IDENTITY
//         self.second = ImplicitConversionKind.IDENTITY
//         self.dimension = ImplicitConversionKind.IDENTITY
//         self.third = ImplicitConversionKind.IDENTITY
//         self.DeprecatedStringLiteralToCharPtr = False
//         self.ReferenceBinding = False
//         self.DirectBinding = False
//         self.IsLvalueReference = False
//         self.BindsToFunctionLvalue = False
//         self.BindsToRvalue = False
//         self.BindsImplicitObjectArgumentWithoutRefQualifier = False
//         self.from_type_ptr = Type()
//         self.to_type_ptrs = [Type(), Type(), Type()]
//         # ...
// 
//     def set_from_type(self, t: Type):
//         self.from_type_ptr = t
// 
//     def set_to_type(self, idx: int, t: Type):
//         assert idx < 3, "To type index is out of range"
//         self.to_type_ptrs[idx] = t
// 
//     def set_all_to_types(self, t: Type):
//         self.to_type_ptrs[0] = t
//         self.to_type_ptrs[1] = t
//         self.to_type_ptrs[2] = t
// 
//     def get_from_type(self) -> Type:
//         return self.from_type_ptr
// 
//     def get_to_type(self, idx: int) -> Type:
//         assert idx < 3, "To type index is out of range"
//         return self.to_type_ptrs[idx]
// 
//     def set_as_identity_conversion(self):
//         self.first = ImplicitConversionKind.IDENTITY
//         self.second = ImplicitConversionKind.IDENTITY
//         self.dimension = ImplicitConversionKind.IDENTITY
//         self.third = ImplicitConversionKind.IDENTITY
//         self.DeprecatedStringLiteralToCharPtr = False
//         self.ReferenceBinding = False
//         self.DirectBinding = False
//         self.IsLvalueReference = True
//         self.BindsToFunctionLvalue = False
//         self.BindsToRvalue = False
//         self.BindsImplicitObjectArgumentWithoutRefQualifier = False
//         # self.CopyConstructor = nullptr;
// 
//     def is_identity_conversion(self) -> bool:
//         return (
//             self.second == ImplicitConversionKind.IDENTITY
//             and self.dimension == ImplicitConversionKind.IDENTITY
//             and self.third == ImplicitConversionKind.IDENTITY
//         )
// 
//     def get_rank(self) -> ImplicitConversionRank:
//         rank = ImplicitConversionRank.EXACT_MATCH
//         if (r := self.first.get_rank()).value > rank.value:
//             rank = r
//         if (r := self.second.get_rank()).value > rank.value:
//             rank = r
//         if (r := self.dimension.get_rank()).value > rank.value:
//             rank = r
//         if (r := self.third.get_rank()).value > rank.value:
//             rank = r
//         return rank
// 
//     def get_narrowing_kind(self) -> NarrowingKind:
//         # NarrowingKind getNarrowingKind(ASTContext &Context, const Expr *Converted, APValue &ConstantValue, QualType &ConstantType, bool IgnoreFloatToIntegralConversion = false) const;
//         # TODO:
//         return NarrowingKind.NOT_NARROWING
// 
//     def is_pointer_conversion_to_bool(self) -> bool:
//         return self.get_to_type(1) == TYPES["bool"] and (
//             isinstance(self.get_from_type(), PointerType)
//             or self.first == ImplicitConversionKind.ARRAY_TO_POINTER
//             or self.first == ImplicitConversionKind.FUNCTION_TO_POINTER
//         )
// 
//     def is_pointer_conversion_to_void_pointer(self) -> bool:
//         from_type = self.get_from_type()
//         to_type = self.get_to_type(1)
//         if self.first == ImplicitConversionKind.ARRAY_TO_POINTER:
//             assert isinstance(from_type, ArrayType)
//             from_type = PointerType(from_type.subtype)
//         if self.second == ImplicitConversionKind.POINTER_CONVERSION and isinstance(
//             from_type, PointerType
//         ):
//             if isinstance(to_type, PointerType):
//                 return type_is_void(to_type.subtype)
//         return False
// 
//     def dump(self):
//         printed_something = False
//         out = ""
//         if self.first != ImplicitConversionKind.IDENTITY:
//             out += self.first.name
//             printed_something = True
// 
//         if self.second != ImplicitConversionKind.IDENTITY:
//             if printed_something:
//                 out += " -> "
//             out += self.second.name
//             # copy_constructor / direct_binding / reference_binding
//             printed_something = True
// 
//         if self.third != ImplicitConversionKind.IDENTITY:
//             if printed_something:
//                 out += " -> "
//             out += self.third.name
//             printed_something = True
// 
//         if not printed_something:
//             return "No conversions required"
// 
//         return out
// 
//     def __str__(self):
//         return self.dump()


// @dataclass
// class UserDefinedConversionSequence:
//     a: int
// 
// 
// @dataclass
// class AmbiguousConversionSequence:
//     a: int
// 
//     def destruct(self):
//         pass
// 
// 
// @dataclass
// class BadConversionSequence:
//     a: int


struct ImplicitConversionSequence;

// @dataclass
// class ImplicitConversionSequence:
//     conversion_kind: (
//         int  # standard, static_obj_arg, user_def, ambig, ellips, bad, uninit
//     )
//     initializer_list_of_incomplete_array: bool
//     initializer_list_container_type: Type
//     val: Any
// 
//     def set_kind(self, k: int):
//         self.destruct()
//         self.conversion_kind = k
// 
//     def destruct(self):
//         if self.conversion_kind == 3:  # ambig
//             self.val.destruct()
// 
//     def __init__(self):
//         self.conversion_kind = 6
//         self.initializer_list_of_incomplete_array = False
//         self.initializer_list_container_type = Type()
//         self.val = StandardConversionSequence()
//         self.val.set_as_identity_conversion()
// 
//     # unsigned getKindRank() const {case StandardConversion: case StaticObjectArgumentConversion: return 0; case UserDefinedConversion: case AmbiguousConversion: return 1; case EllipsisConversion: return 2; case BadConversion: return 3;
//     # void setBad(BadConversionSequence::FailureKind Failure, Expr *FromExpr, QualType ToType) {setKind(BadConversion); Bad.init(Failure, FromExpr, ToType);}
//     # void setBad(BadConversionSequence::FailureKind Failure, QualType FromType, QualType ToType) {setKind(BadConversion); Bad.init(Failure, FromType, ToType);}
//     # void setStandard() { setKind(StandardConversion); }
//     # void setStaticObjectArgument() { setKind(StaticObjectArgumentConversion); }
//     # void setEllipsis() { setKind(EllipsisConversion); }
//     # void setUserDefined() { setKind(UserDefinedConversion); }
//     # void setAmbiguous() {if (ConversionKind == AmbiguousConversion) return; ConversionKind = AmbiguousConversion; Ambiguous.construct();}
//     # void setAsIdentityConversion(QualType T) {setStandard(); Standard.setAsIdentityConversion(); Standard.setFromType(T); Standard.setAllToTypes(T);}
//     # bool hasInitializerListContainerType() const {return !InitializerListContainerType.isNull();}
//     # void setInitializerListContainerType(QualType T, bool IA) {InitializerListContainerType = T; InitializerListOfIncompleteArray = IA;}
//     # bool isInitializerListOfIncompleteArray() const {return InitializerListOfIncompleteArray;}
//     # QualType getInitializerListContainerType() const {assert(hasInitializerListContainerType() && "not initializer list container"); return InitializerListContainerType;}
//     # static ImplicitConversionSequence getNullptrToBool(QualType SourceType, QualType DestType, bool NeedLValToRVal) {ImplicitConversionSequence ICS; ICS.setStandard(); ICS.Standard.setAsIdentityConversion(); ICS.Standard.setFromType(SourceType); if (NeedLValToRVal) ICS.Standard.First = ICK_Lvalue_To_Rvalue; ICS.Standard.setToType(0, SourceType); ICS.Standard.Second = ICK_Boolean_Conversion; ICS.Standard.setToType(1, DestType); ICS.Standard.setToType(2, DestType); return ICS;}
//     # enum CompareKind {Better = -1, Indistinguishable = 0, Worse = 1};
//     # void DiagnoseAmbiguousConversion(Sema &S, SourceLocation CaretLoc, const PartialDiagnostic &PDiag) const;
// 
//     def __str__(self):
//         return "ImplicitConversionSequence(" + self.dump() + ")"
// 
//     def dump(self):
//         out = ""
//         # if (hasInitializerListContainerType()) OS << "Worst list element conversion: ";
//         match self.conversion_kind:
//             case 0:
//                 out = out + f"Standard conversion: {self.val}"
//             case 2:
//                 out = out + f"User-defined conversion: {self.val}"
//             case 3:
//                 out = out + "Ambiguous conversion"
//             case 4:
//                 out = out + "Ellipsis conversion"
//             case 5:
//                 out = out + "Bad conversion"
//         return out
// 
// 
// def is_standard_conversion(
//     from_e: Expr, to_type: Type, scs: StandardConversionSequence, is_explicit: bool
// ):
//     from_type = from_e.ty
//     scs.set_as_identity_conversion()
//     scs.set_from_type(from_type)
//     scs.copy_constructor = None
// 
//     # if from_type.is_record_type() or to_type.is_record_type(): return False
// 
//     arg_is_lvalue = from_e.value_kind != ValueKind.PRVALUE
//     if arg_is_lvalue and not (
//         isinstance(from_type, FunctionType) or isinstance(from_type, ArrayType)
//     ):
//         scs.first = ImplicitConversionKind.LVALUE_TO_RVALUE
//         from_type = from_type.get_unqualified()
//     elif isinstance(from_type, ArrayType):
//         scs.first = ImplicitConversionKind.ARRAY_TO_POINTER
//         from_type = PointerType(from_type.subtype)
//         # if (S.IsStringLiteralToNonConstPointerConversion(From, ToType)) {
//         #     SCS.DeprecatedStringLiteralToCharPtr = true;
//         #     SCS.Second = ICK_Identity;
//         #     SCS.Third = ICK_Qualification;
//         #     SCS.setAllToTypes(FromType);
//         #     return true;
//         # }
//     elif isinstance(from_type, FunctionType) and arg_is_lvalue:
//         scs.first = ImplicitConversionKind.FUNCTION_TO_POINTER
//         # if (auto *DRE = dyn_cast<DeclRefExpr>(From->IgnoreParenCasts())) if (auto *FD = dyn_cast<FunctionDecl>(DRE->getDecl())) if (!S.checkAddressOfFunctionIsAvailable(FD)) return false;
//         from_type = from_type.get_pointer_type()
//     else:
//         scs.first = ImplicitConversionKind.IDENTITY
//     scs.set_to_type(0, from_type)
//     # second_ick = ImplicitConversionKind.IDENTITY
//     # dimension_ick = ImplicitConversionKind.IDENTITY
//     if from_type.get_unqualified() == to_type.get_unqualified():
//         scs.second = ImplicitConversionKind.IDENTITY
//     elif is_integral_promotion(from_e, from_type, to_type):
//         scs.second = ImplicitConversionKind.INTEGRAL_PROMOTION
//         from_type = to_type.get_unqualified()
//     elif is_floating_promotion(from_type, to_type):
//         scs.second = ImplicitConversionKind.FLOATING_PROMOTION
//         from_type = to_type.get_unqualified()
//     elif (
//         to_type == TYPES["bool"] and from_type.is_arithmetic_type()
//     ):  # || FromType->isAnyPointerType() || FromType->isBlockPointerType() || FromType->isMemberPointerType())):
//         scs.second = ImplicitConversionKind.BOOLEAN_CONVERSION
//         from_type = TYPES["bool"]
//     elif (
//         from_type == TYPES["i64"] and to_type == TYPES["i8"]
//     ):  # from_type.is_integral_or_unscoped_enumeration_type() and to_type.is_integral_type():
//         scs.second = ImplicitConversionKind.INTEGRAL_CONVERSION
//         from_type = to_type.get_unqualified()
//     elif is_floating_conversion(from_type, to_type):
//         scs.second = ImplicitConversionKind.FLOATING_CONVERSION
//         from_type = to_type.get_unqualified()
//     # elif ((FromType->isRealFloatingType() && ToType->isIntegralType(S.Context)) || (FromType->isIntegralOrUnscopedEnumerationType() && ToType->isRealFloatingType())):
//     #     scs.second = ImplicitConversionKind.FLOATING_INTEGRAL
//     #     from_type = to_type.get_unqualified()
//     elif is_pointer_conversion(from_e, from_type, to_type, is_explicit):
//         scs.second = ImplicitConversionKind.POINTER_CONVERSION
//         from_type = to_type.get_unqualified()
//     # elif is_member_pointer_conversion(from_e, from_type, to_type, False, from_type):
//     #     scs.second = ImplicitConversionKind.POINTER_MEMBER
//     # elif to_type.is_fixed_point_type() and from_type.is_convertible_to_fixed_point_type() or from_type.is_fixed_point_type() and to_type.is_convertible_to_fixed_point_type():
//     #     scs.second = ImplicitConversionKind.FIXED_POINT_CONVERSION
//     #     from_type = to_type;
//     else:
//         scs.second = ImplicitConversionKind.IDENTITY
//     scs.set_to_type(1, from_type)
// 
//     if is_function_conversion(from_type, to_type, from_type):
//         scs.third = ImplicitConversionKind.FUNCTION_CONVERSION
//     elif is_qualification_conversion(from_type, to_type, is_explicit):
//         scs.third = ImplicitConversionKind.QUALIFICATION
//         from_type = to_type
//     else:
//         scs.third = ImplicitConversionKind.IDENTITY
// 
//     scs.set_to_type(2, from_type)
//     return from_type == to_type
// 
// 
// def is_integral_promotion(from_e: Expr, from_type: Type, to_type: Type) -> bool:
//     _ = from_e  # UNUSED
//     if not isinstance(to_type, BuiltinType):
//         return False
// 
//     if from_type == TYPES["i8"] and to_type.kind == BuiltinTypeKind.I64:
//         return True
//     if from_type == TYPES["i16"] and to_type.kind == BuiltinTypeKind.I64:
//         return True
//     if from_type == TYPES["i32"] and to_type.kind == BuiltinTypeKind.I64:
//         return True
//     if from_type == TYPES["u8"] and to_type.kind == BuiltinTypeKind.I64:
//         return True
//     if from_type == TYPES["u16"] and to_type.kind == BuiltinTypeKind.I64:
//         return True
//     if from_type == TYPES["u32"] and to_type.kind == BuiltinTypeKind.I64:
//         return True
//     if from_type == TYPES["u64"] and to_type.kind == BuiltinTypeKind.I64:
//         return True
//     # if (Context.isPromotableIntegerType(FromType) and from_type != TYPES["bool"] and !FromType->isEnumeralType()) {
//     #    if ((FromType->isSignedIntegerType() || Context.getTypeSize(FromType) < Context.getTypeSize(ToType))) {
//     #        return To->getKind() == BuiltinType::Int;
//     #    }
//     #    return To->getKind() == BuiltinType::UInt;
// 
//     # enum types
//     # char types
//     # bitfields
//     if from_type == TYPES["bool"] and to_type.kind == BuiltinTypeKind.I64:
//         return True
//     return False
// 
// 
// def is_floating_promotion(from_type, to_type) -> bool:
//     # TODO:
//     _ = from_type, to_type  # UNUSED
//     return False
// 
// 
// def is_floating_conversion(from_type, to_type) -> bool:
//     # TODO:
//     _ = from_type, to_type  # UNUSED
//     return False
// 
// 
// def is_pointer_conversion(from_e, from_type, to_type, is_explicit) -> bool:
//     _ = from_e  # UNUSED
//     # TODO: cast ptr to int
//     if isinstance(from_type, PointerType) and to_type == TYPES["i64"] and is_explicit:
//         return True
//     # TODO: cast int to ptr
//     if isinstance(to_type, PointerType) and from_type == TYPES["i64"] and is_explicit:
//         return True
//     if not isinstance(from_type, PointerType) or not isinstance(to_type, PointerType):
//         return False
//     if is_explicit:
//         # TODO: always allow explicit pointer conversion ?
//         return True
//     # Check struct supertype
//     if isinstance(from_type.subtype, StructType):
//         f_ty = from_type.subtype
//         found = False
//         while isinstance(f_ty, StructType) and f_ty.super_type() is not None:
//             super_type = f_ty.super_type()
//             if super_type == to_type.subtype:
//                 found = True
//                 break
//             f_ty = super_type
//         if found:
//             return True
//     # Convert to or from void* is always allowed
//     if type_is_void(from_type.subtype) or type_is_void(to_type.subtype):
//         return True
//     return False
// 
// 
// def is_function_conversion(from_type, to_type, result_ty) -> bool:
//     # TODO:
//     _ = from_type, to_type, result_ty  # UNUSED
//     return False
// 
// 
// def is_qualification_conversion(from_type: Type, to_type: Type, c_style: bool):
//     # TODO:
//     _ = from_type, to_type, c_style  # UNUSED
//     return False


lib fn sema::try_implicit_conversion(e: Expr*, ty: Type*, is_explicit: bool) -> ImplicitConversionSequence*;
lib fn sema::try_contextually_convert_to_bool(val: Expr*) -> ImplicitConversionSequence*;
lib fn sema::perform_contextually_convert_to_bool(val: Expr*) -> Expr*;
