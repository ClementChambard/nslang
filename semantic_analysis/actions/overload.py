from ns_ast.nodes import *
from dataclasses import dataclass
from typing import List, Any
from utils.my_enum import Enum, ENUM_INIT, ENUM_N
from semantic_analysis import TYPES


class ImplicitConversionKind(Enum):
    IDENTITY = ENUM_INIT()
    LVALUE_TO_RVALUE = ENUM_N()
    ARRAY_TO_POINTER = ENUM_N()
    FUNCTION_TO_POINTER = ENUM_N()
    FUNCTION_CONVERSION = ENUM_N()
    QUALIFICATION = ENUM_N()
    INTEGRAL_PROMOTION = ENUM_N()
    FLOATING_PROMOTION = ENUM_N()
    INTEGRAL_CONVERSION = ENUM_N()
    FLOATING_CONVERSION = ENUM_N()
    FLOATING_INTEGRAL = ENUM_N()
    POINTER_CONVERSION = ENUM_N()
    POINTER_MEMBER = ENUM_N()
    BOOLEAN_CONVERSION = ENUM_N()
    COMPATIBLE_CONVERSION = ENUM_N()
    DERIVED_TO_BASE = ENUM_N()
    ZERO_EVENT_CONVERSION = ENUM_N()
    ZERO_QUEUE_CONVERSION = ENUM_N()
    INCOMPATIBLE_POINTER_CONVERSION = ENUM_N()
    FIXED_POINT_CONVERSION = ENUM_N()
    NUM_CONVERSION_KINDS = ENUM_N()

    def get_rank(self):
        return [ImplicitConversionRank.EXACT_MATCH,
                ImplicitConversionRank.EXACT_MATCH,
                ImplicitConversionRank.EXACT_MATCH,
                ImplicitConversionRank.EXACT_MATCH,
                ImplicitConversionRank.EXACT_MATCH,
                ImplicitConversionRank.EXACT_MATCH,
                ImplicitConversionRank.PROMOTION,
                ImplicitConversionRank.PROMOTION,
                ImplicitConversionRank.CONVERSION,
                ImplicitConversionRank.CONVERSION,
                ImplicitConversionRank.CONVERSION,
                ImplicitConversionRank.CONVERSION,
                ImplicitConversionRank.CONVERSION,
                ImplicitConversionRank.CONVERSION,
                ImplicitConversionRank.CONVERSION,
                ImplicitConversionRank.CONVERSION,
                ImplicitConversionRank.EXACT_MATCH,
                ImplicitConversionRank.EXACT_MATCH,
                ImplicitConversionRank.C_CONVERSION_EXTENSION,
                ImplicitConversionRank.CONVERSION][self.value]


class ImplicitConversionRank(Enum):
    EXACT_MATCH = ENUM_INIT()
    PROMOTION = ENUM_N()
    CONVERSION = ENUM_N()
    C_CONVERSION_EXTENSION = ENUM_N()


class NarrowingKind(Enum):
    NOT_NARROWING = ENUM_INIT()
    TYPE_NARROWING = ENUM_N()
    CONSTANT_NARROWING = ENUM_N()
    VARIABLE_NARROWING = ENUM_N()
    DEPENDENT_NARROWING = ENUM_N()


@dataclass
class StandardConversionSequence:
    first: ImplicitConversionKind
    second: ImplicitConversionKind
    dimension: ImplicitConversionKind
    third: ImplicitConversionKind
    DeprecatedStringLiteralToCharPtr: bool
    ReferenceBinding: bool
    DirectBinding: bool
    IsLvalueReference: bool
    BindsToFunctionLvalue: bool
    BindsToRvalue: bool
    BindsImplicitObjectArgumentWithoutRefQualifier: bool
    from_type_ptr: Type
    to_type_ptrs: List[Type] # 3
    # CXXConstructorDecl *CopyConstructor;
    # DeclAccessPair FoundCopyConstructor;

    def __init__(self):
        self.first = ImplicitConversionKind.IDENTITY
        self.second = ImplicitConversionKind.IDENTITY
        self.dimension = ImplicitConversionKind.IDENTITY
        self.third = ImplicitConversionKind.IDENTITY
        self.DeprecatedStringLiteralToCharPtr = False
        self.ReferenceBinding = False
        self.DirectBinding = False
        self.IsLvalueReference = False
        self.BindsToFunctionLvalue = False
        self.BindsToRvalue = False
        self.BindsImplicitObjectArgumentWithoutRefQualifier = False
        self.from_type_ptr = None
        self.to_type_ptrs = [None, None, None]
        # ...

    def set_from_type(self, t: Type):
        self.from_type_ptr = t

    def set_to_type(self, idx: int, t: Type):
        assert idx < 3, "To type index is out of range"
        self.to_type_ptrs[idx] = t

    def set_all_to_types(self, t: Type):
        self.to_type_ptrs[0] = t
        self.to_type_ptrs[1] = t
        self.to_type_ptrs[2] = t

    def get_from_type(self) -> Type:
        return self.from_type_ptr

    def get_to_type(self, idx: int) -> Type:
        assert idx < 3, "To type index is out of range"
        return self.to_type_ptrs[idx]

    def set_as_identity_conversion(self):
        self.first = ImplicitConversionKind.IDENTITY
        self.second = ImplicitConversionKind.IDENTITY
        self.dimension = ImplicitConversionKind.IDENTITY
        self.third = ImplicitConversionKind.IDENTITY
        self.DeprecatedStringLiteralToCharPtr = False
        self.ReferenceBinding = False
        self.DirectBinding = False
        self.IsLvalueReference = True;
        self.BindsToFunctionLvalue = False
        self.BindsToRvalue = False
        self.BindsImplicitObjectArgumentWithoutRefQualifier = False
        # self.CopyConstructor = nullptr;

    def is_identity_conversion(self) -> bool:
        return self.second == ICK_Identity and self.dimension == ICK_Identity and self.third == ICK_Identity

    def get_rank(self) -> ImplicitConversionRank:
        rank = ImplicitConversionRank.EXACT_MATCH
        if (r := self.first.get_rank()) > rank:
            rank = r
        if (r := self.second.get_rank()) > rank:
            rank = r
        if (r := self.dimension.get_rank()) > rank:
            rank = r
        if (r := self.third.get_rank()) > rank:
            rank = r
        return rank

    def get_narrowing_kind(self) -> NarrowingKind:
        # NarrowingKind getNarrowingKind(ASTContext &Context, const Expr *Converted, APValue &ConstantValue, QualType &ConstantType, bool IgnoreFloatToIntegralConversion = false) const;
        # TODO:
        return NarrowingKind.NOT_NARROWING


    def is_pointer_conversion_to_bool(self) -> bool:
        return self.get_to_type(1) == TYPES["bool"] and (isinstance(self.get_from_type(), PointerType) or self.first == ImplicitConversionKind.ARRAY_TO_POINTER or self.first == ImplicitConversionKind.FUNCTION_TO_POINTER)

    def is_pointer_conversion_to_void_pointer(self) -> bool:
        from_type = self.get_from_type()
        to_type = self.get_to_type(1)
        if self.first == ImplicitConversionKind.ARRAY_TO_POINTER:
            from_type = PointerType(from_type.subtype)
        if self.second == ImplicitConversionKind.POINTER_CONVERSION and isinstance(from_type, PointerType):
            if isinstance(to_type, PointerType):
                return type_is_void(to_type.subtype)
        return False

    def dump(self):
        printed_something = False
        out = ""
        if self.first != ImplicitConversionKind.IDENTITY:
            out += self.first.name
            printed_something = True

        if self.second != ImplicitConversionKind.IDENTITY:
            if printed_something:
                out += " -> "
            out += self.second.name
            # copy_constructor / direct_binding / reference_binding
            printed_something = True

        if self.third != ImplicitConversionKind.IDENTITY:
            if printed_something:
                out += " -> "
            out += self.third.name
            printed_something = True

        if not printed_something:
            return "No conversions required"

        return out

    def __str__(self):
        return self.dump()


@dataclass
class UserDefinedConversionSequence:
    a: int


@dataclass
class AmbiguousConversionSequence:
    a: int
    def destruct(self):
        pass


@dataclass
class BadConversionSequence:
    a: int


@dataclass
class ImplicitConversionSequence:
    conversion_kind: int # standard, static_obj_arg, user_def, ambig, ellips, bad, uninit
    initializer_list_of_incomplete_array: bool
    initializer_list_container_type: Type
    val: Any

    def set_kind(self, k: int):
        self.destruct()
        self.conversion_kind = k

    def destruct(self):
        if conversion_kind == 3: # ambig
            self.val.destruct()

    def __init__(self):
        self.conversion_kind = 6
        self.initializer_list_of_incomplete_array = False
        self.initializer_list_container_type = None
        self.val = StandardConversionSequence()
        self.val.set_as_identity_conversion()

    # unsigned getKindRank() const {case StandardConversion: case StaticObjectArgumentConversion: return 0; case UserDefinedConversion: case AmbiguousConversion: return 1; case EllipsisConversion: return 2; case BadConversion: return 3;
    # void setBad(BadConversionSequence::FailureKind Failure, Expr *FromExpr, QualType ToType) {setKind(BadConversion); Bad.init(Failure, FromExpr, ToType);}
    # void setBad(BadConversionSequence::FailureKind Failure, QualType FromType, QualType ToType) {setKind(BadConversion); Bad.init(Failure, FromType, ToType);}
    # void setStandard() { setKind(StandardConversion); }
    # void setStaticObjectArgument() { setKind(StaticObjectArgumentConversion); }
    # void setEllipsis() { setKind(EllipsisConversion); }
    # void setUserDefined() { setKind(UserDefinedConversion); }
    # void setAmbiguous() {if (ConversionKind == AmbiguousConversion) return; ConversionKind = AmbiguousConversion; Ambiguous.construct();}
    # void setAsIdentityConversion(QualType T) {setStandard(); Standard.setAsIdentityConversion(); Standard.setFromType(T); Standard.setAllToTypes(T);}
    # bool hasInitializerListContainerType() const {return !InitializerListContainerType.isNull();}
    # void setInitializerListContainerType(QualType T, bool IA) {InitializerListContainerType = T; InitializerListOfIncompleteArray = IA;}
    # bool isInitializerListOfIncompleteArray() const {return InitializerListOfIncompleteArray;}
    # QualType getInitializerListContainerType() const {assert(hasInitializerListContainerType() && "not initializer list container"); return InitializerListContainerType;}
    # static ImplicitConversionSequence getNullptrToBool(QualType SourceType, QualType DestType, bool NeedLValToRVal) {ImplicitConversionSequence ICS; ICS.setStandard(); ICS.Standard.setAsIdentityConversion(); ICS.Standard.setFromType(SourceType); if (NeedLValToRVal) ICS.Standard.First = ICK_Lvalue_To_Rvalue; ICS.Standard.setToType(0, SourceType); ICS.Standard.Second = ICK_Boolean_Conversion; ICS.Standard.setToType(1, DestType); ICS.Standard.setToType(2, DestType); return ICS;}
    # enum CompareKind {Better = -1, Indistinguishable = 0, Worse = 1};
    # void DiagnoseAmbiguousConversion(Sema &S, SourceLocation CaretLoc, const PartialDiagnostic &PDiag) const;

    def __str__(self):
        return "ImplicitConversionSequence(" + self.dump() + ")"

    def dump(self):
        out = ""
        # if (hasInitializerListContainerType()) OS << "Worst list element conversion: ";
        match self.conversion_kind:
            case 0: out = out + f"Standard conversion: {self.val}"
            case 2: out = out + f"User-defined conversion: {self.val}"
            case 3: out = out + f"Ambiguous conversion"
            case 4: out = out + f"Ellipsis conversion"
            case 5: out = out + f"Bad conversion"
        return out


def is_standard_conversion(from_e: Expr, to_type: Type, scs: StandardConversionSequence, c_style: bool):
    from_type = from_e.ty
    scs.set_as_identity_conversion()
    scs.set_from_type(from_type)
    scs.copy_constructor = None

    # if from_type.is_record_type() or to_type.is_record_type(): return False

    arg_is_lvalue = from_e.value_kind != ValueKind.PRVALUE
    if arg_is_lvalue and not (isinstance(from_type, FunctionType) or isinstance(from_type, ArrayType)):
        scs.first = ImplicitConversionKind.LVALUE_TO_RVALUE
        from_type = from_type.get_unqualified()
    elif isinstance(from_type, ArrayType):
        scs.first = ImplicitConversionKind.ARRAY_TO_POINTER
        from_type = PointerType(from_type.subtype)
        # if (S.IsStringLiteralToNonConstPointerConversion(From, ToType)) {
        #     SCS.DeprecatedStringLiteralToCharPtr = true;
        #     SCS.Second = ICK_Identity;
        #     SCS.Third = ICK_Qualification;
        #     SCS.setAllToTypes(FromType);
        #     return true;
        # }
    elif isinstance(from_type, FunctionType) and arg_is_lvalue:
        scs.first = ImplicitConversionKind.FUNCTION_TO_POINTER
        # if (auto *DRE = dyn_cast<DeclRefExpr>(From->IgnoreParenCasts())) if (auto *FD = dyn_cast<FunctionDecl>(DRE->getDecl())) if (!S.checkAddressOfFunctionIsAvailable(FD)) return false;
        from_type = from_type.get_pointer_type()
    else:
        scs.first = ImplicitConversionKind.IDENTITY
    scs.set_to_type(0, from_type)
    second_ick = ImplicitConversionKind.IDENTITY
    dimension_ick = ImplicitConversionKind.IDENTITY
    if from_type.get_unqualified() == to_type.get_unqualified():
        scs.second = ImplicitConversionKind.IDENTITY
    elif is_integral_promotion(from_e, from_type, to_type):
        scs.second = ImplicitConversionKind.INTEGRAL_PROMOTION
        from_type = to_type.get_unqualified()
    elif is_floating_promotion(from_type, to_type):
        scs.second = ImplicitConversionKind.FLOATING_PROMOTION
        from_type = to_type.get_unqualified()
    elif to_type == TYPES["bool"] and from_type.is_arithmetic_type(): # || FromType->isAnyPointerType() || FromType->isBlockPointerType() || FromType->isMemberPointerType())):
        scs.second = ImplicitConversionKind.BOOLEAN_CONVERSION
        from_type = TYPES["bool"]
    elif from_type == TYPES["i64"] and to_type == TYPES["i8"]: # from_type.is_integral_or_unscoped_enumeration_type() and to_type.is_integral_type():
        scs.second = ImplicitConversionKind.INTEGRAL_CONVERSION
        from_type = to_type.get_unqualified()
    elif is_floating_conversion(from_type, to_type):
        scs.second = ImplicitConversionKind.FLOATING_CONVERSION
        from_type = to_type.get_unqualified()
    # elif ((FromType->isRealFloatingType() && ToType->isIntegralType(S.Context)) || (FromType->isIntegralOrUnscopedEnumerationType() && ToType->isRealFloatingType())):
    #     scs.second = ImplicitConversionKind.FLOATING_INTEGRAL
    #     from_type = to_type.get_unqualified()
    elif is_pointer_conversion(from_e, from_type, to_type, False):
        scs.second = ImplicitConversionKind.POINTER_CONVERSION
        from_type = to_type.get_unqualified()
    # elif is_member_pointer_conversion(from_e, from_type, to_type, False, from_type):
    #     scs.second = ImplicitConversionKind.POINTER_MEMBER
    # elif to_type.is_fixed_point_type() and from_type.is_convertible_to_fixed_point_type() or from_type.is_fixed_point_type() and to_type.is_convertible_to_fixed_point_type():
    #     scs.second = ImplicitConversionKind.FIXED_POINT_CONVERSION
    #     from_type = to_type;
    else:
        scs.second = ImplicitConversionKind.IDENTITY
    scs.set_to_type(1, from_type)

    if is_function_conversion(from_type, to_type, from_type):
        scs.third = ImplicitConversionKind.FUNCTION_CONVERSION
    elif is_qualification_conversion(from_type, to_type, c_style):
        scs.third = ImplicitConversionKind.QUALIFICATION
        from_type = to_type
    else:
        scs.third = ImplicitConversionKind.IDENTITY

    scs.set_to_type(2, from_type)
    return from_type == to_type

def is_integral_promotion(from_e: Expr, from_type: Type, to_type: Type) -> bool:
    if not isinstance(to_type, BuiltinType):
        return False

    if from_type == TYPES["i8"] and to_type.kind == BuiltinTypeKind.I64:
        return True
    #if (Context.isPromotableIntegerType(FromType) and from_type != TYPES["bool"] and !FromType->isEnumeralType()) {
    #    if ((FromType->isSignedIntegerType() || Context.getTypeSize(FromType) < Context.getTypeSize(ToType))) {
    #        return To->getKind() == BuiltinType::Int;
    #    }
    #    return To->getKind() == BuiltinType::UInt;

    # enum types
    # char types
    # bitfields
    if from_type == TYPES["bool"] and to_type.kind == BuiltinTypeKind.I64:
        return True
    return False

def is_floating_promotion(from_type, to_type) -> bool:
    # TODO:
    return False

def is_floating_conversion(from_type, to_type) -> bool:
    # TODO:
    return False

def is_pointer_conversion(from_e, from_type, to_type, a) -> bool:
    if not isinstance(from_type, PointerType) or not isinstance(to_type, PointerType):
        return False
    # TODO: define what 'compatible pointer' mean
    return type_is_void(from_type.subtype) or type_is_void(to_type.subtype)

def is_function_conversion(from_type, to_type, result_ty) -> bool:
    # TODO:
    return False

def is_qualification_conversion(from_type: Type, to_type: Type, c_style: bool):
    # TODO:
    return False

def try_implicit_conversion(f: Expr, to_type: Type, c_style: bool = False): #...
    ics = ImplicitConversionSequence()
    if is_standard_conversion(f, to_type, ics.val, c_style):
        ics.conversion_kind = 0
        return ics
    from_type = f.ty

    # if (ToType->getAs<RecordType>() && FromType->getAs<RecordType>() && (S.Context.hasSameUnqualifiedType(FromType, ToType) || S.IsDerivedFrom(From->getBeginLoc(), FromType, ToType))) {
    #   ICS.setStandard(); ICS.Standard.setAsIdentityConversion(); ICS.Standard.setFromType(FromType); ICS.Standard.setAllToTypes(ToType); ICS.Standard.CopyConstructor = nullptr;
    #   if (!S.Context.hasSameUnqualifiedType(FromType, ToType)) ICS.Standard.Second = ICK_Derived_To_Base;
    #   return ICS;
    # }
    #return TryUserDefinedConversion(S, From, ToType, SuppressUserConversions, AllowExplicit, InOverloadResolution, CStyle, AllowObjCWritebackConversion, AllowObjCConversionOnExplicit);
    ics.conversion_kind = 5 # bad
    return ics


def try_contextually_convert_to_bool(f: Expr):
    # if f is of type nullptr_t, ImplicitConversionSequence::getNullptrToBool(From->getType(), S.Context.BoolTy, From->isGLValue());
    return try_implicit_conversion(f, TYPES["bool"])


def perform_contextually_convert_to_bool(f: Expr) -> Expr | None:
    from .expr import perform_implicit_conversion
    ics = try_contextually_convert_to_bool(f)
    if ics.conversion_kind == 5: #.is_bad()
        return None
    return perform_implicit_conversion(f, TYPES["bool"], ics) #, AA_CONVERTING)

