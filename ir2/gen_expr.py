from dataclasses import dataclass
from typing import Tuple
from ir2.data.instrs.binary import (
    AddInstr,
    AndInstr,
    AshrInstr,
    LshrInstr,
    MulInstr,
    OrInstr,
    SdivInstr,
    ShlInstr,
    SremInstr,
    SubInstr,
    UdivInstr,
    UremInstr,
    XorInstr,
)
from ir2.data.instrs.other import IcmpCondKind, IcmpInstr
from ir2.gen_state import (
    ir2_get_value_for_decl,
    ir2_insert,
    tmp_value_name,
)
from semantic_analysis import TYPES
from ir2.data.instrs.memory import (
    LoadInstr,
    StoreInstr,
    VaargInstr,
)
from ir2.data.instrs.convert import (
    TruncInstr,
    ZextInstr,
    SextInstr,
)
from ir2.data.types import (
    IrPtrType,
    IrType,
    IrTypeInt,
    IrVoidType,
    PointerType,
    ir2_get_type,
)
from ir2.data.value import ConstantInt, IrValue
from lex.src_loc import Loc
from ns_ast.nodes.decl import EnumVariantDecl
from ns_ast.nodes.expr import (
    ArraySubscriptExpr,
    BinaryExpr,
    BinaryOperatorKind,
    BoolLiteral,
    CallExpr,
    CastExpr,
    CastKind,
    CompoundAssignExpr,
    ConditionalExpr,
    DeclRefExpr,
    Expr,
    IntegerLiteral,
    MemberExpr,
    MethodExpr,
    ParenExpr,
    RecoveryExpr,
    SizeofExpr,
    StringLiteral,
    UnaryExpr,
    UnaryOperatorKind,
    VAArgExpr,
    ValueKind,
)
from ns_ast.nodes.types import ArrayType, Type


def ir2_to_memory(v: IrValue) -> IrValue:
    # if v.ty is i1 -> convert to i8
    return v


def ir2_from_memory(v: IrValue, t: Type) -> IrValue:
    # if t is bool -> convert from i8
    return v


def ir2_store_through_lvalue(src: IrValue, dst: IrValue):
    src = ir2_to_memory(src)
    ir2_insert(StoreInstr(dst.ty, dst, src))


def ir2_load_of_lvalue(lv: IrValue, ty: Type) -> IrValue:
    load_ty = IrTypeInt(8) if ty == TYPES["bool"] else ir2_get_type(ty)
    load = LoadInstr(tmp_value_name("load"), load_ty, lv)
    ir2_insert(load)
    return ir2_from_memory(load, ty)


def ir2_binary_operator_lvalue(e: BinaryExpr) -> IrValue:
    assert e.opc == BinaryOperatorKind.ASSIGN, "unexpected binary l-value"
    # src_ty = e.rhs.ty
    rv = ir2_expr(e.rhs)
    lv = ir2_lvalue(e.lhs)
    ir2_store_through_lvalue(rv, lv)
    return lv


def ir2_call_expr_lvalue(e: CallExpr) -> IrValue:
    assert False


def ir2_vaarg_expr_lvalue(e: VAArgExpr) -> IrValue:
    assert False


def ir2_decl_ref_lvalue(e: DeclRefExpr) -> IrValue:
    # TODO:
    # if e is variable:
    #   if glob var:
    #     EmitGlobalVarDeclLValue
    #   else:
    #     find local variable value
    # else: e is function
    #   EmitFunctionDeclLValue
    v = ir2_get_value_for_decl(e.decl)
    assert v is not None
    return v


def ir2_string_literal_lvalue(e: StringLiteral) -> IrValue:
    assert False


def ir2_unary_op_lvalue(e: UnaryExpr) -> IrValue:
    assert False


def ir2_conditional_operator_lvalue(e: Expr) -> IrValue:
    #     cond_val = ir2_expr(e.cond)
    #     true_block = Block([], tmp_value_name())
    #     false_block = Block([], tmp_value_name())
    #     end_block = Block([], tmp_value_name())
    #     ir2_insert(BranchCondInstr(cond_val, true_block, false_block))
    #     ir2_insert_b(true_block)
    #     true_val = ir2_lvalue(e.lhs)
    #     ir2_insert(BranchInstr(end_block))
    #     ir2_insert_b(false_block)
    #     false_val = ir2_lvalue(e.rhs)
    #     ir2_insert(BranchInstr(end_block))
    #     ir2_insert_b(end_block)
    #     out = make_tmp_value(ir2_get_type(e.ty))
    #     ir2_insert(
    #         PhiInstr(out.ty, [true_block, false_block], [true_val, false_val], out)
    #     )
    #     return out
    assert False


def ir2_cast_lvalue(e: Expr) -> IrValue:
    assert False


def ir2_unsupported_lvalue(e: Expr, s: str):
    assert False, s + " " + e.__class__.__name__


def ir2_lvalue(e: Expr) -> IrValue:
    if isinstance(e, CompoundAssignExpr):
        return ir2_compound_assign_lvalue(e)[0]
    elif isinstance(e, BinaryExpr):
        return ir2_binary_operator_lvalue(e)
    elif isinstance(e, CallExpr):
        return ir2_call_expr_lvalue(e)
    elif isinstance(e, VAArgExpr):
        return ir2_vaarg_expr_lvalue(e)
    elif isinstance(e, DeclRefExpr):
        return ir2_decl_ref_lvalue(e)
    elif isinstance(e, ParenExpr):
        return ir2_lvalue(e.val)
    elif isinstance(e, StringLiteral):
        return ir2_string_literal_lvalue(e)
    elif isinstance(e, UnaryExpr):
        return ir2_unary_op_lvalue(e)
    elif isinstance(e, ArraySubscriptExpr):
        return ir2_array_subscript_expr(e)
    elif isinstance(e, MemberExpr):
        return ir2_member_expr(e)
    elif isinstance(e, ConditionalExpr):
        return ir2_conditional_operator_lvalue(e)
    elif isinstance(e, CastExpr):
        return ir2_cast_lvalue(e)
    else:
        return ir2_unsupported_lvalue(e, "l-value expression")


def ir2_int_cast(
    v: IrValue,
    to_type: IrType,
    value_name: str = "",
    signed: bool = False,
    some_flag: bool = False,
) -> IrValue:
    from_type = v.ty
    name = tmp_value_name(value_name)
    assert isinstance(from_type, IrTypeInt) and isinstance(to_type, IrTypeInt)
    if from_type.size == to_type.size:
        return v
    elif from_type.size > to_type.size:
        result = TruncInstr(name, v, from_type, to_type, False, False)
    else:
        result = (ZextInstr, SextInstr)[signed](name, v, from_type, to_type)
    ir2_insert(result)
    return result


def ir2_array_to_pointer_decay(e: Expr):
    assert isinstance(e.ty, ArrayType)
    # lv = ir2_lvalue(e)
    assert False


def ir2_ignored_expr(e: Expr):
    if e.value_kind == ValueKind.PRVALUE:
        return ir2_expr(e)
    # ignored conditional operator...
    ir2_lvalue(e)


def ir2_scalar_conversion(v: IrValue, src_ty: Type, dst_ty: Type, loc: Loc) -> IrValue:
    _ = src_ty
    return ir2_int_cast(v, ir2_get_type(dst_ty), "conv", dst_ty.is_signed())


def ir2_cast_expr(e: CastExpr) -> IrValue:
    if e.kind == CastKind.NOOP:
        return ir2_expr(e.op)
    elif e.kind == CastKind.ARRAY_TO_POINTER_DECAY:
        return ir2_array_to_pointer_decay(
            e.op
        )  # get_as_natural_pointer_to(, e.ty.subtype)
    elif e.kind == CastKind.FUNCTION_TO_POINTER_DECAY:
        return ir2_lvalue(e.op)  # .get_pointer()
    elif e.kind == CastKind.LVALUE_TO_RVALUE:
        return ir2_expr(e.op)
    elif e.kind == CastKind.INTEGRAL_CAST:
        return ir2_scalar_conversion(ir2_expr(e.op), e.op.ty, e.ty, e.get_range()[0])
    elif e.kind == CastKind.TO_VOID:
        ir2_ignored_expr(e.op)
        return IrValue(IrVoidType())
    # POINTER_TO_BOOLEAN = enum.auto()
    # INTEGRAL_TO_BOOLEAN = enum.auto()
    # ARRAY_TO_POINTER_DECAY = enum.auto()
    else:
        assert False, "unimplemented"


@dataclass
class BinOpInfo:
    lhs: IrValue
    rhs: IrValue
    ty: Type
    opc: BinaryOperatorKind
    e: Expr


def ir2_bin_ops(e: BinaryExpr) -> BinOpInfo:
    lhs = ir2_expr(e.lhs)
    rhs = ir2_expr(e.rhs)
    ty = e.ty
    return BinOpInfo(lhs, rhs, ty, e.opc, e)


def ir2_compare(
    e: BinaryExpr, ui_cmp_opc: IcmpCondKind, si_cmp_opc: IcmpCondKind
) -> IrValue:  # float opc
    info = ir2_bin_ops(e)
    result = IcmpInstr(
        tmp_value_name("cmp"),
        ir2_get_type(info.ty),
        (ui_cmp_opc, si_cmp_opc)[e.lhs.ty.is_signed()],
        info.lhs,
        info.rhs,
    )
    ir2_insert(result)
    # return ir2_scalar_conversion(result, TYPES["bool"], e.ty, e.get_range()[0])
    return result  # convert to bool ? should have been done in ast


def ir2_pointer_arithmetic(
    info: BinOpInfo,
    name: str,
    some_flag: bool = False,
) -> IrValue:
    assert False, "todo, pointer arithmetic"
    # expr = op[3]
    # ptr = op[0]
    # ptr_operand = expr.lhs
    # idx = op[1]
    # idx_operand = expr.rhs
    # if not is_sub and not isinstance(ptr, AST.PointerType):
    #     ptr, idx = idx, ptr
    #     ptr_operand, idx_operand = idx_operand, ptr_operand
    # is_signed = idx_operand.ty.is_signed()
    # w = idx.ty.get_bit_width()
    # # auto &DL = CGF.CGM.getDataLayout();
    # ptr_ty = ptr.ty
    # # if (BinaryOperator::isNullPointerArithmeticExtension(CGF.getContext(), op.Opcode, expr->getLHS(), expr->getRHS())) return CGF.Builder().CreateIntToPtr(index, pointer->getType());
    # # if (width != DL.getIndexTypeSizeInBits(PtrTy)) { index = CGF.Builder().CreateIntCast(index, DL.getIndexType(PtrTy), isSigned, "idx.ext"); }
    # if is_sub: idx = builder().create_neg(idx, "idx.neg")
    # elt_ty = ptr_ty.subtype
    # if AST.type_is_void(elt_ty) or isinstance(elt_ty, FunctionType): elt_ty = TYPES["i8"]
    # return emit_checked_in_bounds_gep(elt_ty, ptr, idx, is_signed, is_sub, ops[3].get_range()[0], "add.ptr")


def ir2_compound_assign_lvalue(
    e: CompoundAssignExpr,
) -> Tuple[IrValue, IrValue]:  # lhs, rhs
    info_rhs = ir2_expr(e.rhs)
    info_ty = e.ty
    info_e = e
    info_opc = e.opc.to_non_compound()

    lhs_lv = ir2_lvalue(e.lhs)

    info_lhs = ir2_load_of_lvalue(lhs_lv, e.ty)

    result = ir2_standard_binary_expr(
        BinOpInfo(info_lhs, info_rhs, info_ty, info_opc, info_e)
    )

    ir2_store_through_lvalue(result, lhs_lv)

    return info_lhs, result


def ir2_compound_assign_expr(e: CompoundAssignExpr) -> IrValue:
    _, rhs = ir2_compound_assign_lvalue(e)
    return rhs


# OK
def ir2_standard_binary_expr(info: BinOpInfo):
    opc = info.opc
    lhs = info.lhs
    rhs = info.rhs
    e = info.e
    ty = info.ty

    name = tmp_value_name(opc.name.lower())
    if opc == BinaryOperatorKind.MUL:
        result = MulInstr(name, lhs, rhs, False, ty.is_signed())
    elif opc == BinaryOperatorKind.DIV:
        result = (UdivInstr, SdivInstr)[ty.is_signed()](name, lhs, rhs)
    elif opc == BinaryOperatorKind.REM:
        result = (UremInstr, SremInstr)[ty.is_signed()](name, lhs, rhs)
    elif opc == BinaryOperatorKind.ADD:
        if isinstance(lhs.ty, IrPtrType) or isinstance(rhs.ty, IrPtrType):
            return ir2_pointer_arithmetic(info, name, False)
        result = AddInstr(name, lhs, rhs, False, ty.is_signed())
    elif opc == BinaryOperatorKind.SUB:
        if not isinstance(lhs.ty, PointerType):
            result = SubInstr(name, lhs, rhs, False, ty.is_signed())
        elif not isinstance(rhs.ty, PointerType):
            return ir2_pointer_arithmetic(info, name, True)
        else:
            assert False, "unimplemented: ptr diff"
            # llvm::Value *LHS =
            #     Builder.CreatePtrToInt(op.LHS, CGF.PtrDiffTy, "sub.ptr.lhs.cast");
            # llvm::Value *RHS =
            #     Builder.CreatePtrToInt(op.RHS, CGF.PtrDiffTy, "sub.ptr.rhs.cast");
            # Value *diffInChars = Builder.CreateSub(LHS, RHS, "sub.ptr.sub");
            # // Okay, figure out the element size.
            # const BinaryOperator *expr = cast<BinaryOperator>(op.E);
            # QualType elementType = expr->getLHS()->getType()->getPointeeType();
            # llvm::Value *divisor = nullptr;
            # // For a variable-length array, this is going to be non-constant.
            # CharUnits elementSize;
            # // Handle GCC extension for pointer arithmetic on void* and
            # // function pointer types.
            # if (elementType->isVoidType() || elementType->isFunctionType())
            #   elementSize = CharUnits::One();
            # else
            #   elementSize = CGF.getContext().getTypeSizeInChars(elementType);
            # // Don't even emit the divide for element size of 1.
            # if (elementSize.isOne())
            #   return diffInChars;
            # divisor = CGF.CGM.getSize(elementSize);
            # // Otherwise, do a full sdiv. This uses the "exact" form of sdiv, since
            # // pointer difference in C is only defined in the case where both operands
            # // are pointing to elements of an array.
            # return Builder.CreateExactSDiv(diffInChars, divisor, "sub.ptr.div");
    elif opc == BinaryOperatorKind.SHL:
        if rhs.ty != lhs.ty:
            rhs = ir2_int_cast(rhs, lhs.ty, "sh_prom", ty.is_signed(), False)
        result = ShlInstr(name, lhs, rhs)
    elif opc == BinaryOperatorKind.SHR:
        if rhs.ty != lhs.ty:
            rhs = ir2_int_cast(rhs, lhs.ty, "sh_prom", ty.is_signed(), False)
        result = (LshrInstr, AshrInstr)[e.ty.is_signed()](name, lhs, rhs)
    elif opc == BinaryOperatorKind.AND:
        result = AndInstr(name, lhs, rhs)
    elif opc == BinaryOperatorKind.XOR:
        result = XorInstr(name, lhs, rhs)
    elif opc == BinaryOperatorKind.OR:
        result = OrInstr(name, lhs, rhs)
    else:
        assert False, f"operator not implemented: {opc}"
    ir2_insert(result)
    return result


def ir2_binary_expr(e: BinaryExpr) -> IrValue:
    if e.opc == BinaryOperatorKind.ASSIGN:
        rhs = ir2_expr(e.rhs)
        lhs = ir2_lvalue(e.lhs)
        ir2_store_through_lvalue(rhs, lhs)
        return rhs
    if e.opc == BinaryOperatorKind.LOR:
        assert False, "LOR"
    if e.opc == BinaryOperatorKind.LAND:
        assert False, "LAND"
    if e.opc == BinaryOperatorKind.LT:
        return ir2_compare(e, IcmpCondKind.ULT, IcmpCondKind.SLT)
    if e.opc == BinaryOperatorKind.GT:
        return ir2_compare(e, IcmpCondKind.UGT, IcmpCondKind.SGT)
    if e.opc == BinaryOperatorKind.LE:
        return ir2_compare(e, IcmpCondKind.ULE, IcmpCondKind.SLE)
    if e.opc == BinaryOperatorKind.GE:
        return ir2_compare(e, IcmpCondKind.UGE, IcmpCondKind.SGE)
    if e.opc == BinaryOperatorKind.EQ:
        return ir2_compare(e, IcmpCondKind.EQ, IcmpCondKind.EQ)
    if e.opc == BinaryOperatorKind.NE:
        return ir2_compare(e, IcmpCondKind.NE, IcmpCondKind.NE)
    return ir2_standard_binary_expr(ir2_bin_ops(e))


def ir2_vaarg_expr(e: VAArgExpr) -> IrValue:
    # TODO: VA STUFF IS MANUALLY ADDED IN... ?
    out = VaargInstr(tmp_value_name("vaarg"), ir2_get_type(e.ty))
    ir2_insert(out)
    return out


def ir2_unary_expr(e: UnaryExpr) -> IrValue:
    if e.opc == UnaryOperatorKind.PLUS:
        return ir2_expr(e.arg)
    elif e.opc == UnaryOperatorKind.MINUS:
        op = ir2_expr(e.arg)
        return ir2_standard_binary_expr(
            BinOpInfo(ConstantInt(op.ty, 0), op, e.ty, BinaryOperatorKind.SUB, e)
        )
    elif e.opc == UnaryOperatorKind.ADDROF:
        return ir2_lvalue(e.arg)  # .get_pointer()
    elif e.opc == UnaryOperatorKind.DEREF:
        # if type is void -> just emit expr
        # ir2_load_of_lvalue(e)
        assert False, "deref"
    elif e.opc == UnaryOperatorKind.NOT:
        op = ir2_expr(e.arg)
        return ir2_standard_binary_expr(
            BinOpInfo(op, ConstantInt(op.ty, -1), e.ty, BinaryOperatorKind.XOR, e)
        )
    elif e.opc == UnaryOperatorKind.LNOT:
        op = ir2_expr(e.arg)  # TODO: < convert to bool ? needed ?
        op = ir2_standard_binary_expr(
            BinOpInfo(op, ConstantInt(op.ty, -1), e.ty, BinaryOperatorKind.XOR, e)
        )
        return op  # TODO: < zext
    else:
        assert False, f"unimplemented unary op {e.opc}"

    # POSTINC = enum.auto()
    # POSTDEC = enum.auto()
    # PREINC = enum.auto()
    # PREDEC = enum.auto()


def ir2_call_expr(e: CallExpr) -> IrValue:
    assert False, "call expr"



def ir2_method_expr(e: MethodExpr) -> IrValue:
    assert False, "method expr"


def ir2_conditional_expr(e: ConditionalExpr) -> IrValue:
    # cond_val = ir2_expr(e.cond)
    # true_block = Block([], tmp_value_name())
    # false_block = Block([], tmp_value_name())
    # end_block = Block([], tmp_value_name())
    # ir2_insert(BranchCondInstr(cond_val, true_block, false_block))
    # ir2_insert_b(true_block)
    # true_val = ir2_expr(e.lhs)
    # ir2_insert(BranchInstr(end_block))
    # ir2_insert_b(false_block)
    # false_val = ir2_expr(e.rhs)
    # ir2_insert(BranchInstr(end_block))
    # ir2_insert_b(end_block)
    # out = make_tmp_value(ir2_get_type(e.ty))
    # ir2_insert(
    #     PhiInstr(out.ty, [true_block, false_block], [true_val, false_val], out)
    # )
    # return out
    assert False, "conditional"


# OK ?
def ir2_sizeof_expr(e: SizeofExpr) -> IrValue:
    return ConstantInt(ir2_get_type(e.ty), e.ty_of_sizeof.get_size())


# OK ? -> char literals = OK. nullptr litteral = ?
def ir2_integer_literal(e: IntegerLiteral) -> IrValue:
    return ConstantInt(ir2_get_type(e.ty), e.value)


# OK
def ir2_bool_literal(e: BoolLiteral) -> IrValue:
    return ConstantInt(IrTypeInt(1), int(e.value))


# OK
def ir2_paren_expr(e: ParenExpr) -> IrValue:
    return ir2_expr(e.val)


def ir2_declref_expr(e: DeclRefExpr) -> IrValue:
    if isinstance(e.decl, EnumVariantDecl):
        # TODO: other constant declrefs
        return ConstantInt(ir2_get_type(e.decl.ty), e.decl.val)
    return ir2_load_of_lvalue(ir2_lvalue(e), e.ty)


def ir2_expr(e: Expr) -> IrValue:
    if isinstance(e, CastExpr):
        return ir2_cast_expr(e)
    elif isinstance(e, CompoundAssignExpr):
        return ir2_compound_assign_expr(e)
    elif isinstance(e, BinaryExpr):
        return ir2_binary_expr(e)
    elif isinstance(e, VAArgExpr):
        return ir2_vaarg_expr(e)
    elif isinstance(e, UnaryExpr):
        return ir2_unary_expr(e)
    elif isinstance(e, CallExpr):
        return ir2_call_expr(e)
    elif isinstance(e, MemberExpr):
        # try constant emission
        return ir2_load_of_lvalue(ir2_lvalue(e), e.ty)
    elif isinstance(e, MethodExpr):
        return ir2_method_expr(e)
    elif isinstance(e, ArraySubscriptExpr):
        return ir2_load_of_lvalue(ir2_lvalue(e), e.ty)
    elif isinstance(e, ConditionalExpr):
        return ir2_conditional_expr(e)
    elif isinstance(e, SizeofExpr):
        return ir2_sizeof_expr(e)
    elif isinstance(e, IntegerLiteral):
        return ir2_integer_literal(e)
    elif isinstance(e, BoolLiteral):
        return ir2_bool_literal(e)
    elif isinstance(e, ParenExpr):
        return ir2_paren_expr(e)
    elif isinstance(e, DeclRefExpr):
        return ir2_declref_expr(e)
    elif isinstance(e, RecoveryExpr):
        assert False, "recovery expr in ast"
    else:
        assert False, f"unimplemented {e.__class__}"
