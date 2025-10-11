#include "ast/context.hpp"
#include "ast/nodes/expr.hpp"
#include "ast/nodes/type.hpp"
#include "codegen.hpp"
#include "codegen/context.hpp"
#include "semantic_analysis/constexpr.hpp"
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

LValue gen_lvalue(CGContext &ctx, Expr const &e);

llvm::Value *gen_frommemory_conversions(CGContext &ctx, llvm::Value *value,
                                        Type *ty) {
  if (ty->is_boolean_type()) {
    llvm::Type *to_ty = llvm::IntegerType::get(ctx.llvmctx, 1);
    bool is_signed = ty->is_signed_integer_or_enumeration_type();
    return ctx.builder.CreateIntCast(value, to_ty, is_signed, "loaddv");
  }
  return value;
}

llvm::Value *gen_load_of_scalar(CGContext &ctx, LValue lv, Type *ty) {
  auto ty_mem = convert_type_for_mem(ctx, ty);

  auto load = ctx.builder.CreateAlignedLoad(
      ty_mem, lv, llvm::Align{ctx.astctx.get_type_align(ty)});
  return gen_frommemory_conversions(ctx, load, ty);
}

RValue gen_pre_post_inc_dec(CGContext &ctx, UnaryExpr const &e, LValue lv,
                            bool is_inc, bool is_pre) {
  i32 amount = (is_inc ? 1 : -1);

  auto type = e.arg->type;

  auto value = gen_load_of_scalar(ctx, gen_lvalue(ctx, *e.arg), type);
  auto input = value;

  // TODO: all types
  if (is_inc && type->is_boolean_type()) {
    value = ctx.builder.getTrue();
  } else if (type->is_integer_type()) {
    auto amt = llvm::ConstantInt::get(value->getType(), amount, true);
    value = ctx.builder.CreateAdd(value, amt, is_inc ? "inc" : "dec");
  } else if (auto *ptr = type->dyn_cast<PointerType>()) {
    auto elty = ptr->pointee_type;
    auto amt = ctx.builder.getInt32(amount);
    if (elty->is_function_type()) {
      value = ctx.builder.CreateGEP(llvm::Type::getInt8Ty(ctx.llvmctx), value, amt, "incdec.funcptr");
    } else {
      value = ctx.builder.CreateGEP(convert_type_for_mem(ctx, elty), value, amt, "incdec.ptr");
    }
  }

  gen_store_of_scalar(ctx, value, lv, type);

  return is_pre ? value : input;
}

llvm::Value *gen_cast_expr(CGContext &ctx, CastExpr const &e) {
  switch (e.cast_kind) {
  case CastExpr::ARRAY_TO_POINTER_DECAY: {
    auto addr = gen_lvalue(ctx, *e.op);
    auto elt_ty =
        convert_type(ctx, e.op->type->dyn_cast<ArrayType>()->element_type);
    auto idx = ctx.builder.getInt32(0);
    return ctx.builder.CreateInBoundsGEP(elt_ty, addr, {idx, idx}, "arraydecay");
  }
  case CastExpr::FUNCTION_TO_POINTER_DECAY:
    return gen_lvalue(ctx, *e.op);
  case CastExpr::NULL_TO_POINTER:
    return llvm::ConstantPointerNull::get(
        llvm::PointerType::getUnqual(ctx.llvmctx));
  case CastExpr::NOOP:
  case CastExpr::LVALUE_TO_RVALUE:
    return gen_scalar_expr(ctx, e.op.get());
  case CastExpr::INTEGRAL_TO_POINTER: {
    auto src = gen_scalar_expr(ctx, e.op.get());
    auto int_result = ctx.builder.CreateIntCast(
        src, llvm::Type::getInt64Ty(ctx.llvmctx),
        e.op->type->is_signed_integer_or_enumeration_type(), "conv");
    return ctx.builder.CreateIntToPtr(int_result, convert_type(ctx, e.type));
  }
  case CastExpr::POINTER_TO_INTEGRAL: {
    auto src = gen_scalar_expr(ctx, e.op.get());
    return ctx.builder.CreatePtrToInt(src, convert_type(ctx, e.type));
  }
  case CastExpr::TO_VOID:
    gen_ignored_expr(ctx, *e.op);
    return nullptr;
  case CastExpr::INTEGRAL_CAST:
    return gen_scalar_conversion(ctx, gen_scalar_expr(ctx, e.op.get()),
                                 e.op->type, e.type, e.get_start_loc());
  case CastExpr::INTEGRAL_TO_BOOLEAN:
    return ctx.builder.CreateIsNotNull(gen_scalar_expr(ctx, e.op.get()),
                                       "tobool");
  case CastExpr::POINTER_TO_BOOLEAN:
    return ctx.builder.CreateICmpNE(
        gen_scalar_expr(ctx, e.op.get()),
        llvm::ConstantPointerNull::get(
            llvm::PointerType::getUnqual(ctx.llvmctx)),
        "tobool");
  default:
    // TODO: all types
    assert(false && "unreachable");
  }
}

llvm::Value *gen_call_expr(CGContext &ctx, CallExpr const &e) {
  auto fn = e.fn->ignore_paren_imp_casts();
  FunctionDecl const *fd;
  std::vector<llvm::Value *> args;
  if (auto *dr = fn->dyn_cast<DeclRefExpr>()) {
    fd = dr->decl->dyn_cast<FunctionDecl>();
  } else if (auto *me = fn->dyn_cast<MethodExpr>()) {
    fd = me->method_func;
    llvm::Value *this_arg = me->is_arrow ? gen_scalar_expr(ctx, me->base.get()) : gen_lvalue(ctx, *me->base);
    args.push_back(this_arg);
  }
  for (auto const &a : e.args) {
    args.push_back(gen_scalar_expr(ctx, a.get()));
  }
  auto t = llvm::cast<llvm::FunctionType>(convert_type(ctx, fd->type));
  auto f = get_addr_of_function(ctx, fd, convert_type(ctx, fd->type));

  return ctx.builder.CreateCall(t, f, args, fd->type->dyn_cast<FunctionType>()->result_type->is_void_type() ? "" : "call");
}

struct BinOpInfo {
  llvm::Value *lhs;
  llvm::Value *rhs;
  Type *ty;
  Expr const *e;
  BinaryExpr::OpKind opc;
};

BinOpInfo gen_bin_ops(CGContext &ctx, BinaryExpr const &e) {
  BinOpInfo res;
  res.lhs = gen_scalar_expr(ctx, e.lhs.get());
  res.rhs = gen_scalar_expr(ctx, e.rhs.get());
  res.ty = e.type;
  res.e = &e;
  res.opc = e.opc;
  return res;
}

RValue gen_pointer_arithmetic(CGContext &ctx, BinOpInfo bo, bool subtract) {
  auto pointer = bo.lhs;
  auto index = bo.rhs;
  auto pointer_e = bo.e->dyn_cast<BinaryExpr>()->lhs.get();
  auto index_e = bo.e->dyn_cast<BinaryExpr>()->rhs.get();
  if (!subtract && !pointer->getType()->isPointerTy()) {
    std::swap(pointer, index);
    std::swap(pointer_e, index_e);
  }

  bool is_signed = index_e->type->is_signed_integer_or_enumeration_type();

  unsigned width = cast<llvm::IntegerType>(index->getType())->getBitWidth();

  if (width != 64) {
    index = ctx.builder.CreateIntCast(
        index, llvm::Type::getInt64Ty(ctx.llvmctx), is_signed, "idx.ext");
  }

  if (subtract)
    index = ctx.builder.CreateNeg(index, "idx.neg");

  const PointerType *pointer_type = pointer_e->type->dyn_cast<PointerType>();

  auto elty = pointer_type->pointee_type;

  // Explicitly handle GNU void* and function pointer arithmetic extensions. The
  // GNU void* casts amount to no-ops since our void* type is i8*, but this is
  // future proof.
  llvm::Type *elem_ty;
  if (elty->is_void_type() || elty->is_function_type())
    elem_ty = llvm::Type::getInt8Ty(ctx.llvmctx);
  else
    elem_ty = convert_type_for_mem(ctx, elty);

  return ctx.builder.CreateGEP(elem_ty, pointer, index, "add.ptr");
}

llvm::Value *gen_binary_expr_inner(CGContext &ctx, BinOpInfo ops) {
  switch (ops.opc) {
  case BinaryExpr::ADD:
  case BinaryExpr::ADDASSIGN:
    if (ops.lhs->getType()->isPointerTy() ||
        ops.rhs->getType()->isPointerTy()) {
      return gen_pointer_arithmetic(ctx, ops, false);
    }
    // TODO: other types
    if (ops.ty->is_signed_integer_or_enumeration_type()) {
      return ctx.builder.CreateNSWAdd(ops.lhs, ops.rhs, "add");
    } else {
      return ctx.builder.CreateAdd(ops.lhs, ops.rhs, "add");
    }
  case BinaryExpr::SUB:
  case BinaryExpr::SUBASSIGN:
    if (!ops.lhs->getType()->isPointerTy()) {
      // TODO: other types
      if (ops.ty->is_signed_integer_or_enumeration_type()) {
        return ctx.builder.CreateNSWSub(ops.lhs, ops.rhs, "sub");
      } else {
        return ctx.builder.CreateSub(ops.lhs, ops.rhs, "sub");
      }
    }
    if (!ops.rhs->getType()->isPointerTy()) {
      return gen_pointer_arithmetic(ctx, ops, true);
    } else {
      auto lhs = ctx.builder.CreatePtrToInt(
          ops.lhs, llvm::Type::getInt64Ty(ctx.llvmctx), "sub.ptr.lhs.cast");
      auto rhs = ctx.builder.CreatePtrToInt(
          ops.rhs, llvm::Type::getInt64Ty(ctx.llvmctx), "sub.ptr.rhs.cast");
      auto diff_in_chars = ctx.builder.CreateSub(lhs, rhs, "sub.ptr.sub");
      auto elty = ops.ty->dyn_cast<PointerType>()->pointee_type;
      u32 elt_size = 1;
      if (!elty->is_void_type() && !elty->is_function_type()) {
        elt_size = ctx.astctx.get_type_size(elty);
      }
      if (elt_size == 1)
        return diff_in_chars;
      auto divisor =
          llvm::ConstantInt::get(llvm::Type::getInt64Ty(ctx.llvmctx), elt_size);
      return ctx.builder.CreateExactSDiv(diff_in_chars, divisor, "sub.ptr.div");
    }
  case BinaryExpr::MUL:
  case BinaryExpr::MULASSIGN:
    // TODO: other types
    if (ops.ty->is_signed_integer_or_enumeration_type()) {
      return ctx.builder.CreateNSWMul(ops.lhs, ops.rhs, "mul");
    } else {
      return ctx.builder.CreateMul(ops.lhs, ops.rhs, "mul");
    }
  case BinaryExpr::DIV:
  case BinaryExpr::DIVASSIGN:
    // TODO: other types
    if (ops.ty->is_unsigned_integer_or_enumeration_type()) {
      return ctx.builder.CreateUDiv(ops.lhs, ops.rhs, "div");
    } else {
      return ctx.builder.CreateSDiv(ops.lhs, ops.rhs, "div");
    }
  case BinaryExpr::REM:
  case BinaryExpr::REMASSIGN:
    if (ops.ty->is_unsigned_integer_or_enumeration_type()) {
      return ctx.builder.CreateURem(ops.lhs, ops.rhs, "rem");
    } else {
      return ctx.builder.CreateSRem(ops.lhs, ops.rhs, "rem");
    }
  case BinaryExpr::SHL:
  case BinaryExpr::SHLASSIGN:
    if (ops.lhs->getType() != ops.rhs->getType()) {
      ops.rhs = ctx.builder.CreateIntCast(ops.rhs, ops.lhs->getType(), false,
                                          "sh_prom");
    }
    return ctx.builder.CreateShl(ops.lhs, ops.rhs, "shl");
  case BinaryExpr::SHR:
  case BinaryExpr::SHRASSIGN:
    if (ops.lhs->getType() != ops.rhs->getType()) {
      ops.rhs = ctx.builder.CreateIntCast(ops.rhs, ops.lhs->getType(), false,
                                          "sh_prom");
    }
    if (ops.ty->is_unsigned_integer_or_enumeration_type()) {
      return ctx.builder.CreateLShr(ops.lhs, ops.rhs, "shr");
    } else {
      return ctx.builder.CreateAShr(ops.lhs, ops.rhs, "shr");
    }
  case BinaryExpr::AND:
  case BinaryExpr::ANDASSIGN:
    return ctx.builder.CreateAnd(ops.lhs, ops.rhs, "and");
  case BinaryExpr::XOR:
  case BinaryExpr::XORASSIGN:
    return ctx.builder.CreateXor(ops.lhs, ops.rhs, "xor");
  case BinaryExpr::OR:
  case BinaryExpr::ORASSIGN:
    return ctx.builder.CreateOr(ops.lhs, ops.rhs, "or");
  default:
    return nullptr;
  }
}

LValue gen_compound_assign_lvalue(CGContext &ctx, CompoundAssignExpr const &e) {
  BinOpInfo bo;

  bo.rhs = gen_scalar_expr(ctx, e.rhs.get());
  bo.ty = e.type;
  bo.opc = e.opc;
  bo.e = &e;
  
  LValue lhslv = gen_lvalue(ctx, *e.lhs);

  bo.lhs = gen_load_of_scalar(ctx, lhslv, e.lhs->type);

  auto result = gen_binary_expr_inner(ctx, bo);

  gen_store_of_scalar(ctx, result, lhslv, e.lhs->type);
  return lhslv;
}

llvm::Value *gen_unary_expr(CGContext &ctx, UnaryExpr const &e) {
  switch (e.opc) {
  case UnaryExpr::POSTINC:
    return gen_pre_post_inc_dec(ctx, e, gen_lvalue(ctx, *e.arg), true, false);
  case UnaryExpr::POSTDEC:
    return gen_pre_post_inc_dec(ctx, e, gen_lvalue(ctx, *e.arg), false, false);
  case UnaryExpr::PREINC:
    return gen_pre_post_inc_dec(ctx, e, gen_lvalue(ctx, *e.arg), true, true);
  case UnaryExpr::PREDEC:
    return gen_pre_post_inc_dec(ctx, e, gen_lvalue(ctx, *e.arg), false, true);
  case UnaryExpr::ADDROF:
    return gen_lvalue(ctx, *e.arg);
  case UnaryExpr::DEREF:
    return gen_load_of_scalar(ctx, gen_lvalue(ctx, *e.arg), e.type);
  case UnaryExpr::PLUS:
    return gen_scalar_expr(ctx, e.arg.get());
  case UnaryExpr::MINUS: {
    auto op = gen_scalar_expr(ctx, e.arg.get());
    // float -> fneg
    BinOpInfo bin_op;
    bin_op.rhs = op;
    bin_op.lhs = llvm::Constant::getNullValue(bin_op.rhs->getType());
    bin_op.ty = e.type;
    bin_op.opc = BinaryExpr::SUB;
    bin_op.e = &e;
    return gen_binary_expr_inner(ctx, bin_op);
  }
  case UnaryExpr::NOT:
    return ctx.builder.CreateNot(gen_scalar_expr(ctx, e.arg.get()), "not");
  case UnaryExpr::LNOT: {
    auto bool_val = gen_scalar_conversion(
        ctx, gen_scalar_expr(ctx, e.arg.get()), e.arg->type, ctx.astctx.bool_ty,
        e.arg->get_start_loc());
    bool_val = ctx.builder.CreateNot(bool_val, "lnot");
    return ctx.builder.CreateZExt(bool_val, convert_type(ctx, e.type),
                                  "lnot.ext");
  }
  }
  return nullptr;
}

llvm::Value *gen_conditional_expr(CGContext &ctx, ConditionalExpr const &e) {
  bool cond_v;
  if (constant_folds_to_simple_integer(ctx, e.cond.get(), cond_v)) {
    Expr *live = e.true_expr.get(), *dead = e.false_expr.get();
    if (!cond_v)
      std::swap(live, dead);

    auto result = gen_scalar_expr(ctx, live);

    if (!result && !e.type->is_void_type())
      result = llvm::UndefValue::get(convert_type(ctx, e.type));

    return result;
  }

  if (eval_integer_constexpr(ctx.astctx, e.true_expr.get()).second &&
      eval_integer_constexpr(ctx.astctx, e.false_expr.get()).second) {
    auto cond_v = gen_scalar_conversion(ctx, gen_scalar_expr(ctx, e.cond.get()),
                                        e.cond->type, ctx.astctx.bool_ty,
                                        e.cond->get_start_loc());

    llvm::Value *lhs = gen_scalar_expr(ctx, e.true_expr.get());
    llvm::Value *rhs = gen_scalar_expr(ctx, e.false_expr.get());
    if (!lhs) {
      // If the conditional has void type, make sure we return a null Value*.
      assert(!rhs && "LHS and RHS types must match");
      return nullptr;
    }
    return ctx.builder.CreateSelect(cond_v, lhs, rhs, "cond");
  }

  llvm::BasicBlock *lhs_block =
      llvm::BasicBlock::Create(ctx.llvmctx, "cond.true");
  llvm::BasicBlock *rhs_block =
      llvm::BasicBlock::Create(ctx.llvmctx, "cond.false");
  llvm::BasicBlock *ContBlock =
      llvm::BasicBlock::Create(ctx.llvmctx, "cond.end");

  gen_branch_on_bool_expr(ctx, e.cond.get(), lhs_block, rhs_block);
  gen_block(ctx, lhs_block);
  auto lhs = gen_scalar_expr(ctx, e.true_expr.get());
  lhs_block = ctx.builder.GetInsertBlock();
  ctx.builder.CreateBr(ContBlock);
  gen_block(ctx, rhs_block);
  auto rhs = gen_scalar_expr(ctx, e.false_expr.get());
  rhs_block = ctx.builder.GetInsertBlock();
  gen_block(ctx, ContBlock);

  if (!lhs)
    return rhs;
  if (!rhs)
    return lhs;

  llvm::PHINode *pn = ctx.builder.CreatePHI(lhs->getType(), 2, "cond");
  pn->addIncoming(lhs, lhs_block);
  pn->addIncoming(rhs, rhs_block);
  return pn;
}

LValue gen_assign_lvalue(CGContext &ctx, BinaryExpr const &e) {
  RValue rv;

  rv = gen_scalar_expr(ctx, e.rhs.get());

  LValue lv = gen_lvalue(ctx, *e.lhs);

  gen_store_of_scalar(ctx, rv, lv, e.rhs->type);

  return lv;
}

llvm::Value *gen_land(CGContext &ctx, BinaryExpr const &e) {
  llvm::Type *res_ty = convert_type(ctx, e.type);

  bool lhscond;
  if (constant_folds_to_simple_integer(ctx, e.lhs.get(), lhscond)) {
    if (!lhscond) {
      return llvm::Constant::getNullValue(res_ty);
    }

    auto rhscond = gen_scalar_conversion(ctx, gen_scalar_expr(ctx, e.rhs.get()),
                                         e.rhs->type, ctx.astctx.bool_ty,
                                         e.rhs->get_start_loc());

    return ctx.builder.CreateZExtOrBitCast(rhscond, res_ty, "land.ext");
  }

  llvm::BasicBlock *cont_block =
      llvm::BasicBlock::Create(ctx.llvmctx, "land.end");
  llvm::BasicBlock *rhs_block =
      llvm::BasicBlock::Create(ctx.llvmctx, "land.rhs");

  gen_branch_on_bool_expr(ctx, e.lhs.get(), rhs_block, cont_block);

  llvm::PHINode *pn = llvm::PHINode::Create(llvm::Type::getInt1Ty(ctx.llvmctx),
                                            2, "", cont_block);
  for (llvm::pred_iterator pi = pred_begin(cont_block),
                           pe = pred_end(cont_block);
       pi != pe; ++pi)
    pn->addIncoming(llvm::ConstantInt::getFalse(ctx.llvmctx), *pi);

  gen_block(ctx, rhs_block);
  auto rhscond =
      gen_scalar_conversion(ctx, gen_scalar_expr(ctx, e.rhs.get()), e.rhs->type,
                            ctx.astctx.bool_ty, e.rhs->get_start_loc());
  rhs_block = ctx.builder.GetInsertBlock();

  gen_block(ctx, cont_block);
  pn->addIncoming(rhscond, rhs_block);

  return ctx.builder.CreateZExtOrBitCast(pn, res_ty, "land.ext");
}

llvm::Value *gen_lor(CGContext &ctx, BinaryExpr const &e) {
  llvm::Type *res_ty = convert_type(ctx, e.type);

  bool lhscond;
  if (constant_folds_to_simple_integer(ctx, e.lhs.get(), lhscond)) {
    if (lhscond) {
      return llvm::ConstantInt::get(res_ty, 1);
    }

    auto rhscond = gen_scalar_conversion(ctx, gen_scalar_expr(ctx, e.rhs.get()),
                                         e.rhs->type, ctx.astctx.bool_ty,
                                         e.rhs->get_start_loc());

    return ctx.builder.CreateZExtOrBitCast(rhscond, res_ty, "lor.ext");
  }

  llvm::BasicBlock *cont_block =
      llvm::BasicBlock::Create(ctx.llvmctx, "lor.end");
  llvm::BasicBlock *rhs_block =
      llvm::BasicBlock::Create(ctx.llvmctx, "lor.rhs");

  gen_branch_on_bool_expr(ctx, e.lhs.get(), cont_block, rhs_block);

  llvm::PHINode *pn = llvm::PHINode::Create(llvm::Type::getInt1Ty(ctx.llvmctx),
                                            2, "", cont_block);
  for (llvm::pred_iterator pi = pred_begin(cont_block),
                           pe = pred_end(cont_block);
       pi != pe; ++pi)
    pn->addIncoming(llvm::ConstantInt::getTrue(ctx.llvmctx), *pi);

  gen_block(ctx, rhs_block);
  auto rhscond =
      gen_scalar_conversion(ctx, gen_scalar_expr(ctx, e.rhs.get()), e.rhs->type,
                            ctx.astctx.bool_ty, e.rhs->get_start_loc());
  rhs_block = ctx.builder.GetInsertBlock();

  gen_block(ctx, cont_block);
  pn->addIncoming(rhscond, rhs_block);

  return ctx.builder.CreateZExtOrBitCast(pn, res_ty, "lor.ext");
}

llvm::Value *gen_compare(CGContext &ctx, BinaryExpr const &e,
                         llvm::ICmpInst::Predicate uicmp,
                         llvm::ICmpInst::Predicate sicmp,
                         llvm::ICmpInst::Predicate fcmp, bool signaling) {
  (void)fcmp, (void)signaling;
  BinOpInfo bo = gen_bin_ops(ctx, e);

  llvm::Value *result;
  // TODO: other types
  if (e.lhs->type->is_signed_integer_or_enumeration_type()) {
    result = ctx.builder.CreateICmp(sicmp, bo.lhs, bo.rhs, "cmp");
  } else {
    result = ctx.builder.CreateICmp(uicmp, bo.lhs, bo.rhs, "cmp");
  }

  return gen_scalar_conversion(ctx, result, ctx.astctx.bool_ty, e.type,
                               e.get_start_loc());
}

RValue gen_vaarg_expr(CGContext &ctx, VAArgExpr const &e) {
  auto valist = gen_or_get_valist(ctx);
  auto ty = convert_type_for_mem(ctx, e.type);
  auto vaarg = ctx.builder.CreateVAArg(valist, ty);
  return gen_frommemory_conversions(ctx, vaarg, e.type);
}

RValue gen_enum_variant(CGContext &ctx, EnumVariantDecl const *v) {
  auto t = convert_type(ctx, v->type);
  return llvm::ConstantInt::get(t, v->value);
}

llvm::Value *gen_scalar_expr(CGContext &ctx, Expr const *e, bool ignore) {
  switch (e->kind) {
  default:
    if (e->type->is_void_type())
      return nullptr;
    return llvm::PoisonValue::get(convert_type(ctx, e->type));
  case Expr::PAREN_EXPR:
    return gen_scalar_expr(ctx, e->dyn_cast<ParenExpr>()->val.get(), ignore);
  case Expr::INTEGER_LITERAL:
    return llvm::ConstantInt::get(convert_type(ctx, e->type),
                                  e->dyn_cast<IntegerLiteral>()->value);
  case Expr::CHAR_LITERAL:
    return llvm::ConstantInt::get(convert_type(ctx, e->type),
                                  e->dyn_cast<CharLiteral>()->value);
  case Expr::BOOL_LITERAL:
    return llvm::ConstantInt::get(convert_type(ctx, e->type),
                                  e->dyn_cast<BoolLiteral>()->value);
  case Expr::NULLPTR_LITERAL:
    return llvm::ConstantPointerNull::get(
        llvm::PointerType::getUnqual(ctx.llvmctx));
  case Expr::SIZEOF_EXPR:
    return llvm::ConstantInt::get(
        convert_type(ctx, e->type),
        ctx.astctx.get_type_size(e->dyn_cast<SizeofExpr>()->ty_of_sizeof));
  case Expr::DECLREF_EXPR:
    if (auto *v = e->dyn_cast<DeclRefExpr>()->decl->dyn_cast<EnumVariantDecl>()) {
      return gen_enum_variant(ctx, v);
    }
    [[fallthrough]];
  case Expr::ARRAY_SUBSCRIPT_EXPR:
  case Expr::MEMBER_EXPR:
  case Expr::METHOD_EXPR:
    return gen_load_of_scalar(ctx, gen_lvalue(ctx, *e), e->type);
  case Expr::EXPLICIT_CAST_EXPR:
  case Expr::IMPLICIT_CAST_EXPR:
    return gen_cast_expr(ctx, *e->dyn_cast<CastExpr>());
  case Expr::METHOD_CALL_EXPR:
  case Expr::CALL_EXPR:
    return gen_call_expr(ctx, *e->dyn_cast<CallExpr>());
  case Expr::UNARY_EXPR:
    return gen_unary_expr(ctx, *e->dyn_cast<UnaryExpr>());
  case Expr::COMPOUND_ASSIGN_EXPR: {
    auto lhs =
        gen_compound_assign_lvalue(ctx, *e->dyn_cast<CompoundAssignExpr>());
    if (ignore)
      return nullptr;
    return gen_load_of_scalar(ctx, lhs, e->type);
  }
  case Expr::CONDITIONAL_EXPR:
    return gen_conditional_expr(ctx, *e->dyn_cast<ConditionalExpr>());
  case Expr::BINARY_EXPR: {
    auto be = e->dyn_cast<BinaryExpr>();
    if (be->is_assignment_op()) {
      auto lhs = gen_assign_lvalue(ctx, *be);
      if (ignore)
        return nullptr;
      return gen_load_of_scalar(ctx, lhs, e->type);
    }
    if (be->opc == BinaryExpr::LAND)
      return gen_land(ctx, *be);
    if (be->opc == BinaryExpr::LOR)
      return gen_lor(ctx, *be);
#define COMP(CODE, UI, SI, FP, SIG)                                            \
  if (be->opc == BinaryExpr::CODE)                                             \
  return gen_compare(ctx, *be, llvm::CmpInst::UI, llvm::CmpInst::SI,           \
                     llvm::CmpInst::FP, SIG)
    COMP(LT, ICMP_ULT, ICMP_SLT, FCMP_OLT, true);
    COMP(LE, ICMP_ULE, ICMP_SLE, FCMP_OLE, true);
    COMP(GT, ICMP_UGT, ICMP_SGT, FCMP_OGT, true);
    COMP(GE, ICMP_UGE, ICMP_SGE, FCMP_OGE, true);
    COMP(EQ, ICMP_EQ, ICMP_EQ, FCMP_OEQ, false);
    COMP(NE, ICMP_NE, ICMP_NE, FCMP_UNE, false);
#undef COMP
    return gen_binary_expr_inner(ctx, gen_bin_ops(ctx, *be));
  }
  case Expr::VAARG_EXPR:
    return gen_vaarg_expr(ctx, *e->dyn_cast<VAArgExpr>());
  }
}

LValue gen_decl_ref_lvalue(CGContext &ctx, DeclRefExpr const &e) {
  NamedDecl const *nd = e.decl;

  if (const auto *fd = nd->dyn_cast<FunctionDecl>())
    return get_addr_of_function(ctx, fd, convert_type(ctx, fd->type));

  const auto *vd = nd->dyn_cast<VarDecl>();
  if (!vd)
    assert(false && "unreachable");

  if (vd->is_global)
    return get_addr_of_global_var(ctx, vd, convert_type_for_mem(ctx, vd->type));

  auto iter = ctx.fn.local_decl_map.find(vd);
  if (iter == ctx.fn.local_decl_map.end())
    assert(false && "unreachable");

  return iter->second;
}

LValue gen_pointer(CGContext &ctx, const Expr *e) {
  e = e->ignore_parens();
  if (auto ce = e->dyn_cast<CastExpr>()) {
    switch (ce->cast_kind) {
    case CastExpr::NOOP:
      if (auto ptr_ty = ce->op->type->dyn_cast<PointerType>()) {
        if (ptr_ty->pointee_type->is_void_type())
          break;
        return gen_pointer(ctx, ce->op.get());
      }
      break;
    case CastExpr::ARRAY_TO_POINTER_DECAY: {
      auto addr = gen_lvalue(ctx, *ce->op);
      auto elt_ty =
          convert_type(ctx, ce->op->type->dyn_cast<ArrayType>()->element_type);
      auto idx = ctx.builder.getInt32(0);
      return ctx.builder.CreateInBoundsGEP(elt_ty, addr, {idx, idx}, "arraydecay");
    }
    default:
      break;
    }
  }

  if (auto ue = e->dyn_cast<UnaryExpr>()) {
    if (ue->opc == UnaryExpr::ADDROF) {
      return gen_lvalue(ctx, *ue->arg);
    }
  }

  // Pointer arithmetic: pointer +/- index.
  if (auto be = e->dyn_cast<BinaryExpr>()) {
    if (be->opc == BinaryExpr::ADD || be->opc == BinaryExpr::SUB)
      return gen_pointer_arithmetic(ctx, gen_bin_ops(ctx, *be),
                                    be->opc == BinaryExpr::SUB);
  }

  return gen_scalar_expr(ctx, e);
}

LValue gen_unary_expr_lvalue(CGContext &ctx, UnaryExpr const &e) {
  switch (e.opc) {
  default:
    assert(false && "Unknown unary operator lvalue!");
  case UnaryExpr::DEREF: {
    return gen_pointer(ctx, e.arg.get());
  }
  case UnaryExpr::PREINC:
  case UnaryExpr::PREDEC: {
    LValue lv = gen_lvalue(ctx, *e.arg);
    bool is_inc = e.opc == UnaryExpr::PREINC;
    gen_pre_post_inc_dec(ctx, e, lv, is_inc, true);
    return lv;
  }
  }
}

static const Expr *is_simple_array_decay_operand(const Expr *e) {
  const auto *ce = e->dyn_cast<CastExpr>();
  if (!ce || ce->cast_kind != CastExpr::ARRAY_TO_POINTER_DECAY)
    return nullptr;
  return ce->op.get();
}

LValue gen_array_subscript_expr(CGContext &ctx, ArraySubscriptExpr const &e) {
  auto elt_type = e.type;

  if (const Expr *array = is_simple_array_decay_operand(e.lhs.get())) {
    assert(array->type->is_array_type());
    LValue array_lv;
    if (const auto *ase = array->dyn_cast<ArraySubscriptExpr>())
      array_lv = gen_array_subscript_expr(ctx, *ase);
    else
      array_lv = gen_lvalue(ctx, *array);
    auto *idx = gen_scalar_expr(ctx, e.rhs.get());

    if (idx->getType() != llvm::Type::getInt32Ty(ctx.llvmctx))
      idx = ctx.builder.CreateIntCast(
          idx, llvm::Type::getInt32Ty(ctx.llvmctx),
          e.rhs->type->is_signed_integer_or_enumeration_type(), "idxprom");

    return ctx.builder.CreateGEP(convert_type_for_mem(ctx, elt_type), array_lv, idx, "arrayidx");
  } else {
    // The base must be a pointer; emit it with an estimate of its alignment.
    auto base_addr = gen_pointer(ctx, e.lhs.get());
    auto *idx = gen_scalar_expr(ctx, e.rhs.get());

    if (idx->getType() != llvm::Type::getInt64Ty(ctx.llvmctx))
      idx = ctx.builder.CreateIntCast(
          idx, llvm::Type::getInt64Ty(ctx.llvmctx),
          e.rhs->type->is_signed_integer_or_enumeration_type(), "idxprom");

    return ctx.builder.CreateGEP(convert_type_for_mem(ctx, elt_type), base_addr,
                                 idx, "arrayidx");
  }
}

LValue gen_member_expr(CGContext &ctx, MemberExpr const &e) {
  if (auto *method = e.dyn_cast<MethodExpr>()) {
    auto fd = method->method_func;
    return get_addr_of_function(ctx, fd, convert_type(ctx, fd->type));
  }

  LValue base_lv;
  if (e.is_arrow) {
    base_lv = gen_pointer(ctx, e.base.get());
  } else {
    base_lv = gen_lvalue(ctx, *e.base);
  }

  auto *field = e.field_decl;
  auto *st_decl = field->parent;
  assert(st_decl);

  u32 idx = 0;
  for (auto &f : st_decl->fields) {
    if (f.get() == field)
      break;
    idx++;
  }

  return ctx.builder.CreateStructGEP(convert_type_for_mem(ctx, st_decl->type_for_decl), base_lv,
                                     idx);
}

struct CondInfo {
  llvm::BasicBlock *lhs_b, *rhs_b;
  std::optional<LValue> lhs, rhs;
};

LValue gen_conditional_expr_lvalue(CGContext &ctx, ConditionalExpr const &e) {
  bool cond_v;
  if (constant_folds_to_simple_integer(ctx, e.cond.get(), cond_v)) {
    Expr const *live = e.true_expr.get(), *dead = e.false_expr.get();
    if (!cond_v) std::swap(live, dead);
    return gen_lvalue(ctx, *live);
  }

  auto lhs_b = llvm::BasicBlock::Create(ctx.llvmctx, "cond.true");
  auto rhs_b = llvm::BasicBlock::Create(ctx.llvmctx, "cond.false");
  auto end_block = llvm::BasicBlock::Create(ctx.llvmctx, "cond.end");

  gen_branch_on_bool_expr(ctx, e.cond.get(), lhs_b, rhs_b);
  gen_block(ctx, lhs_b);
  auto lhs = gen_lvalue(ctx, *e.false_expr);
  lhs_b = ctx.builder.GetInsertBlock();
  ctx.builder.CreateBr(end_block);
  gen_block(ctx, rhs_b);
  auto rhs = gen_lvalue(ctx, *e.false_expr);
  rhs_b = ctx.builder.GetInsertBlock();
  gen_block(ctx, end_block);

  ctx.builder.SetInsertPoint(ctx.builder.GetInsertBlock());
  auto ptr_phi = ctx.builder.CreatePHI(lhs->getType(), 2, "cond");
  ptr_phi->addIncoming(lhs, lhs_b);
  ptr_phi->addIncoming(rhs, rhs_b);
  return ptr_phi;
}

LValue gen_cast_lvalue(CGContext &ctx, CastExpr const &e) {
  switch (e.cast_kind) {
  case CastExpr::LVALUE_TO_RVALUE:
  case CastExpr::NOOP:
    return gen_lvalue(ctx, *e.op);
  default:
    return llvm::UndefValue::get(llvm::PointerType::getUnqual(ctx.llvmctx));
  }
}

LValue gen_lvalue(CGContext &ctx, Expr const &e) {
  switch (e.kind) {
  default:
    return llvm::UndefValue::get(llvm::PointerType::getUnqual(ctx.llvmctx));
  case Expr::BINARY_EXPR:
    return gen_assign_lvalue(ctx, *e.dyn_cast<BinaryExpr>());
  case Expr::COMPOUND_ASSIGN_EXPR:
    return gen_compound_assign_lvalue(ctx, *e.dyn_cast<CompoundAssignExpr>());
  case Expr::METHOD_CALL_EXPR:
  case Expr::CALL_EXPR:
    return static_cast<LValue>(gen_call_expr(ctx, *e.dyn_cast<CallExpr>()));
  case Expr::DECLREF_EXPR:
    return gen_decl_ref_lvalue(ctx, *e.dyn_cast<DeclRefExpr>());
  case Expr::PAREN_EXPR:
    return gen_lvalue(ctx, *e.dyn_cast<ParenExpr>()->val);
  case Expr::STRING_LITERAL:
    return get_addr_of_constant_string(ctx, *e.dyn_cast<StringLiteral>());
  case Expr::UNARY_EXPR:
    return gen_unary_expr_lvalue(ctx, *e.dyn_cast<UnaryExpr>());
  case Expr::ARRAY_SUBSCRIPT_EXPR:
    return gen_array_subscript_expr(ctx, *e.dyn_cast<ArraySubscriptExpr>());
  case Expr::MEMBER_EXPR:
  case Expr::METHOD_EXPR:
    return gen_member_expr(ctx, *e.dyn_cast<MemberExpr>());
  case Expr::CONDITIONAL_EXPR:
    return gen_conditional_expr_lvalue(ctx, *e.dyn_cast<ConditionalExpr>());
  case Expr::IMPLICIT_CAST_EXPR:
  case Expr::EXPLICIT_CAST_EXPR:
    return gen_cast_lvalue(ctx, *e.dyn_cast<CastExpr>());
  }
  assert(false && "unreachable");
}

void gen_ignored_expr(CGContext &ctx, Expr const &e) {
  if (e.is_prvalue())
    gen_scalar_expr(ctx, &e, true);
  else
    gen_lvalue(ctx, e);
}

llvm::Value *gen_scalar_conversion(CGContext &ctx, llvm::Value *v,
                                   Type *from_ty, Type *to_ty, Loc l) {
  (void)l;
  from_ty = ctx.astctx.desugar_type(from_ty);
  to_ty = ctx.astctx.desugar_type(to_ty);
  if (from_ty == to_ty) return v;
  if (to_ty->is_void_type()) return nullptr;

  // TODO: handle all types
  auto src_ty = v->getType();

  if (to_ty->is_boolean_type()) {
    if (isa<llvm::IntegerType>(src_ty)) {
      return ctx.builder.CreateIsNotNull(v, "tobool");
    } else {
      // only ptr for now
      return ctx.builder.CreateICmpNE(v,
          llvm::ConstantPointerNull::get(
              llvm::PointerType::getUnqual(ctx.llvmctx)),
          "tobool");
    }
  }

  auto dst_ty = convert_type(ctx, to_ty);

  if (src_ty == dst_ty) return v;

  if (isa<llvm::PointerType>(dst_ty)) {
    if (isa<llvm::PointerType>(src_ty)) return v;
    auto int_res = ctx.builder.CreateIntCast(v, llvm::Type::getInt64Ty(ctx.llvmctx), from_ty->is_signed_integer_or_enumeration_type(), "conv");
    return ctx.builder.CreateIntToPtr(int_res, dst_ty, "conv");
  }
  if (isa<llvm::PointerType>(src_ty)) {
    return ctx.builder.CreatePtrToInt(v, dst_ty);
  }

  return ctx.builder.CreateIntCast(v, dst_ty, from_ty->is_signed_integer_or_enumeration_type(), "conv");
}
