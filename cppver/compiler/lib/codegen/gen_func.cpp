#include <cassert>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include "ast/nodes/decl.hpp"
#include "ast/nodes/stmt.hpp"
#include "ast/nodes/type.hpp"
#include "codegen/codegen.hpp"
#include "codegen/context.hpp"

llvm::Value *gen_tomemory_conversions(CGContext &ctx, llvm::Value *value, Type *ty) {
  if (ty->is_boolean_type()) {
    llvm::Type *store_ty = llvm::IntegerType::get(ctx.llvmctx, 8);
    bool is_signed = ty->is_signed_integer_or_enumeration_type();
    return ctx.builder.CreateIntCast(value, store_ty, is_signed, "storedv");
  }
  return value;
}

void gen_store_of_scalar(CGContext &ctx, llvm::Value *value, llvm::Value *addr, Type *t) {
  value = gen_tomemory_conversions(ctx, value, t);

  llvm::StoreInst *store = ctx.builder.CreateAlignedStore(value, addr, llvm::Align{ctx.astctx.get_type_align(t)});
  (void)store;
}

llvm::AllocaInst *gen_temp_alloca(CGContext &ctx, llvm::Type *ty, u32 align, std::string const &name) {
  llvm::AllocaInst *alloca;
  if (ctx.fn.alloca_insert_pt) {
    auto it = ctx.fn.alloca_insert_pt->getIterator();
    alloca = new llvm::AllocaInst(ty, 0, nullptr, name, ++it);
  } else {
    alloca = ctx.builder.CreateAlloca(ty, nullptr, name);
  }
  ctx.fn.alloca_insert_pt = alloca;
  // TODO:
  // - put it at the top of the function
  // - track all allocas ?
  alloca->setAlignment(llvm::Align{align});
  return alloca;
}


void gen_param_decl(CGContext &ctx, ParamDecl const &d, llvm::Value *arg) {
  if (!isa<llvm::GlobalValue>(arg)) arg->setName(d.get_name());
  Type *ty = d.type;
  llvm::Value *decl_ptr = gen_temp_alloca(ctx, convert_type_for_mem(ctx, ty), ctx.astctx.get_type_align(ty), d.get_name() + ".addr");
  gen_store_of_scalar(ctx, arg, decl_ptr, d.type);
 
  ctx.fn.local_decl_map[&d] = decl_ptr;
}

llvm::Type *get_valist_type(CGContext &ctx) {
  auto i32_ty = ctx.builder.getInt32Ty();
  auto ptr_ty = ctx.builder.getPtrTy();
  return llvm::StructType::get(i32_ty, i32_ty, ptr_ty, ptr_ty);
}

llvm::Value *gen_or_get_valist(CGContext &ctx) {
  if (ctx.fn.valist) return ctx.fn.valist;
  ctx.fn.valist = gen_temp_alloca(ctx, get_valist_type(ctx), 16, "valist");
  // after all allocas
  auto it = ctx.fn.alloca_insert_pt->getIterator();
  it++;

  auto ip = ctx.builder.saveIP();

  ctx.builder.SetInsertPoint(it);

  ctx.builder.CreateIntrinsic(ctx.builder.getVoidTy(), llvm::Intrinsic::vastart, {ctx.fn.valist});

  ctx.builder.restoreIP(ip);

  return ctx.fn.valist;
}



void gen_func(CGContext &ctx, llvm::Function *fn, FunctionDecl const &d) {
  assert(fn);

  Stmt *body = d.body.get();
  assert(body);

  ctx.fn.cur_fn = fn;
  ctx.fn.entry_bb =  llvm::BasicBlock::Create(ctx.llvmctx, "entry", fn, nullptr);
  ctx.fn.alloca_insert_pt = nullptr;
  ctx.fn.local_decl_map.clear();
  ctx.fn.valist = nullptr;

  if (d.is_lib || d.get_name() == "main") {
    fn->setLinkage(llvm::Function::ExternalLinkage);
  }

  ctx.builder.SetInsertPoint(ctx.fn.entry_bb);

  for (u32 i = 0; i < d.params.size(); i++) {
    gen_param_decl(ctx, *d.params[i].get(), fn->getArg(i));
  }

  fn->addFnAttr(llvm::Attribute::MustProgress);
  if (auto *b = body->dyn_cast<CompoundStmt>()) {
    gen_compound_stmt(ctx, *b);
  } else {
    gen_stmt(ctx, *body);
  }

  assert(ctx.fn.break_continue_stack.empty());

  auto last_block = ctx.builder.GetInsertBlock();
  if (last_block && !last_block->getTerminator()) {
    auto ret_ty = d.type->dyn_cast<FunctionType>()->result_type;
    if (ret_ty->is_void_type()) {
      ctx.builder.CreateRetVoid();
    } else {
      ctx.builder.CreateRet(llvm::Constant::getNullValue(convert_type(ctx, ret_ty)));
    }
  }
}
