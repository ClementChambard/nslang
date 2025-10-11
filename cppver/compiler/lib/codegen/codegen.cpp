#include "context.hpp"
#include "codegen.hpp"

void CGContext::gen_tu(TranslationUnitDecl *tu) {
  for (auto const & d : tu->decls) {
    gen_top_level_decl(*this, *d.get());
  }
}

llvm::Type *convert_type_for_mem(CGContext &ctx, Type *type) {
  auto r = convert_type(ctx, type);
  if (r->isIntegerTy(1)) return llvm::IntegerType::get(ctx.llvmctx, 8); // bool
  return r;
}

llvm::Type *convert_type(CGContext &ctx, Type* type) {
  if (auto *bt = type->dyn_cast<BuiltinType>()) {
    switch (bt->builtin.kind) {
    case BuiltinType::U8:
    case BuiltinType::I8:
    case BuiltinType::VOID: // why not getVoidTy() ?
      return llvm::IntegerType::get(ctx.llvmctx, 8);
    case BuiltinType::U16:
    case BuiltinType::I16:
      return llvm::IntegerType::get(ctx.llvmctx, 16);
    case BuiltinType::U32:
    case BuiltinType::I32:
      return llvm::IntegerType::get(ctx.llvmctx, 32);
    case BuiltinType::U64:
    case BuiltinType::I64:
      return llvm::IntegerType::get(ctx.llvmctx, 64);
    case BuiltinType::BOOL:
      return llvm::IntegerType::get(ctx.llvmctx, 1);
    case BuiltinType::NULLPTR:
      return llvm::PointerType::getUnqual(ctx.llvmctx);
    }
  } else if (type->isa<PointerType>()) {
    return llvm::PointerType::getUnqual(ctx.llvmctx);
  } else if (auto *at = type->dyn_cast<ArrayType>()) {
    // arrays of empty structs ?
    return llvm::ArrayType::get(convert_type_for_mem(ctx, at->element_type), at->element_count);
  } else if (auto *ed = type->get_as_enum_decl()) {
    return convert_type(ctx, ed->int_ty ? ed->int_ty : ctx.astctx.i32_ty);
  } else if (auto *at = type->dyn_cast<AliasType>()) {
    return convert_type(ctx, at->decl->dyn_cast<AliasDecl>()->underlying_type);
  } else if (auto *sd = type->get_as_struct_decl()) {
    std::vector<llvm::Type *> args;
    for (auto &d : sd->fields) {
      args.push_back(convert_type(ctx, d->type));
    }
    return llvm::StructType::get(ctx.llvmctx, args);
  } else if (auto *ft = type->dyn_cast<FunctionType>()){
    std::vector<llvm::Type *> args;
    for (auto &d : ft->param_types) {
      args.push_back(convert_type(ctx, d));
    }
    auto res_ty = llvm::Type::getVoidTy(ctx.llvmctx);
    if (!ft->result_type->is_void_type()) {
      res_ty = convert_type(ctx, ft->result_type);
    }
    return llvm::FunctionType::get(res_ty, args, ft->function.is_variadic);
  }
  assert(false && "unknown type kind ...");
  return nullptr;
}
