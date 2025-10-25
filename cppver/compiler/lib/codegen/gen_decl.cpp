#include "ast/context.hpp"
#include "ast/nodes/expr.hpp"
#include "ast/nodes/type.hpp"
#include "codegen/codegen.hpp"
#include "codegen/context.hpp"
#include <llvm/IR/Attributes.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/Support/Alignment.h>

llvm::Constant *get_constant_array_from_string_literal(CGContext &ctx,
                                                       const StringLiteral &e) {
  std::string s = e.value;
  s.resize(e.type->dyn_cast<ArrayType>()->element_count);
  return llvm::ConstantDataArray::getString(ctx.llvmctx, s, false);
}

llvm::Constant *get_addr_of_constant_string(CGContext &ctx,
                                            StringLiteral const &s,
                                            std::string const &name) {

  llvm::Constant *c = get_constant_array_from_string_literal(ctx, s);
  auto entry = ctx.constant_string_map[c];
  if (entry) {
    return entry;
  }

  auto *GV = new llvm::GlobalVariable(
      ctx.module, c->getType(), true, llvm::GlobalValue::PrivateLinkage, c,
      name, nullptr, llvm::GlobalVariable::NotThreadLocal, 0);
  GV->setAlignment(llvm::Align{1});
  GV->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
  return GV;
}

llvm::Constant *get_or_create_llvm_function(CGContext &ctx,
                                            std::string const &name,
                                            llvm::Type *ty) {
  llvm::GlobalValue *entry = ctx.module.getNamedValue(name);
  if (entry)
    return entry;

  auto fty = cast<llvm::FunctionType>(ty);

  llvm::Function *f = llvm::Function::Create(
      fty, llvm::Function::ExternalLinkage, name, ctx.module);

  assert(f->getName() == name && "name was uniqued!");

  // llvm::AttrBuilder func_attrs{ctx.llvmctx};
  // f->addFnAttrs(func_attrs);

  return f;
}

llvm::Constant *get_addr_of_function(CGContext &ctx, FunctionDecl const *d,
                                     llvm::Type *ty) {
  if (!ty)
    ty = convert_type(ctx, d->type);

  std::string name = d->get_mangled_name();
  return get_or_create_llvm_function(ctx, name, ty);
}

void gen_global(CGContext &ctx, FunctionDecl const *d) {
  if (!d->body) {
    return;
  }

  // maybe don't always emit it, or defer it

  // Emit the definition if it can't be deferred.

  // const CGFunctionInfo &FI = getTypes().arrangeGlobalDeclaration(d);
  llvm::FunctionType *ty = dyn_cast<llvm::FunctionType>(
      convert_type(ctx, d->type)); // getTypes().GetFunctionType(FI);

  llvm::GlobalValue *gv =
      cast<llvm::GlobalValue>(get_addr_of_function(ctx, d, ty));

  if (!gv->isDeclaration())
    return;

  auto *fn = cast<llvm::Function>(gv);

  fn->setLinkage(d->is_lib ? llvm::Function::ExternalLinkage
                           : llvm::Function::InternalLinkage);
  if (d->is_lib) {
    fn->setDLLStorageClass(llvm::Function::DLLExportStorageClass);
  }
  // setGlobalVisibility(gv, d);
  // setDSOLocal(gv);

  gen_func(ctx, fn, *d); // FI

  llvm::AttrBuilder attrs(ctx.llvmctx);
  attrs.addAttribute(llvm::Attribute::NoUnwind);
  fn->addFnAttrs(attrs);
}

llvm::Constant *get_or_create_llvm_global(CGContext &ctx,
                                          std::string const &name,
                                          llvm::Type *ty, VarDecl const *d) {
  auto entry = ctx.module.getNamedValue(name);
  if (entry)
    return entry;

  auto *gv = new llvm::GlobalVariable(
      ctx.module, ty, false, llvm::GlobalValue::ExternalLinkage, nullptr, name);

  if (d) {
    gv->setAlignment(llvm::Align{ctx.astctx.get_type_align(d->type)});
  }

  return gv;
}

llvm::Constant *get_addr_of_global_var(CGContext &ctx, VarDecl const *d,
                                       llvm::Type *ty) {
  if (!ty)
    ty = convert_type_for_mem(ctx, d->type);
  auto name = d->get_name();
  return get_or_create_llvm_global(ctx, name, ty, d);
}

void gen_global(CGContext &ctx, VarDecl const *d) {
  // TODO: if it refers to outside file, return early
  //  .. for now, no 'extern' variables

  // maybe don't always emit it, or defer it

  auto init = llvm::Constant::getNullValue(convert_type_for_mem(ctx, d->type));
  llvm::Constant *entry = get_addr_of_global_var(ctx, d, init->getType());
  entry = entry->stripPointerCasts();
  auto *gv = dyn_cast<llvm::GlobalVariable>(entry);
  gv->setInitializer(init);
  gv->setConstant(false);
  gv->setAlignment(llvm::Align{ctx.astctx.get_type_align(d->type)});
  gv->setLinkage(llvm::GlobalValue::InternalLinkage);
  gv->setDLLStorageClass(llvm::GlobalVariable::DefaultStorageClass);
}

void gen_top_level_decl(CGContext &ctx, Decl const &d) {
  if (auto *f = d.dyn_cast<FunctionDecl>()) {
    gen_global(ctx, f);
  } else if (auto *v = d.dyn_cast<VarDecl>()) {
    gen_global(ctx, v);
  }
}

void gen_var_decl(CGContext &ctx, const VarDecl &d) {
  auto address =
      gen_temp_alloca(ctx, convert_type_for_mem(ctx, d.type),
                      ctx.astctx.get_type_align(d.type), d.get_name());
  ctx.fn.local_decl_map[&d] = address;
  if (d.initializer) {
    auto res = gen_scalar_expr(ctx, d.initializer.get());
    if (!d.initializer->dyn_cast<InitCallExpr>()) {
      gen_store_of_scalar(ctx, res, address, d.type);
    }
  }
}

void gen_decl(CGContext &ctx, Decl const &d) {
  auto *vd = d.dyn_cast<VarDecl>();
  assert(vd && "only var decl supported");
  gen_var_decl(ctx, *vd);
}
