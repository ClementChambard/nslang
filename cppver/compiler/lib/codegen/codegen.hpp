#ifndef CODEGEN_CODEGEN_HPP_INCLUDED
#define CODEGEN_CODEGEN_HPP_INCLUDED

#include "ast/nodes/decl.hpp"
#include "ast/nodes/stmt.hpp"
#include "codegen/context.hpp"
#include <llvm/IR/Value.h>

llvm::Type *convert_type(CGContext &ctx, Type *type);
llvm::Type *convert_type_for_mem(CGContext &ctx, Type *type);

using LValue = llvm::Value *;
using RValue = llvm::Value *;

void gen_top_level_decl(CGContext &ctx, Decl const &d);
void gen_decl(CGContext &ctx, Decl const &d);
void gen_func(CGContext &ctx, llvm::Function *f, FunctionDecl const &d);
void gen_compound_stmt(CGContext &ctx, CompoundStmt const &s);
void gen_stmt(CGContext &ctx, Stmt const &s);
void gen_ignored_expr(CGContext &ctx, Expr const &e);
void gen_store_of_scalar(CGContext &ctx, RValue value, LValue addr, Type *t);
llvm::Value *gen_load_of_scalar(CGContext &ctx, LValue addr, Type *t);

llvm::Constant *get_addr_of_function(CGContext &ctx, FunctionDecl const *d,
                                     llvm::Type *ty);
llvm::Constant *get_addr_of_global_var(CGContext &ctx, VarDecl const *d,
                                       llvm::Type *ty);
llvm::Constant *get_addr_of_constant_string(CGContext &ctx,
                                            StringLiteral const &s,
                                            std::string const &name = ".str");

llvm::Value *gen_scalar_expr(CGContext &ctx, Expr const *e,
                             bool ignore = false);
llvm::Value *gen_scalar_conversion(CGContext &ctx, llvm::Value *v,
                                   Type *from_ty, Type *to_ty, Loc l);

llvm::AllocaInst *gen_temp_alloca(CGContext &ctx, llvm::Type *ty, u32 align,
                                  std::string const &name);

bool constant_folds_to_simple_integer(CGContext &ctx, Expr const *cond, bool &result);

void gen_branch_on_bool_expr(CGContext &ctx, Expr const *cond, llvm::BasicBlock *true_block, llvm::BasicBlock *false_block);
void gen_block(CGContext &ctx, llvm::BasicBlock *bb);

llvm::Value *gen_or_get_valist(CGContext &ctx);

#endif // !CODEGEN_CODEGEN_HPP_INCLUDED
