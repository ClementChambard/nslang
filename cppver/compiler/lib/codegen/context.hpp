#ifndef  CODEGEN_CONTEXT_HPP_INCLUDED
#define  CODEGEN_CONTEXT_HPP_INCLUDED

#include "ast/context.hpp"
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalValue.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>
#include <unordered_map>

struct CGFunctionCtx {
  llvm::Function *cur_fn{};
  llvm::BasicBlock *entry_bb{};
  llvm::Instruction *alloca_insert_pt{};
  std::unordered_map<Decl const*, llvm::Value*> local_decl_map{};
  struct BreakContinue { Stmt const *scope_stmt{}; llvm::BasicBlock *break_d{}; llvm::BasicBlock *continue_d{}; };
  std::vector<BreakContinue> break_continue_stack;
  llvm::SwitchInst *switch_insn{};
  llvm::Value *valist = nullptr;
};

struct CGContext {
  ASTContext &astctx;
  llvm::LLVMContext llvmctx{};
  llvm::Module module;
  llvm::IRBuilder<> builder{llvmctx};
  CGFunctionCtx fn{};
  std::unordered_map<llvm::Constant*, llvm::GlobalVariable *> constant_string_map{};
  std::string filename;

  CGContext(std::string const &filename, ASTContext &astctx) :
    astctx(astctx), module(filename, llvmctx), filename(filename) {}

  void gen_tu(TranslationUnitDecl const *tu);
};

#endif // ! CODEGEN_CONTEXT_HPP_INCLUDED
