#include "ast/context.hpp"
#include "ast/nodes/expr.hpp"
#include "ast/nodes/stmt.hpp"
#include "codegen.hpp"
#include "context.hpp"
#include <functional>
#include <llvm/ADT/APInt.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>

void gen_break_stmt(CGContext &ctx, const BreakStmt &_) {
  (void)_;
  if (!ctx.builder.GetInsertBlock())
    return;
  auto &loc = ctx.fn.break_continue_stack.back().break_d;
  ctx.builder.CreateBr(loc);
  ctx.builder.ClearInsertionPoint();
}

void gen_continue_stmt(CGContext &ctx, const ContinueStmt &_) {
  (void)_;
  if (!ctx.builder.GetInsertBlock())
    return;
  auto &loc = ctx.fn.break_continue_stack.back().continue_d;
  ctx.builder.CreateBr(loc);
  ctx.builder.ClearInsertionPoint();
}

void gen_decl_stmt(CGContext &ctx, const DeclStmt &s) {
  gen_decl(ctx, *s.decl);
}

void gen_branch(CGContext &ctx, llvm::BasicBlock *target) {
  auto cur_bb = ctx.builder.GetInsertBlock();
  if (!cur_bb || cur_bb->getTerminator()) {

  } else {
    ctx.builder.CreateBr(target);
  }
  ctx.builder.ClearInsertionPoint();
}

void gen_block(CGContext &ctx, llvm::BasicBlock *bb) {
  gen_branch(ctx, bb);
  auto cur_bb = ctx.builder.GetInsertBlock();
  if (cur_bb && cur_bb->getParent()) {
    ctx.fn.cur_fn->insert(std::next(cur_bb->getIterator()), bb);
  } else {
    ctx.fn.cur_fn->insert(ctx.fn.cur_fn->end(), bb);
  }
  ctx.builder.SetInsertPoint(bb);
}

void gen_default_stmt(CGContext &ctx, const DefaultStmt &s) {
  if (!ctx.fn.switch_insn)
    return gen_stmt(ctx, *s.sub_stmt);
  auto default_block = ctx.fn.switch_insn->getDefaultDest();
  assert(default_block->empty());
  gen_block(ctx, default_block);
  gen_stmt(ctx, *s.sub_stmt);
}

void gen_case_stmt(CGContext &ctx, const CaseStmt &s) {
  if (!ctx.fn.switch_insn)
    return gen_stmt(ctx, *s.sub_stmt);

  llvm::ConstantInt *case_val =
      ctx.builder.getInt64(s.case_val); // TODO: what should the size be ?

  if (s.sub_stmt->isa<BreakStmt>()) {
    auto block = ctx.fn.break_continue_stack.back().break_d;
    ctx.fn.switch_insn->addCase(case_val, block);
    if (ctx.builder.GetInsertBlock()) {
      ctx.builder.CreateBr(block);
      ctx.builder.ClearInsertionPoint();
    }
    return;
  }

  auto case_dest =
      llvm::BasicBlock::Create(ctx.llvmctx, "sw.bb", nullptr, nullptr);
  gen_block(ctx, case_dest);
  ctx.fn.switch_insn->addCase(case_val, case_dest);

  const CaseStmt *cur_case = &s;
  const CaseStmt *next_case = s.sub_stmt->dyn_cast<CaseStmt>();

  while (next_case) {
    cur_case = next_case;

    llvm::ConstantInt *case_val = ctx.builder.getInt64(
        cur_case->case_val); // TODO: what should the size be ?
    ctx.fn.switch_insn->addCase(case_val, case_dest);
    next_case = cur_case->sub_stmt->dyn_cast<CaseStmt>();
  }

  gen_stmt(ctx, *cur_case->sub_stmt);
}

bool gen_simple_stmt(CGContext &ctx, Stmt const &s) {
  switch (s.kind) {
  default:
    return false;
  case Stmt::NULL_STMT:
    return true;
  case Stmt::COMPOUND_STMT:
    gen_compound_stmt(ctx, *s.dyn_cast<CompoundStmt>());
    return true;
  case Stmt::DECL_STMT:
    gen_decl_stmt(ctx, *s.dyn_cast<DeclStmt>());
    return true;
  case Stmt::BREAK_STMT:
    gen_break_stmt(ctx, *s.dyn_cast<BreakStmt>());
    return true;
  case Stmt::CONTINUE_STMT:
    gen_continue_stmt(ctx, *s.dyn_cast<ContinueStmt>());
    return true;
  case Stmt::DEFAULT_STMT:
    gen_default_stmt(ctx, *s.dyn_cast<DefaultStmt>());
    return true;
  case Stmt::CASE_STMT:
    gen_case_stmt(ctx, *s.dyn_cast<CaseStmt>());
    return true;
  }
}

#include "semantic_analysis/constexpr.hpp"

bool constant_folds_to_simple_integer(CGContext &ctx, Expr const *cond,
                                      bool &result) {
  auto [res, ok] = eval_integer_constexpr(ctx.astctx, cond);
  if (!ok)
    return false;
  result = res != 0;
  return true;
}

void gen_branch_on_bool_expr(CGContext &ctx, Expr const *cond,
                             llvm::BasicBlock *true_block,
                             llvm::BasicBlock *false_block) {
  cond = cond->ignore_parens();

  if (auto be = cond->dyn_cast<BinaryExpr>()) {
    if (be->opc == BinaryExpr::LAND) {
      bool c_bool = false;

      if (constant_folds_to_simple_integer(ctx, be->lhs.get(), c_bool) &&
          c_bool) {
        gen_branch_on_bool_expr(ctx, be->rhs.get(), true_block, false_block);
        return;
      }

      if (constant_folds_to_simple_integer(ctx, be->rhs.get(), c_bool) &&
          c_bool) {
        gen_branch_on_bool_expr(ctx, be->lhs.get(), true_block, false_block);
        return;
      }

      llvm::BasicBlock *lhs_true =
          llvm::BasicBlock::Create(ctx.llvmctx, "land.lhs.true");
      gen_branch_on_bool_expr(ctx, be->lhs.get(), lhs_true, false_block);
      gen_block(ctx, lhs_true);
      gen_branch_on_bool_expr(ctx, be->rhs.get(), true_block, false_block);
      return;
    }

    if (be->opc == BinaryExpr::LOR) {
      bool c_bool = false;

      if (constant_folds_to_simple_integer(ctx, be->lhs.get(), c_bool) &&
          !c_bool) {
        gen_branch_on_bool_expr(ctx, be->rhs.get(), true_block, false_block);
        return;
      }

      if (constant_folds_to_simple_integer(ctx, be->rhs.get(), c_bool) &&
          !c_bool) {
        gen_branch_on_bool_expr(ctx, be->lhs.get(), true_block, false_block);
        return;
      }

      llvm::BasicBlock *lhs_false =
          llvm::BasicBlock::Create(ctx.llvmctx, "lor.lhs.false");
      gen_branch_on_bool_expr(ctx, be->lhs.get(), true_block, lhs_false);
      gen_block(ctx, lhs_false);
      gen_branch_on_bool_expr(ctx, be->rhs.get(), true_block, false_block);
      return;
    }
  }

  if (auto ue = cond->dyn_cast<UnaryExpr>()) {
    if (ue->opc == UnaryExpr::LNOT) {
      return gen_branch_on_bool_expr(ctx, ue->arg.get(), false_block,
                                     true_block);
    }
  }

  if (auto ce = cond->dyn_cast<ConditionalExpr>()) {
    llvm::BasicBlock *lhs_block =
        llvm::BasicBlock::Create(ctx.llvmctx, "cond.true");
    llvm::BasicBlock *rhs_block =
        llvm::BasicBlock::Create(ctx.llvmctx, "cond.false");
    gen_branch_on_bool_expr(ctx, ce->cond.get(), lhs_block, rhs_block);
    gen_block(ctx, lhs_block);
    gen_branch_on_bool_expr(ctx, ce->true_expr.get(), true_block, false_block);
    gen_block(ctx, rhs_block);
    gen_branch_on_bool_expr(ctx, ce->false_expr.get(), true_block, false_block);
    return;
  }

  auto cond_v =
      gen_scalar_conversion(ctx, gen_scalar_expr(ctx, cond), cond->type,
                            ctx.astctx.bool_ty, cond->get_start_loc());

  ctx.builder.CreateCondBr(cond_v, true_block, false_block);
}

void gen_if_stmt(CGContext &ctx, IfStmt const &s) {
  bool cond_cst;
  if (constant_folds_to_simple_integer(ctx, s.cond.get(), cond_cst)) {
    Stmt const *executed = s.then_stmt.get();
    Stmt const *skipped = s.else_stmt.get();
    if (!cond_cst)
      std::swap(executed, skipped);
    if (executed) {
      gen_stmt(ctx, *executed);
    }
    return;
  }

  llvm::BasicBlock *then_block =
      llvm::BasicBlock::Create(ctx.llvmctx, "if.then");
  llvm::BasicBlock *cont_block =
      llvm::BasicBlock::Create(ctx.llvmctx, "if.end");
  llvm::BasicBlock *else_block = cont_block;
  if (s.else_stmt)
    else_block = llvm::BasicBlock::Create(ctx.llvmctx, "if.else");

  gen_branch_on_bool_expr(ctx, s.cond.get(), then_block, else_block);

  gen_block(ctx, then_block);
  gen_stmt(ctx, *s.then_stmt);
  gen_branch(ctx, cont_block);

  if (s.else_stmt) {
    gen_block(ctx, else_block);
    gen_stmt(ctx, *s.else_stmt);
    gen_branch(ctx, cont_block);
  }
  gen_block(ctx, cont_block);
}

void gen_while_stmt(CGContext &ctx, WhileStmt const &s) {
  auto loop_header = llvm::BasicBlock::Create(ctx.llvmctx, "while.cond");
  gen_block(ctx, loop_header);

  auto loop_exit = llvm::BasicBlock::Create(ctx.llvmctx, "while.end");

  ctx.fn.break_continue_stack.emplace_back(&s, loop_exit, loop_header);

  auto *cond = s.cond.get();
  llvm::Value *bool_cond_val =
      gen_scalar_conversion(ctx, gen_scalar_expr(ctx, cond), cond->type,
                            ctx.astctx.bool_ty, cond->get_start_loc());

  llvm::ConstantInt *c = dyn_cast<llvm::ConstantInt>(bool_cond_val);
  bool emit_cond = !c || !c->isOne();

  llvm::BasicBlock *loop_body =
      llvm::BasicBlock::Create(ctx.llvmctx, "while.body");
  if (emit_cond) {
    ctx.builder.CreateCondBr(bool_cond_val, loop_body, loop_exit);
  }

  gen_block(ctx, loop_body);
  gen_stmt(ctx, *s.body);

  ctx.fn.break_continue_stack.pop_back();
  gen_branch(ctx, loop_header);
  gen_block(ctx, loop_exit);
  // if (!emit_cond) SimplifyForwardingBlocks(LoopHeader.getBlock());
}

void gen_do_stmt(CGContext &ctx, DoStmt const &s) {
  auto loop_exit = llvm::BasicBlock::Create(ctx.llvmctx, "do.end");
  auto loop_cond = llvm::BasicBlock::Create(ctx.llvmctx, "do.cond");

  ctx.fn.break_continue_stack.emplace_back(&s, loop_exit, loop_cond);

  auto loop_body = llvm::BasicBlock::Create(ctx.llvmctx, "do.body");

  gen_block(ctx, loop_body);
  gen_stmt(ctx, *s.body);
  gen_block(ctx, loop_cond);

  auto *cond = s.cond.get();
  llvm::Value *bool_cond_val =
      gen_scalar_conversion(ctx, gen_scalar_expr(ctx, cond), cond->type,
                            ctx.astctx.bool_ty, cond->get_start_loc());

  ctx.fn.break_continue_stack.pop_back();

  llvm::ConstantInt *c = dyn_cast<llvm::ConstantInt>(bool_cond_val);
  bool emit_cond = !c || !c->isZero();
  if (emit_cond) {
    ctx.builder.CreateCondBr(bool_cond_val, loop_body, loop_exit);
  }

  gen_block(ctx, loop_exit);
  // if (!emit_cond) SimplifyForwardingBlocks(LoopCond.getBlock());
}

void gen_for_stmt(CGContext &ctx, ForStmt const &s) {
  (void)ctx, (void)s;
  assert(false && "no for stmt yet");
}

void gen_return_stmt(CGContext &ctx, ReturnStmt const &s) {
  // TODO: check validity
  if (s.return_value) {
    auto r = gen_scalar_expr(ctx, s.return_value.get());
    ctx.builder.CreateRet(r);
    ctx.builder.ClearInsertionPoint();
  } else {
    ctx.builder.CreateRetVoid();
    ctx.builder.ClearInsertionPoint();
  }
}

void gen_switch_stmt(CGContext &ctx, SwitchStmt const &s) {
  (void)ctx, (void)s;
  assert(false && "no switch yet");
#if false
  llvm::SwitchInst *saved_switch_ins = ctx.fn.switch_insn;
  llvm::BasicBlock *SavedCRBlock = CaseRangeBlock;
  // See if we can constant fold the condition of the switch and therefore only
  // emit the live case statement (if any) of the switch.
  llvm::APSInt ConstantCondValue;
  if (ConstantFoldsToSimpleInteger(S.getCond(), ConstantCondValue)) {
    SmallVector<const Stmt*, 4> CaseStmts;
    const SwitchCase *Case = nullptr;
    if (FindCaseStatementsForValue(S, ConstantCondValue, CaseStmts,
                                   getContext(), Case)) {
      if (Case)
        incrementProfileCounter(Case);
      RunCleanupsScope ExecutedScope(*this);

      if (S.getInit())
        EmitStmt(S.getInit());

      // Emit the condition variable if needed inside the entire cleanup scope
      // used by this special case for constant folded switches.
      if (S.getConditionVariable())
        EmitDecl(*S.getConditionVariable(), /*EvaluateConditionDecl=*/true);

      // At this point, we are no longer "within" a switch instance, so
      // we can temporarily enforce this to ensure that any embedded case
      // statements are not emitted.
      SwitchInsn = nullptr;

      // Okay, we can dead code eliminate everything except this case.  Emit the
      // specified series of statements and we're good.
      for (const Stmt *CaseStmt : CaseStmts)
        EmitStmt(CaseStmt);
      incrementProfileCounter(&S);
      PGO->markStmtMaybeUsed(S.getBody());

      // Now we want to restore the saved switch instance so that nested
      // switches continue to function properly
      SwitchInsn = SavedSwitchInsn;

      return;
    }
  }

  JumpDest SwitchExit = getJumpDestInCurrentScope("sw.epilog");

  RunCleanupsScope ConditionScope(*this);

  if (S.getInit())
    EmitStmt(S.getInit());

  if (S.getConditionVariable())
    EmitDecl(*S.getConditionVariable());
  llvm::Value *CondV = EmitScalarExpr(S.getCond());
  MaybeEmitDeferredVarDeclInit(S.getConditionVariable());

  // Create basic block to hold stuff that comes after switch
  // statement. We also need to create a default block now so that
  // explicit case ranges tests can have a place to jump to on
  // failure.
  llvm::BasicBlock *DefaultBlock = createBasicBlock("sw.default");
  SwitchInsn = Builder.CreateSwitch(CondV, DefaultBlock);
  addInstToNewSourceAtom(SwitchInsn, CondV);

  if (HLSLControlFlowAttr != HLSLControlFlowHintAttr::SpellingNotCalculated) {
    llvm::MDBuilder MDHelper(CGM.getLLVMContext());
    llvm::ConstantInt *BranchHintConstant =
        HLSLControlFlowAttr ==
                HLSLControlFlowHintAttr::Spelling::Microsoft_branch
            ? llvm::ConstantInt::get(CGM.Int32Ty, 1)
            : llvm::ConstantInt::get(CGM.Int32Ty, 2);
    llvm::Metadata *Vals[] = {MDHelper.createString("hlsl.controlflow.hint"),
                              MDHelper.createConstant(BranchHintConstant)};
    SwitchInsn->setMetadata("hlsl.controlflow.hint",
                            llvm::MDNode::get(CGM.getLLVMContext(), Vals));
  }

  if (PGO->haveRegionCounts()) {
    // Walk the SwitchCase list to find how many there are.
    uint64_t DefaultCount = 0;
    unsigned NumCases = 0;
    for (const SwitchCase *Case = S.getSwitchCaseList();
         Case;
         Case = Case->getNextSwitchCase()) {
      if (isa<DefaultStmt>(Case))
        DefaultCount = getProfileCount(Case);
      NumCases += 1;
    }
    SwitchWeights = new SmallVector<uint64_t, 16>();
    SwitchWeights->reserve(NumCases);
    // The default needs to be first. We store the edge count, so we already
    // know the right weight.
    SwitchWeights->push_back(DefaultCount);
  } else if (CGM.getCodeGenOpts().OptimizationLevel) {
    SwitchLikelihood = new SmallVector<Stmt::Likelihood, 16>();
    // Initialize the default case.
    SwitchLikelihood->push_back(Stmt::LH_None);
  }

  CaseRangeBlock = DefaultBlock;

  // Clear the insertion point to indicate we are in unreachable code.
  Builder.ClearInsertionPoint();

  // All break statements jump to NextBlock. If BreakContinueStack is non-empty
  // then reuse last ContinueBlock.
  JumpDest OuterContinue;
  if (!BreakContinueStack.empty())
    OuterContinue = BreakContinueStack.back().ContinueBlock;

  BreakContinueStack.push_back(BreakContinue(S, SwitchExit, OuterContinue));

  // Emit switch body.
  EmitStmt(S.getBody());

  BreakContinueStack.pop_back();

  // Update the default block in case explicit case range tests have
  // been chained on top.
  SwitchInsn->setDefaultDest(CaseRangeBlock);

  // If a default was never emitted:
  if (!DefaultBlock->getParent()) {
    // If we have cleanups, emit the default block so that there's a
    // place to jump through the cleanups from.
    if (ConditionScope.requiresCleanups()) {
      EmitBlock(DefaultBlock);

    // Otherwise, just forward the default block to the switch end.
    } else {
      DefaultBlock->replaceAllUsesWith(SwitchExit.getBlock());
      delete DefaultBlock;
    }
  }

  ConditionScope.ForceCleanup();

  // Emit continuation.
  EmitBlock(SwitchExit.getBlock(), true);
  incrementProfileCounter(&S);

  // If the switch has a condition wrapped by __builtin_unpredictable,
  // create metadata that specifies that the switch is unpredictable.
  // Don't bother if not optimizing because that metadata would not be used.
  auto *Call = dyn_cast<CallExpr>(S.getCond());
  if (Call && CGM.getCodeGenOpts().OptimizationLevel != 0) {
    auto *FD = dyn_cast_or_null<FunctionDecl>(Call->getCalleeDecl());
    if (FD && FD->getBuiltinID() == Builtin::BI__builtin_unpredictable) {
      llvm::MDBuilder MDHelper(getLLVMContext());
      SwitchInsn->setMetadata(llvm::LLVMContext::MD_unpredictable,
                              MDHelper.createUnpredictable());
    }
  }

  if (SwitchWeights) {
    assert(SwitchWeights->size() == 1 + SwitchInsn->getNumCases() &&
           "switch weights do not match switch cases");
    // If there's only one jump destination there's no sense weighting it.
    if (SwitchWeights->size() > 1)
      SwitchInsn->setMetadata(llvm::LLVMContext::MD_prof,
                              createProfileWeights(*SwitchWeights));
    delete SwitchWeights;
  } else if (SwitchLikelihood) {
    assert(SwitchLikelihood->size() == 1 + SwitchInsn->getNumCases() &&
           "switch likelihoods do not match switch cases");
    std::optional<SmallVector<uint64_t, 16>> LHW =
        getLikelihoodWeights(*SwitchLikelihood);
    if (LHW) {
      llvm::MDBuilder MDHelper(CGM.getLLVMContext());
      SwitchInsn->setMetadata(llvm::LLVMContext::MD_prof,
                              createProfileWeights(*LHW));
    }
    delete SwitchLikelihood;
  }
  SwitchInsn = SavedSwitchInsn;
  CaseRangeBlock = SavedCRBlock;
#endif
}

void gen_stmt(CGContext &ctx, Stmt const &s) {
  if (gen_simple_stmt(ctx, s))
    return;

  if (!ctx.builder.GetInsertBlock()) {
    return;
  }

  if (auto *e = s.dyn_cast<Expr>()) {
    llvm::BasicBlock *incoming = ctx.builder.GetInsertBlock();
    assert(incoming);
    gen_ignored_expr(ctx, *e);
    llvm::BasicBlock *outgoing = ctx.builder.GetInsertBlock();
    assert(outgoing);
    if (incoming != outgoing && outgoing->use_empty()) {
      outgoing->eraseFromParent();
      ctx.builder.ClearInsertionPoint();
    }
    return;
  }

  if (auto *ss = s.dyn_cast<IfStmt>())
    return gen_if_stmt(ctx, *ss);
  if (auto *ss = s.dyn_cast<WhileStmt>())
    return gen_while_stmt(ctx, *ss);
  if (auto *ss = s.dyn_cast<DoStmt>())
    return gen_do_stmt(ctx, *ss);
  if (auto *ss = s.dyn_cast<ForStmt>())
    return gen_for_stmt(ctx, *ss);
  if (auto *ss = s.dyn_cast<ReturnStmt>())
    return gen_return_stmt(ctx, *ss);
  if (auto *ss = s.dyn_cast<SwitchStmt>())
    return gen_switch_stmt(ctx, *ss);

  assert(false && "unhandled stmt kind");
}

void gen_compound_stmt(CGContext &ctx, CompoundStmt const &s) {
  for (auto &cur_stmt : s.inner) {
    gen_stmt(ctx, *cur_stmt);
  }
}
