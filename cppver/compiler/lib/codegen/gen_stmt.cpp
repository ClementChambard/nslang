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

  auto ty = dyn_cast<llvm::IntegerType>(ctx.fn.switch_insn->getCondition()->getType());

  llvm::ConstantInt *case_val = llvm::ConstantInt::get(ty, s.case_val);

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
  if (s.init_stmt) gen_stmt(ctx, *s.init_stmt);

  
  auto loop_header = llvm::BasicBlock::Create(ctx.llvmctx, "for.cond");
  gen_block(ctx, loop_header);

  auto loop_exit = llvm::BasicBlock::Create(ctx.llvmctx, "for.end");
  auto loop_latch = llvm::BasicBlock::Create(ctx.llvmctx, "for.latch");

  ctx.fn.break_continue_stack.emplace_back(&s, loop_exit, loop_latch);

  auto *cond = s.cond.get();
  bool emit_cond = false;
  llvm::Value *bool_cond_val;
  if (cond) {
    bool_cond_val = gen_scalar_conversion(ctx, gen_scalar_expr(ctx, cond), cond->type,
                              ctx.astctx.bool_ty, cond->get_start_loc());
    llvm::ConstantInt *c = dyn_cast<llvm::ConstantInt>(bool_cond_val);
    emit_cond = !c || !c->isOne();
  }

  llvm::BasicBlock *loop_body =
      llvm::BasicBlock::Create(ctx.llvmctx, "for.body");
  if (emit_cond) {
    ctx.builder.CreateCondBr(bool_cond_val, loop_body, loop_exit);
  }

  gen_block(ctx, loop_body);
  gen_stmt(ctx, *s.body);

  gen_block(ctx, loop_latch);
  if (s.latch)
    gen_stmt(ctx, *s.latch);

  ctx.fn.break_continue_stack.pop_back();
  gen_branch(ctx, loop_header);
  gen_block(ctx, loop_exit);
}

void gen_return_stmt(CGContext &ctx, ReturnStmt const &s) {
  // TODO: check validity
  if (s.return_value) {
    auto r = gen_scalar_expr(ctx, s.return_value.get());
    if (s.return_value->type->is_void_type()) {
      ctx.builder.CreateRetVoid();
    } else {
      ctx.builder.CreateRet(r);
    }
    ctx.builder.ClearInsertionPoint();
  } else {
    ctx.builder.CreateRetVoid();
    ctx.builder.ClearInsertionPoint();
  }
}


bool contains_break(const Stmt *s) {
  // Null statement, not a label!
  if (!s) return false;

  // If this is a switch or loop that defines its own break scope, then we can
  // include it and anything inside of it.
  if (s->isa<SwitchStmt>() || s->isa<WhileStmt>() || s->isa<DoStmt>() ||
      s->isa<ForStmt>())
    return false;

  if (s->isa<BreakStmt>())
    return true;

  // Scan subexpressions for verboten breaks.
  if (auto ss = s->dyn_cast<SwitchCase>()) return contains_break(ss->sub_stmt.get());
  if (auto ss = s->dyn_cast<IfStmt>()) {
    if (contains_break(ss->then_stmt.get())) return true;
    return !ss->else_stmt || contains_break(ss->else_stmt.get());
  }
  if (auto ss = s->dyn_cast<CompoundStmt>()) {
    for (auto &sss : ss->inner) {
      if (contains_break(sss.get())) return true;
    }
  }

  return false;
}

bool might_add_decl_to_scope(const Stmt *s) {
  if (!s) return false;

  // Some statement kinds add a scope and thus never add a decl to the current
  // scope. Note, this list is longer than the list of statements that might
  // have an unscoped decl nested within them, but this way is conservatively
  // correct even if more statement kinds are added.
  if (s->isa<IfStmt>() || s->isa<SwitchStmt>() || s->isa<WhileStmt>() ||
      s->isa<DoStmt>() || s->isa<ForStmt>() || s->isa<CompoundStmt>())
    return false;

  if (s->isa<DeclStmt>())
    return true;

  if (auto ss = s->dyn_cast<SwitchCase>()) return might_add_decl_to_scope(ss->sub_stmt.get());

  return false;
}

enum CSFC_Result { CSFC_Failure, CSFC_FallThrough, CSFC_Success };
static CSFC_Result CollectStatementsForCase(const Stmt *s, const SwitchCase *Case, bool &found_case, std::vector<const Stmt*> &res) {
  // If this is a null statement, just succeed.
  if (!s || s->dyn_cast<NullStmt>())
    return Case ? CSFC_Success : CSFC_FallThrough;

  // If this is the switchcase (case 4: or default) that we're looking for, then
  // we're in business.  Just add the substatement.
  if (const SwitchCase *SC = s->dyn_cast<SwitchCase>()) {
    if (s == Case) {
      found_case = true;
      return CollectStatementsForCase(SC->sub_stmt.get(), nullptr, found_case, res);
    }

    // Otherwise, this is some other case or default statement, just ignore it.
    return CollectStatementsForCase(SC->sub_stmt.get(), Case, found_case, res);
  }

  // If we are in the live part of the code and we found our break statement,
  // return a success!
  if (!Case && s->isa<BreakStmt>())
    return CSFC_Success;

  // If this is a switch statement, then it might contain the SwitchCase, the
  // break, or neither.
  if (const CompoundStmt *CS = s->dyn_cast<CompoundStmt>()) {
    // Handle this as two cases: we might be looking for the SwitchCase (if so
    // the skipped statements must be skippable) or we might already have it.
    auto I = CS->inner.begin(), E = CS->inner.end();
    bool StartedInLiveCode = found_case;
    unsigned StartSize = res.size();

    // If we've not found the case yet, scan through looking for it.
    if (Case) {
      // Keep track of whether we see a skipped declaration.  The code could be
      // using the declaration even if it is skipped, so we can't optimize out
      // the decl if the kept statements might refer to it.
      bool HadSkippedDecl = false;

      // If we're looking for the case, just see if we can skip each of the
      // substatements.
      for (; Case && I != E; ++I) {
        HadSkippedDecl |= might_add_decl_to_scope(I->get());

        switch (CollectStatementsForCase(I->get(), Case, found_case, res)) {
        case CSFC_Failure: return CSFC_Failure;
        case CSFC_Success:
          // A successful result means that either 1) that the statement doesn't
          // have the case and is skippable, or 2) does contain the case value
          // and also contains the break to exit the switch.  In the later case,
          // we just verify the rest of the statements are elidable.
          if (found_case) {
            // If we found the case and skipped declarations, we can't do the
            // optimization.
            if (HadSkippedDecl)
              return CSFC_Failure;
            return CSFC_Success;
          }
          break;
        case CSFC_FallThrough:
          // If we have a fallthrough condition, then we must have found the
          // case started to include statements.  Consider the rest of the
          // statements in the compound statement as candidates for inclusion.
          assert(found_case && "Didn't find case but returned fallthrough?");
          // We recursively found Case, so we're not looking for it anymore.
          Case = nullptr;

          // If we found the case and skipped declarations, we can't do the
          // optimization.
          if (HadSkippedDecl)
            return CSFC_Failure;
          break;
        }
      }

      if (!found_case)
        return CSFC_Success;

      assert(!HadSkippedDecl && "fallthrough after skipping decl");
    }

    // If we have statements in our range, then we know that the statements are
    // live and need to be added to the set of statements we're tracking.
    bool AnyDecls = false;
    for (; I != E; ++I) {
      AnyDecls |= might_add_decl_to_scope(I->get());

      switch (CollectStatementsForCase(I->get(), nullptr, found_case, res)) {
      case CSFC_Failure: return CSFC_Failure;
      case CSFC_FallThrough:
        // A fallthrough result means that the statement was simple and just
        // included in ResultStmt, keep adding them afterwards.
        break;
      case CSFC_Success:
        // A successful result means that we found the break statement and
        // stopped statement inclusion.  We just ensure that any leftover stmts
        // are skippable and return success ourselves.
        return CSFC_Success;
      }
    }

    // If we're about to fall out of a scope without hitting a 'break;', we
    // can't perform the optimization if there were any decls in that scope
    // (we'd lose their end-of-lifetime).
    if (AnyDecls) {
      // If the entire compound statement was live, there's one more thing we
      // can try before giving up: emit the whole thing as a single statement.
      // We can do that unless the statement contains a 'break;'.
      // FIXME: Such a break must be at the end of a construct within this one.
      // We could emit this by just ignoring the BreakStmts entirely.
      if (StartedInLiveCode && !contains_break(s)) {
        res.resize(StartSize);
        res.push_back(s);
      } else {
        return CSFC_Failure;
      }
    }

    return CSFC_FallThrough;
  }

  // Okay, this is some other statement that we don't handle explicitly, like a
  // for statement or increment etc.
  if (Case) {
    return CSFC_Success;
  }

  // Otherwise, we want to include this statement.  Everything is cool with that
  // so long as it doesn't contain a break out of the switch we're in.
  if (contains_break(s)) return CSFC_Failure;

  // Otherwise, everything is great.  Include the statement and tell the caller
  // that we fall through and include the next statement as well.
  res.push_back(s);
  return CSFC_FallThrough;
}


static bool find_case_stmts_for_val(const SwitchStmt &s, const i64 val, std::vector<const Stmt*> &res) {
  const SwitchCase *case_stmt = s.first_case;
  const DefaultStmt *default_case = nullptr;

  for (; case_stmt; case_stmt = case_stmt->next_switch_case) {
    if (const DefaultStmt *ds = case_stmt->dyn_cast<DefaultStmt>()) {
      default_case = ds;
      continue;
    }
    const CaseStmt *cs = case_stmt->dyn_cast<CaseStmt>();
    if (cs->case_val == val) break;
  }

  if (!case_stmt) {
    if (!default_case)
      return true;
    case_stmt = default_case;
  }

  bool found = false;
  return CollectStatementsForCase(s.body.get(), case_stmt, found, res) != CSFC_Failure && found;
}


void gen_switch_stmt(CGContext &ctx, SwitchStmt const &s) {
  llvm::SwitchInst *saved_switch_ins = ctx.fn.switch_insn;

  auto [constant_cond_value, is_const] = eval_integer_constexpr(ctx.astctx, s.cond.get());
  if (is_const) {
    std::vector<const Stmt*> stmts;
    if (find_case_stmts_for_val(s, constant_cond_value, stmts)) {
      ctx.fn.switch_insn = nullptr;

      for (const Stmt *stmt : stmts)
        gen_stmt(ctx, *stmt);

      ctx.fn.switch_insn = saved_switch_ins;

      return;
    }
  }

  auto switch_exit = llvm::BasicBlock::Create(ctx.llvmctx, "sw.epilog");

  llvm::Value *cond_v = gen_scalar_expr(ctx, s.cond.get());

  llvm::BasicBlock *default_block = llvm::BasicBlock::Create(ctx.llvmctx, "sw.default");
  ctx.fn.switch_insn = ctx.builder.CreateSwitch(cond_v, default_block);

  ctx.builder.ClearInsertionPoint();

  llvm::BasicBlock *outer_continue = nullptr;
  if (!ctx.fn.break_continue_stack.empty())
    outer_continue = ctx.fn.break_continue_stack.back().continue_d;

  ctx.fn.break_continue_stack.push_back(CGFunctionCtx::BreakContinue(&s, switch_exit, outer_continue));

  gen_stmt(ctx, *s.body);

  ctx.fn.break_continue_stack.pop_back();

  if (!default_block->getParent()) {
    default_block->replaceAllUsesWith(switch_exit);
    delete default_block;
  }

  gen_block(ctx, switch_exit);

  ctx.fn.switch_insn = saved_switch_ins;
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
