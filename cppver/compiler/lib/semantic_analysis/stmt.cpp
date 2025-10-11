#include "ast/nodes/stmt.hpp"
#include "ast/nodes/type.hpp"
#include "convert.hpp"
#include "diags/diagnostic.hpp"
#include "sema.hpp"
#include <cassert>
#include <memory>
#include <ranges>

ExprUPtr Sema::act_on_expr_stmt(ExprUPtr fe, bool discarded_value) {
  if (!fe) return nullptr;
  auto loc = fe->get_start_loc();
  return act_on_finish_full_expr(std::move(fe), loc, discarded_value);
}

StmtUPtr Sema::act_on_expr_stmt_error() { return nullptr; }

UPtr<CompoundStmt>
Sema::act_on_compound_stmt(Loc lp_loc, Loc rp_loc,
                           std::vector<StmtUPtr> &&elts) {
  return std::make_unique<CompoundStmt>(std::move(elts), lp_loc, rp_loc);
}

UPtr<DeclStmt> Sema::act_on_decl_stmt(DeclUPtr d, Loc start_loc, Loc end_loc) {
  if (!d) return nullptr;
  return std::make_unique<DeclStmt>(std::move(d), start_loc, end_loc);
}

UPtr<IfStmt> Sema::act_on_if_stmt(Loc il, Loc lp, ExprUPtr cond, Loc rp,
                                  StmtUPtr ts, Loc el, StmtUPtr es) {
  auto cond_expr = check_boolean_condition(std::move(cond));
  if (!cond_expr) return nullptr;
  cond_expr = act_on_finish_full_expr(std::move(cond_expr), il, false);
  if (!cond_expr) return nullptr;
  return std::make_unique<IfStmt>(std::move(cond_expr), std::move(ts), std::move(es), il, lp, rp, el);
}

UPtr<SwitchStmt> Sema::act_on_start_of_switch_stmt(Loc switch_loc, Loc lp_loc,
                                                   ExprUPtr cond, Loc rp_loc) {
  auto cond_expr = act_on_finish_full_expr(std::move(cond), switch_loc, false);
  if (!cond_expr) return nullptr;
  if (!cond_expr->type->is_integral_or_enumeration_type()) return nullptr;
  return std::make_unique<SwitchStmt>(std::move(cond_expr), nullptr, switch_loc, lp_loc, rp_loc);
}

UPtr<SwitchStmt> Sema::act_on_finish_switch_stmt(Loc sl, UPtr<SwitchStmt> stmt,
                                                 StmtUPtr body) {
  (void)sl;
  stmt->body = std::move(body);
  return stmt;
}

UPtr<WhileStmt> Sema::act_on_while_stmt(Loc while_loc, Loc lparen_loc,
                                        ExprUPtr cond, Loc rparen_loc,
                                        StmtUPtr body) {
  auto cond_expr = check_boolean_condition(std::move(cond));
  if (!cond_expr) return nullptr;
  cond_expr = act_on_finish_full_expr(std::move(cond_expr), while_loc, false);
  if (!cond_expr) return nullptr;
  return std::make_unique<WhileStmt>(std::move(cond_expr), std::move(body), while_loc, lparen_loc, rparen_loc);
}

UPtr<DoStmt> Sema::act_on_do_stmt(Loc do_loc, StmtUPtr body, Loc while_loc,
                                  Loc lp_loc, ExprUPtr cond, Loc rp_loc) {
  (void)lp_loc;
  auto cond_expr = check_boolean_condition(std::move(cond));
  if (!cond_expr) return nullptr;
  cond_expr = act_on_finish_full_expr(std::move(cond_expr), while_loc, false);
  if (!cond_expr) return nullptr;
  return std::make_unique<DoStmt>(std::move(cond_expr), std::move(body), do_loc, while_loc, rp_loc);
}

UPtr<DefaultStmt> Sema::act_on_default_stmt(Loc default_loc, Loc colon_loc,
                                            StmtUPtr sub_stmt, Scope *scope) {
  // TODO: check in switch
  (void)scope;
  return std::make_unique<DefaultStmt>(std::move(sub_stmt), default_loc, colon_loc);
}

ExprUPtr Sema::act_on_case_expr(Loc case_loc, ExprUPtr val) {
  // TODO:
  (void) case_loc;
  return val;
}

UPtr<CaseStmt> Sema::act_on_case_stmt(Loc case_loc, ExprUPtr e, Loc colon_loc) {
  // TODO: check in switch
  assert(e);
  return std::make_unique<CaseStmt>(nullptr, case_loc, colon_loc, eval_integer_constexpr(e.get()));
}

void Sema::act_on_case_stmt_body(CaseStmt *stmt, StmtUPtr sub_stmt) {
  stmt->sub_stmt = std::move(sub_stmt);
}

UPtr<ContinueStmt> Sema::act_on_continue_stmt(Loc cl, Scope *cur_scope) {
  auto s = cur_scope->continue_parent;
  if (!s) {
    Diag(diag::ERROR, cl, "'continue' statement not in loop");
    return nullptr;
  }
  if (s->is_condition_var_scope()) {
    Diag(diag::ERROR, cl, "cannot jump from this continue statement to the loop increment");
  }
  return std::make_unique<ContinueStmt>(cl);
}

UPtr<BreakStmt> Sema::act_on_break_stmt(Loc bl, Scope *cur_scope) {
  auto s = cur_scope->break_parent;
  if (!s) {
    Diag(diag::ERROR, bl, "'break' statement not in loop or switch");
    return nullptr;
  }
  return std::make_unique<BreakStmt>(bl);
}

UPtr<NullStmt> Sema::act_on_null_stmt(Loc semi_loc) {
  return std::make_unique<NullStmt>(semi_loc);
}

UPtr<ReturnStmt> Sema::act_on_return_stmt(Loc return_loc, ExprUPtr ret_val_expr,
                                          Scope *scope, bool allow_recovery) {
  (void)scope, (void)allow_recovery;
  if (!cur_fn_decl) return nullptr;
  auto ret_type = cur_fn_decl->type->dyn_cast<FunctionType>()->result_type;

  if (ret_type->is_void_type()) {
    if (ret_val_expr) {
      if (!ret_val_expr->type->is_void_type()) {
        ret_val_expr = ignored_value_conversions(std::move(ret_val_expr));
        if (!ret_val_expr) return nullptr;
        ret_val_expr = imp_cast_expr_to_type(std::move(ret_val_expr), ctx.void_ty, CastExpr::TO_VOID);
        Diag(diag::ERROR, return_loc, "void function '%s' should not return a value", cur_fn_decl->get_name().c_str())
          << ret_val_expr->get_range();
      }
      ret_val_expr = act_on_finish_full_expr(std::move(ret_val_expr), return_loc, false);
      if (!ret_val_expr) return nullptr;
    }
  } else if (ret_val_expr == nullptr) {
    Diag(diag::WARNING, return_loc, "return missing expr");
  } else {
    ConversionSequence ics = try_implicit_conversion(ret_val_expr.get(), ret_type);
    ret_val_expr = perform_implicit_conversion(std::move(ret_val_expr), ret_type, &ics);
    ret_val_expr = act_on_finish_full_expr(std::move(ret_val_expr), return_loc, false);
    if (!ret_val_expr) return nullptr;
  }
  return std::make_unique<ReturnStmt>(std::move(ret_val_expr), return_loc);
}

void Sema::diagnose_unused_expr_result(Stmt *s, std::string const &diag_id) {
  (void)s, (void)diag_id;
}

