from ns_ast.nodes import *
from semantic_analysis import Scope, ScopeFlags, eval_const_expr
from .expr import *
from . import state


def act_on_expr_stmt(fe: Expr | None, discarded_value: bool) -> Stmt:
    if fe is None:
        return None
    fe = act_on_finish_full_expr(fe, fe.get_range()[0], discarded_value)
    if fe is None:
        return None
    return fe


def act_on_expr_stmt_error() -> Stmt | None:
    # DiscardCleanupsInEvaluationContext();
    return None


def act_on_compound_stmt(l: Loc, r: Loc, elts: List[Stmt], is_stmt_expr: bool) -> Stmt:
    num_elts = len(elts)
    # if (num_elts != 0 && !CurrentInstantiationScope && getCurCompoundScope().HasEmptyLoopBodies):
    #   for i in range(num_elts):
    #     DiagnoseEmptyLoopBody(elts[i], elts[i + 1]);
    return CompoundStmt(elts, l, r)


def act_on_decl_stmt(d: Decl, start_loc: Loc, end_loc: Loc) -> Stmt:
    if d is None:
        return None
    return DeclStmt(start_loc, end_loc, d)


def act_on_if_stmt(if_loc: Loc, lparen_loc: Loc, cond: Expr, rparen_loc: Loc, then_stmt: Stmt, else_loc: Loc, else_stmt: Stmt | None):
    cond = check_boolean_condition(if_loc, cond);
    if cond is None:
        return None
    cond = act_on_finish_full_expr(cond, if_loc, False)
    if cond is None:
        return None
    # if (!elseStmt) DiagnoseEmptyStmtBody(RParenLoc, thenStmt, diag::warn_empty_if_body);
    return IfStmt(if_loc, cond, lparen_loc, rparen_loc, then_stmt, else_loc, else_stmt);


def act_on_start_of_switch_stmt(sl: Loc, lp: Loc, cond: Expr, rp: Loc) -> Stmt:
    assert cond is not None
    cond = act_on_finish_full_expr(cond, while_loc, False)
    if cond is None:
        return None
    if not cond.ty.is_integral_or_enumeration_type():
        return None
    # if cond.isKnownToHaveBooleanValue(): diag(SwitchLoc, diag::warn_bool_switch_condition) << CondExpr->getSourceRange();
    # set_function_has_branch_into_scope()
    ss = SwitchStmt(sl, cond, lp, rp, Stmt())
    # get_cur_function().switch_stack.append(switch_info(ss, False))
    return ss


def act_on_finish_switch_stmt(sl: Loc, switch: Stmt, body: Stmt) -> Stmt:
    assert isinstance(switch, SwitchStmt)
    # TODO: switch stmt semantic analysis
    switch.body = body
    return switch


def act_on_while_stmt(while_loc: Loc, lparen_loc: Loc, cond: Expr, rparen_loc: Loc, body: Stmt):
    assert cond is not None
    cond = check_boolean_condition(while_loc, cond);
    if cond is None:
        return None
    cond = act_on_finish_full_expr(cond, while_loc, False)
    if cond is None:
        return None
    if isinstance(body, NullStmt):
        # TODO:
        # getCurCompoundScope().setHasEmptyLoopBodies();
        pass
    return WhileStmt(while_loc, cond, lparen_loc, rparen_loc, body);


def act_on_do_stmt(do_loc: Loc, body: Stmt, while_loc: Loc, lp: Loc, cond: Expr, rp: Loc) -> Stmt:
    assert cond is not None
    cond = check_boolean_condition(do_loc, cond);
    if cond is None:
        return None
    cond = act_on_finish_full_expr(cond, do_loc, False)
    if cond is None:
        return None
    return DoStmt(body, cond, do_loc, while_loc, rp)


def act_on_default_stmt(default_loc, colon_loc, sub_stmt, scope):
    # if (getCurFunction()->SwitchStack.empty()) {Diag(DefaultLoc, diag::err_default_not_in_switch); return SubStmt;}
    ds = DefaultStmt(default_loc, colon_loc, sub_stmt)
    # getCurFunction()->SwitchStack.back().getPointer()->addSwitchCase(DS);
    return ds


def act_on_case_expr(case_loc: Loc, val: Expr):
    if val is None:
        return val
    # if (DiagnoseUnexpandedParameterPack(Val.get())) return ExprError();
    # if (getCurFunction()->SwitchStack.empty())
    #   return ActOnFinishFullExpr(Val.get(), Val.get()->getExprLoc(), false, getLangOpts().CPlusPlus11);
    #
    # Expr *CondExpr = getCurFunction()->SwitchStack.back().getPointer()->getCond();
    # if (!CondExpr) return ExprError();
    # QualType CondType = CondExpr->getType();
    #
    # auto CheckAndFinish = [&](Expr *E) {
    #   if (CondType->isDependentType() || E->isTypeDependent())
    #     return ExprResult(E);
    #
    #   if (getLangOpts().CPlusPlus11) {
    #     llvm::APSInt TempVal;
    #     return CheckConvertedConstantExpression(E, CondType, TempVal, CCEK_CaseValue);
    #   }
    #
    #   ExprResult ER = E;
    #   if (!E->isValueDependent())
    #     ER = VerifyIntegerConstantExpression(E, AllowFold);
    #   if (!ER.isInvalid())
    #     ER = DefaultLvalueConversion(ER.get());
    #   if (!ER.isInvalid())
    #     ER = ImpCastExprToType(ER.get(), CondType, CK_IntegralCast);
    #   if (!ER.isInvalid())
    #     ER = ActOnFinishFullExpr(ER.get(), ER.get()->getExprLoc(), false);
    #   return ER;
    # };
    #
    # ExprResult Converted = CorrectDelayedTyposInExpr(
    #     Val, /*InitDecl=*/nullptr, /*RecoverUncorrectedTypos=*/false,
    #     CheckAndFinish);
    # if (Converted.get() == Val.get())
    #   Converted = CheckAndFinish(Val.get());
    # return Converted;
    return val


def act_on_case_stmt(case_loc: Loc, lhs: Expr, colon_loc: Loc):
    assert lhs is not None, "missing LHS value"

    # if (getCurFunction()->SwitchStack.empty()) {Diag(CaseLoc, diag::err_case_not_in_switch); return StmtError();}
    # if (LHSVal.isInvalid()) {getCurFunction()->SwitchStack.back().setInt(true); return StmtError();}
    cs = CaseStmt(case_loc, colon_loc, Stmt(), eval_const_expr(lhs))
    # getCurFunction()->SwitchStack.back().getPointer()->addSwitchCase(CS);
    return cs


def act_on_case_stmt_body(s: Stmt, sub_stmt: Stmt):
    assert isinstance(s, CaseStmt)
    s.sub_stmt = sub_stmt


def act_on_continue_stmt(cl: Loc, cur_scope: Scope):
    s = cur_scope.continue_parent
    if s is None:
        diag(cl, "'continue' statement not in loop statement", Diag.ERROR)
        return None
    if s.is_condition_var_scope():
        diag(cl, "cannot jump from this continue statement to the loop increment; ", Diag.ERROR)
        return None
    return ContinueStmt(cl)


def act_on_break_stmt(bl: Loc, cur_scope: Scope):
    s = cur_scope.break_parent
    if s is None:
        diag(bl, "'break' statement not in loop or switch statement", Diag.ERROR)
        return None
    return BreakStmt(bl)


def act_on_null_stmt(semi_loc):
    return NullStmt(semi_loc)


def act_on_return_stmt(return_loc, ret_val_expr, scope, allow_recovery = False):
    if ret_val_expr is not None:
        ret_val_expr = correct_delayed_typos_in_expr(ret_val_expr, None, True)
        if ret_val_expr is None:
            return None

    fd = state.CUR_FN_DECL
    if fd is None:
        return None

    fn_ret_type = fd.ty.return_type
    # if (fd->isNoReturn()) Diag(ReturnLoc, diag::warn_noreturn_function_has_return_expr) << fd;
    # if (fd->isMain() && RetValExp && isa<CXXBoolLiteralExpr>(RetValExp)) Diag(ReturnLoc, diag::warn_main_returns_bool_literal) << RetValExp->getSourceRange();

    if type_is_void(fn_ret_type):
        if ret_val_expr is not None:
            if not type_is_void(ret_val_expr.ty):
                ret_val_expr = ignored_value_conversions(ret_val_expr)
                if ret_val_expr is None:
                    return None
                ret_val_expr = imp_cast_expr_to_type(ret_val_expr, Type(), CastKind.TO_VOID)
                diag(return_loc, f"void function {fd.name} should not return a value", Diag.ERROR, [ret_val_expr.get_range()])
            ret_val_expr = act_on_finish_full_expr(ret_val_expr, return_loc, False)
            if ret_val_expr is None:
                return None
    elif ret_val_expr is None:
        diag(return_loc, "diag::warn_return_missing_expr) << cast<NamedDecl>(fd) << false;", Diag.WARNING)
    else:
        # InitializedEntity Entity = InitializedEntity::InitializeResult(ReturnLoc, FnRetType);
        # ret_val_exp = PerformMoveOrCopyInitialization(Entity, NRInfo, RetValExp, SupressSimplerImplicitMoves);
        # if (Res.isInvalid() && AllowRecovery) Res = CreateRecoveryExpr(RetValExp->getBeginLoc(), RetValExp->getEndLoc(), RetValExp, FnRetType);
        if ret_val_expr is None:
            return None
        # CheckReturnValExpr(RetValExp, FnRetType, ReturnLoc, false, Attrs, getCurFunctionDecl());
        ret_val_expr = act_on_finish_full_expr(ret_val_expr, return_loc, False)
        if ret_val_expr is None:
            return None

    return ReturnStmt(return_loc, ret_val_expr)



def diagnose_unused_expr_result(s: Stmt, diag_id: str):
    pass
