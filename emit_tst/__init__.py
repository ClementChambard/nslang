from dataclasses import dataclass
import ns_ast.nodes as AST
from semantic_analysis import TYPES
from .builder import builder
from .value import *
import enum

return_block = None
return_value = None

def emit_fn_start(f):
    start_function(f)
    # global return_block
    # func_start = BasicBlock("entry")
    # return_block = BasicBlock("return")
    # return_value =
    # emit_block(func_start)

def create_temp_alloca(ty: AST.Type, name: str, array_size: int = 0, align = -1):
    alloca = None
    if array_size > 0:
        alloca = builder().create_alloca(ty, array_size, name)
    else:
        alloca = AllocaInstr(ty, array_size, name) # , CGM.getDataLayout().getAllocaAddrSpace(), AllocaInsertPt->getIterator());
    if align != -1:
        alloca.align = align
    builder().insert(alloca)
    #if (Allocas) {Allocas->Add(Alloca);}
    return alloca

def create_ir_temp(ty: AST.Type, name: str = "tmp"):
    # align = ty.get_align()
    return create_temp_alloca(ty, name)

@dataclass
class ParamValue:
    aov: Address | Value
    is_indirect: bool

    @staticmethod
    def for_direct(v: Value):
        return ParamValue(v, False)


def emit_to_memory(v, ty):
    if ty == TYPES["bool"]: # or ty.is_bit_int_type()
        store_ty = TYPES["i8"]
        return builder().create_int_cast(v, store_ty, True, "storedv")
    return v

def emit_store_of_scalar(v, addr, volatile, ty, bi, tbaa, is_init, is_nt):
    v = emit_to_memory(v, ty)
    store = builder().create_store(v, addr, volatile)

def emit_param_decl(d, arg, arg_no):
  decl_ptr = create_temp_alloca(d.ty, d.name + ".addr", 0, d.ty.get_align())
  lv = make_addr_lvalue(decl_ptr, d.ty)
  emit_store_of_scalar(arg.aov, lv.addr, False, lv.ty, None, None, True, False)
  set_addr_of_local_var(d, decl_ptr)

LOCAL_DECL_MAP = {}

def set_addr_of_local_var(vd, addr):
    global LOCAL_DECL_MAP
    LOCAL_DECL_MAP[id(vd)] = addr

def emit_function_prolog(f):
  # if (const FunctionDecl *FD = dyn_cast_or_null<FunctionDecl>(CurCodeDecl)) { if (FD->hasImplicitReturnZero()) { QualType RetTy = FD->getReturnType().getUnqualifiedType(); llvm::Type* LLVMTy = CGM.getTypes().ConvertType(RetTy); llvm::Constant* Zero = llvm::Constant::getNullValue(LLVMTy); Builder().CreateStore(Zero, ReturnValue); } }
  arg_vals = []
  for v in cur_fn.params:
      arg_vals.append(ParamValue.for_direct(v))
  for i in range(len(arg_vals)):
      emit_param_decl(f.param_decls[i], arg_vals[i], i)

def start_function(d):
    ret_ty = d.ty.return_type
    # const Decl *D = GD.getDecl();
    # DidCallStackSave = false;
    # CurCodeDecl = D;
    # const FunctionDecl *FD = dyn_cast_or_null<FunctionDecl>(D);
    # CurFuncDecl = (D ? D->getNonClosureContext() : nullptr);
    # FnRetTy = RetTy;
    # CurFn = Fn;
    # CurFnInfo = &FnInfo;
    # if (FD && FD->isMain()) Fn->addFnAttr(llvm::Attribute::NoRecurse);
    # llvm::RoundingMode RM = getLangOpts().getDefaultRoundingMode(); llvm::fp::ExceptionBehavior FPExceptionBehavior = ToConstrainedExceptMD(getLangOpts().getDefaultExceptionMode()); Builder().setDefaultConstrainedRounding(RM); Builder().setDefaultConstrainedExcept(FPExceptionBehavior);
    entry_bb = BasicBlock("entry", cur_fn)
    # llvm::Value *Undef = llvm::UndefValue::get(Int32Ty); AllocaInsertPt = new llvm::BitCastInst(Undef, Int32Ty, "allocapt", EntryBB);
    global return_block
    return_block = get_jump_dest_in_current_scope("return")
    emit_block(entry_bb)
    global return_value
    if AST.type_is_void(ret_ty):
        return_value = None
        # if (!endsWithReturn(D)) ++NumReturnExprs;
        # } else if (CurFnInfo->getReturnInfo().getKind() == ABIArgInfo::Indirect) { # TODO
        # } else if (CurFnInfo->getReturnInfo().getKind() == ABIArgInfo::InAlloca && !hasScalarEvaluationKind(CurFnInfo->getReturnType())) { # TODO
    else:
        return_value = create_ir_temp(ret_ty, "retval")
    emit_function_prolog(d)
    # for (const VarDecl *VD : Args) { QualType Ty; if (const ParmVarDecl *PVD = dyn_cast<ParmVarDecl>(VD)) Ty = PVD->getOriginalType(); else Ty = VD->getType(); if (Ty->isVariablyModifiedType()) EmitVariablyModifiedType(Ty); }



def emit_fn_end(f):
    global return_block
    global return_value
    emit_block(return_block.block)
    if return_value is not None:
        r = builder().create_load(return_value, "", False)
        builder().insert(RetInstr(r))
    else:
        builder().insert(RetInstr(None))
    return_value = None
    return_block = None

def tst_emit(ast, output_filename="a.ll"):
    with open(output_filename, 'w') as f:
        for d in ast:
            if isinstance(d, AST.FnDecl) and d.body is not None:
                global cur_fn
                params = []
                for p in d.param_decls:
                    params.append(Value(p.name, p.ty))
                cur_fn = Function(d.name, d.ty, params)
                emit_fn_start(d)
                emit_compound_stmt_without_scope(d.body)
                emit_fn_end(d)
                f.write(f"{cur_fn}\n")
            elif isinstance(d, AST.FnDecl):
                f.write(f"declare {ty_str(d.ty.return_type)} @{d.name}{args_str(d.ty)}\n")
        for _, g in CONSTANT_STRING_MAP.items():
            f.write(f"{g}\n")




cur_fn = None





def have_insert_point() -> bool:
    return builder().insert_block is not None

def get_debug_info():
    return None

def emit_stop_point(s: AST.Stmt):
    if (di := get_debug_info()) is not None:
        loc = s.get_range()[0]
        di.emit_location(builder(), loc)
        last_stop_point = loc

def emit_stmt(s: AST.Stmt):
    assert s is not None, "Null stmt ?"

    # PGO.setCurrentStmt(S);

    if emit_simple_stmt(s):
        return

    if not have_insert_point():
        assert not isinstance(s, AST.DeclStmt), "Unexpected DeclStmt!"
        return

    emit_stop_point(s)

    if isinstance(s, AST.Expr):
        incoming = builder().insert_block
        assert incoming is not None, "expression emission must have an insertion point"
        emit_ignored_expr(s)
        outgoing = builder().insert_block
        assert outgoing is not None, "expression emission must have an insertion point"
        if incoming != outgoing and outgoing.use_empty():
            outgoing.erase_from_parent()
            builder().clear_insertion_point()
    elif isinstance(s, AST.IfStmt):
        emit_if_stmt(s)
    elif isinstance(s, AST.WhileStmt):
        emit_while_stmt(s)
    elif isinstance(s, AST.DoStmt):
        emit_do_stmt(s)
    elif isinstance(s, AST.ForStmt):
        emit_for_stmt(s)
    elif isinstance(s, AST.ReturnStmt):
        emit_return_stmt(s)
    elif isinstance(s, AST.SwitchStmt):
        emit_switch_stmt(s)
    else:
        assert False, "unreachable"

def emit_simple_stmt(s: AST.Stmt) -> bool:
    if isinstance(s, AST.NullStmt):
        pass
    elif isinstance(s, AST.CompoundStmt):
        emit_compound_stmt(s)
    elif isinstance(s, AST.DeclStmt):
        emit_decl_stmt(s)
    elif isinstance(s, AST.BreakStmt):
        emit_break_stmt(s)
    elif isinstance(s, AST.ContinueStmt):
        emit_continue_stmt(s)
    elif isinstance(s, AST.CaseStmt):
        emit_case_stmt(s)
    elif isinstance(s, AST.DefaultStmt):
        emit_default_stmt(s)
    else:
        return False
    return True

def emit_return_stmt(s: AST.ReturnStmt):
    # if (requiresReturnValueCheck()) {
    #     llvm::Constant *SLoc = EmitCheckSourceLocation(S.getBeginLoc());
    #     auto *SLocPtr = new llvm::GlobalVariable(CGM.getModule(), SLoc->getType(), false, llvm::GlobalVariable::PrivateLinkage, SLoc);
    #     SLocPtr->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    #     CGM.getSanitizerMetadata()->disableSanitizerForGlobal(SLocPtr);
    #     assert(ReturnLocation.isValid() && "No valid return location");
    #     Builder().CreateStore(SLocPtr, ReturnLocation);
    # }
    rv = s.ret_expr

    # SaveRetExprRAII SaveRetExpr(RV, *this);

    # RunCleanupsScope cleanupScope(*this);
    # if (const auto *EWC = dyn_cast_or_null<ExprWithCleanups>(RV)) rv = EWC->getSubExpr();

    if (rv is not None and AST.type_is_void(rv.ty)): # or not return_value.is_valid()
        if rv is not None:
            emit_any_expr(rv)
    elif rv is None:
        pass
    else:
      # if Aggregate: EmitAggExpr(RV, AggValueSlot::forAddr(ReturnValue, Qualifiers(), AggValueSlot::IsDestructed, AggValueSlot::DoesNotNeedGCBarriers, AggValueSlot::IsNotAliased, getOverlapForReturnValue()));
      ret = emit_scalar_expr(rv);
      # if (CurFnInfo->getReturnInfo().getKind() == ABIArgInfo::Indirect) EmitStoreOfScalar(Ret, MakeAddrLValue(ReturnValue, RV->getType()), /*isInit*/ true); else

      builder().create_store(ret, return_value)

    # ++NumReturnExprs;
    # if (!RV || RV->isEvaluatable(getContext())) ++NumSimpleReturnExprs;

    # cleanupScope.ForceCleanup();
    emit_branch_through_cleanup(return_block)

def emit_break_stmt(s: AST.BreakStmt):
    assert len(BREAK_CONTINUE_STACK) > 0
    if have_insert_point():
        emit_stop_point(s)
    emit_branch_through_cleanup(BREAK_CONTINUE_STACK[-1].break_block)

def emit_continue_stmt(s: AST.BreakStmt):
    assert len(BREAK_CONTINUE_STACK) > 0
    if have_insert_point():
        emit_stop_point(s)
    emit_branch_through_cleanup(BREAK_CONTINUE_STACK[-1].continue_block)

def emit_branch_through_cleanup(dest: "JumpDest"):
    #assert(Dest.getScopeDepth().encloses(EHStack.stable_begin()) && "stale jump destination");
    if not have_insert_point(): return
    bi = builder().create_br(dest.block)
    # EHScopeStack::stable_iterator TopCleanup = EHStack.getInnermostActiveNormalCleanup();
    # if (TopCleanup == EHStack.stable_end() || TopCleanup.encloses(Dest.getScopeDepth())) { Builder().ClearInsertionPoint(); return; }
    # if (!Dest.getScopeDepth().isValid()) { BranchFixup &Fixup = EHStack.addBranchFixup(); Fixup.Destination = Dest.getBlock(); Fixup.DestinationIndex = Dest.getDestIndex(); Fixup.InitialBranch = BI; Fixup.OptimisticBranchBlock = nullptr; Builder().ClearInsertionPoint(); return; }
    # llvm::ConstantInt *Index = Builder().getInt32(Dest.getDestIndex());
    # createStoreInstBefore(Index, getNormalCleanupDestSlot(), BI->getIterator(), *this);
    # { EHCleanupScope &Scope = cast<EHCleanupScope>(*EHStack.find(TopCleanup)); BI->setSuccessor(0, CreateNormalEntry(*this, Scope)); }
    # EHScopeStack::stable_iterator I = TopCleanup;
    # EHScopeStack::stable_iterator E = Dest.getScopeDepth();
    # if (E.strictlyEncloses(I)) { while (true) { EHCleanupScope &Scope = cast<EHCleanupScope>(*EHStack.find(I)); assert(Scope.isNormalCleanup()); I = Scope.getEnclosingNormalCleanup(); if (!E.strictlyEncloses(I)) { Scope.addBranchAfter(Index, Dest.getBlock()); break; } if (!Scope.addBranchThrough(Dest.getBlock())) break; } }
    builder().insert_block = None

def emit_compound_stmt(s: AST.CompoundStmt): # returns Address::invalid()
    # LexicalScope Scope(*this, S.getSourceRange())
    emit_compound_stmt_without_scope(s)

def emit_compound_stmt_without_scope(s: AST.CompoundStmt): # returns Address::invalid()
    for sub_stmt in s.inner:
        emit_stmt(sub_stmt)


def simplify_forwarding_blocks(bb: BasicBlock):
    bb_ter = bb.get_terminator()
    bi = bb_ter if isinstance(bb_ter, BranchInstr) else None

    # if len(eh_stack) > 0: return

    if bi is None or not bi.is_unconditional():
        return

    if True: # bi.get_iterator() != bb.begin():
        return

    bb.replace_all_uses_with(bi.get_successor(0))
    bi.erase_from_parent()
    bb.erase_from_parent()


def emit_block(bb: BasicBlock, is_finished: bool = False):
    cur_bb = builder().insert_block

    emit_branch(bb)

    if is_finished and False:#  and bb.use_empty():
        return

    if cur_bb is not None and cur_bb.parent is not None:
        cur_fn.insert_at(std.next(cur_bb.get_iterator()), bb)
    else:
        cur_fn.insert(bb)

    builder().insert_block = bb

def emit_block_with_fall_through(bb: BasicBlock, s: AST.Stmt):
    emit_block(bb)
    # uint64_t CurrentCount = getCurrentProfileCount();
    # incrementProfileCounter(S);
    # setCurrentProfileCount(getCurrentProfileCount() + CurrentCount);

def emit_branch(target: BasicBlock):
    cur_bb = builder().insert_block
    if cur_bb is None or cur_bb.get_terminator() is not None:
        pass
    else:
        builder().create_br(target)
    builder().insert_block = None

def emit_block_after_uses(block: BasicBlock):
    inserted = False

    for u in block.users():
        if isinstance(u, Instruction):
            cur_fn.insert(std.next(u.parent.get_iterator()), block)
            inserted = True
            break

    if not inserted:
        cur_fn.insert(cur_fn.end(), block)

    builder().insert_block = block


def evaluate(e: AST.Expr):
    if isinstance(e, AST.IntegerLiteral):
        return True, e.value
    elif isinstance(e, AST.BoolLiteral):
        return True, e.value
    elif isinstance(e, AST.BinaryExpr):
        a, lhs = evaluate(e.lhs)
        b, rhs = evaluate(e.rhs)
        if not a or not b:
            return False, 0
        match e.opc:
            case AST.BinaryOperatorKind.MUL: return True, (lhs * rhs)
            case AST.BinaryOperatorKind.DIV: return True, (lhs // rhs)
            case AST.BinaryOperatorKind.REM: return True, (lhs % rhs)
            case AST.BinaryOperatorKind.ADD: return True, (lhs + rhs)
            case AST.BinaryOperatorKind.SUB: return True, (lhs - rhs)
            case AST.BinaryOperatorKind.SHL: return True, (lhs << rhs)
            case AST.BinaryOperatorKind.SHR: return True, (lhs >> rhs)
            case AST.BinaryOperatorKind.LT: return True, (lhs < rhs)
            case AST.BinaryOperatorKind.GT: return True, (lhs > rhs)
            case AST.BinaryOperatorKind.LE: return True, (lhs <= rhs)
            case AST.BinaryOperatorKind.GE: return True, (lhs >= rhs)
            case AST.BinaryOperatorKind.EQ: return True, (lhs == rhs)
            case AST.BinaryOperatorKind.NE: return True, (lhs != rhs)
            case AST.BinaryOperatorKind.AND: return True, (lhs & rhs)
            case AST.BinaryOperatorKind.XOR: return True, (lhs ^ rhs)
            case AST.BinaryOperatorKind.OR: return True, (lhs | rhs)
            case AST.BinaryOperatorKind.LAND: return True, (lhs and rhs)
            case AST.BinaryOperatorKind.LOR: return True, (lhs or rhs)
            case _: return False, 0
    elif isinstance(e, AST.UnaryExpr):
        a, arg = evaluate(e.arg)
        if not a:
            return False, 0
        match e.opc:
            case AST.UnaryOperatorKind.PLUS: return True, arg
            case AST.UnaryOperatorKind.MINUS: return True, -arg
            case AST.UnaryOperatorKind.NOT: return True, ~arg
            case AST.UnaryOperatorKind.LNOT: return True, not arg
            case _: return False, 0
    elif isinstance(e, AST.CastExpr):
        return evaluate(e.op) # TODO: convert if necessary
    elif isinstance(e, AST.ParenExpr):
        return evaluate(e.val)
    else:
        return False, 0

def constant_folds_to_simple_integer(e: AST.Expr): # Tuple[bool, constant]
    a, b = evaluate(e)
    if a == False:
        return a, b
    try:
        b = int(b)
    except:
        a = False
    return a, b

def emit_branch_on_bool_expr(cond, true_block, false_block): #, a, b):
    # Cond = Cond->IgnoreParens();
    # if (const BinaryOperator *CondBOp = dyn_cast<BinaryOperator>(Cond)) {
    #   if (CondBOp->getOpcode() == BO_LAnd) {} # TODO Handle X && Y in a condition.
    #   if (CondBOp->getOpcode() == BO_LOr) {} # TODO Handle X || Y in a condition.
    # }
    # if (const UnaryOperator *CondUOp = dyn_cast<UnaryOperator>(Cond)) {
    #   if (CondUOp->getOpcode() == UO_LNot) {} # TODO Handle !X in a condition.
    # }
    # if (const ConditionalOperator *CondOp = dyn_cast<ConditionalOperator>(Cond)) {} # TODO: br(c ? x : y, t, f) -> br(c, br(x, t, f), br(y, t, f))

    # {
    #   ApplyDebugLocation DL(*this, Cond);
    cond_v = evaluate_expr_as_bool(cond)
    # }
    builder().create_cond_br(cond_v, true_block, false_block)

def emit_scalar_conversion(src, src_ty, dst_ty, loc):
    return ScalarExprEmitter().emit_scalar_conversion(src, src_ty, dst_ty, loc)

def evaluate_expr_as_bool(e):
    # PGO.setCurrentStmt(E);
    return emit_scalar_conversion(emit_scalar_expr(e), e.ty, TYPES["bool"], e.get_range()[0]);

def emit_if_stmt(s: AST.IfStmt):
    # LexicalScope ConditionScope(*this, S.getCond()->getSourceRange());

    if (x := constant_folds_to_simple_integer(s.cond))[0]:
        executed = s.then_stmt
        skipped = s.else_stmt
        if not x[1]:
            executed, skipped = skipped, executed
        # if x[1]: increment_profile_counter(s)
        if executed is not None:
            # RunCleanupsScope executed_scope(*this)
            emit_stmt(executed)
        return

    then_block = BasicBlock("if.then")
    cont_block = BasicBlock("if.end")
    else_block = cont_block
    if s.else_stmt is not None:
        else_block = BasicBlock("if.else")

    emit_branch_on_bool_expr(s.cond, then_block, else_block) #, get_profile_count(s.then_stmt), Stmt.LH_None)
    emit_block(then_block)
    # increment_profile_counter(s);
    #{
    # RuncCleanupScope then_scope(*this)
    emit_stmt(s.then_stmt)
    #}
    emit_branch(cont_block)

    if (else_stmt := s.else_stmt) is not None:
        emit_block(else_block)
        #{
        # RuncCleanupScope else_scope(*this)
        emit_stmt(else_stmt)
        #}
        emit_branch(cont_block)

    emit_block(cont_block, True)

@dataclass
class JumpDest:
    block: BasicBlock
    scope_depth: None
    index: int
    def is_valid(self) -> bool:
        return block is not None

@dataclass
class BreakContinue:
    break_block: JumpDest
    continue_block: JumpDest

@dataclass
class JumpDest:
    block: BasicBlock
    scope_depth: None
    index: int

    def __init__(self, block = None, depth = None, index = 0):
        self.block = block
        self.scope_depth = depth
        self.index = index

    def is_valid(self) -> bool:
        return self.block is not None

BREAK_CONTINUE_STACK = []

def get_jump_dest_in_current_scope(target):
    if isinstance(target, str):
        return get_jump_dest_in_current_scope(BasicBlock(target))
    return JumpDest(target, None, 0) # EHStack.getInnermostNormalCleanup(), NextCleanupDestIndex++

def emit_while_stmt(s: AST.WhileStmt):
    loop_header = get_jump_dest_in_current_scope("while.cond")
    emit_block(loop_header.block)

    loop_exit = get_jump_dest_in_current_scope("while.end")

    BREAK_CONTINUE_STACK.append(BreakContinue(loop_exit, loop_header))

    # RunCleanupsScope ConditionScope(*this);

    bool_cond_val = evaluate_expr_as_bool(s.cond)
    emit_bool_cond_branch = not isinstance(bool_cond_val, ConstantInt) or not bool_cond_val.val == 1
    r = s.get_range()
    # loop_stack.append(LoopHeader.getBlock(), CGM.getContext(), CGM.getCodeGenOpts(), WhileAttrs, SourceLocToDebugLoc(R.getBegin()), SourceLocToDebugLoc(R.getEnd()), checkIfLoopMustProgress(S.getCond(), hasEmptyLoopBody(S)))

    loop_body = BasicBlock("while.body")

    if emit_bool_cond_branch:
        exit_block = loop_exit.block
        # if (ConditionScope.requiresCleanups()) ExitBlock = createBasicBlock("while.exit");
        # llvm::MDNode *Weights = createProfileWeightsForLoop(S.getCond(), getProfileCount(S.getBody()));
        # if (!Weights && CGM.getCodeGenOpts().OptimizationLevel) BoolCondVal = emitCondLikelihoodViaExpectIntrinsic(BoolCondVal, Stmt::getLikelihood(S.getBody()));
        builder().create_cond_br(bool_cond_val, loop_body, exit_block) #, Weights

        if exit_block != loop_exit.block:
            emit_block(exit_block)
            emit_branch_through_cleanup(loop_exit)
    #{
    #  RunCleanupsScope BodyScope(*this);
    emit_block(loop_body);
    #  incrementProfileCounter(&S);
    emit_stmt(s.while_stmt);
    #}

    BREAK_CONTINUE_STACK.pop()
    # ConditionScope.ForceCleanup();

    emit_stop_point(s)
    emit_branch(loop_header.block)
    # loop_stack.pop();
    emit_block(loop_exit.block, True)

    if not emit_bool_cond_branch:
        simplify_forwarding_blocks(loop_header.block)

def emit_do_stmt(s: AST.DoStmt):
    loop_exit = get_jump_dest_in_current_scope("do.end")
    loop_cond = get_jump_dest_in_current_scope("do.cond")

    #uint64_t ParentCount = getCurrentProfileCount();

    BREAK_CONTINUE_STACK.append(BreakContinue(loop_exit, loop_cond))

    loop_body = BasicBlock("do.body")

    emit_block_with_fall_through(loop_body, s)

    #{
    #    RunCleanupsScope BodyScope(*this);
    emit_stmt(s.body)
    #}

    emit_block(loop_cond.block)

    bool_cond_val = evaluate_expr_as_bool(s.expr)

    BREAK_CONTINUE_STACK.pop()

    emit_bool_cond_branch = not isinstance(bool_cond_val, ConstantInt) or not bool_cond_val.val == 0

    r = s.get_range()
    # LoopStack.push(LoopBody, CGM.getContext(), CGM.getCodeGenOpts(), DoAttrs, SourceLocToDebugLoc(R.getBegin()), SourceLocToDebugLoc(R.getEnd()), checkIfLoopMustProgress(S.getCond(), hasEmptyLoopBody(S)));

    if emit_bool_cond_branch:
        # uint64_t BackedgeCount = getProfileCount(S.getBody()) - ParentCount;
        builder().create_cond_br(bool_cond_val, loop_body, loop_exit.block) # createProfileWeightsForLoop(S.getCond(), BackedgeCount));

    # LoopStack.pop();
    emit_block(loop_exit.block)

    if not emit_bool_cond_branch:
        simplify_forwarding_blocks(loop_cond.block)

def emit_call_arg(args, e, ty):
    # DisableDebugLocationUpdates Dis(*this, E);
    if e.value_kind != AST.ValueKind.PRVALUE:
        # assert(E->getObjectKind() == OK_Ordinary);
        args.append((emit_reference_binding_to_expr(e), ty))
    # bool HasAggregateEvalKind = hasAggregateEvaluationKind(type);
    # if (type->isRecordType() && type->castAs<RecordType>()->getDecl()->isParamDestroyedInCallee()) # TODO
    # if (HasAggregateEvalKind && isa<ImplicitCastExpr>(E) && cast<CastExpr>(E)->getCastKind() == CK_LValueToRValue && !type->isArrayParameterType()) # TODO
    args.append((emit_any_expr_to_temp(e), ty))

def emit_call_args(args, prototype: AST.FunctionType, arg_range, ac):
    arg_types = []
    is_variadic = prototype.is_variadic
    arg_types = prototype.param_types.copy()
    for a in arg_range[len(prototype.param_types):]:
        arg_types.append(a.ty)
    assert len(arg_types) == len(arg_range)
    for i in range(len(arg_types)):
        arg = arg_range[i]
        initial_arg_size = len(args)
        emit_call_arg(args, arg, arg_types[i])
        # if args[-1].has_lvalue():
        #     rv_arg = args[-1].get_known_rvalue()
        #     emit_non_null_arg_check(rvarg, arg_types[i], arg.get_range()[0], ac, i)

def emit_call_expr(e: AST.CallExpr, return_value):
    callee = e.fn.decl.name
    args = []
    emit_call_args(args, e.fn.ty, e.args, e.fn)
    # const CGFunctionInfo &FnInfo = CGM.getTypes().arrangeFreeFunctionCall(Args, e.fn.ty, /*ChainCall=*/Chain);
    # llvm::CallBase *CallOrInvoke = nullptr;
    # call = emit_call(FnInfo, Callee, ReturnValue, Args, &CallOrInvoke, E == MustTailCall, E->getExprLoc());
    call = builder().insert(CallInstr(e.fn.ty, callee, [a[0] for a in args]))
    # debug info
    return call;

def emit_any_expr_to_temp(e: AST.Expr):
    aggslot = 'ignored'
    if isinstance(e.ty, AST.StructType) or isinstance(e.ty, AST.ArrayType):
        aggslot = create_agg_temp(e.ty, "agg.tmp")
    return emit_any_expr(e, aggslot)

"""
RValue CodeGenFunction::EmitCall(const CGFunctionInfo &CallInfo, const CGCallee &Callee, ReturnValueSlot ReturnValue, const CallArgList &CallArgs, llvm::CallBase **callOrInvoke, bool IsMustTail, SourceLocation Loc, bool IsVirtualFunctionPointerThunk) {
  assert(Callee.isOrdinary() || Callee.isVirtual());
  QualType RetTy = CallInfo.getReturnType();
  const ABIArgInfo &RetAI = CallInfo.getReturnInfo();

  llvm::FunctionType *IRFuncTy = getTypes().GetFunctionType(CallInfo);

  const Decl *TargetDecl = Callee.getAbstractInfo().getCalleeDecl().getDecl();

  CGM.getTargetCodeGenInfo().checkFunctionCallABI(CGM, Loc, dyn_cast_or_null<FunctionDecl>(CurCodeDecl), dyn_cast_or_null<FunctionDecl>(TargetDecl), CallArgs, RetTy);

  RawAddress ArgMemory = RawAddress::invalid();
  if (llvm::StructType *ArgStruct = CallInfo.getArgStruct()) {
    const llvm::DataLayout &DL = CGM.getDataLayout();
    llvm::Instruction *IP = CallArgs.getStackBase();
    llvm::AllocaInst *AI;
    if (IP) {
      IP = IP->getNextNode();
      AI = new llvm::AllocaInst(ArgStruct, DL.getAllocaAddrSpace(), "argmem", IP->getIterator());
    } else {
      AI = CreateTempAlloca(ArgStruct, "argmem");
    }
    auto Align = CallInfo.getArgStructAlignment();
    AI->setAlignment(Align.getAsAlign());
    AI->setUsedWithInAlloca(true);
    assert(AI->isUsedWithInAlloca() && !AI->isStaticAlloca());
    ArgMemory = RawAddress(AI, ArgStruct, Align);
  }

  ClangToLLVMArgMapping IRFunctionArgs(CGM.getContext(), CallInfo);
  SmallVector<llvm::Value *, 16> IRCallArgs(IRFunctionArgs.totalIRArgs());

  Address SRetPtr = Address::invalid();
  RawAddress SRetAlloca = RawAddress::invalid();
  llvm::Value *UnusedReturnSizePtr = nullptr;
  if (RetAI.isIndirect() || RetAI.isInAlloca() || RetAI.isCoerceAndExpand()) {
    if (IsVirtualFunctionPointerThunk && RetAI.isIndirect()) {
      SRetPtr = makeNaturalAddressForPointer(CurFn->arg_begin() + IRFunctionArgs.getSRetArgNo(), RetTy, CharUnits::fromQuantity(1));
    } else if (!ReturnValue.isNull()) {
      SRetPtr = ReturnValue.getAddress();
    } else {
      SRetPtr = CreateMemTemp(RetTy, "tmp", &SRetAlloca);
      if (HaveInsertPoint() && ReturnValue.isUnused()) {
        llvm::TypeSize size = CGM.getDataLayout().getTypeAllocSize(ConvertTypeForMem(RetTy));
        UnusedReturnSizePtr = EmitLifetimeStart(size, SRetAlloca.getPointer());
      }
    }
    if (IRFunctionArgs.hasSRetArg()) {
      IRCallArgs[IRFunctionArgs.getSRetArgNo()] = getAsNaturalPointerTo(SRetPtr, RetTy);
    } else if (RetAI.isInAlloca()) {
      Address Addr = Builder().CreateStructGEP(ArgMemory, RetAI.getInAllocaFieldIndex());
      Builder().CreateStore(getAsNaturalPointerTo(SRetPtr, RetTy), Addr);
    }
  }

  RawAddress swiftErrorTemp = RawAddress::invalid();
  Address swiftErrorArg = Address::invalid();

  SmallVector<CallLifetimeEnd, 2> CallLifetimeEndAfterCall;

  assert(CallInfo.arg_size() == CallArgs.size() && "Mismatch between function signature & arguments.");
  unsigned ArgNo = 0;
  CGFunctionInfo::const_arg_iterator info_it = CallInfo.arg_begin();
  for (CallArgList::const_iterator I = CallArgs.begin(), E = CallArgs.end(); I != E; ++I, ++info_it, ++ArgNo) {
    const ABIArgInfo &ArgInfo = info_it->info;
    if (IRFunctionArgs.hasPaddingArg(ArgNo)) IRCallArgs[IRFunctionArgs.getPaddingArgNo(ArgNo)] = llvm::UndefValue::get(ArgInfo.getPaddingType());
    unsigned FirstIRArg, NumIRArgs;
    std::tie(FirstIRArg, NumIRArgs) = IRFunctionArgs.getIRArgs(ArgNo);
    bool ArgHasMaybeUndefAttr = IsArgumentMaybeUndef(TargetDecl, CallInfo.getNumRequiredArgs(), ArgNo);
    switch (ArgInfo.getKind()) {
    case ABIArgInfo::Indirect: {
      assert(NumIRArgs == 1);
      if (I->isAggregate()) {
        Address Addr = I->hasLValue() ? I->getKnownLValue().getAddress() : I->getKnownRValue().getAggregateAddress();
        CharUnits Align = ArgInfo.getIndirectAlign();
        const llvm::DataLayout *TD = &CGM.getDataLayout();
        assert((FirstIRArg >= IRFuncTy->getNumParams() || IRFuncTy->getParamType(FirstIRArg)->getPointerAddressSpace() == TD->getAllocaAddrSpace()) && "indirect argument must be in alloca address space");
        bool NeedCopy = false;
        if (Addr.getAlignment() < Align && llvm::getOrEnforceKnownAlignment(Addr.emitRawPointer(*this), Align.getAsAlign(), *TD) < Align.getAsAlign()) {
          NeedCopy = true;
        } else if (I->hasLValue()) {
          auto LV = I->getKnownLValue();
          auto AS = LV.getAddressSpace();
          bool isByValOrRef = ArgInfo.isIndirectAliased() || ArgInfo.getIndirectByVal();
          if (!isByValOrRef || (LV.getAlignment() < getContext().getTypeAlignInChars(I->Ty))) {
            NeedCopy = true;
          }
          if ((isByValOrRef &&
              (AS != LangAS::Default &&
               AS != CGM.getASTAllocaAddressSpace()))) {
            NeedCopy = true;
          }
        }
        if (!NeedCopy) {
          llvm::Value *V = getAsNaturalPointerTo(Addr, I->Ty);
          auto *T = llvm::PointerType::get(CGM.getLLVMContext(), CGM.getDataLayout().getAllocaAddrSpace());
          llvm::Value *Val = getTargetHooks().performAddrSpaceCast(*this, V, LangAS::Default, CGM.getASTAllocaAddressSpace(), T, true);
          if (ArgHasMaybeUndefAttr) Val = Builder().CreateFreeze(Val);
          IRCallArgs[FirstIRArg] = Val;
          break;
        }
      }
      RawAddress AI = CreateMemTempWithoutCast(I->Ty, ArgInfo.getIndirectAlign(), "byval-temp");
      llvm::Value *Val = getAsNaturalPointerTo(AI, I->Ty);
      if (ArgHasMaybeUndefAttr) Val = Builder().CreateFreeze(Val);
      IRCallArgs[FirstIRArg] = Val;
      llvm::TypeSize ByvalTempElementSize = CGM.getDataLayout().getTypeAllocSize(AI.getElementType());
      llvm::Value *LifetimeSize = EmitLifetimeStart(ByvalTempElementSize, AI.getPointer());
      if (LifetimeSize) CallLifetimeEndAfterCall.emplace_back(AI, LifetimeSize);
      I->copyInto(*this, AI);
      break;
    }
    case ABIArgInfo::Ignore: break;
    case ABIArgInfo::Direct: {
      if (!isa<llvm::StructType>(ArgInfo.getCoerceToType()) && ArgInfo.getCoerceToType() == ConvertType(info_it->type) && ArgInfo.getDirectOffset() == 0) {
        assert(NumIRArgs == 1);
        llvm::Value *V;
        if (!I->isAggregate()) V = I->getKnownRValue().getScalarVal();
        else V = Builder().CreateLoad(I->hasLValue() ? I->getKnownLValue().getAddress() : I->getKnownRValue().getAggregateAddress());
        if (CallInfo.getExtParameterInfo(ArgNo).getABI() == ParameterABI::SwiftErrorResult) {
          assert(!swiftErrorTemp.isValid() && "multiple swifterror args");
          QualType pointeeTy = I->Ty->getPointeeType();
          swiftErrorArg = makeNaturalAddressForPointer(V, pointeeTy, getContext().getTypeAlignInChars(pointeeTy));
          swiftErrorTemp = CreateMemTemp(pointeeTy, getPointerAlign(), "swifterror.temp");
          V = swiftErrorTemp.getPointer();
          cast<llvm::AllocaInst>(V)->setSwiftError(true);
          llvm::Value *errorValue = Builder().CreateLoad(swiftErrorArg);
          Builder().CreateStore(errorValue, swiftErrorTemp);
        }
        if (ArgInfo.getCoerceToType() != V->getType() && V->getType()->isIntegerTy()) V = Builder().CreateZExt(V, ArgInfo.getCoerceToType());
        if (FirstIRArg < IRFuncTy->getNumParams() && V->getType() != IRFuncTy->getParamType(FirstIRArg)) V = Builder().CreateBitCast(V, IRFuncTy->getParamType(FirstIRArg));
        if (ArgHasMaybeUndefAttr) V = Builder().CreateFreeze(V);
        IRCallArgs[FirstIRArg] = V;
        break;
      }
      llvm::StructType *STy = dyn_cast<llvm::StructType>(ArgInfo.getCoerceToType());
      if (STy && ArgInfo.isDirect() && !ArgInfo.getCanBeFlattened()) {
        llvm::Type *SrcTy = ConvertTypeForMem(I->Ty);
        [[maybe_unused]] llvm::TypeSize SrcTypeSize = CGM.getDataLayout().getTypeAllocSize(SrcTy);
        [[maybe_unused]] llvm::TypeSize DstTypeSize = CGM.getDataLayout().getTypeAllocSize(STy);
        if (STy->containsHomogeneousScalableVectorTypes()) {
          assert(SrcTypeSize == DstTypeSize && "Only allow non-fractional movement of structure with " "homogeneous scalable vector type");
          IRCallArgs[FirstIRArg] = I->getKnownRValue().getScalarVal();
          break;
        }
      }
      Address Src = Address::invalid();
      if (!I->isAggregate()) {
        Src = CreateMemTemp(I->Ty, "coerce");
        I->copyInto(*this, Src);
      } else {
        Src = I->hasLValue() ? I->getKnownLValue().getAddress() : I->getKnownRValue().getAggregateAddress();
      }
      Src = emitAddressAtOffset(*this, Src, ArgInfo);
      if (STy && ArgInfo.isDirect() && ArgInfo.getCanBeFlattened()) {
        llvm::Type *SrcTy = Src.getElementType();
        llvm::TypeSize SrcTypeSize = CGM.getDataLayout().getTypeAllocSize(SrcTy);
        llvm::TypeSize DstTypeSize = CGM.getDataLayout().getTypeAllocSize(STy);
        if (SrcTypeSize.isScalable()) {
          assert(STy->containsHomogeneousScalableVectorTypes() && "ABI only supports structure with homogeneous scalable vector " "type");
          assert(SrcTypeSize == DstTypeSize && "Only allow non-fractional movement of structure with " "homogeneous scalable vector type");
          assert(NumIRArgs == STy->getNumElements());
          llvm::Value *StoredStructValue = Builder().CreateLoad(Src, Src.getName() + ".tuple");
          for (unsigned i = 0, e = STy->getNumElements(); i != e; ++i) {
            llvm::Value *Extract = Builder().CreateExtractValue(StoredStructValue, i, Src.getName() + ".extract" + Twine(i));
            IRCallArgs[FirstIRArg + i] = Extract;
          }
        } else {
          uint64_t SrcSize = SrcTypeSize.getFixedValue();
          uint64_t DstSize = DstTypeSize.getFixedValue();
          if (SrcSize < DstSize) {
            Address TempAlloca = CreateTempAlloca(STy, Src.getAlignment(), Src.getName() + ".coerce");
            Builder().CreateMemCpy(TempAlloca, Src, SrcSize);
            Src = TempAlloca;
          } else {
            Src = Src.withElementType(STy);
          }
          assert(NumIRArgs == STy->getNumElements());
          for (unsigned i = 0, e = STy->getNumElements(); i != e; ++i) {
            Address EltPtr = Builder().CreateStructGEP(Src, i);
            llvm::Value *LI = Builder().CreateLoad(EltPtr);
            if (ArgHasMaybeUndefAttr) LI = Builder().CreateFreeze(LI);
            IRCallArgs[FirstIRArg + i] = LI;
          }
        }
      } else {
        assert(NumIRArgs == 1);
        llvm::Value *Load = CreateCoercedLoad(Src, ArgInfo.getCoerceToType(), *this);
        if (CallInfo.isCmseNSCall()) {
          auto *ATy = dyn_cast<llvm::ArrayType>(Load->getType());
          if (ATy != nullptr && isa<RecordType>(I->Ty.getCanonicalType())) Load = EmitCMSEClearRecord(Load, ATy, I->Ty);
        }
        if (ArgHasMaybeUndefAttr) Load = Builder().CreateFreeze(Load);
        IRCallArgs[FirstIRArg] = Load;
      }
      break;
    }
    }
  }
  const CGCallee &ConcreteCallee = Callee.prepareConcreteCallee(*this);
  llvm::Value *CalleePtr = ConcreteCallee.getFunctionPointer();
  if (ArgMemory.isValid()) {
    llvm::Value *Arg = ArgMemory.getPointer();
    assert(IRFunctionArgs.hasInallocaArg());
    IRCallArgs[IRFunctionArgs.getInallocaArgNo()] = Arg;
  }
  auto simplifyVariadicCallee = [](llvm::FunctionType *CalleeFT, llvm::Value *Ptr) -> llvm::Function * {
    if (!CalleeFT->isVarArg()) return nullptr;
    if (llvm::ConstantExpr *CE = dyn_cast<llvm::ConstantExpr>(Ptr)) {
      if (CE->getOpcode() == llvm::Instruction::BitCast) Ptr = CE->getOperand(0);
    }
    llvm::Function *OrigFn = dyn_cast<llvm::Function>(Ptr);
    if (!OrigFn) return nullptr;
    llvm::FunctionType *OrigFT = OrigFn->getFunctionType();
    if (OrigFT->isVarArg() || OrigFT->getNumParams() != CalleeFT->getNumParams() || OrigFT->getReturnType() != CalleeFT->getReturnType()) return nullptr;
    for (unsigned i = 0, e = OrigFT->getNumParams(); i != e; ++i) if (OrigFT->getParamType(i) != CalleeFT->getParamType(i)) return nullptr;
    return OrigFn;
  };
  if (llvm::Function *OrigFn = simplifyVariadicCallee(IRFuncTy, CalleePtr)) {
    CalleePtr = OrigFn;
    IRFuncTy = OrigFn->getFunctionType();
  }
  if (!CallArgs.getCleanupsToDeactivate().empty()) deactivateArgCleanupsBeforeCall(*this, CallArgs);
  for (unsigned i = 0; i < IRCallArgs.size(); ++i) LargestVectorWidth = std::max(LargestVectorWidth, getMaxVectorWidth(IRCallArgs[i]->getType()));
  unsigned CallingConv;
  llvm::AttributeList Attrs;
  CGM.ConstructAttributeList(CalleePtr->getName(), CallInfo, Callee.getAbstractInfo(), Attrs, CallingConv, /*AttrOnCallSite=*/true, /*IsThunk=*/false);
  if (const FunctionDecl *FD = dyn_cast_or_null<FunctionDecl>(CurFuncDecl)) {
    if (FD->hasAttr<StrictFPAttr>()) Attrs = Attrs.addFnAttribute(getLLVMContext(), llvm::Attribute::StrictFP);
    if (FD->hasAttr<OptimizeNoneAttr>() && getLangOpts().FastMath) CGM.AdjustMemoryAttribute(CalleePtr->getName(), Callee.getAbstractInfo(), Attrs);
  }
  if (CurCodeDecl && CurCodeDecl->hasAttr<FlattenAttr>() && !InNoInlineAttributedStmt && !(TargetDecl && TargetDecl->hasAttr<NoInlineAttr>())) {
    Attrs = Attrs.addFnAttribute(getLLVMContext(), llvm::Attribute::AlwaysInline);
  }
  bool CannotThrow = true;
  if (UnusedReturnSizePtr) pushFullExprCleanup<CallLifetimeEnd>(NormalEHLifetimeMarker, SRetAlloca, UnusedReturnSizePtr);
  llvm::BasicBlock *InvokeDest = CannotThrow ? nullptr : getInvokeDest();
  SmallVector<llvm::OperandBundleDef, 1> BundleList = getBundlesForFunclet(CalleePtr);
  EmitPointerAuthOperandBundle(ConcreteCallee.getPointerAuthInfo(), BundleList);
  AssumeAlignedAttrEmitter AssumeAlignedAttrEmitter(*this, TargetDecl);
  Attrs = AssumeAlignedAttrEmitter.TryEmitAsCallSiteAttribute(Attrs);
  AllocAlignAttrEmitter AllocAlignAttrEmitter(*this, TargetDecl, CallArgs);
  Attrs = AllocAlignAttrEmitter.TryEmitAsCallSiteAttribute(Attrs);
  llvm::CallBase *CI;
  if (!InvokeDest) {
    CI = Builder().CreateCall(IRFuncTy, CalleePtr, IRCallArgs, BundleList);
  } else {
    llvm::BasicBlock *Cont = createBasicBlock("invoke.cont");
    CI = Builder().CreateInvoke(IRFuncTy, CalleePtr, Cont, InvokeDest, IRCallArgs, BundleList);
    EmitBlock(Cont);
  }
  if (CI->getCalledFunction() && CI->getCalledFunction()->hasName() && CI->getCalledFunction()->getName().starts_with("_Z4sqrt")) {
    SetSqrtFPAccuracy(CI);
  }
  if (callOrInvoke) *callOrInvoke = CI;
  if (const auto *FD = dyn_cast_or_null<FunctionDecl>(CurFuncDecl)) {
    if (const auto *A = FD->getAttr<CFGuardAttr>()) {
      if (A->getGuard() == CFGuardAttr::GuardArg::nocf && !CI->getCalledFunction()) Attrs = Attrs.addFnAttribute(getLLVMContext(), "guard_nocf");
    }
  }
  CI->setAttributes(Attrs);
  CI->setCallingConv(static_cast<llvm::CallingConv::ID>(CallingConv));
  if (!CI->getType()->isVoidTy()) CI->setName("call");
  if (CGM.shouldEmitConvergenceTokens() && CI->isConvergent()) CI = addControlledConvergenceToken(CI);
  LargestVectorWidth = std::max(LargestVectorWidth, getMaxVectorWidth(CI->getType()));
  if (!CI->getCalledFunction()) PGO.valueProfile(builder(), llvm::IPVK_IndirectCallTarget, CI, CalleePtr);
  if (llvm::CallInst *Call = dyn_cast<llvm::CallInst>(CI)) {
    if (TargetDecl && TargetDecl->hasAttr<NotTailCalledAttr>()) Call->setTailCallKind(llvm::CallInst::TCK_NoTail);
    else if (IsMustTail) Call->setTailCallKind(llvm::CallInst::TCK_MustTail);
  }
  if (CI->doesNotReturn()) {
    if (UnusedReturnSizePtr) PopCleanupBlock();
    EmitUnreachable(Loc);
    Builder().ClearInsertionPoint();
    EnsureInsertPoint();
    return GetUndefRValue(RetTy);
  }
  if (IsMustTail) {
    for (auto it = EHStack.find(CurrentCleanupScopeDepth); it != EHStack.end(); ++it) {
      EHCleanupScope *Cleanup = dyn_cast<EHCleanupScope>(&*it);
      if (!(Cleanup && Cleanup->getCleanup()->isRedundantBeforeReturn())) CGM.ErrorUnsupported(MustTailCall, "tail call skipping over cleanups");
    }
    if (CI->getType()->isVoidTy()) Builder().CreateRetVoid();
    else Builder().CreateRet(CI);
    Builder().ClearInsertionPoint();
    EnsureInsertPoint();
    return GetUndefRValue(RetTy);
  }
  if (swiftErrorTemp.isValid()) {
    llvm::Value *errorResult = Builder().CreateLoad(swiftErrorTemp);
    Builder().CreateStore(errorResult, swiftErrorArg);
  }
  if (CallArgs.hasWritebacks()) emitWritebacks(*this, CallArgs);
  CallArgs.freeArgumentMemory(*this);
  RValue Ret;
  if (IsVirtualFunctionPointerThunk) {
    Ret = RValue::get(CI);
  } else {
    Ret = [&] {
      switch (RetAI.getKind()) {
      case ABIArgInfo::CoerceAndExpand: {
        auto coercionType = RetAI.getCoerceAndExpandType();
        Address addr = SRetPtr.withElementType(coercionType);
        assert(CI->getType() == RetAI.getUnpaddedCoerceAndExpandType());
        bool requiresExtract = isa<llvm::StructType>(CI->getType());
        unsigned unpaddedIndex = 0;
        for (unsigned i = 0, e = coercionType->getNumElements(); i != e; ++i) {
          llvm::Type *eltType = coercionType->getElementType(i);
          if (ABIArgInfo::isPaddingForCoerceAndExpand(eltType)) continue;
          Address eltAddr = Builder().CreateStructGEP(addr, i);
          llvm::Value *elt = CI;
          if (requiresExtract) elt = Builder().CreateExtractValue(elt, unpaddedIndex++);
          else assert(unpaddedIndex == 0);
          Builder().CreateStore(elt, eltAddr);
        }
        [[fallthrough]];
      }
      case ABIArgInfo::InAlloca:
      case ABIArgInfo::Indirect: {
        RValue ret = convertTempToRValue(SRetPtr, RetTy, SourceLocation());
        if (UnusedReturnSizePtr) PopCleanupBlock();
        return ret;
      }
      case ABIArgInfo::Ignore:
        return GetUndefRValue(RetTy);
      case ABIArgInfo::Extend:
      case ABIArgInfo::Direct: {
        llvm::Type *RetIRTy = ConvertType(RetTy);
        if (RetAI.getCoerceToType() == RetIRTy && RetAI.getDirectOffset() == 0) {
          switch (getEvaluationKind(RetTy)) {
          case TEK_Complex: {
            llvm::Value *Real = Builder().CreateExtractValue(CI, 0);
            llvm::Value *Imag = Builder().CreateExtractValue(CI, 1);
            return RValue::getComplex(std::make_pair(Real, Imag));
          }
          case TEK_Aggregate: break;
          case TEK_Scalar: {
            llvm::Value *V = CI;
            if (V->getType() != RetIRTy) V = Builder().CreateBitCast(V, RetIRTy);
            return RValue::get(V);
          }
          }
        }
        if (auto *FixedDstTy = dyn_cast<llvm::FixedVectorType>(RetIRTy)) {
          llvm::Value *V = CI;
          if (auto *ScalableSrcTy = dyn_cast<llvm::ScalableVectorType>(V->getType())) {
            if (FixedDstTy->getElementType() == ScalableSrcTy->getElementType()) {
              llvm::Value *Zero = llvm::Constant::getNullValue(CGM.Int64Ty);
              V = Builder().CreateExtractVector(FixedDstTy, V, Zero, "cast.fixed");
              return RValue::get(V);
            }
          }
        }
        Address DestPtr = ReturnValue.getValue();
        bool DestIsVolatile = ReturnValue.isVolatile();
        uint64_t DestSize = getContext().getTypeInfoDataSizeInChars(RetTy).Width.getQuantity();
        if (!DestPtr.isValid()) {
          DestPtr = CreateMemTemp(RetTy, "coerce");
          DestIsVolatile = false;
          DestSize = getContext().getTypeSizeInChars(RetTy).getQuantity();
        }
        if (!isEmptyRecord(getContext(), RetTy, true)) {
          Address StorePtr = emitAddressAtOffset(*this, DestPtr, RetAI);
          CreateCoercedStore(CI, StorePtr, llvm::TypeSize::getFixed(DestSize - RetAI.getDirectOffset()), DestIsVolatile);
        }
        return convertTempToRValue(DestPtr, RetTy, SourceLocation());
      }
      case ABIArgInfo::Expand:
      case ABIArgInfo::IndirectAliased:
        llvm_unreachable("Invalid ABI kind for return argument");
      }
      llvm_unreachable("Unhandled ABIArgInfo::Kind");
    }();
  }
  if (Ret.isScalar() && TargetDecl) {
    AssumeAlignedAttrEmitter.EmitAsAnAssumption(Loc, RetTy, Ret);
    AllocAlignAttrEmitter.EmitAsAnAssumption(Loc, RetTy, Ret);
  }
  for (CallLifetimeEnd &LifetimeEnd : CallLifetimeEndAfterCall) LifetimeEnd.Emit(*this, /*Flags=*/{});
  if (!ReturnValue.isExternallyDestructed() && RetTy.isDestructedType() == QualType::DK_nontrivial_c_struct) pushDestroy(QualType::DK_nontrivial_c_struct, Ret.getAggregateAddress(), RetTy);
  return Ret;
}"""

def emit_ignored_expr(e):
    if e.value_kind == AST.ValueKind.PRVALUE:
        emit_any_expr(e, 'ignored', True)
    else:
        emit_lvalue(e)

def emit_promoted_scalar_expr(e, promotion_ty):
    if promotion_ty is None:
        return ScalarExprEmitter().visit(e)
    else:
        return ScalarExprEmitter().visit(e, promotion_ty)

def emit_lvalue(e, is_known_non_null = False):
    lv = emit_lvalue_helper(e, is_known_non_null)
    # if (is_known_non_null and not lv.is_known_non_null: lv.is_known_non_null = True
    return lv

def emit_lvalue_helper(e, is_known_non_null):
    # ApplyDebugLocation DL(*this, E);
    if isinstance(e, AST.CompoundAssignExpr):
        return emit_compound_assignment_lvalue(e)
    elif isinstance(e, AST.BinaryExpr):
        return emit_binary_operator_lvalue(e)
    elif isinstance(e, AST.CallExpr):
        return emit_call_expr_lvalue(e)
    elif isinstance(e, AST.VAArgExpr):
        return emit_vaarg_expr_lvalue(e)
    elif isinstance(e, AST.DeclRefExpr):
        return emit_decl_ref_lvalue(e)
    elif isinstance(e, AST.ParenExpr):
        return emit_lvalue(e.val)
    elif isinstance(e, AST.StringLiteral):
        return emit_string_literal_lvalue(e)
    elif isinstance(e, AST.UnaryExpr):
        return emit_unary_op_lvalue(e)
    elif isinstance(e, AST.ArraySubscriptExpr):
        return emit_array_subscript_expr(e)
    elif isinstance(e, AST.MemberExpr):
        return emit_member_expr(e)
    elif isinstance(e, AST.ConditionalExpr):
        return emit_conditional_operator_lvalue(e)
    elif isinstance(e, AST.CastExpr):
        return emit_cast_lvalue(e)
    else:
        return emit_unsupported_lvalue(e, "l-value expression")

CONSTANT_STRING_MAP = {}

def generate_string_literal(c, lt, name, alignment):
    # unsigned AddrSpace = CGM.getContext().getTargetAddressSpace(CGM.GetGlobalConstantAddressSpace());
    gv = GlobalVariable(c[0], True, lt, c[1], name) # nullptr, llvm::GlobalVariable::NotThreadLocal, AddrSpace);
    gv.align = alignment
    # gv->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);
    # CGM.setDSOLocal(GV);
    return gv

def get_addr_of_constant_string_from_literal(s: AST.StringLiteral, name: str = ".str"):
    alignment = 1 # getContext().getAlignOfGlobalVarInChars(S->getType(), /*VD=*/nullptr);
    entry = None
    if s.value in CONSTANT_STRING_MAP.keys():
        gv = CONSTANT_STRING_MAP[s.value]
        # if (uint64_t(Alignment.getQuantity()) > GV->getAlignment()) GV->setAlignment(Alignment.getAsAlign());
        return Address(gv, gv.ty, alignment) # cast_string_literal_to_default_address_space(

    gv = generate_string_literal((s.ty, s.value), 'private', name, alignment);

    # CGDebugInfo *DI = getModuleDebugInfo();
    # if (DI && getCodeGenOpts().hasReducedDebugInfo()) DI->AddStringLiteralDebugInfo(GV, S);
    CONSTANT_STRING_MAP[s.value] = gv
    return Address(gv, gv.ty, alignment) # cast_string_literal_to_default_address_space(

def emit_string_literal_lvalue(e: AST.StringLiteral):
    return make_addr_lvalue(get_addr_of_constant_string_from_literal(e), e.ty, source='decl')

def make_addr_lvalue(addr, t, **kwargs):
    return LValue(addr, t)

def emit_array_to_pointer_decay(e: AST.Expr, base_info = None, tbaa_info = None):
    assert isinstance(e.ty, AST.ArrayType)
    lv = emit_lvalue(e)
    addr = lv.addr
    new_ty = e.ty
    addr = addr.with_element_type(new_ty)
    # if (!E->getType()->isVariableArrayType()) { assert(isa<llvm::ArrayType>(Addr.getElementType()) && "Expected pointer to array"); Addr = Builder().CreateConstArrayGEP(Addr, 0, "arraydecay"); }
    elt_type = e.ty.subtype
    # if (BaseInfo) *BaseInfo = LV.getBaseInfo();
    # if (TBAAInfo) *TBAAInfo = CGM.getTBAAAccessInfo(EltType);
    return addr.with_element_type(elt_type)

def emit_any_expr(e, aggslot, ignore_result = False):
    if isinstance(e.ty, AST.ArrayType) or isinstance(e.ty, AST.StructType):
        if not ignore_result and aggslot == 'ignored':
            aggslot = create_agg_temp(e.ty, "agg-temp")
        emit_agg_expr(e, aggslot)
        return aggslot.as_rvalue()
    return emit_scalar_expr(e, ignore_result) # .as_rvalue()

def emit_scalar_expr(e, ignore_result_assign = False):
    assert e is not None and not (isinstance(e.ty, AST.ArrayType) or isinstance(e.ty, AST.StructType))
    return ScalarExprEmitter(ignore_result_assign).visit(e)

from ns_ast.visitor import StmtVisitor

def try_emit_as_constant(ref_expr):
    value = ref_expr.decl
    if isinstance(value, AST.ParamDecl):
        return
    assert False, "TODO"

def emit_from_memory(v, ty):
    if ty == TYPES["bool"]: # or isbitinttype
        return builder.create_trunc(v, ty, "loadedv")
    return v

def emit_load_of_scalar(addr, volatile, ty, loc, bi, tbaa, is_nt):
    # addr = addr.with_element_type(convertTypeForLoadStore(Ty, Addr.getElementType())))
    load = builder().create_load(addr, "", volatile)
    return emit_from_memory(load, ty)

def emit_load_of_lvalue(lv, loc = 0):
  # if (LV.isSimple()) {
  # assert(!LV.getType()->isFunctionType());
  return emit_load_of_scalar(lv.addr, False, lv.ty, loc, None, None, False) # RValue::get()
  # }
  # if (LV.isGlobalReg()) return EmitLoadOfGlobalRegLValue(LV);
  # return EmitLoadOfBitfieldLValue(LV, Loc);

def emit_decl_ref_lvalue(e: AST.DeclRefExpr):
    nd = e.decl
    t = e.ty
    if isinstance(nd, AST.VarDecl):
        # if (VD->hasLinkage() || VD->isStaticDataMember()) return EmitGlobalVarDeclLValue(*this, E, VD);
        addr = LOCAL_DECL_MAP[id(nd)]
        return make_addr_lvalue(addr, t, source='decl')

    # if (const auto *FD = dyn_cast<FunctionDecl>(ND)) return EmitFunctionDeclLValue(*this, E, FD);
    assert False

class ScalarExprEmitter(StmtVisitor):
    ira: bool
    def __init__(self, b: bool = False):
        self.ira = b

    def visit(self, e: AST.Stmt, *args, **kwargs):
        # ApplyDebugLocation DL(CGF, E);
        return StmtVisitor.visit(self, e, *args, **kwargs)

    def visit_stmt(self, s: AST.Stmt, *args, **kwargs):
        print(f"{s}\n: Stmt can't have complex result type!")

    def visit_expr(self, e: AST.Expr, *args, **kwargs):
        builder().insert(ExprInstr())

    def visit_paren_expr(self, e: AST.ParenExpr, *args, **kwargs):
        return self.visit(e.val)

    def visit_integer_literal(self, e: AST.IntegerLiteral, *args, **kwargs):
        return builder().get_int(e.value)

    def visit_bool_literal(self, e: AST.BoolLiteral, *args, **kwargs):
        return ConstantInt.get(e.value, e.ty)

    def emit_load_of_lvalue(self, e: AST.Expr):
        v = emit_load_of_lvalue(emit_lvalue(e), e.get_range()[0])
        # emit_lvalue_alignment_assumption(e, v)
        return v

    def visit_decl_ref_expr(self, e: AST.DeclRefExpr, *args, **kwargs):
        if (constant := try_emit_as_constant(e)) is not None:
            return emit_scalar_constant(constant, e)
        return self.emit_load_of_lvalue(e)

    def visit_member_expr(self, e: AST.MemberExpr, *args, **kwargs):
        pass

    def visit_array_subscript_expr(self, e: AST.ArraySubscriptExpr, *args, **kwargs):
        pass

    def visit_cast_expr(self, e: AST.CastExpr, *args, **kwargs):
        kind = e.kind
        dest_ty = e.ty
        e = e.op
        # bool Ignored = TestAndClearIgnoreResultAssign();
        match kind:
            case AST.CastKind.NOOP:
                return self.visit(e)
            case AST.CastKind.ARRAY_TO_POINTER_DECAY:
                return emit_array_to_pointer_decay(e) #get_as_natural_pointer_to(, e.ty.subtype)
            case AST.CastKind.FUNCTION_TO_POINTER_DECAY:
                return emit_lvalue(e).get_pointer()
            case AST.CastKind.LVALUE_TO_RVALUE:
                return self.visit(e)
            case AST.CastKind.INTEGRAL_TO_BOOLEAN:
                return self.emit_int_to_bool_conversion(self.visit(e))
            case _:
                print("UNIMPLEMENTED CAST KIND:", kind)
                assert False

    # Value *VisitExplicitCastExpr(ExplicitCastExpr *E) { CGF.CGM.EmitExplicitCastExprType(E, &CGF); return VisitCastExpr(E); }

    def visit_call_expr(self, e: AST.CallExpr, *args, **kwargs):
        return emit_call_expr(e, None) # .get_scalar_val()

    def visit_unary_post_dec(self, e: AST.UnaryExpr, *args, **kwargs):
        lv = emit_lvalue(e.arg)
        return self.emit_scalar_pre_post_inc_dec(e, lv, False, False)

    def visit_unary_post_inc(self, e: AST.UnaryExpr, *args, **kwargs):
        lv = emit_lvalue(e.arg)
        return self.emit_scalar_pre_post_inc_dec(e, lv, True, False)

    def visit_unary_pre_dec(self, e: AST.UnaryExpr, *args, **kwargs):
        lv = emit_lvalue(e.arg)
        return self.emit_scalar_pre_post_inc_dec(e, lv, False, True)

    def visit_unary_pre_inc(self, e: AST.UnaryExpr, *args, **kwargs):
        lv = emit_lvalue(e.arg)
        return self.emit_scalar_pre_post_inc_dec(e, lv, True, True)

    def emit_scapar_pre_post_inc_dec(self, e: AST.UnaryExpr, lv, is_inc, is_pre):
        pass

    def visit_unary_addrof(self, e: AST.UnaryExpr, *args, **kwargs):
        return emit_lvalue(e.arg).get_pointer()

    def visit_unary_deref(self, e: AST.UnaryExpr, *args, **kwargs):
        if AST.type_is_void(e.ty):
            return self.visit(e.arg)
        return self.emit_load_of_lvalue(e)

    def visit_unary_plus(self, e: AST.UnaryExpr, *args, **kwargs):
        promotion_type = self.get_promotion_type(e.arg.ty) if "promotion_ty" not in kwargs.keys() else kwargs["promotion_ty"]
        result = self.visit_plus(e, promotion_type)
        if result is not None and promotion_type is not None:
            result = self.emit_unpromoted_value(result, e.ty)
        return result

    def visit_plus(self, e: AST.UnaryExpr, promotion_type):
        # TestAndClearIgnoreResultAssign();
        if promotion_type is not None:
            return emit_promoted_scalar_expr(e.arg, promotion_type)
        return self.visit(e.arg)

    def visit_unary_minus(self, e: AST.UnaryExpr, *args, **kwargs):
        promotion_type = self.get_promotion_type(e.arg.ty) if "promotion_ty" not in kwargs.keys() else kwargs["promotion_ty"]
        result = self.visit_minus(e, promotion_type)
        if result is not None and promotion_type is not None:
            result = self.emit_unpromoted_value(result, e.ty)
        return result

    def visit_minus(self, e: AST.UnaryExpr, promotion_type):
        # TestAndClearIgnoreResultAssign();
        op = None
        if promotion_type is not None:
            op =  emit_promoted_scalar_expr(e.arg, promotion_type)
        else:
            op = self.visit(e.arg)
        ops = [ConstantInt(op.ty, 0), op, e.ty, e, AST.BinaryOperatorKind.SUB]
        return self.emit_sub(ops)

    def visit_unary_not(self, e: AST.UnaryExpr, *args, **kwargs):
        # TestAndClearIgnoreResultAssign();
        op = self.visit(e.arg)
        return builder().create_not(op, "not")

    def visit_unary_lnot(self, e: AST.UnaryExpr, *args, **kwargs):
        bool_val = evaluate_expr_as_bool(e.arg)
        bool_val = builder().create_not(bool_val, "lnot")
        return builder().create_zext(bool_val, e.ty, "lnot.ext")

    def emit_overflow_checked_bin_op(self, ops):
        pass

    def emit_undefined_behavior_integer_div_and_rem_check(self, ops, zero, is_div):
        pass

    def get_maximum_shift_amount(self, lhs, rhs, rhs_is_signed):
        ty = lhs.ty
        rhs_ty = rhs.ty
        # rhs_max = rhs.ty.max_value()
        # if (RHSMax.ult(Ty->getBitWidth())) return llvm::ConstantInt::get(RHSTy, RHSMax);
        return ConstantInt.get(rhs_ty, ty.get_bit_width() - 1)

    def constrain_shift_lvalue(self, lhs, rhs, name):
        return builder().create_and(rhs, self.get_maximum_shift_amount(lhs, rhs, False), name)

    def emit_pointer_arithmetic(self, op, is_sub):
        expr = op[3]
        ptr = op[0]
        ptr_operand = expr.lhs
        idx = op[1]
        idx_operand = expr.rhs
        if not is_sub and not isinstance(ptr, AST.PointerType):
            ptr, idx = idx, ptr
            ptr_operand, idx_operand = idx_operand, ptr_operand
        is_signed = idx_operand.ty.is_signed()
        w = idx.ty.get_bit_width()
        # auto &DL = CGF.CGM.getDataLayout();
        ptr_ty = ptr.ty
        # if (BinaryOperator::isNullPointerArithmeticExtension(CGF.getContext(), op.Opcode, expr->getLHS(), expr->getRHS())) return CGF.Builder().CreateIntToPtr(index, pointer->getType());
        # if (width != DL.getIndexTypeSizeInBits(PtrTy)) { index = CGF.Builder().CreateIntCast(index, DL.getIndexType(PtrTy), isSigned, "idx.ext"); }
        if is_sub: idx = builder().create_neg(idx, "idx.neg")
        elt_ty = ptr_ty.subtype
        if AST.type_is_void(elt_ty) or isinstance(elt_ty, FunctionType): elt_ty = TYPES["i8"]
        return emit_checked_in_bounds_gep(elt_ty, ptr, idx, is_signed, is_sub, ops[3].get_range()[0], "add.ptr")

    def emit_mul(self, ops):
        if ops[2].is_signed():
            return builder().create_nsw_mul(ops[0], ops[1], "mul")
        return builder().create_mul(ops[0], ops[1], "mul")

    def emit_div(self, ops):
        if ops[2].is_signed():
            return builder().create_sdiv(ops[0], ops[1], "div")
        return builder().create_udiv(ops[0], ops[1], "div")

    def emit_rem(self, ops):
        if ops[2].is_signed():
            return builder().create_srem(ops[0], ops[1], "rem")
        return builder().create_urem(ops[0], ops[1], "rem")

    def emit_add(self, ops):
        if isinstance(ops[0].ty, AST.PointerType) or isinstance(ops[1].ty, AST.PointerType):
            return self.emit_pointer_arithmetic(ops, False)
        # if ops[2].is_signed_integer_or_enumeration_type(): return builder().create_nsw_add(ops[0], ops[1], "add")
        return builder().create_add(ops[0], ops[1], "add")

    def emit_sub(self, ops):
        if not isinstance(ops[0].ty, AST.PointerType):
            # if (op.Ty->isSignedIntegerOrEnumerationType()) return Builder().CreateNSWSub(op.LHS, op.RHS, "sub");
            return builder().create_sub(ops[0], ops[1], "sub");
        if not isinstance(ops[1].ty, AST.PointerType):
            return self.emit_pointer_arithmetic(ops, True)
        lhs = builder().create_ptr_to_int(ops[0], 'i64', "sub.ptr.lhs.cast")
        rhs = builder().create_ptr_to_int(ops[1], 'i64', "sub.ptr.rhs.cast")
        diff_in_chars = builder().create_sub(lhs, rhs, "sub.ptr.sub")
        expr = ops[3]
        elt_ty = expr.lhs.ty.subtype
        divisor = None
        assert False, "unimplemented => ptr diff"

    def emit_shl(self, ops):
        rhs = ops[1]
        if ops[0].ty != rhs.ty:
            rhs = builder().create_int_cast(rhs, ops[0].ty, False, "sh_prom")
        return builder().create_shl(ops[0], rhs, "shl")

    def emit_shr(self, ops):
        rhs = ops[1]
        if ops[0].ty != rhs.ty:
            rhs = builder().create_int_cast(rhs, ops[0].ty, False, "sh_prom")
        if ops[0].ty.is_signed():
            return builder().create_ashr(ops[0], rhs, "shr")
        return builder().create_lshr(ops[0], rhs, "shr")

    def emit_and(self, ops):
        return builder().create_and(ops[0], ops[1], "and")

    def emit_xor(self, ops):
        return builder().create_xor(ops[0], ops[1], "xor")

    def emit_or(self, ops):
        return builder().create_or(ops[0], ops[1], "or")

    def emit_bin_ops(self, e, promotion_ty = None):
        # test_and_clear_ignore_result_assign()
        result = [None, None, None, None, None]
        result[0] = emit_promoted_scalar_expr(e.lhs, promotion_ty)
        result[1] = emit_promoted_scalar_expr(e.rhs, promotion_ty)
        if promotion_ty is None:
            result[2] = e.ty
        else:
            result[2] = promotion_ty
        result[4] = e.opc
        result[3] = e
        return result

    def emit_promoted_value(self, result, promotion_ty):
        pass

    def emit_unpromoted_value(self, result, expr_ty):
        pass

    def emit_promoted(self, e, promotion_ty):
        pass

    def emit_compound_assign_lvalue(self, e, fn, result):
        pass

    def emit_compound_assign(self, e, fn):
        pass

    def get_promotion_type(self, ty):
        # { const auto &Ctx = CGF.getContext(); if (auto *CT = Ty->getAs<ComplexType>()) {QualType ElementType = CT->getElementType(); if (ElementType.UseExcessPrecision(Ctx)) return Ctx.getComplexType(Ctx.FloatTy);} if (Ty.UseExcessPrecision(Ctx)) {if (auto *VT = Ty->getAs<VectorType>()) {unsigned NumElements = VT->getNumElements(); return Ctx.getVectorType(Ctx.FloatTy, NumElements, VT->getVectorKind());} return Ctx.FloatTy;} return QualType();}
        pass

    def visit_bin_mul(self, e: AST.BinaryExpr, *args, **kwargs): # -> Value*
        promotion_ty = self.get_promotion_type(e.ty)
        result = self.emit_mul(self.emit_bin_ops(e, promotion_ty))
        if result is not None and promotion_ty is not None:
            result = self.emit_unpromoted_value(result, e.ty)
        return result

    def visit_bin_div(self, e: AST.BinaryExpr, *args, **kwargs): # -> Value*
        promotion_ty = self.get_promotion_type(e.ty)
        result = self.emit_div(self.emit_bin_ops(e, promotion_ty))
        if result is not None and promotion_ty is not None:
            result = self.emit_unpromoted_value(result, e.ty)
        return result

    def visit_bin_rem(self, e: AST.BinaryExpr, *args, **kwargs): # -> Value*
        promotion_ty = self.get_promotion_type(e.ty)
        result = self.emit_rem(self.emit_bin_ops(e, promotion_ty))
        if result is not None and promotion_ty is not None:
            result = self.emit_unpromoted_value(result, e.ty)
        return result

    def visit_bin_add(self, e: AST.BinaryExpr, *args, **kwargs): # -> Value*
        promotion_ty = self.get_promotion_type(e.ty)
        result = self.emit_add(self.emit_bin_ops(e, promotion_ty))
        if result is not None and promotion_ty is not None:
            result = self.emit_unpromoted_value(result, e.ty)
        return result

    def visit_bin_sub(self, e: AST.BinaryExpr, *args, **kwargs): # -> Value*
        promotion_ty = self.get_promotion_type(e.ty)
        result = self.emit_sub(self.emit_bin_ops(e, promotion_ty))
        if result is not None and promotion_ty is not None:
            result = self.emit_unpromoted_value(result, e.ty)
        return result

    def visit_bin_shl(self, e: AST.BinaryExpr, *args, **kwargs): # -> Value*
        promotion_ty = self.get_promotion_type(e.ty)
        result = self.emit_shl(self.emit_bin_ops(e, promotion_ty))
        if result is not None and promotion_ty is not None:
            result = self.emit_unpromoted_value(result, e.ty)
        return result

    def visit_bin_shr(self, e: AST.BinaryExpr, *args, **kwargs): # -> Value*
        promotion_ty = self.get_promotion_type(e.ty)
        result = self.emit_shr(self.emit_bin_ops(e, promotion_ty))
        if result is not None and promotion_ty is not None:
            result = self.emit_unpromoted_value(result, e.ty)
        return result

    def visit_bin_and(self, e: AST.BinaryExpr, *args, **kwargs): # -> Value*
        promotion_ty = self.get_promotion_type(e.ty)
        result = self.emit_and(self.emit_bin_ops(e, promotion_ty))
        if result is not None and promotion_ty is not None:
            result = self.emit_unpromoted_value(result, e.ty)
        return result

    def visit_bin_xor(self, e: AST.BinaryExpr, *args, **kwargs): # -> Value*
        promotion_ty = self.get_promotion_type(e.ty)
        result = self.emit_xor(self.emit_bin_ops(e, promotion_ty))
        if result is not None and promotion_ty is not None:
            result = self.emit_unpromoted_value(result, e.ty)
        return result

    def visit_bin_or(self, e: AST.BinaryExpr, *args, **kwargs): # -> Value*
        promotion_ty = self.get_promotion_type(e.ty)
        result = self.emit_or(self.emit_bin_ops(e, promotion_ty))
        if result is not None and promotion_ty is not None:
            result = self.emit_unpromoted_value(result, e.ty)
        return result

    def visit_bin_mul_assign(self, e: AST.CompoundAssignExpr, *args, **kwargs): # -> Value*
        return self.emit_compound_assign(e, ScalarExprEmitter.emit_mul)

    def visit_bin_div_assign(self, e: AST.CompoundAssignExpr, *args, **kwargs): # -> Value*
        return self.emit_compound_assign(e, ScalarExprEmitter.emit_div)

    def visit_bin_rem_assign(self, e: AST.CompoundAssignExpr, *args, **kwargs): # -> Value*
        return self.emit_compound_assign(e, ScalarExprEmitter.emit_rem)

    def visit_bin_add_assign(self, e: AST.CompoundAssignExpr, *args, **kwargs): # -> Value*
        return self.emit_compound_assign(e, ScalarExprEmitter.emit_add)

    def visit_bin_sub_assign(self, e: AST.CompoundAssignExpr, *args, **kwargs): # -> Value*
        return self.emit_compound_assign(e, ScalarExprEmitter.emit_sub)

    def visit_bin_shl_assign(self, e: AST.CompoundAssignExpr, *args, **kwargs): # -> Value*
        return self.emit_compound_assign(e, ScalarExprEmitter.emit_shl)

    def visit_bin_shr_assign(self, e: AST.CompoundAssignExpr, *args, **kwargs): # -> Value*
        return self.emit_compound_assign(e, ScalarExprEmitter.emit_shr)

    def visit_bin_and_assign(self, e: AST.CompoundAssignExpr, *args, **kwargs): # -> Value*
        return self.emit_compound_assign(e, ScalarExprEmitter.emit_and)

    def visit_bin_xor_assign(self, e: AST.CompoundAssignExpr, *args, **kwargs): # -> Value*
        return self.emit_compound_assign(e, ScalarExprEmitter.emit_xor)

    def visit_bin_or_assign(self, e: AST.CompoundAssignExpr, *args, **kwargs): # -> Value*
        return self.emit_compound_assign(e, ScalarExprEmitter.emit_or)

    def emit_compare(self, e: AST.BinaryExpr, ui_cmp_opc, si_cmp_opc, f_cmp_opc, is_signaling):
        # TestAndClearIgnoreResultAssign();
        result = None
        bo_info = self.emit_bin_ops(e)
        if e.lhs.ty.is_signed():
            result = builder().create_icmp(si_cmp_opc, bo_info[0], bo_info[1], "cmp")
        else:
            result = builder().create_icmp(ui_cmp_opc, bo_info[0], bo_info[1], "cmp")
        return self.emit_scalar_conversion(result, TYPES["bool"], e.ty, e.get_range()[0])

    def visit_bin_lt(self, e: AST.BinaryExpr, *args, **kwargs): # -> Value*
        return self.emit_compare(e, ICmpInstr.ULT, ICmpInstr.SLT, ICmpInstr.OLT, True)
    def visit_bin_gt(self, e: AST.BinaryExpr, *args, **kwargs): # -> Value*
        return self.emit_compare(e, ICmpInstr.UGT, ICmpInstr.SGT, ICmpInstr.OGT, True)
    def visit_bin_le(self, e: AST.BinaryExpr, *args, **kwargs): # -> Value*
        return self.emit_compare(e, ICmpInstr.ULE, ICmpInstr.SLE, ICmpInstr.OLE, True)
    def visit_bin_ge(self, e: AST.BinaryExpr, *args, **kwargs): # -> Value*
        return self.emit_compare(e, ICmpInstr.UGE, ICmpInstr.SGE, ICmpInstr.OGE, True)
    def visit_bin_eq(self, e: AST.BinaryExpr, *args, **kwargs): # -> Value*
        return self.emit_compare(e, ICmpInstr.EQ, ICmpInstr.EQ, ICmpInstr.OEQ, False)
    def visit_bin_ne(self, e: AST.BinaryExpr, *args, **kwargs): # -> Value*
        return self.emit_compare(e, ICmpInstr.NE, ICmpInstr.NE, ICmpInstr.UNE, False)

    def visit_bin_assign(self, e: AST.BinaryExpr, *args, **kwargs):
        pass
    def visit_bin_land(self, e: AST.BinaryExpr, *args, **kwargs):
        pass
    def visit_bin_lor(self, e: AST.BinaryExpr, *args, **kwargs):
        pass

    def visit_vaarg_expr(self, e: AST.VAArgExpr, *args, **kwargs):
        pass

    def emit_pointer_to_bool_conversion(self, v: Value, t: AST.Type):
        assert False
        # Value *Zero = CGF.CGM.getNullPointer(cast<llvm::PointerType>(V->getType()), QT);
        # return Builder().CreateICmpNE(V, Zero, "tobool");

    def emit_int_to_bool_conversion(self, v):
        # if (llvm::ZExtInst *ZI = dyn_cast<llvm::ZExtInst>(V)) {
        #   if (ZI->getOperand(0)->getType() == Builder().getInt1Ty()) {
        #     Value *Result = ZI->getOperand(0);
        #     if (ZI->use_empty()) ZI->eraseFromParent();
        #     return Result;
        #   }
        # }
        return builder().create_is_not_null(v, "tobool")

    def emit_conversion_to_bool(self, src, src_type):
        if isinstance(src_type, AST.PointerType):
            return self.emit_pointer_to_bool_conversion(src, src_type)
        return self.emit_int_to_bool_conversion(src)

    def emit_scalar_conversion(self, src, src_type, dst_type, loc, opts = None):
        if src_type == dst_type:
            return src
        if AST.type_is_void(dst_type):
            return None
        orig_src = src
        orig_src_type = src_type
        src_ty = src.ty
        if dst_type == TYPES["bool"]:
            return self.emit_conversion_to_bool(src, src_type)
        dst_ty = dst_type
        if src_ty == dst_ty:
            # if (Opts.EmitImplicitIntegerSignChangeChecks) EmitIntegerSignChangeCheck(Src, src_type, Src, dst_type, Loc);
            return src
        if isinstance(dst_ty, AST.PointerType):
            assert False
            # TODO:
            # if (isa<llvm::PointerType>(SrcTy)) return Src;
            # assert(SrcType->isIntegerType() && "Not ptr->ptr or int->ptr conversion?");
            # llvm::Type *MiddleTy = CGF.CGM.getDataLayout().getIntPtrType(DstPT);
            # bool InputSigned = SrcType->isSignedIntegerOrEnumerationType();
            # llvm::Value* IntResult = Builder().CreateIntCast(Src, MiddleTy, InputSigned, "conv");
            # return Builder().CreateIntToPtr(IntResult, DstTy, "conv");
        if isinstance(src_ty, AST.PointerType):
            assert False
            # TODO:
            # assert(isa<llvm::IntegerType>(DstTy) && "not ptr->int?");
            # return Builder().CreatePtrToInt(Src, DstTy, "conv");
        res = None
        res_ty = dst_ty
        res = self.emit_scalar_cast(src, src_type, dst_type, src_ty, dst_ty, opts)
        # if (DstTy != ResTy) {
        #     Res = Builder().CreateFPTrunc(Res, ResTy, "conv");
        # }
        # if (Opts.EmitImplicitIntegerTruncationChecks) EmitIntegerTruncationCheck(Src, src_type, Res, dst_type, Loc);
        # if (Opts.EmitImplicitIntegerSignChangeChecks) EmitIntegerSignChangeCheck(Src, src_type, Res, dst_type, Loc);
        return res;
