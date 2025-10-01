from dataclasses import dataclass
from typing import Dict, List, Self
from ir2.data.instrs._base import Instr
from ir2.data.instrs.memory import LoadInstr, LocalVarInstr, StoreInstr
from ir2.data.instrs.other import PhiInstr
from ir2.data.prog import Function
from ir2.data.value import IrPoisonValue, IrUndefValue, IrValue, Use
from ir2.optim.passes import FunctionPass


@dataclass
class SroaPass(FunctionPass):
    def run_on_function(self, f: Function) -> bool:
        return SROA().run_sroa(f)


@dataclass
class Slice:
    begin_offset: int = 0
    end_offset: int = 0
    use: Use | None = None
    is_splittable: bool = False

    def is_dead(self) -> bool:
        return self.use is None

    def kill(self):
        self.use = None

    def __lt__(self, o: Self | int) -> bool:
        if isinstance(o, int):
            return self.begin_offset < o
        if self.begin_offset < o.begin_offset:
            return True
        if self.begin_offset > o.begin_offset:
            return False
        if self.is_splittable != o.is_splittable:
            return not self.is_splittable
        if self.end_offset > o.end_offset:
            return True
        return False

    def __eq__(self, o: object, /) -> bool:
        if not isinstance(o, "Slice"):
            return False
        return (
            self.is_splittable == o.is_splittable
            and self.begin_offset == o.begin_offset
            and self.end_offset == o.end_offset
        )

    def __ne__(self, value: object, /) -> bool:
        return not (self == value)


class SliceBuilder:  # (PtrUseVisitor<SliceBuilder>):
    alloc_size: int
    AS: "AllocaSlices"
    mem_transfer_slice_map: Dict[int, int]
    phi_or_select_sizes: Dict[int, int]
    visited_dead_insts: List[Instr]

    def __init__(self, ai: LocalVarInstr, AS: "AllocaSlices"):
        self.alloc_size = 0  # ai.ty.get_size()
        self.AS = AS
        self.mem_transfer_slice_map = dict()
        self.phi_or_select_sizes = dict()
        self.visited_dead_insts = []


# private:
#  void markAsDead(Instruction &I) {
#    if (VisitedDeadInsts.insert(&I).second)
#      AS.DeadUsers.push_back(&I);
#  }
#
#  void insertUse(Instruction &I, const APInt &Offset, uint64_t Size,
#                 bool IsSplittable = false) {
#    // Completely skip uses which have a zero size or start either before or
#    // past the end of the allocation.
#    if (Size == 0 || Offset.uge(AllocSize)) {
#      LLVM_DEBUG(dbgs() << "WARNING: Ignoring " << Size << " byte use @"
#                        << Offset
#                        << " which has zero size or starts outside of the "
#                        << AllocSize << " byte alloca:\n"
#                        << "    alloca: " << AS.AI << "\n"
#                        << "       use: " << I << "\n");
#      return markAsDead(I);
#    }
#
#    uint64_t BeginOffset = Offset.getZExtValue();
#    uint64_t EndOffset = BeginOffset + Size;
#
#    // Clamp the end offset to the end of the allocation. Note that this is
#    // formulated to handle even the case where "BeginOffset + Size" overflows.
#    // This may appear superficially to be something we could ignore entirely,
#    // but that is not so! There may be widened loads or PHI-node uses where
#    // some instructions are dead but not others. We can't completely ignore
#    // them, and so have to record at least the information here.
#    assert(AllocSize >= BeginOffset); // Established above.
#    if (Size > AllocSize - BeginOffset) {
#      LLVM_DEBUG(dbgs() << "WARNING: Clamping a " << Size << " byte use @"
#                        << Offset << " to remain within the " << AllocSize
#                        << " byte alloca:\n"
#                        << "    alloca: " << AS.AI << "\n"
#                        << "       use: " << I << "\n");
#      EndOffset = AllocSize;
#    }
#
#    AS.Slices.push_back(Slice(BeginOffset, EndOffset, U, IsSplittable));
#  }
#
#  void visitBitCastInst(BitCastInst &BC) {
#    if (BC.use_empty())
#      return markAsDead(BC);
#
#    return Base::visitBitCastInst(BC);
#  }
#
#  void visitAddrSpaceCastInst(AddrSpaceCastInst &ASC) {
#    if (ASC.use_empty())
#      return markAsDead(ASC);
#
#    return Base::visitAddrSpaceCastInst(ASC);
#  }
#
#  void visitGetElementPtrInst(GetElementPtrInst &GEPI) {
#    if (GEPI.use_empty())
#      return markAsDead(GEPI);
#
#    return Base::visitGetElementPtrInst(GEPI);
#  }
#
#  void handleLoadOrStore(Type *Ty, Instruction &I, const APInt &Offset,
#                         uint64_t Size, bool IsVolatile) {
#    // We allow splitting of non-volatile loads and stores where the type is an
#    // integer type. These may be used to implement 'memcpy' or other "transfer
#    // of bits" patterns.
#    bool IsSplittable =
#        Ty->isIntegerTy() && !IsVolatile && DL.typeSizeEqualsStoreSize(Ty);
#
#    insertUse(I, Offset, Size, IsSplittable);
#  }
#
#  void visitLoadInst(LoadInst &LI) {
#    assert((!LI.isSimple() || LI.getType()->isSingleValueType()) &&
#           "All simple FCA loads should have been pre-split");
#
#    // If there is a load with an unknown offset, we can still perform store
#    // to load forwarding for other known-offset loads.
#    if (!IsOffsetKnown)
#      return PI.setEscapedReadOnly(&LI);
#
#    TypeSize Size = DL.getTypeStoreSize(LI.getType());
#    if (Size.isScalable()) {
#      unsigned VScale = LI.getFunction()->getVScaleValue();
#      if (!VScale)
#        return PI.setAborted(&LI);
#
#      Size = TypeSize::getFixed(Size.getKnownMinValue() * VScale);
#    }
#
#    return handleLoadOrStore(LI.getType(), LI, Offset, Size.getFixedValue(),
#                             LI.isVolatile());
#  }
#
#  void visitStoreInst(StoreInst &SI) {
#    Value *ValOp = SI.getValueOperand();
#    if (ValOp == *U)
#      return PI.setEscapedAndAborted(&SI);
#    if (!IsOffsetKnown)
#      return PI.setAborted(&SI);
#
#    TypeSize StoreSize = DL.getTypeStoreSize(ValOp->getType());
#    if (StoreSize.isScalable()) {
#      unsigned VScale = SI.getFunction()->getVScaleValue();
#      if (!VScale)
#        return PI.setAborted(&SI);
#
#      StoreSize = TypeSize::getFixed(StoreSize.getKnownMinValue() * VScale);
#    }
#
#    uint64_t Size = StoreSize.getFixedValue();
#
#    // If this memory access can be shown to *statically* extend outside the
#    // bounds of the allocation, it's behavior is undefined, so simply
#    // ignore it. Note that this is more strict than the generic clamping
#    // behavior of insertUse. We also try to handle cases which might run the
#    // risk of overflow.
#    // FIXME: We should instead consider the pointer to have escaped if this
#    // function is being instrumented for addressing bugs or race conditions.
#    if (Size > AllocSize || Offset.ugt(AllocSize - Size)) {
#      LLVM_DEBUG(dbgs() << "WARNING: Ignoring " << Size << " byte store @"
#                        << Offset << " which extends past the end of the "
#                        << AllocSize << " byte alloca:\n"
#                        << "    alloca: " << AS.AI << "\n"
#                        << "       use: " << SI << "\n");
#      return markAsDead(SI);
#    }
#
#    assert((!SI.isSimple() || ValOp->getType()->isSingleValueType()) &&
#           "All simple FCA stores should have been pre-split");
#    handleLoadOrStore(ValOp->getType(), SI, Offset, Size, SI.isVolatile());
#  }
#
#  void visitMemSetInst(MemSetInst &II) {
#    assert(II.getRawDest() == *U && "Pointer use is not the destination?");
#    ConstantInt *Length = dyn_cast<ConstantInt>(II.getLength());
#    if ((Length && Length->getValue() == 0) ||
#        (IsOffsetKnown && Offset.uge(AllocSize)))
#      // Zero-length mem transfer intrinsics can be ignored entirely.
#      return markAsDead(II);
#
#    if (!IsOffsetKnown)
#      return PI.setAborted(&II);
#
#    insertUse(II, Offset,
#              Length ? Length->getLimitedValue()
#                     : AllocSize - Offset.getLimitedValue(),
#              (bool)Length);
#  }
#
#  void visitMemTransferInst(MemTransferInst &II) {
#    ConstantInt *Length = dyn_cast<ConstantInt>(II.getLength());
#    if (Length && Length->getValue() == 0)
#      // Zero-length mem transfer intrinsics can be ignored entirely.
#      return markAsDead(II);
#
#    // Because we can visit these intrinsics twice, also check to see if the
#    // first time marked this instruction as dead. If so, skip it.
#    if (VisitedDeadInsts.count(&II))
#      return;
#
#    if (!IsOffsetKnown)
#      return PI.setAborted(&II);
#
#    // This side of the transfer is completely out-of-bounds, and so we can
#    // nuke the entire transfer. However, we also need to nuke the other side
#    // if already added to our partitions.
#    // FIXME: Yet another place we really should bypass this when
#    // instrumenting for ASan.
#    if (Offset.uge(AllocSize)) {
#      SmallDenseMap<Instruction *, unsigned>::iterator MTPI =
#          MemTransferSliceMap.find(&II);
#      if (MTPI != MemTransferSliceMap.end())
#        AS.Slices[MTPI->second].kill();
#      return markAsDead(II);
#    }
#
#    uint64_t RawOffset = Offset.getLimitedValue();
#    uint64_t Size = Length ? Length->getLimitedValue() : AllocSize - RawOffset;
#
#    // Check for the special case where the same exact value is used for both
#    // source and dest.
#    if (*U == II.getRawDest() && *U == II.getRawSource()) {
#      // For non-volatile transfers this is a no-op.
#      if (!II.isVolatile())
#        return markAsDead(II);
#
#      return insertUse(II, Offset, Size, /*IsSplittable=*/false);
#    }
#
#    // If we have seen both source and destination for a mem transfer, then
#    // they both point to the same alloca.
#    bool Inserted;
#    SmallDenseMap<Instruction *, unsigned>::iterator MTPI;
#    std::tie(MTPI, Inserted) =
#        MemTransferSliceMap.insert(std::make_pair(&II, AS.Slices.size()));
#    unsigned PrevIdx = MTPI->second;
#    if (!Inserted) {
#      Slice &PrevP = AS.Slices[PrevIdx];
#
#      // Check if the begin offsets match and this is a non-volatile transfer.
#      // In that case, we can completely elide the transfer.
#      if (!II.isVolatile() && PrevP.beginOffset() == RawOffset) {
#        PrevP.kill();
#        return markAsDead(II);
#      }
#
#      // Otherwise we have an offset transfer within the same alloca. We can't
#      // split those.
#      PrevP.makeUnsplittable();
#    }
#
#    // Insert the use now that we've fixed up the splittable nature.
#    insertUse(II, Offset, Size, /*IsSplittable=*/Inserted && Length);
#
#    // Check that we ended up with a valid index in the map.
#    assert(AS.Slices[PrevIdx].getUse()->getUser() == &II &&
#           "Map index doesn't point back to a slice with this user.");
#  }
#
#  // Disable SRoA for any intrinsics except for lifetime invariants.
#  // FIXME: What about debug intrinsics? This matches old behavior, but
#  // doesn't make sense.
#  void visitIntrinsicInst(IntrinsicInst &II) {
#    if (II.isDroppable()) {
#      AS.DeadUseIfPromotable.push_back(U);
#      return;
#    }
#
#    if (!IsOffsetKnown)
#      return PI.setAborted(&II);
#
#    if (II.isLifetimeStartOrEnd()) {
#      insertUse(II, Offset, AllocSize, true);
#      return;
#    }
#
#    Base::visitIntrinsicInst(II);
#  }
#
#  Instruction *hasUnsafePHIOrSelectUse(Instruction *Root, uint64_t &Size) {
#    // We consider any PHI or select that results in a direct load or store of
#    // the same offset to be a viable use for slicing purposes. These uses
#    // are considered unsplittable and the size is the maximum loaded or stored
#    // size.
#    SmallPtrSet<Instruction *, 4> Visited;
#    SmallVector<std::pair<Instruction *, Instruction *>, 4> Uses;
#    Visited.insert(Root);
#    Uses.push_back(std::make_pair(cast<Instruction>(*U), Root));
#    const DataLayout &DL = Root->getDataLayout();
#    // If there are no loads or stores, the access is dead. We mark that as
#    // a size zero access.
#    Size = 0;
#    do {
#      Instruction *I, *UsedI;
#      std::tie(UsedI, I) = Uses.pop_back_val();
#
#      if (LoadInst *LI = dyn_cast<LoadInst>(I)) {
#        TypeSize LoadSize = DL.getTypeStoreSize(LI->getType());
#        if (LoadSize.isScalable()) {
#          PI.setAborted(LI);
#          return nullptr;
#        }
#        Size = std::max(Size, LoadSize.getFixedValue());
#        continue;
#      }
#      if (StoreInst *SI = dyn_cast<StoreInst>(I)) {
#        Value *Op = SI->getOperand(0);
#        if (Op == UsedI)
#          return SI;
#        TypeSize StoreSize = DL.getTypeStoreSize(Op->getType());
#        if (StoreSize.isScalable()) {
#          PI.setAborted(SI);
#          return nullptr;
#        }
#        Size = std::max(Size, StoreSize.getFixedValue());
#        continue;
#      }
#
#      if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(I)) {
#        if (!GEP->hasAllZeroIndices())
#          return GEP;
#      } else if (!isa<BitCastInst>(I) && !isa<PHINode>(I) &&
#                 !isa<SelectInst>(I) && !isa<AddrSpaceCastInst>(I)) {
#        return I;
#      }
#
#      for (User *U : I->users())
#        if (Visited.insert(cast<Instruction>(U)).second)
#          Uses.push_back(std::make_pair(I, cast<Instruction>(U)));
#    } while (!Uses.empty());
#
#    return nullptr;
#  }
#
#  void visitPHINodeOrSelectInst(Instruction &I) {
#    assert(isa<PHINode>(I) || isa<SelectInst>(I));
#    if (I.use_empty())
#      return markAsDead(I);
#
#    // If this is a PHI node before a catchswitch, we cannot insert any non-PHI
#    // instructions in this BB, which may be required during rewriting. Bail out
#    // on these cases.
#    if (isa<PHINode>(I) &&
#        I.getParent()->getFirstInsertionPt() == I.getParent()->end())
#      return PI.setAborted(&I);
#
#    // TODO: We could use simplifyInstruction here to fold PHINodes and
#    // SelectInsts. However, doing so requires to change the current
#    // dead-operand-tracking mechanism. For instance, suppose neither loading
#    // from %U nor %other traps. Then "load (select undef, %U, %other)" does not
#    // trap either.  However, if we simply replace %U with undef using the
#    // current dead-operand-tracking mechanism, "load (select undef, undef,
#    // %other)" may trap because the select may return the first operand
#    // "undef".
#    if (Value *Result = foldPHINodeOrSelectInst(I)) {
#      if (Result == *U)
#        // If the result of the constant fold will be the pointer, recurse
#        // through the PHI/select as if we had RAUW'ed it.
#        enqueueUsers(I);
#      else
#        // Otherwise the operand to the PHI/select is dead, and we can replace
#        // it with poison.
#        AS.DeadOperands.push_back(U);
#
#      return;
#    }
#
#    if (!IsOffsetKnown)
#      return PI.setAborted(&I);
#
#    // See if we already have computed info on this node.
#    uint64_t &Size = PHIOrSelectSizes[&I];
#    if (!Size) {
#      // This is a new PHI/Select, check for an unsafe use of it.
#      if (Instruction *UnsafeI = hasUnsafePHIOrSelectUse(&I, Size))
#        return PI.setAborted(UnsafeI);
#    }
#
#    // For PHI and select operands outside the alloca, we can't nuke the entire
#    // phi or select -- the other side might still be relevant, so we special
#    // case them here and use a separate structure to track the operands
#    // themselves which should be replaced with poison.
#    // FIXME: This should instead be escaped in the event we're instrumenting
#    // for address sanitization.
#    if (Offset.uge(AllocSize)) {
#      AS.DeadOperands.push_back(U);
#      return;
#    }
#
#    insertUse(I, Offset, Size);
#  }
#
#  void visitPHINode(PHINode &PN) { visitPHINodeOrSelectInst(PN); }
#
#  void visitSelectInst(SelectInst &SI) { visitPHINodeOrSelectInst(SI); }
#
#  /// Disable SROA entirely if there are unhandled users of the alloca.
#  void visitInstruction(Instruction &I) { PI.setAborted(&I); }
#
#  void visitCallBase(CallBase &CB) {
#    // If the call operand is read-only and only does a read-only or address
#    // capture, then we mark it as EscapedReadOnly.
#    if (CB.isDataOperand(U) &&
#        !capturesFullProvenance(CB.getCaptureInfo(U->getOperandNo())) &&
#        CB.onlyReadsMemory(U->getOperandNo())) {
#      PI.setEscapedReadOnly(&CB);
#      return;
#    }
#
#    Base::visitCallBase(CB);
#  }
# };


@dataclass
class AllocaSlices:
    ai: LocalVarInstr
    pointer_escaping_instr: Instr | None
    pointer_escaping_instr_read_only: Instr | None
    slices: List[Slice]
    dead_users: List[Instr]
    dead_use_if_promotable: List[Use]
    dead_operands: List[Use]

    def __init__(self, ai: LocalVarInstr):
        self.ai = ai
        self.pointer_escaping_instr = None
        self.pointer_escaping_instr_read_only = None
        self.dead_users = []
        self.slices = []
        self.dead_operands = []

        pb = SliceBuilder(ai, self)

        # ptri = pb.visit_ptr(ai)

        # if ptri.is_escaped() or ptri.is_aborted():
        #     self.pointer_escaping_instr = (
        #         ptri.get_escaping_inst()
        #         if ptri.get_escaping_inst()
        #         else ptri.get_aborting_inst()
        #     )
        #     return
        # self.pointer_escaping_instr_read_only = ptri.get_escaped_read_only_inst()

        new_slices = []
        for s in self.slices:
            if not s.is_dead():
                new_slices.append(s)
        self.slices = new_slices

        self.slices.sort()

    def __iter__(self):
        return self.slices.__iter__()

    def is_escaped(self) -> bool:
        return self.pointer_escaping_instr is not None

    def is_escaped_read_only(self) -> bool:
        return self.pointer_escaping_instr_read_only is not None

    # def erase(self, )

    def insert(self, new_slices: List[Slice]):
        self.slices.extend(new_slices)
        self.slices.sort()


@dataclass
class SROA:
    worklist: List[LocalVarInstr]
    dead_insts: List[IrValue]
    post_promotion_worklist: List[LocalVarInstr]
    promotable_allocas: List[LocalVarInstr]
    speculatable_phis: List[PhiInstr]
    # selects_to_rewrite: List[SelectInstr]

    def __init__(self):
        self.worklist = []
        self.dead_insts = []
        self.post_promotion_worklist = []
        self.promotable_allocas = []
        self.speculatable_phis = []

    def run_sroa(self, f: Function) -> bool:
        entry_bb = f.first_block
        for i in entry_bb.instructions:
            if isinstance(i, LocalVarInstr):
                if False:  # promotable ...
                    self.promotable_allocas.append(i)
                else:
                    self.worklist.append(i)

        changed = False
        deleted_allocas = []

        while True:
            while len(self.worklist) > 0:
                changed |= self.run_on_alloca(self.worklist.pop())
                changed |= self.delete_dead_instructions(deleted_allocas)

                if len(deleted_allocas) > 0:
                    ids = [id(a) for a in deleted_allocas]
                    new_worklist = []
                    for a in self.worklist:
                        if id(a) not in ids:
                            new_worklist.append(a)
                    self.worklist = new_worklist
                    new_post_promotion_worklist = []
                    for a in self.post_promotion_worklist:
                        if id(a) not in ids:
                            new_post_promotion_worklist.append(a)
                    self.post_promotion_worklist = new_worklist
                    new_promotable_allocas = []
                    for a in self.promotable_allocas:
                        if id(a) not in ids:
                            new_promotable_allocas.append(a)
                    self.promotable_allocas = new_worklist
                    deleted_allocas = []

            changed |= self.promote_allocas()

            self.worklist = self.post_promotion_worklist
            self.post_promotion_worklist = []

            if len(self.worklist) == 0:
                break

        return changed

    def promote_allocas(self) -> bool:
        if len(self.promotable_allocas) == 0:
            return False
        assert False

        # promote_mem_to_reg(self.promotable_allocas)
        # self.promotable_allocas = []
        # return True

    def run_on_alloca(self, a: LocalVarInstr) -> bool:
        changed = False
        if len(a.uses) == 0:
            a.erase_from_parent()
            changed = True
            return changed

        # IRBuilderTy IRB(&AI);
        # AggLoadStoreRewriter AggRewriter(DL, IRB);
        # Changed |= AggRewriter.rewrite(AI);

        AS = AllocaSlices(a)

        if AS.is_escaped():
            return changed

        if AS.is_escaped_read_only():
            changed |= self.propagate_stored_values_to_loads(a, AS)
            return changed

        for dead_user in AS.dead_users:
            for dead_op in dead_user.operands:
                self.clobber_use(dead_op)

            dead_user.replace_uses(IrPoisonValue())

            self.dead_insts.append(dead_user)
            changed = True

        for dead_op in AS.dead_operands:
            self.clobber_use(dead_op)
            changed = True

        if len(AS.slices) == 0:
            return changed

        changed |= self.split_alloca(a, AS)

        while len(self.speculatable_phis) > 0:
            speculate_phi_node_loads(self.speculatable_phis.pop())

        # auto RemainingSelectsToRewrite = SelectsToRewrite.takeVector();
        # while (!RemainingSelectsToRewrite.empty()) {
        #   const auto [K, V] = RemainingSelectsToRewrite.pop_back_val();
        #   CFGChanged |= rewriteSelectInstMemOps(*K, V, IRB, PreserveCFG ? nullptr : DTU);
        # }

        return changed

    def delete_dead_instructions(self, out: List[LocalVarInstr]) -> bool:
        changed = False

        while len(self.dead_insts) > 0:
            i = self.dead_insts.pop()
            if not isinstance(i, Instr):
                continue

            if isinstance(i, LocalVarInstr):
                out.append(i)

            i.replace_uses(IrUndefValue())

            for o in i.operands:
                if isinstance(o.v, Instr):
                    i = o.v
                    o.remove_use()
                    if i.is_trivially_dead():
                        self.dead_insts.append(i)

            i.erase_from_parent()
            changed = True

        return changed

    # bool presplitLoadsAndStores(AllocaInst &AI, AllocaSlices &AS);
    # AllocaInst *rewritePartition(AllocaInst &AI, AllocaSlices &AS, Partition &P);

    def propagate_stored_values_to_loads(
        self, ai: LocalVarInstr, AS: AllocaSlices
    ) -> bool:
        all_same_and_valid = True
        partition_type = None
        insts = []
        begin_offset = 0
        end_offset = 0

        for s in AS:
            assert s.use
            user = s.use.user
            assert isinstance(user, Instr)
            if s.begin_offset >= end_offset:
                if all_same_and_valid and len(insts) > 0:
                    new_phis = []
                    ssa = SSAUpdater(new_phis)
                    insts.append(ai)
                    promoter = BasicLoadAndStorePromoter(insts, ssa, partition_type)
                    promoter.run(insts)
                all_same_and_valid = True
                partition_type = None
                insts = []
                begin_offset = s.begin_offset
                end_offset = s.end_offset
            elif s.begin_offset != begin_offset or s.end_offset != end_offset:
                if all_same_and_valid:
                    all_same_and_valid = False
                end_offset = max(end_offset, s.end_offset)
                continue

            if isinstance(user, LoadInstr):
                user_ty = user.ty
                if partition_type is not None and user_ty != partition_type:
                    all_same_and_valid = False
                partition_type = user_ty
                insts.append(user)
            elif isinstance(user, StoreInstr):
                user_ty = user.val.ty
                if partition_type is not None and user_ty != partition_type:
                    all_same_and_valid = False
                partition_type = user_ty
                insts.append(user)
            else:
                all_same_and_valid = False

        if all_same_and_valid and len(insts) > 0:
            new_phis = []
            ssa = SSAUpdater(new_phis)
            insts.append(ai)
            promoter = BasicLoadAndStorePromoter(insts, ssa, partition_type)
            promoter.run(insts)
        return True

    def split_alloca(self, ai: LocalVarInstr, AS: AllocaSlices) -> bool:
        return False

    def clobber_use(self, u: Use):
        oldv = u.v
        u.set(IrPoisonValue())

        if isinstance(oldv, Instr):
            if oldv.is_trivially_dead():
                self.dead_insts.append(oldv)


def speculate_phi_node_loads(pn: PhiInstr):
    # TODO:
    pass
