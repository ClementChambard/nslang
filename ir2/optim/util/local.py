from typing import Set, Tuple
from typing_extensions import List
from ir2.data.instrs._base import Instr
from ir2.data.instrs.other import PhiInstr
from ir2.data.instrs.term import BranchInstr, UnreachableInstr
from ir2.data.prog import Block, Function
from ir2.data.value import ConstantInt, IrValue
from ir2.optim.util.basic_block import delete_dead_block, delete_dead_blocks


def recursively_delete_trivially_dead_instructions(
    v: IrValue, tli: None, mssau: None = None, atdc: None = None
) -> bool:
    if not isinstance(v, Instr):
        return False
    # TODO: ...
    return False


def eliminate_duplicate_phi_nodes_nr(bb: Block, to_remove: List[PhiInstr]) -> bool:
    if len(bb.phis()) < 32:
        return eliminate_duplicate_phi_nodes_naive_impl(bb, to_remove)
    return eliminate_duplicate_phi_nodes_set_based_impl(bb, to_remove)


def eliminate_duplicate_phi_nodes_naive_impl(
    bb: Block, to_remove: List[PhiInstr]
) -> bool:
    changed = False
    phis = bb.phis()
    remove_set = set()
    i = 0
    while i < len(phis):
        pn = phis[i]
        i += 1
        j = i
        while j < len(phis):
            duplicate_pn = phis[j]
            if id(duplicate_pn) in remove_set:
                j += 1
                continue
            if not duplicate_pn.is_identical_to_when_defined(pn):
                j += 1
                continue
            duplicate_pn.replace_uses(pn)
            remove_set.add(duplicate_pn)
            changed = True
            i = 0
            break

    [to_remove.append(p) for p in remove_set]

    return changed


def eliminate_duplicate_phi_nodes_set_based_impl(bb, to_remove) -> bool:
    # TODO:
    return eliminate_duplicate_phi_nodes_naive_impl(bb, to_remove)


def eliminate_duplicate_phi_nodes(bb: Block):
    to_remove = []
    changed = eliminate_duplicate_phi_nodes_nr(bb, to_remove)
    for pn in to_remove:
        pn.erase_from_parent()
    return changed


def constant_fold_terminator(bb: Block, delete_dead_conditions: bool):
    t = bb.get_terminator()
    assert t

    if isinstance(t, BranchInstr):
        if not t.is_conditional:
            return False

        dest1 = t.true_block
        dest2 = t.false_block

        if dest1 is dest2:
            # dest1.removePredecessor(t.parent)
            new_bi = BranchInstr(dest1)
            new_bi.parent = bb
            bb.instructions.append(new_bi)
            cond = t.cond
            t.erase_from_parent()
            if delete_dead_conditions:
                recursively_delete_trivially_dead_instructions(cond, None)
            return True

        if isinstance(t.cond, ConstantInt):
            destination = (dest2, dest1)[t.cond.val != 0]
            # old_dest = (dest1, dest2)[t.cond.val != 0]
            # old_dest.remove_predecessor(t.parent)
            new_bi = BranchInstr(destination)
            new_bi.parent = bb
            bb.instructions.append(new_bi)
            t.erase_from_parent()
            return True

        return False

    # switch, indirectbr
    return False


def mark_alive_blocks(f: Function, reachable: Set[int]) -> bool:
    worklist: List[Block] = []
    bb = f.first_block
    worklist.append(bb)
    reachable.add(id(bb))
    changed = False
    while len(worklist) > 0:
        bb = worklist.pop()

        terminator = bb.get_terminator()
        assert terminator is not None

        changed |= constant_fold_terminator(bb, True)

        for successor in bb.successors:
            if id(successor) not in reachable:
                reachable.add(id(successor))
                worklist.append(successor)

    return changed


def remove_unreachable_blocks(f: Function) -> bool:
    reachable = set()
    changed = mark_alive_blocks(f, reachable)

    if len(reachable) == len(f.blocks):
        return changed

    assert len(reachable) < len(f.blocks)

    blocks_to_remove = []
    for bb in f:
        if id(bb) in reachable:
            continue
        blocks_to_remove.append(bb)

    if len(blocks_to_remove) == 0:
        return changed

    changed = True

    delete_dead_blocks(blocks_to_remove)

    return changed


def can_propagate_predecessors_for_phis(
    bb: Block, succ: Block, bbpreds: List[Block]
) -> bool:
    if succ.get_single_predecessor():
        return True

    # for i in succ.phis():
    #    pn = i
    # for (BasicBlock::iterator I = Succ->begin(); isa<PHINode>(I); ++I) {
    #   PHINode *PN = cast<PHINode>(I);
    #
    #   // If the incoming value from BB is again a PHINode in
    #   // BB which has the same incoming value for *PI as PN does, we can
    #   // merge the phi nodes and then the blocks can still be merged
    #   PHINode *BBPN = dyn_cast<PHINode>(PN->getIncomingValueForBlock(BB));
    #   if (BBPN && BBPN->getParent() == BB) {
    #     for (unsigned PI = 0, PE = PN->getNumIncomingValues(); PI != PE; ++PI) {
    #       BasicBlock *IBB = PN->getIncomingBlock(PI);
    #       if (BBPreds.count(IBB) && !CanMergeValues(BBPN->getIncomingValueForBlock(IBB), PN->getIncomingValue(PI))) {
    #         return false;
    #       }
    #     }
    #   } else {
    #     Value* Val = PN->getIncomingValueForBlock(BB);
    #     for (unsigned PI = 0, PE = PN->getNumIncomingValues(); PI != PE; ++PI) {
    #       // See if the incoming value for the common predecessor is equal to the
    #       // one for BB, in which case this phi node will not prevent the merging
    #       // of the block.
    #       BasicBlock *IBB = PN->getIncomingBlock(PI);
    #       if (BBPreds.count(IBB) &&
    #           !CanMergeValues(Val, PN->getIncomingValue(PI))) {
    #         return false;
    #       }
    #     }
    #   }
    # }

    return False


def introduce_too_many_phi_entries(bb, succ) -> bool:
    # TODO:
    return False


def can_redirect_preds_of_empty_bb_to_succ(
    bb, succ, bbpreds
) -> Tuple[Block | None, bool]:
    if len(bb.phis()) == 0 or len(succ.phis()) == 0:
        return None, False

    if len(bb.get_predecessors()) < 2:
        return None, False

    return None, False  # TODO:


def redirect_values_from_predecessors_to_phi(
    bb: Block, bbpreds: List[Block], pn: PhiInstr, common_pred: Block | None
):
    return
    old_val = pn.remove_incoming_value(bb, False)
    #
    # IncomingValueMap IncomingValues;
    #
    # gatherIncomingValuesToPhi(PN, IncomingValues);
    #
    # if (isa<PHINode>(OldVal) && cast<PHINode>(OldVal)->getParent() == BB) {
    #   PHINode *OldValPN = cast<PHINode>(OldVal);
    #   for (unsigned i = 0, e = OldValPN->getNumIncomingValues(); i != e; ++i) {
    #     BasicBlock *PredBB = OldValPN->getIncomingBlock(i);
    #
    #     if (PredBB == CommonPred)
    #       continue;
    #
    #     Value *PredVal = OldValPN->getIncomingValue(i);
    #     Value *Selected =
    #         selectIncomingValueForBlock(PredVal, PredBB, IncomingValues);
    #
    #     // And add a new incoming value for this predecessor for the
    #     // newly retargeted branch.
    #     PN->addIncoming(Selected, PredBB);
    #   }
    #   if (CommonPred)
    #     PN->addIncoming(OldValPN->getIncomingValueForBlock(CommonPred), BB);
    #
    # } else {
    #   for (BasicBlock *PredBB : BBPreds) {
    #     // Update existing incoming values in PN for this
    #     // predecessor of BB.
    #     if (PredBB == CommonPred)
    #       continue;
    #
    #     Value *Selected =
    #         selectIncomingValueForBlock(OldVal, PredBB, IncomingValues);
    #
    #     // And add a new incoming value for this predecessor for the
    #     // newly retargeted branch.
    #     PN->addIncoming(Selected, PredBB);
    #   }
    #   if (CommonPred)
    #     PN->addIncoming(OldVal, BB);
    # }
    #
    # replaceUndefValuesInPhi(PN, IncomingValues);


def try_to_simplify_uncond_branch_from_empty_block(bb: Block) -> bool:
    # my impl: propably wrong with phi instrs

    assert bb is not bb.parent.first_block
    term = bb.get_terminator()
    assert isinstance(term, BranchInstr)
    if bb.instructions[0] is not term:
        return False

    succ = term.true_block

    # TODO: WRONG: uses in phi nodes should be replaced by predecessor
    pred = bb.get_single_predecessor()
    if not pred:
        return False

    uses = [u for u in bb.uses]
    for u in uses:
        if isinstance(u.user, PhiInstr):
            u.set(pred)

    bb.replace_uses(succ)

    bb.instructions[-1].erase_from_parent()
    i = UnreachableInstr()
    i.parent = bb
    bb.instructions.append(i)

    delete_dead_block(bb)

    return True

    # assert bb is not bb.parent.first_block
    # term = bb.get_terminator()
    # assert isinstance(term, BranchInstr)
    # succ = term.true_block
    # if bb is succ:
    #     return False

    # bbpreds = bb.get_predecessors()
    # bbkillable = can_propagate_predecessors_for_phis(bb, succ, bbpreds)
    # common_pred, crp = can_redirect_preds_of_empty_bb_to_succ(bb, succ, bbpreds)
    # bbphismergeable = bbkillable or crp

    # if (not bbkillable and not bbphismergeable) or introduce_too_many_phi_entries(
    #     bb, succ
    # ):
    #     return False

    # if not succ.get_single_predecessor():
    #     for bbi in bb.phis():
    #         for u in bbi.uses:
    #             i = 0  # TODO:
    #             pn = u.user
    #             if isinstance(pn, PhiInstr):
    #                 if pn.coerce_blocks[i] is not bb:
    #                     return False
    #             else:
    #                 return False

    # if isinstance(succ.instructions[0], PhiInstr):
    #     bbpreds2 = bb.get_predecessors()
    #     for pn in succ.phis():
    #         redirect_values_from_predecessors_to_phi(bb, bbpreds2, pn, common_pred)

    # if succ.get_single_predecessor():
    #     term.erase_from_parent()
    #     # FUNCTION
    #     for i in bb.instructions:
    #         i.parent = succ
    #     succ.instructions = succ.phis() + bb.instructions + succ.non_phis_instrs()
    # else:
    #     for pn in bb.phis():
    #         assert len(pn.uses) == 0
    #         pn.erase_from_parent()

    # if bbkillable:
    #     bb.replace_uses(succ)

    #     if not succ.name:
    #         succ.name = bb.name

    #     if bb.get_terminator():
    #         bb.instructions[-1].erase_from_parent()

    #     i = UnreachableInstr()
    #     i.parent = bb
    #     bb.instructions.append(i)
    # elif bbphismergeable:
    #     assert False  # TODO:

    # if bbkillable:
    #     delete_dead_block(bb)

    # return True
