from typing import List
from ir2.data.instrs.other import PhiInstr
from ir2.data.instrs.term import UnreachableInstr
from ir2.data.prog import Block
from ir2.data.value import IrPoisonValue


def delete_dead_block(bb: Block, keep_one_input_phis: bool = False):
    delete_dead_blocks([bb], keep_one_input_phis)


def delete_dead_blocks(bbs: List[Block], keep_one_input_phis: bool = False):
    detach_dead_blocks(bbs, keep_one_input_phis)
    for bb in bbs:
        bb.erase_from_parent()


def detach_dead_blocks(bbs: List[Block], keep_one_input_phis: bool = False):
    for bb in bbs:
        empty_and_detach_block(bb, keep_one_input_phis)


def empty_and_detach_block(bb: Block, keep_one_input_phis: bool = False):
    # for succ in bb.successors:
    #     succ.remove_predecessor(bb, keep_one_input_phis)

    while len(bb.instructions) > 0:
        i = bb.instructions[-1]
        if len(i.uses) > 0:
            i.replace_uses(IrPoisonValue())  # poison values have type ?
        i.erase_from_parent()

    i = UnreachableInstr()
    i.parent = bb
    bb.instructions.append(i)


def merge_block_into_predecessor(bb: Block) -> bool:
    # TODO: arg: PredecessorWithTwoSuccessors was removed.
    pred_bb = bb.get_single_predecessor()
    if pred_bb is None:
        return False
    if pred_bb is bb:
        return False
    pti = pred_bb.get_terminator()
    assert pti is not None
    if len(pred_bb.successors) != 1 or pred_bb.successors[0] is not bb:
        return False

    for pn in bb.phis():
        if id(pn) in [id(i) for i in pn.coerce_vals]:
            return False

    incoming_values = []
    if len(bb.phis()) > 0:
        for pn in bb.phis():
            if (
                not isinstance(pn.coerce_vals[0], PhiInstr)
                or pn.coerce_vals[0].parent is not bb
            ):
                incoming_values.append(pn.coerce_vals[0])
        fold_single_entry_phi_nodes(bb)

    sti = bb.get_terminator()
    assert sti

    # FUNC FOR THIS ?
    for i in bb.instructions[:-1]:
        i.parent = pred_bb
        pred_bb.instructions.insert(-1, i)

    bb.replace_uses(pred_bb)

    pred_bb.instructions[-1].erase_from_parent()

    # FUNC FOR THIS ?
    for i in range(len(bb.instructions)):
        if bb.instructions[i] is sti:
            bb.instructions.pop(i)
            break
    sti.parent = pred_bb
    pred_bb.instructions.append(sti)  # FUNC FOR THIS ?

    i = UnreachableInstr()
    i.parent = bb
    bb.instructions.append(i)

    if pred_bb.name is None:
        pred_bb.name = bb.name

    delete_dead_block(bb)

    return True


def fold_single_entry_phi_nodes(bb: Block) -> bool:
    phis = bb.phis()
    if len(phis) == 0:
        return False

    for pn in phis:
        if pn.coerce_vals[0] is not pn:
            pn.replace_uses(pn.coerce_vals[0])
        else:
            pn.replace_uses(IrPoisonValue())  # type
        pn.erase_from_parent()

    return True
