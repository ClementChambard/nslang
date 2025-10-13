from dataclasses import dataclass
from typing import List

from ir2.data.instrs.other import IcmpInstr, PhiInstr
from ir2.data.instrs.term import BranchInstr, RetInstr, TermInstr, UnreachableInstr
from ir2.data.prog import Block, Function
from ir2.data.value import ConstantInt
from ir2.optim.passes import FunctionPass
from ir2.optim.util.basic_block import delete_dead_block, merge_block_into_predecessor
from ir2.optim.util.local import (
    constant_fold_terminator,
    eliminate_duplicate_phi_nodes,
    remove_unreachable_blocks,
    try_to_simplify_uncond_branch_from_empty_block,
)


@dataclass
class SimplifycfgPass(FunctionPass):
    def run_on_function(self, f: Function) -> bool:
        changed = False
        changed |= remove_unreachable_blocks(f)
        changed |= tail_merge_blocks_with_similar_function_terminators(f)
        changed |= self.simplify_cfg(f)

        if not changed:
            return False

        if not remove_unreachable_blocks(f):
            return True

        while changed:
            changed = self.simplify_cfg(f)
            changed |= remove_unreachable_blocks(f)

        return True

    def simplify_cfg(self, f: Function) -> bool:
        changed = False
        local_change = True
        edges = f.get_backedges()
        unique_loop_headers = set()
        loop_headers = []
        for _, b in edges:
            if id(b) not in unique_loop_headers:
                unique_loop_headers.add(id(b))
                loop_headers.append(b)
        iter_cnt = 0
        while local_change:
            iter_cnt += 1
            assert iter_cnt < 1000, "SimplifycfgPass -> didn't converge"
            local_change = False
            for bb in f:
                local_change |= simplify_cfg(bb, loop_headers)
            changed |= local_change
        return changed


def tail_merge_blocks_with_similar_function_terminators(f: Function) -> bool:
    structure = dict()

    for bb in f:
        if not bb.is_terminated:
            continue
        if len(bb.successors) > 0:
            continue
        term = bb.get_terminator()
        assert term is not None
        if not isinstance(term, RetInstr):  # other kinds of terminators
            continue
        k = term.__class__.__name__.lower().removesuffix("instr")
        if k not in structure.keys():
            structure[k] = []
        structure[k].append(bb)

    changed = False
    for bbs in structure.values():
        changed |= perform_block_tail_merging(f, bbs)

    return changed


def perform_block_tail_merging(f: Function, bbs: List[Block]) -> bool:
    if len(bbs) < 2:
        return False
    new_ops = []
    term = bbs[0].get_terminator()
    assert term
    canonical_bb = Block(
        "common." + term.__class__.__name__.lower().removesuffix("instr"), f
    )
    f.blocks.append(canonical_bb)
    assert canonical_bb.name
    canonical_term = term.clone()

    for i, op in enumerate(term.operands):
        new_op = PhiInstr(canonical_bb.name + ".op" + str(i), op.v.ty)
        new_op.parent = canonical_bb
        canonical_bb.instructions.append(new_op)
        new_ops.append(new_op)

    canonical_term.parent = canonical_bb
    canonical_bb.instructions.append(canonical_term)

    for i in range(len(new_ops)):
        canonical_term.operands[i].set(new_ops[i])

    for bb in bbs:
        term = bb.get_terminator()
        assert term and isinstance(term, canonical_term.__class__)

        for t_o, n_o in zip(term.operands, new_ops):
            n_o.add_incoming(t_o.v, bb)

        term.erase_from_parent()

        # FUNCTION ?
        bi = BranchInstr(canonical_bb)
        bi.parent = bb
        bb.instructions.append(bi)

    return True


@dataclass
class SimplifyCfgState:
    resimplify: bool
    loop_headers: List[Block]

    def request_resimplify(self) -> bool:
        self.resimplify = True
        return True


def simplify_once(bb: Block, state: SimplifyCfgState) -> bool:
    changed = False
    bb_term = bb.get_terminator()
    assert bb_term
    assert bb.parent

    if (
        len(bb.get_predecessors()) == 0 and bb is not bb.parent.first_block
    ) or bb.get_single_predecessor() is bb:
        delete_dead_block(bb)
        return True

    changed |= constant_fold_terminator(bb, True)

    changed |= eliminate_duplicate_phi_nodes(bb)

    # => no undef value yet
    # if remove_undef_introducing_predecessor(bb): return state.request_resimplify()

    if merge_block_into_predecessor(bb):
        return True

    if sink_common_code_from_predecessors(bb):
        return True

    if isinstance(bb.instructions[0], PhiInstr):
        if bb.instructions[0].num_incoming == 2:
            if fold_two_entry_phi_node(bb.instructions[0]):
                return True

    bb_term = bb.get_terminator()
    assert bb_term
    assert bb_term.parent
    # assert bb_term.parent

    if isinstance(bb_term, BranchInstr):
        changed |= simplify_branch(bb_term, state)
    if isinstance(bb_term, UnreachableInstr):
        changed |= simplify_unreachable(bb_term)
    # TODO: other terminators

    return changed


def simplify_branch(branch: BranchInstr, state: SimplifyCfgState) -> bool:
    if branch.is_conditional:
        return simplify_cond_branch(branch)
    else:
        return simplify_uncond_branch(branch, state)


def simplify_uncond_branch(bi: BranchInstr, state: SimplifyCfgState) -> bool:
    assert not bi.is_conditional
    bb = bi.parent
    assert bb
    succ = bi.true_block
    need_canonical_loop = (
        len(state.loop_headers) > 0
        and len(bb.get_predecessors()) >= 2
        and (
            id(bb) in [id(a) for a in state.loop_headers]
            or id(succ) in [id(a) for a in state.loop_headers]
        )
    )

    non_phis = bb.non_phis_instrs()
    if (
        isinstance(non_phis[0], TermInstr)
        and bb is not bb.parent.first_block
        and not need_canonical_loop
    ):
        if try_to_simplify_uncond_branch_from_empty_block(bb):
            return True

    if isinstance(non_phis[0], IcmpInstr):
        ici = non_phis[0]
        assert isinstance(ici, IcmpInstr)
        if ici.cond.is_equality() and isinstance(ici.operands[1].v, ConstantInt):
            if isinstance(non_phis[1], TermInstr):
                if try_to_simplify_uncond_branch_with_icmp_in_it(ici):
                    return True

    if fold_branch_to_common_dest(bi):
        return state.request_resimplify()

    return False


def try_to_simplify_uncond_branch_with_icmp_in_it(ici: IcmpInstr) -> bool:
    bb = ici.parent
    assert bb
    if isinstance(bb.instructions[0], PhiInstr) or len(ici.uses) != 1:
        return False

    v = ici.operands[0].v
    cst = ici.operands[1].v
    assert isinstance(cst, ConstantInt)

    pred = bb.get_single_predecessor()
    if not pred or True:  # switch instr required
        return False


def fold_branch_to_common_dest(bi: BranchInstr) -> bool:
    return False


def simplify_cond_branch(branch: BranchInstr) -> bool:
    assert branch.is_conditional
    return False


def simplify_unreachable(a):
    # remove previous instructions (they should be are unreachable)
    # for each pred:
    #  if successors are all bb: pred is unreachable too
    #  else branch to other successor
    return False


def fold_two_entry_phi_node(pn: PhiInstr) -> bool:
    bb = pn.parent
    assert bb
    return False


def sink_common_code_from_predecessors(bb: Block) -> bool:
    have_non_unconditional_predecessors = False
    unconditional_preds = []
    for pred_bb in bb.get_predecessors():
        pred_br = pred_bb.get_terminator()
        if isinstance(pred_br, BranchInstr) and not pred_br.is_conditional:
            unconditional_preds.append(pred_bb)
        else:
            have_non_unconditional_predecessors = True
    if len(unconditional_preds) < 2:
        return False

    return False  # TODO:


def simplify_cfg(bb: Block, loop_headers: List[Block]) -> bool:
    changed = False
    state = SimplifyCfgState(True, loop_headers)
    while state.resimplify:
        state.resimplify = False
        changed = simplify_once(bb, state)
    return changed


# Removes basic blocks with no predecessors.
#
# Merges a basic block into its predecessor if there is only one and the predecessor only has one successor.
#
# Eliminates PHI nodes for basic blocks with a single predecessor.
#
# Eliminates a basic block that only contains an unconditional branch.
