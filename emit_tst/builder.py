from dataclasses import dataclass
from .value import *
from .folder import folder

@dataclass
class Builder:
    insert_block: BasicBlock
    def __init__(self):
        self.insert_block = None

    def insert(self, instr: Instr):
        self.insert_block.instrs.append(instr)

    def insert_v(self, instr: Instr, name: str):
        self.insert_block.instrs.append(instr)
        instr.set_name(name)
        return instr

    def create_br(self, dest: BasicBlock):
        return self.insert(BranchInstr(dest))

    def create_cond_br(self, cond: Value, true_block: BasicBlock, false_block: BasicBlock):
        #MDNode *BranchWeights = nullptr, MDNode *Unpredictable = nullptr) {
        return self.insert(BranchInstr(true_block, false_block, cond)) # addBranchMetadata

    def create_is_not_null(self, arg: Value, name: str = ""):
        return self.create_icmp_ne(arg, ConstantInt.get(0, arg.ty), name)

    def create_store(self, val, addr, is_volatile = False):
        return self.create_aligned_store(val, addr, addr.alignment, is_volatile) # emitRawPointerFromAddress(Addr),

    def create_load(self, addr, name: str = "", volatile = False):
        return self.create_aligned_load(addr.elt_ty, addr, addr.alignment, name, volatile)

    def create_aligned_store(self, val, ptr, align, is_volatile = False):
        if align is None or align <= 0:
            align = val.ty.get_align()
        return self.insert(StoreInstr(val, ptr, is_volatile, align))

    def create_aligned_load(self, ty, ptr, align, name = "", is_volatile = False):
        if align is None or align <= 0:
            align = ty.get_align()
        return self.insert_v(LoadInstr(ty, ptr, "", is_volatile, align), name)

    def create_icmp_ne(self, lhs, rhs, name: str = ""):
        return self.create_icmp(ICmpInstr.NE, lhs, rhs, name)

    def create_icmp(self, p, lhs, rhs, name: str = ""):
        if (v := folder().fold_cmp(p, lhs, rhs)) is not None:
            return v
        return self.insert_v(ICmpInstr(p, lhs, rhs), name)

    def create_insert_nuwnsw_bin_op(self, opc, lhs, rhs, name, has_nuw, has_nsw):
        bo = self.insert_v(BinaryOpInstr(opc, lhs, rhs), name)
        # if has_nuw: bo.set_has_no_unsigned_wrap()
        # if has_nsw: bo.set_has_no_signed_wrap()
        return bo

    def create_add(self, lhs, rhs, label = "", has_nuw = False, has_nsw = False):
        if (v := folder().fold_no_wrap_bin_op(BinaryOpInstr.ADD, lhs, rhs, has_nuw, has_nsw)) is not None:
            return v
        return self.create_insert_nuwnsw_bin_op(BinaryOpInstr.ADD, lhs, rhs, label, has_nuw, has_nsw)

    def create_nsw_add(self, lhs, rhs, label = ""):
        return self.create_add(lhs, rhs, label, False, True)

    def create_nuw_add(self, lhs, rhs, label = ""):
        return self.create_add(lhs, rhs, label, True, False)

    def create_sub(self, lhs, rhs, label = "", has_nuw = False, has_nsw = False):
        if (v := folder().fold_no_wrap_bin_op(BinaryOpInstr.SUB, lhs, rhs, has_nuw, has_nsw)) is not None:
            return v
        return self.create_insert_nuwnsw_bin_op(BinaryOpInstr.SUB, lhs, rhs, label, has_nuw, has_nsw)

    def create_nsw_sub(self, lhs, rhs, label = ""):
        return self.create_sub(lhs, rhs, label, False, True)

    def create_nuw_sub(self, lhs, rhs, label = ""):
        return self.create_sub(lhs, rhs, label, True, False)

    def create_mul(self, lhs, rhs, label = "", has_nuw = False, has_nsw = False):
        if (v := folder().fold_no_wrap_bin_op(BinaryOpInstr.MUL, lhs, rhs, has_nuw, has_nsw)) is not None:
            return v
        return self.create_insert_nuwnsw_bin_op(BinaryOpInstr.MUL, lhs, rhs, label, has_nuw, has_nsw)

    def create_nsw_mul(self, lhs, rhs, label = ""):
        return self.create_mul(lhs, rhs, label, False, True)

    def create_nuw_mul(self, lhs, rhs, label = ""):
        return self.create_mul(lhs, rhs, label, True, False)

    def create_udiv(self, lhs, rhs, label = "", is_exact = False):
        if (v := folder().fold_exact_bin_op(BinaryOpInstr.UDIV, lhs, rhs, is_exact)) is not None:
            return v
        op = BinaryOpInstr(BinaryOpInstr.UDIV, lhs, rhs)
        op.is_exact = is_exact
        return self.insert_v(op, label)

    def create_exact_udiv(self, lhs, rhs, label = ""):
        return self.create_udiv(lhs, rhs, label, True)

    def create_sdiv(self, lhs, rhs, label = "", is_exact = False):
        if (v := folder().fold_exact_bin_op(BinaryOpInstr.SDIV, lhs, rhs, is_exact)) is not None:
            return v
        op = BinaryOpInstr(BinaryOpInstr.SDIV, lhs, rhs)
        op.is_exact = is_exact
        return self.insert_v(op, label)

    def create_exact_sdiv(self, lhs, rhs, label = ""):
        return self.create_sdiv(lhs, rhs, label, True)

    def create_urem(self, lhs, rhs, label = ""):
        if (v := folder().fold_bin_op(BinaryOpInstr.UREM, lhs, rhs)) is not None:
            return v
        return self.insert_v(BinaryOpInstr(BinaryOpInstr.UREM, lhs, rhs), label)

    def create_srem(self, lhs, rhs, label = ""):
        if (v := folder().fold_bin_op(BinaryOpInstr.SREM, lhs, rhs)) is not None:
            return v
        return self.insert_v(BinaryOpInstr(BinaryOpInstr.SREM, lhs, rhs), label)

    def create_shl(self, lhs, rhs, label = "", has_nuw = False, has_nsw = False):
        if (v := folder().fold_no_wrap_bin_op(BinaryOpInstr.SHL, lhs, rhs, has_nuw, has_nsw)) is not None:
            return v
        return self.create_insert_nuwnsw_bin_op(BinaryOpInstr.SHL, lhs, rhs, label, has_nuw, has_nsw)

    def create_nsw_shl(self, lhs, rhs, label = ""):
        return self.create_shl(lhs, rhs, label, False, True)

    def create_nuw_shl(self, lhs, rhs, label = ""):
        return self.create_shl(lhs, rhs, label, True, False)

    def create_lshr(self, lhs, rhs, label = "", is_exact = False):
        if isinstance(rhs, int):
            return create_lshr(lhs, ConstantInt.get(lhs.ty, rhs), label, is_exact)
        if (v := folder().fold_exact_bin_op(BinaryOpInstr.LSHR, lhs, rhs, is_exact)) is not None:
            return v
        op = BinaryOpInstr(BinaryOpInstr.LSHR, lhs, rhs)
        op.is_exact = is_exact
        return self.insert_v(op, label)

    def create_ashr(self, lhs, rhs, label = "", is_exact = False):
        if isinstance(rhs, int):
            return create_ashr(lhs, ConstantInt.get(lhs.ty, rhs), label, is_exact)
        if (v := folder().fold_exact_bin_op(BinaryOpInstr.ASHR, lhs, rhs, is_exact)) is not None:
            return v
        op = BinaryOpInstr(BinaryOpInstr.ASHR, lhs, rhs)
        op.is_exact = is_exact
        return self.insert_v(op, label)

    def create_and(self, lhs, rhs, label = ""):
        if isinstance(rhs, int):
            return create_and(lhs, ConstantInt.get(lhs.ty, rhs), label)
        if (v := folder().fold_bin_op(BinaryOpInstr.AND, lhs, rhs)) is not None:
            return v
        return self.insert_v(BinaryOpInstr(BinaryOpInstr.AND, lhs, rhs), label)

    def create_ands(self, ops):
        assert len(ops) > 0
        accum = ops[0]
        for o in ops[1:]:
            accum = self.create_and(accum, o)
        return accum

    def create_or(self, lhs, rhs, label = ""):
        if isinstance(rhs, int):
            return create_or(lhs, ConstantInt.get(lhs.ty, rhs), label)
        if (v := folder().fold_bin_op(BinaryOpInstr.OR, lhs, rhs)) is not None:
            return v
        return self.insert_v(BinaryOpInstr(BinaryOpInstr.OR, lhs, rhs), label)

    def create_ors(self, ops):
        assert len(ops) > 0
        accum = ops[0]
        for o in ops[1:]:
            accum = self.create_or(accum, o)
        return accum

    def create_xor(self, lhs, rhs, label = ""):
        if isinstance(rhs, int):
            return create_xor(lhs, ConstantInt.get(lhs.ty, rhs), label)
        if (v := folder().fold_bin_op(BinaryOpInstr.XOR, lhs, rhs)) is not None:
            return v
        return self.insert_v(BinaryOpInstr(BinaryOpInstr.XOR, lhs, rhs), label)

    def get_int(self, value: int):
        return ConstantInt.get(value)

THE_BUILDER = Builder()

def builder():
    return THE_BUILDER
