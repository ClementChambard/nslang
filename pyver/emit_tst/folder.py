from dataclasses import dataclass
from .value import *

@dataclass
class Folder:
    def fold_bin_op_common(self, lhs, rhs):
        if not (lhs.is_constant() and rhs.is_constant()):
            return False, None, None, None
        # TODO: constant kind (for now Int)
        return True, ConstantInt, lhs.get_constant_value(), rhs.get_constant_value()

    def fold_cmp(self, opc, lhs, rhs):
        b, _, lhs, rhs = self.fold_bin_op_common(lhs, rhs)
        if not b: return None
        res = 0
        match opc:
            case ICmpInstr.ICMP_NE:
                res = int(lhs != rhs)
            case _:
                assert False
        return ConstantInt.get(res, TYPES["bool"])

    def fold_exact_bin_op(self, opc, lhs, rhs, is_exact):
        b, kind, lhs, rhs = self.fold_bin_op_common(lhs, rhs)
        if not b: return None
        res = 0
        match opc:
            case BinaryOpInstr.SDIV | BinaryOpInstr.UDIV:
                res = lhs // rhs
            case BinaryOpInstr.LSHR | BinaryOpInstr.ASHR:
                res = lhs >> rhs
            case _:
                assert False, "TODO:"
        return kind.get(res)

    def fold_bin_op(self, opc, lhs, rhs):
        b, kind, lhs, rhs = self.fold_bin_op_common(lhs, rhs)
        if not b: return None
        res = 0
        match opc:
            case BinaryOpInstr.UREM | BinaryOpInstr.SREM:
                res = lhs % rhs
            case BinaryOpInstr.AND:
                res = lhs & rhs
            case BinaryOpInstr.OR:
                res = lhs | rhs
            case BinaryOpInstr.XOR:
                res = lhs ^ rhs
            case _:
                assert False, "TODO:"
        return kind.get(res)

    def fold_no_wrap_bin_op(self, opc, lhs, rhs, has_nuw, has_nsw):
        b, kind, lhs, rhs = self.fold_bin_op_common(lhs, rhs)
        if not b: return None
        res = 0
        match opc:
            case BinaryOpInstr.SUB:
                res = lhs - rhs
            case BinaryOpInstr.ADD:
                res = lhs + rhs
            case BinaryOpInstr.MUL:
                res = lhs * rhs
            case BinaryOpInstr.SHL:
                res = lhs << rhs
            case _:
                assert False, "TODO:"
        return kind.get(res)

THE_FOLDER = Folder()
def folder():
    return THE_FOLDER
