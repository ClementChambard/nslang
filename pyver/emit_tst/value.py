from dataclasses import dataclass
import ns_ast.nodes as AST
from semantic_analysis import TYPES
import enum

def ty_str(ty) -> str:
    if ty is None or ty == AST.Type():
        return "void"
    elif isinstance(ty, AST.PointerType):
        return "ptr"
    elif isinstance(ty, AST.BuiltinType):
        if ty.kind == AST.BuiltinTypeKind.BOOL:
            return "i1"
        return f"{ty}"
    elif isinstance(ty, AST.FunctionType):
        return f"{ty_str(ty.return_type)} {args_str(ty)}"
    elif isinstance(ty, AST.ArrayType):
        return f"[{ty.count} x {ty_str(ty.subtype)}]"
    else:
        assert False

def args_str(ty) -> str:
    param_str = ""
    comma = False
    for a in ty.param_types:
        if comma:
            param_str += ", "
        param_str += ty_str(a)
        comma = True
    if ty.is_variadic:
        if comma:
            param_str += ", "
        param_str += "..."
    return f"({param_str})"

@dataclass
class Value:
    parent: None
    ty: AST.Type
    name: str

    def __init__(self, name = None, ty = AST.Type(), parent: "Value" = None):
        self.parent = parent
        self.ty = ty
        if name is not None:
            self.set_name(name)

    def ref_str(self):
        return f"{ty_str(self.ty)} %{self.name}"

    def ref_noty_str(self):
        return f"%{self.name}"

    def set_name(self, name):
        self.name = Value.make_label(name)

    def is_constant(self) -> bool:
        return False

    def __str__(self):
        return self.ref_str()

    CREATED_LABELS = []
    LABEL_ID = 0
    @staticmethod
    def make_label(lbl: str) -> str:
        if lbl in Value.CREATED_LABELS:
            Value.LABEL_ID += 1
            lbl += str(Value.LABEL_ID)
        Value.CREATED_LABELS.append(lbl)
        return lbl


@dataclass
class Function(Value):
    blocks: "[BasicBlock]"
    params: [Value]

    def __init__(self, name, ty, params):
        super().__init__(name, ty)
        self.blocks = []
        self.params = params

    def insert(self, b):
        self.blocks.append(b)

    def __str__(self):
        arg_strs = ""
        for i in range(len(self.params)):
            if i != 0:
                arg_strs += ", "
            arg_strs += f"{self.params[i].ref_str()}"
        # TODO: VARARG
        return f"define {ty_str(self.ty.return_type)} @{self.name}({arg_strs}) {{\n" + "\n".join([f"{b}" for b in self.blocks]) + "\n}\n"


@dataclass
class BasicBlock(Value):
    instrs: "[Instr]"

    def __init__(self, name = "", parent = None, insert_before = None): # ctx
        super().__init__(name, parent = parent)
        self.instrs = []
        self.parent = None

    def get_terminator(self):
        a = self.instrs[-1] if len(self.instrs) > 0 else None
        if a is None:
            return None
        if isinstance(a, BranchInstr) or isinstance(a, RetInstr):
            return a
        else:
            return None

    def ref_str(self):
        return f"%{self.name}"

    def __str__(self):
        return f"{self.name}:\n" + "\n".join([f"{i}" for i in self.instrs])


@dataclass
class Instr(Value):
    def __init__(self, name: str = "", ty: AST.Type = AST.Type(), parent: Value = None):
        from .builder import builder
        if parent is None:
            parent = builder().insert_block
        super().__init__(name, ty, parent)

    def __str__(self):
        return f"    %{self.name} = nop"


@dataclass
class AllocaInstr(Instr):
    elt_ty: AST.Type
    array_size: int
    alignment: int
    def __init__(self, ty, array_size, name):
        super().__init__(name, AST.PointerType(ty))
        self.elt_ty = ty
        self.array_size = array_size
        self.alignment = ty.get_align()

    def __str__(self):
        opt = ""
        if self.alignment > 0:
            opt += f", align {self.alignment}"
        return f"    %{self.name} = alloca {ty_str(self.ty)}{opt}"


@dataclass
class TXTInstr(Instr):
    txt: str

    def __init__(self, txt):
        super().__init__()
        self.txt = txt

    def __str__(self):
        return self.txt


@dataclass
class StoreInstr(Instr):
    val: Value
    ptr: Value
    volatile: bool
    align: int

    def __init__(self, val, ptr, volatile, align):
        super().__init__()
        self.val = val
        self.ptr = ptr
        self.volatile = volatile
        self.align = align

    def __str__(self):
        return f"    store {self.val.ref_str()}, {self.ptr.ref_str()}, align {self.align}"

@dataclass
class LoadInstr(Instr):
    ptr: Value
    volatile: bool
    align: int

    def __init__(self, ty, ptr, name, volatile, align):
        super().__init__(name, ty)
        self.ptr = ptr
        self.volatile = volatile
        self.align = align

    def __str__(self):
        rs = ""
        if self.name is not None:
            rs = f"%{self.name} = "
        return f"    {rs}load {ty_str(self.ty)}, {self.ptr.ref_str()}, align {self.align}"

@dataclass
class ICmpInstr(Instr):
    NE = enum.auto()
    UNE = enum.auto()
    EQ = enum.auto()
    OEQ = enum.auto()
    UGE = enum.auto()
    SGE = enum.auto()
    OGE = enum.auto()
    ULE = enum.auto()
    SLE = enum.auto()
    OLE = enum.auto()
    UGT = enum.auto()
    SGT = enum.auto()
    OGT = enum.auto()
    ULT = enum.auto()
    SLT = enum.auto()
    OLT = enum.auto()

    op: int
    lhs: Value
    rhs: Value

    def __init__(self, opc, lhs, rhs):
        super().__init__("", TYPES["bool"])
        self.op = opc
        self.lhs = lhs
        self.rhs = rhs

    def __str__(self) -> str:
        rs = ""
        if self.name is not None:
            rs = f"%{self.name} = "
        op = "nop"
        if self.op == ICmpInstr.NE:
            op = "ne"
        elif self.op == ICmpInstr.UNE:
            op = "une"
        elif self.op == ICmpInstr.EQ:
            op = "eq"
        elif self.op == ICmpInstr.OEQ:
            op = "oeq"
        elif self.op == ICmpInstr.UGE:
            op = "uge"
        elif self.op == ICmpInstr.SGE:
            op = "sge"
        elif self.op == ICmpInstr.OGE:
            op = "oge"
        elif self.op == ICmpInstr.ULE:
            op = "ule"
        elif self.op == ICmpInstr.SLE:
            op = "sle"
        elif self.op == ICmpInstr.OLE:
            op = "ole"
        elif self.op == ICmpInstr.UGT:
            op = "ugt"
        elif self.op == ICmpInstr.SGT:
            op = "sgt"
        elif self.op == ICmpInstr.OGT:
            op = "ogt"
        elif self.op == ICmpInstr.ULT:
            op = "ult"
        elif self.op == ICmpInstr.SLT:
            op = "slt"
        elif self.op == ICmpInstr.OLT:
            op = "olt"
        return f"    {rs}icmp {op} {ty_str(self.lhs.ty)} {self.lhs.ref_noty_str()}, {self.rhs.ref_noty_str()}"


@dataclass
class BinaryOpInstr(Instr):
    MUL = enum.auto()
    UDIV = enum.auto()
    SDIV = enum.auto()
    UREM = enum.auto()
    SREM = enum.auto()
    ADD = enum.auto()
    SUB = enum.auto()
    SHL = enum.auto()
    LSHR = enum.auto()
    ASHR = enum.auto()
    AND = enum.auto()
    OR = enum.auto()
    XOR = enum.auto()

    op: int
    lhs: Value
    rhs: Value
    is_exact: bool

    def __init__(self, opc, lhs, rhs):
        super().__init__(None, lhs.ty)
        self.op = opc
        self.lhs = lhs
        self.rhs = rhs
        self.is_exact = False

    def __str__(self) -> str:
        rs = ""
        if self.name is not None:
            rs = f"%{self.name} = "
        op = ""
        if self.op == BinaryOpInstr.ADD:
            op = "add"
        elif self.op == BinaryOpInstr.SUB:
            op = "sub"
        elif self.op == BinaryOpInstr.MUL:
            op = "mul"
        elif self.op == BinaryOpInstr.UDIV:
            op = "udiv"
        elif self.op == BinaryOpInstr.SDIV:
            op = "sdiv"
        elif self.op == BinaryOpInstr.UREM:
            op = "urem"
        elif self.op == BinaryOpInstr.SREM:
            op = "srem"
        elif self.op == BinaryOpInstr.SHL:
            op = "shl"
        elif self.op == BinaryOpInstr.LSHR:
            op = "lshr"
        elif self.op == BinaryOpInstr.ASHR:
            op = "ashr"
        elif self.op == BinaryOpInstr.AND:
            op = "and"
        elif self.op == BinaryOpInstr.XOR:
            op = "xor"
        elif self.op == BinaryOpInstr.OR:
            op = "or"
        else:
            assert False
        return f"    {rs}{op} {ty_str(self.ty)} {self.lhs.ref_noty_str()}, {self.rhs.ref_noty_str()}"


@dataclass
class CallInstr(Instr):
    fn_name: str
    params: [Value]

    def __init__(self, ty, fn_name, params):
        super().__init__("", ty)
        self.fn_name = fn_name
        self.params = params

    def __str__(self) -> str:
        rs = ""
        if not AST.type_is_void(self.ty.return_type):
            rs = f"%{self.name} = "
        arg_str = ""
        for (i, a) in enumerate(self.params):
            if i != 0:
                arg_str += ", "
            arg_str += f"{a.ref_str()}"
        return f"    {rs}call {ty_str(self.ty)} @{self.fn_name}({arg_str})"


@dataclass
class BranchInstr(Instr):
    dest: BasicBlock
    dest_false: BasicBlock | None
    cond: Value
    def __init__(self, dest: BasicBlock, dest_false: BasicBlock | None = None, expr: Value = None):
        super().__init__()
        self.dest = dest
        self.dest_false = dest_false
        self.cond = expr

    def __str__(self) -> str:
        if self.is_unconditional():
            return f"    br label {self.dest.ref_str()}"
        else:
            return f"    br {self.cond.ref_str()}, label {self.dest.ref_str()}, label {self.dest_false.ref_str()}"

    def is_unconditional(self) -> bool:
        return self.dest_false is None


@dataclass
class RetInstr(Instr):
    res: Value

    def __init__(self, res = None):
        super().__init__()
        self.res = res

    def __str__(self) -> str:
        return "    ret" + (f" {self.res.ref_str()}" if self.res is not None else " void")


@dataclass
class ConstantInt(Value):
    val: int
    def __init__(self, ty, val):
        super().__init__("int.cste", ty)
        self.ty = ty
        self.val = val

    def get(value: int, ty = None):
        ty = TYPES["i64"] if ty is None else ty
        return ConstantInt(ty, value)
        # ConstantInt *ConstantInt::get(LLVMContext &Context, const APInt &V) {
        #   LLVMContextImpl *pImpl = Context.pImpl;
        #   std::unique_ptr<ConstantInt> &Slot = V.isZero()  ? pImpl->IntZeroConstants[V.getBitWidth()] : V.isOne() ? pImpl->IntOneConstants[V.getBitWidth()] : pImpl->IntConstants[V];
        #   if (!Slot) {
        #     IntegerType *ITy = IntegerType::get(Context, V.getBitWidth());
        #     Slot.reset(new ConstantInt(ITy, V));
        #   }
        #   assert(Slot->getType() == IntegerType::get(Context, V.getBitWidth()));
        #   return Slot.get();
        # }
    def ref_str(self):
        return f"{ty_str(self.ty)} {int(self.val)}"

    def ref_noty_str(self):
        return f"{int(self.val)}"

    def is_constant(self) -> bool:
        return True

    def get_constant_value(self) -> int:
        return self.val


@dataclass
class GlobalVariable(Value):
    constant: bool
    lt: str
    data: bytes
    align: int
    def __init__(self, ty, constant, lt, data, name):
        super().__init__(name, ty)
        self.align = 1
        self.constant = constant
        self.data = data
        self.lt = lt

    def __str__(self):
        cs = ""
        if self.constant:
            cs = " constant"
        s = ""
        for b in self.data:
            c = chr(b)
            if c.isprintable():
                s += c
            else:
                s += f"\\{b:02X}"
        return f"@{self.name} = {self.lt}{cs} {ty_str(self.ty)} c\"{s}\", align {self.align}"

    def ref_str(self):
        return f"{self.ty} @{self.name}"

    def ref_noty_str(self):
        return f"@{self.name}"


class Address(Value):
    elt_ty: AST.Type
    ptr: Value
    alignment: int
    iknn: bool
    def __init__(self, ptr, elt_ty, alignment, iknn = False):
        super().__init__("", AST.PointerType(elt_ty))
        self.ptr = ptr
        self.name = ptr.name
        self.elt_ty = elt_ty
        self.alignment = alignment
        self.iknn = iknn

    def with_element_type(self, elt_ty):
        return Address(self.ptr, elt_ty, self.alignment, self.iknn)

    @staticmethod
    def invalid():
        return Address()

    def __str__(self):
        return f"{self.ptr}"

    def ref_str(self):
        return f"{ty_str(self.ty)} {self.ptr.ref_noty_str()}"

    def ref_noty_str(self):
        return self.ptr.ref_noty_str()


class LValue(Value):
    addr: Address

    def __init__(self, addr, ty):
        super().__init__("", ty)
        self.name = addr.name
        self.addr = addr

    def __str__(self):
        return f"{self.addr}"

    def ref_str(self):
        return self.addr.ref_str()

    def ref_noty_str(self):
        return self.addr.ref_noty_str()
