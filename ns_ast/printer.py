from typing import Callable
from .visitor import StmtVisitor, DeclVisitor, ASTNodeTraverser
from .nodes import *
from lex import Loc, LocRge, OpenedFile

class StmtPrinter(StmtVisitor):
    os: str
    indent_level: int
    helper: None # PrinterHelper
    policy: None # PrintingPolicy
    nl: str
    context: None # AstContext

    def __init__(self):
        self.os = ""
        self.indent_level = 0
        self.helper = None
        self.policy = None
        self.nl = "\n"
        self.context = None

    def print_stmt(self, s: Stmt, sub_indent: int = 1):
        self.indent_level += sub_indent
        if s is not None and isinstance(s, Expr):
            self.indent()
            self.visit(s)
            self.os += ";" + self.nl
        elif s is not None:
            self.visit(s)
        else:
            self.indent()
            self.os += "<<<NULL STATEMENT>>>" + self.nl
        self.indent_level -= sub_indent

    def print_init_stmt(self, s: Stmt, prefix_width: int):
        self.indent_level += (prefix_width + 1) // 2
        if isinstance(s, DeclStmt):
            self.print_raw_decl_stmt(s)
        else:
            assert isinstance(s, Expr)
            self.print_expr(s)
        self.os += "; "
        self.indent_level -= (prefix_width + 1) // 2

    def print_controlled_stmt(self, s: Stmt):
        if isinstance(s, CompoundStmt):
            self.os += " "
            self.print_raw_compound_stmt(s)
            self.os += self.nl
        else:
            self.os += self.nl
            self.print_stmt(s)

    def print_raw_compound_stmt(self, node: CompoundStmt):
        assert node is not None
        self.os += "{" + self.nl
        for i in node.inner:
            self.print_stmt(i)
        self.indent()
        self.os += "}"

    def print_raw_decl(self, d: Decl):
        self.os += f"{d}"

    def print_raw_decl_stmt(self, s: DeclStmt):
        self.print_raw_decl(s.decl)

    def print_raw_if_stmt(self, s: IfStmt):
        self.os += "if ("
        self.print_expr(s.cond)
        self.os += ")"
        if isinstance(s.then_stmt, CompoundStmt):
            self.os += " "
            self.print_raw_compound_stmt(s.then_stmt)
            self.os += " " if s.else_stmt is not None else self.nl
        else:
            self.os += self.nl
            self.print_stmt(s.then_stmt)
            if s.else_stmt is not None: self.indent()
        if s.else_stmt is None: return
        self.os += "else"
        if isinstance(s.else_stmt, CompoundStmt):
            self.os += " "
            self.print_raw_compound_stmt(s.else_stmt)
            self.os += self.nl
        elif isinstance(s.else_stmt, IfStmt):
            self.os += " "
            self.print_raw_if_stmt(s.else_stmt)
        else:
            self.os += self.nl
            self.print_stmt(s.else_stmt)

    def print_call_args(self, call: CallExpr | BuiltinExpr):
        for i in range(len(call.args)):
            if i != 0:
                self.os += ", "
            self.print_expr(call.args[i])

    def print_expr(self, e: Expr):
        if e is not None:
            self.visit(e)
        else:
            self.os += "<null expr>"

    def indent(self, delta: int = 0):
        for _ in range(self.indent_level + delta):
            self.os += "  "

    def visit_stmt(self, node: Stmt, *args, **kwargs):
        self.indent()
        self.os += "<<unknown stmt type>>" + self.nl

    def visit_expr(self, node: Expr, *args, **kwargs):
        self.os += "<<unknown expr type>>"

    def visit_binary_expr(self, s: BinaryExpr, *args, **kwargs):
        self.print_expr(s.lhs)
        self.os += f" {s.opc} "
        self.print_expr(s.rhs)
    def visit_unary_expr(self, s: UnaryExpr, *args, **kwargs):
        if s.opc == UnaryOperatorKind.POSTINC:
            self.print_expr(s.arg)
            self.os += "++"
        elif s.opc == UnaryOperatorKind.POSTDEC:
            self.print_expr(s.arg)
            self.os += "--"
        else:
            self.os += f"{s.opc}"
            self.print_expr(s.arg)
    def visit_vaarg_expr(self, s: VAArgExpr, *args, **kwargs):
        self.os += f"vaarg<{s.ty}>"
    def visit_cast_expr(self, s: CastExpr, *args, **kwargs):
        self.os += f"cast<{s.ty}>("
        self.print_expr(s.op)
        self.os += ")"
    def visit_builtin_expr(self, s: BuiltinExpr, *args, **kwargs):
        self.os += s.builtin_name + "("
        self.print_call_args(s)
        self.os += ")"
    def visit_recovery_expr(self, s: RecoveryExpr, *args, **kwargs):
        self.os += "<<<recovery expr>>>"
    def visit_conditional_expr(self, s: ConditionalExpr, *args, **kwargs):
        self.print_expr(s.cond)
        self.os += " ? "
        self.print_expr(s.lhs)
        self.os += " : "
        self.print_expr(s.rhs)
    def visit_array_subscript_expr(self, s: ArraySubscriptExpr, *args, **kwargs):
        self.print_expr(s.lhs)
        self.os += "["
        self.print_expr(s.rhs)
        self.os += "]"
    def visit_member_expr(self, s: MemberExpr, *args, **kwargs):
        self.print_expr(s.base)
        self.os += "->" if s.is_arrow else "."
        self.os += s.name
    def visit_sizeof_expr(self, s: SizeofExpr, *args, **kwargs):
        self.os += "sizeof("
        if s.expr is not None: self.print_expr(s.expr)
        else: self.os += f"{s.ty_of_sizeof}"
        self.os += ")"
    def visit_call_expr(self, s: CallExpr, *args, **kwargs):
        self.print_expr(s.fn)
        self.os += "("
        self.print_call_args(s)
        self.os += ")"
    def visit_paren_expr(self, s: ParenExpr, *args, **kwargs):
        self.os += "("
        self.print_expr(s.val)
        self.os += ")"
    def visit_string_literal(self, s: StringLiteral, *args, **kwargs):
        self.os += f"\"{str(s.value)}\""
    def visit_bool_literal(self, s: BoolLiteral, *args, **kwargs):
        self.os += "true" if s.value else "false"
    def visit_integer_literal(self, s: IntegerLiteral, *args, **kwargs):
        self.os += str(s.value)
    def visit_decl_ref_expr(self, s: DeclRefExpr, *args, **kwargs):
        self.os += s.decl.name
    def visit_return_stmt(self, s: ReturnStmt, *args, **kwargs):
        self.indent()
        self.os += "return"
        if s.ret_expr is not None:
            self.os += " "
            self.print_expr(s.ret_expr)
        self.os += ";" + self.nl
    def visit_break_stmt(self, s: BreakStmt, *args, **kwargs):
        self.indent()
        self.os += "break;" + self.nl
    def visit_continue_stmt(self, s: ContinueStmt, *args, **kwargs):
        self.indent()
        self.os += "continue;" + self.nl
    def visit_for_stmt(self, s: ForStmt, *args, **kwargs):
        self.indent()
        self.os += "for (<<TODO>>)" # TODO:
    def visit_do_stmt(self, s: DoStmt, *args, **kwargs):
        self.indent()
        self.os += "do "
        if isinstance(s.body, CompoundStmt):
            self.print_raw_compound_stmt(s.body)
            self.os += " "
        else:
            self.os += self.nl
            self.print_stmt(s.body)
            self.indent()
        self.os += "while ("
        self.print_expr(s.expr)
        self.os += ");" + self.nl
    def visit_while_stmt(self, s: WhileStmt, *args, **kwargs):
        self.indent()
        self.os += "while ("
        self.print_expr(s.cond)
        self.os += ")" + self.nl
        self.print_stmt(s.while_stmt)
    def visit_switch_stmt(self, s: SwitchStmt, *args, **kwargs):
        self.indent()
        self.os += "switch ("
        self.print_expr(s.cond)
        self.os += ")"
        self.print_controlled_stmt(s.body)
    def visit_if_stmt(self, s: IfStmt, *args, **kwargs):
        self.indent()
        self.print_raw_if_stmt(s)
    def visit_default_stmt(self, s: DefaultStmt, *args, **kwargs):
        self.indent(-1)
        self.os += "default:" + self.nl
        self.print_stmt(s.sub_stmt)
    def visit_case_stmt(self, s: CaseStmt, *args, **kwargs):
        self.indent(-1)
        self.os += f"case {s.case_val}:" + self.nl
        self.print_stmt(s.sub_stmt)
    def visit_compound_stmt(self, s: CompoundStmt, *args, **kwargs):
        self.indent()
        self.print_raw_compound_stmt(s)
        self.os += self.nl
    def visit_null_stmt(self, s: NullStmt, *args, **kwargs):
        self.indent()
        self.os += ";" + self.nl
    def visit_decl_stmt(self, s: DeclStmt, *args, **kwargs):
        self.indent()
        self.print_raw_decl_stmt(s)
        self.os += ";" + self.nl

class DeclPrinter(DeclVisitor):
    os: str
    policy: None
    ctx: None
    indentation: int

    def __init__(self):
        self.os = ""
        self.policy = None
        self.ctx = None
        self.indentation = 0

    def indent(self, indentation = None):
        pass
    # TODO:


RESET_COL = "\x1b[0m"
DECL_COL = "\x1b[1;32m"          # Green Bold
ATTR_COL = "\x1b[1;34m"          # Blue  Bold
STMT_COL = "\x1b[1;35m"          # Magenta Bold
CMT_COL = "\x1b[34m"             # Blue
TYPE_COL = "\x1b[32m"            # Green
ADDR_COL = "\x1b[33m"            # Yellow
LOC_COL = "\x1b[33m"             # Yellow
VK_COL = "\x1b[36m"              # Cyan
OK_COL = "\x1b[36m"              # Cyan
ERROR_COL = "\x1b[1;31m"         # Red   Bold
NULL_COL = "\x1b[34m"            # Blue
UNDERSERIALID_COL = "\x1b[1;32m" # Green Bold
CAST_COL = "\x1b[31m"            # Red
VALUE_COL = "\x1b[1;36m"         # Cyan  Bold
DECL_NAME_COL = "\x1b[1;36m"     # Cyan  Bold
INDENT_COL = "\x1b[34m"          # Blue


class TextTreeStructure:
    pending: List[Callable[[bool], None]]
    top_level: bool
    first_child: bool
    prefix: str

    def __init__(self):
        self.pending = []
        self.top_level = True
        self.first_child = True
        self.prefix = ""

    @staticmethod
    def dump_with_indent(this, do_add_child, label, is_last_child):
        print()
        last_pfx = ['├', '╰'][is_last_child]
        label_pfx = f"{label}: " if label != "" else " "
        print(f"{INDENT_COL}{this.prefix}{last_pfx}─{label_pfx}{RESET_COL}", end='')
        this.prefix += ["│  ", "   "][is_last_child]
        this.first_child = True
        depth = len(this.pending)
        do_add_child()
        while depth < len(this.pending):
            this.pending.pop()(True)
        this.prefix = this.prefix[:-3]

    def add_child(self, do_add_child, label=""):
        if self.top_level:
            self.top_level = False
            do_add_child()
            while len(self.pending) > 0:
                self.pending.pop()(True)
            self.prefix = ""
            print()
            self.top_level = True
            return
        if self.first_child:
            self.pending.append(lambda x: TextTreeStructure.dump_with_indent(self, do_add_child, label, x))
        else:
            self.pending.pop()(False)
            self.pending.append(lambda x: TextTreeStructure.dump_with_indent(self, do_add_child, label, x))
        self.first_child = False

class TextNodeDumper(StmtVisitor, DeclVisitor, TextTreeStructure):
    def p(self, *args, **kwargs):
        print(*args, **kwargs, end='')

    def __init__(self):
        TextTreeStructure.__init__(self)
        self.last_loc = ("", 0, 0)

    def dump_location(self, loc: Loc):
        T = OpenedFile.get_loc(loc)
        if T is None:
            self.p(f"{LOC_COL}<invalid sloc>{RESET_COL}")
            return
        f, l, c = T
        if f != self.last_loc[0]:
            self.p(f"{LOC_COL}{f}:{l}:{c}{RESET_COL}")
            self.last_loc = T
        elif l != self.last_loc[1]:
            self.p(f"{LOC_COL}line:{l}:{c}{RESET_COL}")
            self.last_loc = T
        else:
            self.p(f"{LOC_COL}col:{c}{RESET_COL}")

    def dump_source_range(self, r: LocRge):
        self.p(" <")
        self.dump_location(r[0])
        if r[0] != r[1]:
            self.p(", ")
            self.dump_location(r[1])
        self.p(">")

    def dump_name(self, nd: NamedDecl):
        if nd.name is not None:
            self.p(f" {DECL_NAME_COL}{nd.name}{RESET_COL}")

    def dump_type(self, t: Type):
        self.p(f" {TYPE_COL}'{t}'{RESET_COL}")
        # TODO: desugared type (for aliases)

    def decl_visit(self, d: Decl):
        if d is None:
            self.p(f"{NULL_COL}<<<NULL>>>{RESET_COL}")
            return
        self.p(f"{DECL_COL}{d.__class__.__name__}{RESET_COL}")
        # self.dump_pointer(d)
        # if (D->getLexicalDeclContext() != D->getDeclContext()) OS << " parent " << cast<Decl>(D->getDeclContext());
        # dumpPreviousDecl(OS, D);
        self.dump_source_range(d.get_range())
        self.p(" ")
        self.dump_location(d.get_range()[0])
        # if (D->isFromASTFile()) OS << " imported";
        # if (Module *M = D->getOwningModule()) OS << " in " << M->getFullModuleName();
        # if (auto *ND = dyn_cast<NamedDecl>(D)) for (Module *M : D->getASTContext().getModulesWithMergedDefinition(const_cast<NamedDecl *>(ND))) AddChild([=] { OS << "also in " << M->getFullModuleName(); });
        # if (const NamedDecl *ND = dyn_cast<NamedDecl>(D)) if (!ND->isUnconditionallyVisible()) OS << " hidden";
        # if (D->isImplicit()) OS << " implicit";
        # if (D->isUsed()) OS << " used";
        # else if (D->isThisDeclarationReferenced()) OS << " referenced";
        # if (D->isInvalidDecl()) OS << " invalid";
        # if (const FunctionDecl *FD = dyn_cast<FunctionDecl>(D)) { if (FD->isConstexprSpecified()) OS << " constexpr"; if (FD->isConsteval()) OS << " consteval"; else if (FD->isImmediateFunction()) OS << " immediate"; if (FD->isMultiVersion()) OS << " multiversion"; }
        DeclVisitor.visit(self, d)

    def visit_fn_decl(self, d: FnDecl, *args, **kwargs):
        self.dump_name(d)
        self.dump_type(d.ty)
        # StorageClass SC = D->getStorageClass();
        # if (SC != SC_None) OS << ' ' << VarDecl::getStorageClassSpecifierString(SC);
        # if (D->isInlineSpecified()) OS << " inline";
        # if (D->isVirtualAsWritten()) OS << " virtual";
        # if (D->isModulePrivate()) OS << " __module_private__";
        # if (D->isPureVirtual()) OS << " pure";
        # if (D->isDefaulted()) { OS << " default"; if (D->isDeleted()) OS << "_delete"; }
        # if (D->isDeletedAsWritten()) OS << " delete";
        # if (D->isTrivial()) OS << " trivial";
        # if (const StringLiteral *M = D->getDeletedMessage()) AddChild("delete message", [=] { Visit(M); });
        # if (D->isIneligibleOrNotSelected()) OS << (isa<CXXDestructorDecl>(D) ? " not_selected" : " ineligible");
        # if (const auto *FPT = D->getType()->getAs<FunctionProtoType>()) { FunctionProtoType::ExtProtoInfo EPI = FPT->getExtProtoInfo(); switch (EPI.ExceptionSpec.Type) { default: break; case EST_Unevaluated: OS << " noexcept-unevaluated " << EPI.ExceptionSpec.SourceDecl; break; case EST_Uninstantiated: OS << " noexcept-uninstantiated " << EPI.ExceptionSpec.SourceTemplate; break; } }
        # if (!D->isInlineSpecified() && D->isInlined()) { OS << " implicit-inline"; }

    def visit_var_decl(self, d: VarDecl, *args, **kwargs):
        # dumpNestedNameSpecifier(D->getQualifier());
        self.dump_name(d)
        # if (const auto *P = dyn_cast<ParmVarDecl>(D); P && P->isExplicitObjectParameter()) OS << " this";
        self.dump_type(d.ty)
        # StorageClass SC = D->getStorageClass();
        # if (SC != SC_None) OS << ' ' << VarDecl::getStorageClassSpecifierString(SC);
        # switch (D->getTLSKind()) {
        # case VarDecl::TLS_None: break;
        # case VarDecl::TLS_Static: OS << " tls"; break;
        # case VarDecl::TLS_Dynamic: OS << " tls_dynamic"; break;
        # }
        # if (D->isModulePrivate()) OS << " __module_private__";
        # if (D->isNRVOVariable()) OS << " nrvo";
        # if (D->isInline()) OS << " inline";
        # if (D->isConstexpr()) OS << " constexpr";
        # if (D->hasInit()) {
        #   switch (D->getInitStyle()) {
        #   case VarDecl::CInit: OS << " cinit"; break;
        #   case VarDecl::CallInit: OS << " callinit"; break;
        #   case VarDecl::ListInit: OS << " listinit"; break;
        #   case VarDecl::ParenListInit: OS << " parenlistinit";
        #   }
        # }
        # if (D->hasInit()) {
        #   const Expr *E = D->getInit();
        #   if (E && !E->isValueDependent() && D->isConstexpr() && !D->getType()->isDependentType()) {
        #     const APValue *Value = D->evaluateValue();
        #     if (Value) AddChild("value", [=] { Visit(*Value, E->getType()); });
        #   }
        # }

    def visit_field_decl(self, d: FieldDecl, *args, **kwargs):
        self.dump_name(d)
        self.dump_type(d.ty)
        # if (D->isMutable()) OS << " mutable";
        # if (D->isModulePrivate()) OS << " __module_private__";

    def visit_type_decl(self, d: TypeDecl, *args, **kwargs):
        self.dump_name(d)
        self.dump_type(d.ty)

    def visit_binary_expr(self, e: BinaryExpr, *args, **kwargs):
        self.p(f" '{e.opc}'")

    def visit_compound_assign_expr(self, e: CompoundAssignExpr, *args, **kwargs):
        self.p(f" '{e.opc}' ComputeLHSTy=<TODO> ComputeResultTy=<TODO>")

    def visit_member_expr(self, e: MemberExpr, *args, **kwargs):
        arrow_str = [".", "->"][e.is_arrow]
        self.p(f" {arrow_str}{e.name}")
        # self.dump_pointer(e.decl)
        # self.dump_nested_name_specifier(e.qual)
        # switch (Node->isNonOdrUse()) {
        # case NOUR_None: break;
        # case NOUR_Unevaluated: OS << " non_odr_use_unevaluated"; break;
        # case NOUR_Constant: OS << " non_odr_use_constant"; break;
        # case NOUR_Discarded: OS << " non_odr_use_discarded"; break;
        # }

    def visit_unary_expr(self, e: UnaryExpr, *args, **kwargs):
        self.p(f" '{e.opc}'")
        # if (!Node->canOverflow()) OS << " cannot overflow";

    def visit_string_literal(self, e: StringLiteral, *args, **kwargs):
        val = f"{e.value}"[2:-5]
        self.p(f" {VALUE_COL}\"{val}\"{RESET_COL}")

    def visit_cast_expr(self, e: CastExpr, *args, **kwargs):
        bp = "" # dumpBasePath(os, node)
        self.p(f" <{CAST_COL}{e.kind}{bp}{RESET_COL}>")

    # void TextNodeDumper::VisitImplicitCastExpr(const ImplicitCastExpr *Node) { VisitCastExpr(Node); if (Node->isPartOfExplicitCast()) OS << " part_of_explicit_cast"; }

    # def visit_character_literal(self, e: CharLiteral, *args, **kwargs): self.p(" {VALUE_COL}{e.val}{RESET_COL}")

    def dump_bare_decl_ref(self, d: Decl):
        if d is None:
            self.p(f"{NULL_COL}<<<NULL>>>{RESET_COL}")
            return
        self.p(f"{DECL_COL}{d.__class__.__name__}{RESET_COL}")
        # self.dump_pointer(d)
        if isinstance(d, NamedDecl):
            self.p(f" {DECL_NAME_COL}'{d.name}'{RESET_COL}")
        if isinstance(d, ValueDecl):
            self.dump_type(d.ty)

    def visit_enum_decl(self, d: EnumDecl, *args, **kwargs):
        self.dump_name(d)
        assert isinstance(d.ty, EnumType)
        if d.ty.aliased_type is not None:
            self.dump_type(d.ty)

    def visit_enum_variant_decl(self, d: EnumVariantDecl, *args, **kwargs):
        self.dump_name(d)
        self.p(f" = {d.val}")

    def visit_struct_decl(self, d: StructDecl, *args, **kwargs):
        self.p(" struct")
        self.dump_name(d)
        # if (D->isModulePrivate()) OS << " __module_private__";
        # if (D->isCompleteDefinition()) OS << " definition";

    def visit_decl_ref_expr(self, e: DeclRefExpr, *args, **kwargs):
        self.p(" ")
        self.dump_bare_decl_ref(e.decl)
        # self.dump_nested_name_specifier(e.qual)
        # if node.decl != node.found_decl: self.p(" ({dump_bare_decl_ref ... })")
        # switch (Node->isNonOdrUse()) {
        # case NOUR_None: break;
        # case NOUR_Unevaluated: OS << " non_odr_use_unevaluated"; break;
        # case NOUR_Constant: OS << " non_odr_use_constant"; break;
        # case NOUR_Discarded: OS << " non_odr_use_discarded"; break;
        # }
        # if (Node->isCapturedByCopyInLambdaWithExplicitObjectParameter()) OS << " dependent_capture";
        # else if (Node->refersToEnclosingVariableOrCapture()) OS << " refers_to_enclosing_variable_or_capture";
        # if (Node->isImmediateEscalating()) OS << " immediate-escalating";

    def visit_integer_literal(self, e: IntegerLiteral, *args, **kwargs):
        # TODO: depending on type, print signed/unsigned
        self.p(f" {VALUE_COL}{e.value}{RESET_COL}")

    def visit_bool_literal(self, e: BoolLiteral, *args, **kwargs):
        self.p(["false", "true"][e.value])

    def stmt_visit(self, s: Stmt):
        if s is None:
            print(f"{NULL_COL}<<<NULL>>>{RESET_COL}", end='')
            return
        print(f"{STMT_COL}{s.__class__.__name__}{RESET_COL}", end='')
        # self.dump_pointer(s)
        self.dump_source_range(s.get_range())
        if isinstance(s, Expr):
            self.dump_type(s.ty)
            # if s.contains_errors(): self.p(f" {ERROR_COL}contains-errors{RESET_COL}")
            if s.value_kind == ValueKind.LVALUE:
                print(f" {VK_COL}lvalue{RESET_COL}", end='')
            elif s.value_kind == ValueKind.XVALUE:
                print(f" {VK_COL}xvalue{RESET_COL}", end='')
            # TODO: ObjectKind
        StmtVisitor.visit(self, s)

    def visit_builtin_expr(self, e: BuiltinExpr, *args, **kwargs):
        self.p(f" '{e.builtin_name}'")

    def visit(self, d_or_s: Stmt | Decl):
        if isinstance(d_or_s, Stmt):
            self.stmt_visit(d_or_s)
        elif isinstance(d_or_s, Decl):
            self.decl_visit(d_or_s)
        else:
            assert False

class AstDumper(ASTNodeTraverser):
    def __init__(self):
        super().__init__(TextNodeDumper())

def print_ast(ast):
    tu = TranslationUnitDecl(ast)
    dumper = AstDumper()
    dumper.visit(tu)
