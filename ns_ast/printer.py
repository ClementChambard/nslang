from typing import Callable
from .visitor import StmtVisitor, DeclVisitor, ASTNodeTraverser
from .nodes import *
from lex import Loc, LocRge, OpenedFile


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

    def visit_method_expr(self, e: MethodExpr, *args, **kwargs):
        arrow_str = [".", "->"][e.is_arrow]
        self.p(f" {arrow_str}({e.method_func.name})")

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
