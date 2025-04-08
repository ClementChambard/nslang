from .nodes import *

class StmtVisitor:
    def visit(self, s: Stmt, *args, **kwargs):
        if isinstance(s, CompoundAssignExpr):
            match s.opc:
                case BinaryOperatorKind.MULASSIGN: return self.visit_bin_mul_assign      (s, *args, **kwargs)
                case BinaryOperatorKind.DIVASSIGN: return self.visit_bin_div_assign      (s, *args, **kwargs)
                case BinaryOperatorKind.REMASSIGN: return self.visit_bin_rem_assign      (s, *args, **kwargs)
                case BinaryOperatorKind.ADDASSIGN: return self.visit_bin_add_assign      (s, *args, **kwargs)
                case BinaryOperatorKind.SUBASSIGN: return self.visit_bin_sub_assign      (s, *args, **kwargs)
                case BinaryOperatorKind.SHLASSIGN: return self.visit_bin_shl_assign      (s, *args, **kwargs)
                case BinaryOperatorKind.SHRASSIGN: return self.visit_bin_shr_assign      (s, *args, **kwargs)
                case BinaryOperatorKind.ANDASSIGN: return self.visit_bin_and_assign      (s, *args, **kwargs)
                case BinaryOperatorKind.XORASSIGN: return self.visit_bin_xor_assign      (s, *args, **kwargs)
                case BinaryOperatorKind.ORASSIGN:  return self.visit_bin_or_assign       (s, *args, **kwargs)
                case _: assert False
        elif isinstance(s, BinaryExpr):
            match s.opc:
                case BinaryOperatorKind.MUL:       return self.visit_bin_mul             (s, *args, **kwargs)
                case BinaryOperatorKind.DIV:       return self.visit_bin_div             (s, *args, **kwargs)
                case BinaryOperatorKind.REM:       return self.visit_bin_rem             (s, *args, **kwargs)
                case BinaryOperatorKind.ADD:       return self.visit_bin_add             (s, *args, **kwargs)
                case BinaryOperatorKind.SUB:       return self.visit_bin_sub             (s, *args, **kwargs)
                case BinaryOperatorKind.SHL:       return self.visit_bin_shl             (s, *args, **kwargs)
                case BinaryOperatorKind.SHR:       return self.visit_bin_shr             (s, *args, **kwargs)
                case BinaryOperatorKind.LT:        return self.visit_bin_lt              (s, *args, **kwargs)
                case BinaryOperatorKind.GT:        return self.visit_bin_gt              (s, *args, **kwargs)
                case BinaryOperatorKind.LE:        return self.visit_bin_le              (s, *args, **kwargs)
                case BinaryOperatorKind.GE:        return self.visit_bin_ge              (s, *args, **kwargs)
                case BinaryOperatorKind.EQ:        return self.visit_bin_eq              (s, *args, **kwargs)
                case BinaryOperatorKind.NE:        return self.visit_bin_ne              (s, *args, **kwargs)
                case BinaryOperatorKind.AND:       return self.visit_bin_and             (s, *args, **kwargs)
                case BinaryOperatorKind.XOR:       return self.visit_bin_xor             (s, *args, **kwargs)
                case BinaryOperatorKind.OR:        return self.visit_bin_or              (s, *args, **kwargs)
                case BinaryOperatorKind.LAND:      return self.visit_bin_land            (s, *args, **kwargs)
                case BinaryOperatorKind.LOR:       return self.visit_bin_lor             (s, *args, **kwargs)
                case BinaryOperatorKind.ASSIGN:    return self.visit_bin_assign          (s, *args, **kwargs)
                case _: assert False
        elif isinstance(s, UnaryExpr):
            match s.opc:
                case UnaryOperatorKind.POSTINC:    return self.visit_unary_post_inc      (s, *args, **kwargs)
                case UnaryOperatorKind.POSTDEC:    return self.visit_unary_post_dec      (s, *args, **kwargs)
                case UnaryOperatorKind.PREINC:     return self.visit_unary_pre_inc       (s, *args, **kwargs)
                case UnaryOperatorKind.PREDEC:     return self.visit_unary_pre_dec       (s, *args, **kwargs)
                case UnaryOperatorKind.ADDROF:     return self.visit_unary_addrof        (s, *args, **kwargs)
                case UnaryOperatorKind.DEREF:      return self.visit_unary_deref         (s, *args, **kwargs)
                case UnaryOperatorKind.PLUS:       return self.visit_unary_plus          (s, *args, **kwargs)
                case UnaryOperatorKind.MINUS:      return self.visit_unary_minus         (s, *args, **kwargs)
                case UnaryOperatorKind.NOT:        return self.visit_unary_not           (s, *args, **kwargs)
                case UnaryOperatorKind.LNOT:       return self.visit_unary_lnot          (s, *args, **kwargs)
        elif isinstance(s, VAArgExpr):             return self.visit_vaarg_expr          (s, *args, **kwargs)
        elif isinstance(s, ImplicitCastExpr):      return self.visit_implicit_cast_expr  (s, *args, **kwargs)
        elif isinstance(s, CastExpr):              return self.visit_cast_expr           (s, *args, **kwargs)
        elif isinstance(s, BuiltinExpr):           return self.visit_builtin_expr        (s, *args, **kwargs)
        elif isinstance(s, RecoveryExpr):          return self.visit_recovery_expr       (s, *args, **kwargs)
        elif isinstance(s, ConditionalExpr):       return self.visit_conditional_expr    (s, *args, **kwargs)
        elif isinstance(s, ArraySubscriptExpr):    return self.visit_array_subscript_expr(s, *args, **kwargs)
        elif isinstance(s, MemberExpr):            return self.visit_member_expr         (s, *args, **kwargs)
        elif isinstance(s, SizeofExpr):            return self.visit_sizeof_expr         (s, *args, **kwargs)
        elif isinstance(s, CallExpr):              return self.visit_call_expr           (s, *args, **kwargs)
        elif isinstance(s, ParenExpr):             return self.visit_paren_expr          (s, *args, **kwargs)
        elif isinstance(s, StringLiteral):         return self.visit_string_literal      (s, *args, **kwargs)
        elif isinstance(s, BoolLiteral):           return self.visit_bool_literal        (s, *args, **kwargs)
        elif isinstance(s, IntegerLiteral):        return self.visit_integer_literal     (s, *args, **kwargs)
        elif isinstance(s, DeclRefExpr):           return self.visit_decl_ref_expr       (s, *args, **kwargs)
        elif isinstance(s, Expr):                  return self.visit_expr                (s, *args, **kwargs)
        elif isinstance(s, ReturnStmt):            return self.visit_return_stmt         (s, *args, **kwargs)
        elif isinstance(s, BreakStmt):             return self.visit_break_stmt          (s, *args, **kwargs)
        elif isinstance(s, ContinueStmt):          return self.visit_continue_stmt       (s, *args, **kwargs)
        elif isinstance(s, ForStmt):               return self.visit_for_stmt            (s, *args, **kwargs)
        elif isinstance(s, DoStmt):                return self.visit_do_stmt             (s, *args, **kwargs)
        elif isinstance(s, WhileStmt):             return self.visit_while_stmt          (s, *args, **kwargs)
        elif isinstance(s, SwitchStmt):            return self.visit_switch_stmt         (s, *args, **kwargs)
        elif isinstance(s, IfStmt):                return self.visit_if_stmt             (s, *args, **kwargs)
        elif isinstance(s, DefaultStmt):           return self.visit_default_stmt        (s, *args, **kwargs)
        elif isinstance(s, CaseStmt):              return self.visit_case_stmt           (s, *args, **kwargs)
        elif isinstance(s, SwitchCase):            return self.visit_switch_case         (s, *args, **kwargs)
        elif isinstance(s, CompoundStmt):          return self.visit_compound_stmt       (s, *args, **kwargs)
        elif isinstance(s, NullStmt):              return self.visit_null_stmt           (s, *args, **kwargs)
        elif isinstance(s, DeclStmt):              return self.visit_decl_stmt           (s, *args, **kwargs)
        elif isinstance(s, Stmt):                  return self.visit_stmt                (s, *args, **kwargs)
        else: assert False, "unreachable: Unknown stmt kind!"

    def visit_bin_mul(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_div(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_rem(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_add(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_sub(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_shl(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_shr(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_lt(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_gt(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_le(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_ge(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_eq(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_ne(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_and(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_xor(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_or(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_land(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_lor(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_assign(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_bin_mul_assign(self, s: CompoundAssignExpr, *args, **kwargs):
        return self.visit_compound_assign_expr(s, *args, **kwargs)
    def visit_bin_div_assign(self, s: CompoundAssignExpr, *args, **kwargs):
        return self.visit_compound_assign_expr(s, *args, **kwargs)
    def visit_bin_rem_assign(self, s: CompoundAssignExpr, *args, **kwargs):
        return self.visit_compound_assign_expr(s, *args, **kwargs)
    def visit_bin_add_assign(self, s: CompoundAssignExpr, *args, **kwargs):
        return self.visit_compound_assign_expr(s, *args, **kwargs)
    def visit_bin_sub_assign(self, s: CompoundAssignExpr, *args, **kwargs):
        return self.visit_compound_assign_expr(s, *args, **kwargs)
    def visit_bin_shl_assign(self, s: CompoundAssignExpr, *args, **kwargs):
        return self.visit_compound_assign_expr(s, *args, **kwargs)
    def visit_bin_shr_assign(self, s: CompoundAssignExpr, *args, **kwargs):
        return self.visit_compound_assign_expr(s, *args, **kwargs)
    def visit_bin_and_assign(self, s: CompoundAssignExpr, *args, **kwargs):
        return self.visit_compound_assign_expr(s, *args, **kwargs)
    def visit_bin_xor_assign(self, s: CompoundAssignExpr, *args, **kwargs):
        return self.visit_compound_assign_expr(s, *args, **kwargs)
    def visit_bin_or_assign(self, s: CompoundAssignExpr, *args, **kwargs):
        return self.visit_compound_assign_expr(s, *args, **kwargs)
    def visit_unary_post_inc(self, s: UnaryExpr, *args, **kwargs):
        return self.visit_unary_expr(s, *args, **kwargs)
    def visit_unary_post_dec(self, s: UnaryExpr, *args, **kwargs):
        return self.visit_unary_expr(s, *args, **kwargs)
    def visit_unary_pre_inc(self, s: UnaryExpr, *args, **kwargs):
        return self.visit_unary_expr(s, *args, **kwargs)
    def visit_unary_pre_dec(self, s: UnaryExpr, *args, **kwargs):
        return self.visit_unary_expr(s, *args, **kwargs)
    def visit_unary_addrof(self, s: UnaryExpr, *args, **kwargs):
        return self.visit_unary_expr(s, *args, **kwargs)
    def visit_unary_deref(self, s: UnaryExpr, *args, **kwargs):
        return self.visit_unary_expr(s, *args, **kwargs)
    def visit_unary_plus(self, s: UnaryExpr, *args, **kwargs):
        return self.visit_unary_expr(s, *args, **kwargs)
    def visit_unary_minus(self, s: UnaryExpr, *args, **kwargs):
        return self.visit_unary_expr(s, *args, **kwargs)
    def visit_unary_not(self, s: UnaryExpr, *args, **kwargs):
        return self.visit_unary_expr(s, *args, **kwargs)
    def visit_unary_lnot(self, s: UnaryExpr, *args, **kwargs):
        return self.visit_unary_expr(s, *args, **kwargs)
    def visit_binary_expr(self, s: BinaryExpr, *args, **kwargs):
        return self.visit_expr(s, *args, **kwargs)
    def visit_unary_expr(self, s: UnaryExpr, *args, **kwargs):
        return self.visit_expr(s, *args, **kwargs)
    def visit_compound_assign_expr(self, s: CompoundAssignExpr, *args, **kwargs):
        return self.visit_binary_expr(s, *args, **kwargs)
    def visit_vaarg_expr(self, s: VAArgExpr, *args, **kwargs):
        return self.visit_expr(s, *args, **kwargs)
    def visit_implicit_cast_expr(self, s: ImplicitCastExpr, *args, **kwargs):
        return self.visit_cast_expr(s, *args, **kwargs)
    def visit_cast_expr(self, s: CastExpr, *args, **kwargs):
        return self.visit_expr(s, *args, **kwargs)
    def visit_builtin_expr(self, s: BuiltinExpr, *args, **kwargs):
        return self.visit_expr(s, *args, **kwargs)
    def visit_recovery_expr(self, s: RecoveryExpr, *args, **kwargs):
        return self.visit_expr(s, *args, **kwargs)
    def visit_conditional_expr(self, s: ConditionalExpr, *args, **kwargs):
        return self.visit_expr(s, *args, **kwargs)
    def visit_array_subscript_expr(self, s: ArraySubscriptExpr, *args, **kwargs):
        return self.visit_expr(s, *args, **kwargs)
    def visit_member_expr(self, s: MemberExpr, *args, **kwargs):
        return self.visit_expr(s, *args, **kwargs)
    def visit_sizeof_expr(self, s: SizeofExpr, *args, **kwargs):
        return self.visit_expr(s, *args, **kwargs)
    def visit_call_expr(self, s: CallExpr, *args, **kwargs):
        return self.visit_expr(s, *args, **kwargs)
    def visit_paren_expr(self, s: ParenExpr, *args, **kwargs):
        return self.visit_expr(s, *args, **kwargs)
    def visit_string_literal(self, s: StringLiteral, *args, **kwargs):
        return self.visit_expr(s, *args, **kwargs)
    def visit_bool_literal(self, s: BoolLiteral, *args, **kwargs):
        return self.visit_expr(s, *args, **kwargs)
    def visit_integer_literal(self, s: IntegerLiteral, *args, **kwargs):
        return self.visit_expr(s, *args, **kwargs)
    def visit_decl_ref_expr(self, s: DeclRefExpr, *args, **kwargs):
        return self.visit_expr(s, *args, **kwargs)
    def visit_expr(self, s: Expr, *args, **kwargs):
        self.visit_stmt(s, *args, **kwargs)
    def visit_return_stmt(self, s: ReturnStmt, *args, **kwargs):
        self.visit_stmt(s, *args, **kwargs)
    def visit_break_stmt(self, s: BreakStmt, *args, **kwargs):
        self.visit_stmt(s, *args, **kwargs)
    def visit_continue_stmt(self, s: ContinueStmt, *args, **kwargs):
        self.visit_stmt(s, *args, **kwargs)
    def visit_for_stmt(self, s: ForStmt, *args, **kwargs):
        self.visit_stmt(s, *args, **kwargs)
    def visit_do_stmt(self, s: DoStmt, *args, **kwargs):
        self.visit_stmt(s, *args, **kwargs)
    def visit_while_stmt(self, s: WhileStmt, *args, **kwargs):
        self.visit_stmt(s, *args, **kwargs)
    def visit_switch_stmt(self, s: SwitchStmt, *args, **kwargs):
        self.visit_stmt(s, *args, **kwargs)
    def visit_if_stmt(self, s: IfStmt, *args, **kwargs):
        self.visit_stmt(s, *args, **kwargs)
    def visit_default_stmt(self, s: DefaultStmt, *args, **kwargs):
        self.visit_switch_case(s, *args, **kwargs)
    def visit_case_stmt(self, s: CaseStmt, *args, **kwargs):
        self.visit_switch_case(s, *args, **kwargs)
    def visit_switch_case(self, s: SwitchCase, *args, **kwargs):
        self.visit_stmt(s, *args, **kwargs)
    def visit_compound_stmt(self, s: CompoundStmt, *args, **kwargs):
        self.visit_stmt(s, *args, **kwargs)
    def visit_null_stmt(self, s: NullStmt, *args, **kwargs):
        self.visit_stmt(s, *args, **kwargs)
    def visit_decl_stmt(self, s: DeclStmt, *args, **kwargs):
        self.visit_stmt(s, *args, **kwargs)
    def visit_stmt(self, s: Stmt, *args, **kwargs):
        return None

class DeclVisitor:
    def visit(self, d: Decl, *args, **kwargs):
        if isinstance(d, FnDecl):
            self.visit_fn_decl(d, *args, **kwargs)
        elif isinstance(d, ParamDecl):
            self.visit_param_decl(d, *args, **kwargs)
        elif isinstance(d, VarDecl):
            self.visit_var_decl(d, *args, **kwargs)
        elif isinstance(d, EnumVariantDecl):
            self.visit_enum_variant_decl(d, *args, **kwargs)
        elif isinstance(d, FieldDecl):
            self.visit_field_decl(d, *args, **kwargs)
        elif isinstance(d, ValueDecl):
            self.visit_value_decl(d, *args, **kwargs)
        elif isinstance(d, StructDecl):
            self.visit_struct_decl(d, *args, **kwargs)
        elif isinstance(d, EnumDecl):
            self.visit_enum_decl(d, *args, **kwargs)
        elif isinstance(d, TypeDecl):
            self.visit_type_decl(d, *args, **kwargs)
        elif isinstance(d, NamedDecl):
            self.visit_named_decl(d, *args, **kwargs)
        elif isinstance(d, TranslationUnitDecl):
            self.visit_translation_unit_decl(d, *args, **kwargs)
        elif isinstance(d, Decl):
            self.visit_decl(d, *args, **kwargs)
        else:
            assert False, "unreachable: unknown decl kind"

    def visit_fn_decl(self, d: FnDecl, *args, **kwargs):
        return self.visit_value_decl(d, *args, **kwargs)
    def visit_param_decl(self, d: ParamDecl, *args, **kwargs):
        return self.visit_var_decl(d, *args, **kwargs)
    def visit_var_decl(self, d: VarDecl, *args, **kwargs):
        return self.visit_value_decl(d, *args, **kwargs)
    def visit_enum_variant_decl(self, d: EnumVariantDecl, *args, **kwargs):
        return self.visit_value_decl(d, *args, **kwargs)
    def visit_field_decl(self, d: FieldDecl, *args, **kwargs):
        return self.visit_value_decl(d, *args, **kwargs)
    def visit_value_decl(self, d: ValueDecl, *args, **kwargs):
        return self.visit_named_decl(d, *args, **kwargs)
    def visit_struct_decl(self, d: StructDecl, *args, **kwargs):
        return self.visit_named_decl(d, *args, **kwargs)
    def visit_enum_decl(self, d: EnumDecl, *args, **kwargs):
        return self.visit_named_decl(d, *args, **kwargs)
    def visit_type_decl(self, d: TypeDecl, *args, **kwargs):
        return self.visit_named_decl(d, *args, **kwargs)
    def visit_named_decl(self, d: NamedDecl, *args, **kwargs):
        return self.visit_decl(d, *args, **kwargs)
    def visit_translation_unit_decl(self, d: TranslationUnitDecl, *args, **kwargs):
        return self.visit_decl(d, *args, **kwargs)
    def visit_decl(self, d: Decl, *args, **kwargs):
        return None

class ASTNodeTraverser(DeclVisitor, StmtVisitor):

    def __init__(self, delegate):
        self.delegate = delegate
        self.deserialize = False
        self.visit_locs = False
        self.traversal = "TK_AsIs"

    def get_node_delegate(self):
        return self.delegate

    @staticmethod
    def decl_visit_func(d: Decl, slf):
        slf.get_node_delegate().visit(d)
        if d is None:
            return
        DeclVisitor.visit(slf, d)

    def decl_visit(self, d: Decl):
        # if self.traversal == "TK_IgnoreUnlessSpelledInSource" and d.is_implicit(): return
        self.get_node_delegate().add_child(lambda: ASTNodeTraverser.decl_visit_func(d, self))

    @staticmethod
    def stmt_visit_func(s: Stmt, slf, label: str):
        slf.get_node_delegate().visit(s)
        if s is None:
            return
        StmtVisitor.visit(slf, s)
        if isinstance(s, DeclStmt): return
        for substmt in s.children():
            slf.visit(substmt)

    def stmt_visit(self, s: Stmt, label: str = ""):
        self.get_node_delegate().add_child(lambda: ASTNodeTraverser.stmt_visit_func(s, self, label))

    def visit(self, d_or_s: Decl | Stmt, *args, **kwargs):
        if isinstance(d_or_s, Decl):
            self.decl_visit(d_or_s, *args, **kwargs)
        elif isinstance(d_or_s, Stmt):
            self.stmt_visit(d_or_s, *args, **kwargs)
        else:
            assert False, f"{d_or_s.__class__}"

    def visit_fn_decl(self, d: FnDecl, *args, **kwargs):
        for p in d.param_decls:
            self.visit(p)
        if d.body is not None:
            self.visit(d.body)

    def visit_enum_decl(self, d: EnumDecl, *args, **kwargs):
        for p in d.variants:
            self.visit(p)

    def visit_struct_decl(self, d: StructDecl, *args, **kwargs):
        for f in d.fields:
            self.visit(f)

    def visit_translation_unit_decl(self, tu: TranslationUnitDecl, *args, **kwargs):
        for d in tu.decls:
            self.visit(d)

    # initializers for var/field decls
