#include ast_nodes_expr.nsh
#include ast_nodes_type.nsh
#include ast_nodes_unqualified_id.nsh
#include loc.nsh

struct OperandResult {
  lhs: Expr*;
  rhs: Expr*;
  ty: Type*;
};

struct CondOperandResult {
  ty: Type*;
  cond: Expr*;
  lhs: Expr*;
  rhs: Expr*;
  vk: ValueKind;
};

struct sema;
struct Scope;
struct NamedDecl;
struct DArray;
struct ImplicitConversionSequence;

lib fn sema::get_expr_range(e: Expr*, start: Loc*, end: Loc*);
lib fn sema::invalid_operands(loc: Loc, lhs: Expr*, rhs: Expr*, res: OperandResult*);
lib fn sema::default_function_array_conversion(e: Expr*, diagnose: bool) -> Expr*;
lib fn sema::default_lvalue_conversion(e: Expr*) -> Expr*;
lib fn sema::default_function_array_lvalue_conversion(e: Expr*, diagnose: bool) -> Expr*;
lib fn sema::usual_unary_conversions(expr: Expr*) -> Expr*;
lib fn sema::usual_arithmetic_conversions(lhs: Expr*, rhs: Expr*, loc: Loc, is_comp_assign: bool, res: OperandResult*);
lib fn sema::check_arithmetic_op_pointer_operand(loc: Loc, operand: Expr*) -> bool;
lib fn sema::check_array_access(base_expr: Expr*, index_expr: Expr*);
lib fn sema::check_addition_operands(lhs: Expr*, rhs: Expr*, tok_loc: Loc, opc: BinaryOperatorKind, comp_lhs_ty: Type*, res: OperandResult*);
lib fn sema::check_subtraction_operands(lhs: Expr*, rhs: Expr*, loc: Loc, comp_lhs_ty: Type*, res: OperandResult*);
lib fn sema::check_multiply_divide_operands(lhs: Expr*, rhs: Expr*, loc: Loc, is_comp_assign: bool, is_div: bool, res: OperandResult*);
lib fn sema::check_remainder_operands(lhs: Expr*, rhs: Expr*, loc: Loc, is_comp_assign: bool, res: OperandResult*);
lib fn sema::check_shift_operands(lhs: Expr*, rhs: Expr*, loc: Loc, opc: BinaryOperatorKind, is_comp_assign: bool, res: OperandResult*);
lib fn sema::check_bitwise_operands(lhs: Expr*, rhs: Expr*, loc: Loc, opc: BinaryOperatorKind, res: OperandResult*);
lib fn sema::check_logical_operands(lhs: Expr*, rhs: Expr*, loc: Loc, opc: BinaryOperatorKind, res: OperandResult*);
lib fn sema::check_for_modifiable_lvalue(e: Expr*, loc: Loc) -> bool;
lib fn sema::check_assignment_operands(lhs: Expr*, rhs: Expr*, op_loc: Loc, compound_type: Type*, opc: BinaryOperatorKind, res: OperandResult*);
lib fn sema::check_compare_operands(lhs: Expr*, rhs: Expr*, loc: Loc, opc: BinaryOperatorKind, res: OperandResult*);
lib fn sema::scalar_type_to_boolean_cast_kind(scalar_ty: Type*) -> CastKind;
// lib fn sema::create_materialize_temporary_expr(*args) -> Expr*;
lib fn sema::imp_cast_expr_to_type(e: Expr*, ty: Type*, kind: CastKind, vk: ValueKind) -> Expr*;
lib fn sema::build_bin_op(scope: Scope*, tok_loc: Loc, opc: BinaryOperatorKind, lhs: Expr*, rhs: Expr*) -> BinaryExpr*;
lib fn sema::check_address_of_operand(e: Expr*, op_loc: Loc) -> Type*;
lib fn sema::check_indirection_operand(e: Expr*, op_loc: Loc) -> Type*;
// lib fn sema::check_increment_decrement_operand(*args);
lib fn sema::build_unary_op(scope: Scope*, op_loc: Loc, opc: UnaryOperatorKind, arg: Expr*) -> UnaryExpr*;
lib fn sema::act_on_bin_op(scope: Scope*, tok_loc: Loc, kind: Tok, lhs: Expr*, rhs: Expr*) -> BinaryExpr*;
lib fn sema::act_on_unary_op(scope: Scope*, op_loc: Loc, op: Tok, arg: Expr*) -> UnaryExpr*;
lib fn sema::act_on_postfix_unary_op(scope: Scope*, op_loc: Loc, op: Tok, arg: Expr*) -> UnaryExpr*;
lib fn sema::lookup_field_in_struct(struct_type: StructType*, name: CStr, out_type: Type**) -> i64;
lib fn sema::lookup_method_in_struct(struct_type: StructType*, name: CStr, loc: Loc) -> FnDecl*;
lib fn sema::build_member_reference_expr(base: Expr*, base_type: Type*, oploc: Loc, is_arrow: bool, name: CStr, name_loc: Loc) -> Expr*;
lib fn sema::act_on_member_access_expr(scope: Scope*, base: Expr*, oploc: Loc, opkind: Tok, ss: void*, name: UnqualifiedId*) -> Expr*;
lib fn sema::create_builtin_array_subscript_expr(base: Expr*, lloc: Loc, idx: Expr*, rloc: Loc) -> Expr*;
lib fn sema::gather_arguments_for_call(call_loc: Loc, fdecl: FnDecl*, proto: FunctionType*, args: Expr**, args_count: i64, all_args: DArray*) -> bool;
lib fn sema::convert_arguments_for_call(call: CallExpr*, fun: Expr*, fdecl: FnDecl*, proto: FunctionType*, args: Expr**, args_count: i64, rparen_loc: Loc, is_exec_config: bool) -> bool;
lib fn sema::build_resolved_call_expr(fun: Expr*, ndecl: NamedDecl*, lparen_loc: Loc, args: Expr**, args_count: i64, rparen_loc: Loc, is_exec_config: bool, uses_adl: i64) -> Expr*;
lib fn sema::build_call_expr(scope: Scope*, fun: Expr*, lparen_loc: Loc, arg_exprs: Expr**, arg_exprs_count: i64, rparen_loc: Loc, is_exec_config: bool, allow_recovery: bool) -> Expr*;
lib fn sema::act_on_call_expr(scope: Scope*, fun: Expr*, lparen_loc: Loc, arg_exprs: Expr**, arg_exprs_count: i64, rparen_loc: Loc) -> Expr*;
lib fn sema::act_on_array_subscript_expr(scope: Scope*, base: Expr*, lbloc: Loc, arg_exprs: Expr**, arg_exprs_count: i64, rbloc: Loc) -> Expr*;
lib fn sema::perform_implicit_conversion(f: Expr*, to_type: Type*, ics: ImplicitConversionSequence*) -> Expr *;
lib fn sema::check_boolean_condition(loc: Loc, cond_expr: Expr*, is_constexpr: bool, fst_checks: bool) -> Expr*;
lib fn sema::check_conditional_operands(cond: Expr*, lhs: Expr*, rhs: Expr*, vk: ValueKind, question_loc: Loc, res: CondOperandResult*);
lib fn sema::act_on_conditional_op(question_loc: Loc, colon_loc: Loc, cond_expr: Expr*, lhs: Expr*, rhs: Expr*) -> ConditionalExpr*;
lib fn sema::act_on_numeric_constant(tok: Token*) -> IntegerLiteral*;
lib fn sema::act_on_character_constant(tok: Token*) -> IntegerLiteral*;
lib fn sema::act_on_bool_literal(op_loc: Loc, kind: Tok) -> BoolLiteral*;
lib fn sema::act_on_string_literal(string_toks: Token**, string_toks_count: i64) -> StringLiteral*;
lib fn sema::build_decl_ref_expr_nns(d: ValueDecl*, ty: Type*, vk: ValueKind, name: CStr, ii: IdentInfo*, nameloc: Loc, nns: void*) -> DeclRefExpr*;
lib fn sema::build_decl_ref_expr(d: ValueDecl*, ty: Type*, vk: ValueKind, name: CStr, ii: IdentInfo*, nameloc: Loc, ss: void*) -> DeclRefExpr*;
lib fn sema::build_declaration_name_expr(ss: void*, name: CStr, ii: IdentInfo*, nameloc: Loc, d: NamedDecl*, accept_invalid_decl: bool) -> Expr*;
lib fn sema::act_on_id_expression(s: Scope*, ss: void*, i: UnqualifiedId*, has_trailing_lparen: bool, keyword_replacement: Token*) -> Expr*;
lib fn sema::act_on_paren_expr(lp_loc: Loc, rp_loc: Loc, e: Expr*) -> ParenExpr*;
lib fn sema::create_recovery_expr(begin: Loc, end: Loc, sub_exprs: Expr**, expr_count: i64, ty: Type*) -> RecoveryExpr*;
lib fn sema::check_completed_expr(e: Expr*, check_loc: Loc, is_constexpr: bool);
lib fn sema::maybe_create_expr_with_cleanups(sub_expr: Expr*) -> Expr*;
lib fn sema::act_on_finish_full_expr(fe: Expr*, cc: Loc, discarded_value: bool, is_constexpr: bool) -> Expr*;
lib fn sema::ignored_value_conversions(e: Expr*) -> Expr*;
lib fn sema::act_on_nullptr_literal(loc: Loc) -> Expr*;
lib fn sema::act_on_explicit_cast(ty: Type*, e: Expr*, sl: Loc, el: Loc) -> Expr*;
lib fn sema::act_on_vaarg_expr(ty: Type*, sl: Loc, el: Loc) -> VAArgExpr*;
lib fn sema::act_on_sizeof_expr(ty: Type*, expr: Expr*, sl: Loc, el: Loc) -> SizeofExpr*;
lib fn sema::act_on_scoped_identifier(scope: Scope*, ii: IdentInfo*, loc: Loc) -> StructType*;
