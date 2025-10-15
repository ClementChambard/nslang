#ifndef SEMA_SEMA_HPP_INCLUDED
#define SEMA_SEMA_HPP_INCLUDED

#include "ast/context.hpp"
#include "ast/nodes/decl.hpp"
#include "ast/nodes/expr.hpp"
#include "ast/nodes/stmt.hpp"
#include "ast/nodes/type.hpp"
#include "lexer/ident.hpp"
#include "lexer/token.hpp"
#include "semantic_analysis/scope.hpp"
#include <memory>

template <typename T> using UPtr = std::unique_ptr<T>;

struct Sema {
  ASTContext &ctx;
  FunctionDecl *cur_fn_decl;

  Sema(ASTContext &ctx) : ctx(ctx) {}

  // decl sema
  UPtr<FunctionDecl> act_on_fn_decl(Scope *scope, IdentInfo *name,
                                    std::vector<UPtr<ParamDecl>> &params,
                                    Type *return_type, Loc fn_loc, Loc semi_loc,
                                    bool is_vararg, StructDecl *struct_scope,
                                    bool has_init);
  UPtr<FunctionDecl>
  act_on_start_fn_definition(Scope *scope, IdentInfo *name,
                             std::vector<UPtr<ParamDecl>> &params,
                             Type *return_type, Loc fn_loc, bool is_vararg,
                             StructDecl *struct_scope, bool has_init);
  UPtr<FunctionDecl> act_on_end_fn_definition(UPtr<FunctionDecl> decl,
                                              UPtr<CompoundStmt> body);
  std::pair<std::string, StructType *>
  act_on_method_decl_name(Scope *scope, std::string struct_name, Loc loc,
                          std::string method_name, Loc name_loc);
  void act_on_start_of_translation_unit();
  void act_on_end_of_translation_unit();
  DeclUPtr act_on_lib_decl(DeclUPtr decl, Loc lib_loc);
  UPtr<EnumDecl> act_on_enum_decl(Scope *scope, Loc kw_loc, Loc end_loc,
                                  IdentInfo *name, Type *aliased_type);
  UPtr<EnumDecl> act_on_start_enum_decl(Scope *scope, Loc kw_loc,
                                        IdentInfo *name, Type *aliased_type);
  UPtr<EnumDecl> act_on_end_enum_decl(UPtr<EnumDecl> decl, Loc end_loc);
  void act_on_enum_variant_decl(Scope *scope, Loc start_loc, Loc end_loc,
                                IdentInfo *name, ExprUPtr e, EnumDecl *decl,
                                i64 val);
  UPtr<ParamDecl> act_on_param_decl(Scope *scope, Loc id_loc, Loc end_loc,
                                    IdentInfo *name, Type *type);
  UPtr<ParamDecl> act_on_valist_param_decl(Loc sl, Loc el);
  UPtr<VarDecl> act_on_var_decl(Scope *scope, Loc kw_loc, Loc id_loc,
                                Loc end_loc, IdentInfo *name, Type *type,
                                bool global, ExprUPtr initializer);

  UPtr<VarDecl> act_on_var_decl_init_method(Scope *scope, Loc kw_loc,
                                            Loc id_loc, Loc end_loc,
                                            Loc mname_loc, IdentInfo *name,
                                            Type *type,
                                            FunctionDecl const *method,
                                            std::vector<ExprUPtr> &&args);
  void act_on_field_decl(StructDecl *scope, Loc id_loc, Loc end_loc,
                         IdentInfo *name, Type *type);
  UPtr<AliasDecl> act_on_alias_decl(Scope *scope, Loc kw_loc, Loc end_loc,
                                    IdentInfo *name, Type *aliased_type);
  UPtr<StructDecl> act_on_struct_decl(Scope *scope, Loc kw_loc, Loc id_loc,
                                      IdentInfo *type_name);
  UPtr<StructDecl> act_on_start_struct_decl(Scope *scope, Loc kw_loc,
                                            Loc id_loc, IdentInfo *type_name);
  UPtr<StructDecl> act_on_end_struct_decl(UPtr<StructDecl> decl, Loc end_loc);

  // stmt sema
  ExprUPtr act_on_expr_stmt(ExprUPtr fe, bool discarded_value);
  StmtUPtr act_on_expr_stmt_error();
  UPtr<CompoundStmt> act_on_compound_stmt(Loc lp_loc, Loc rp_loc,
                                          std::vector<StmtUPtr> &&elts);
  UPtr<DeclStmt> act_on_decl_stmt(DeclUPtr d, Loc start_loc, Loc end_loc);
  UPtr<IfStmt> act_on_if_stmt(Loc il, Loc lp, ExprUPtr cond, Loc rp,
                              StmtUPtr ts, Loc el, StmtUPtr es);
  UPtr<SwitchStmt> act_on_start_of_switch_stmt(Loc switch_loc, Loc lp_loc,
                                               ExprUPtr cond, Loc rp_loc);
  UPtr<SwitchStmt> act_on_finish_switch_stmt(Loc sl, UPtr<SwitchStmt> stmt,
                                             StmtUPtr body);
  UPtr<WhileStmt> act_on_while_stmt(Loc while_loc, Loc lparen_loc,
                                    ExprUPtr cond, Loc rparen_loc,
                                    StmtUPtr body);
  UPtr<DoStmt> act_on_do_stmt(Loc do_loc, StmtUPtr body, Loc while_loc,
                              Loc lp_loc, ExprUPtr cond, Loc rp_loc);
  UPtr<DefaultStmt> act_on_default_stmt(Loc default_loc, Loc colon_loc,
                                        StmtUPtr sub_stmt, Scope *scope);
  ExprUPtr act_on_case_expr(Loc case_loc, ExprUPtr val);
  UPtr<CaseStmt> act_on_case_stmt(Loc case_loc, ExprUPtr e, Loc colon_loc);
  void act_on_case_stmt_body(CaseStmt *stmt, StmtUPtr sub_stmt);
  UPtr<ContinueStmt> act_on_continue_stmt(Loc cl, Scope *cur_scope);
  UPtr<BreakStmt> act_on_break_stmt(Loc bl, Scope *cur_scope);
  UPtr<NullStmt> act_on_null_stmt(Loc semi_loc);
  UPtr<ReturnStmt> act_on_return_stmt(Loc return_loc, ExprUPtr ret_val_expr,
                                      Scope *scope,
                                      bool allow_recovery = false);
  void diagnose_unused_expr_result(Stmt *s, std::string const &diag_id);

  struct OperandsRes {
    ExprUPtr lhs, rhs;
    Type *type;
  };
  struct CondOperandsRes {
    ExprUPtr cond, true_e, false_e;
    Type *type;
    ValueKind vk;
  };

  // expr sema
  Type *invalid_operands(Loc loc, ExprUPtr &lhs, ExprUPtr &rhs);
  ExprUPtr default_function_array_conversion(ExprUPtr e, bool diagnose = true);
  ExprUPtr default_lvalue_conversion(ExprUPtr e);
  ExprUPtr default_function_array_lvalue_conversion(ExprUPtr e,
                                                    bool diagnose = true);
  CastExpr::CastKind scalar_type_to_boolean_cast_kind(Type *scalar_ty);
  CastExpr::CastKind prepare_scalar_cast(ExprUPtr &src, Type *dest_ty);
  ExprUPtr usual_unary_conversions(ExprUPtr expr);
  Type *usual_arithmetic_conversions(ExprUPtr &lhs, ExprUPtr &rhs,
                                     bool is_comp_assign);
  void check_array_access(Expr *base_expr, Expr *index_expr);
  Type *check_addition_operands(ExprUPtr &lhs, ExprUPtr &rhs, Loc tok_loc,
                                Type **comp_lhs_ty = nullptr);
  Type *check_subtraction_operands(ExprUPtr &lhs, ExprUPtr &rhs, Loc loc,
                                   Type **comp_lhs_ty = nullptr);
  Type *check_multiply_divide_operands(ExprUPtr &lhs, ExprUPtr &rhs, Loc loc,
                                       BinaryExpr::OpKind opc);
  Type *check_remainder_operands(ExprUPtr &lhs, ExprUPtr &rhs, Loc loc,
                                 bool is_comp_assign);
  Type *check_shift_operands(ExprUPtr &lhs, ExprUPtr &rhs, Loc loc,
                             BinaryExpr::OpKind opc, bool is_comp_assign);
  Type *check_bitwise_operands(ExprUPtr &lhs, ExprUPtr &rhs, Loc loc,
                               BinaryExpr::OpKind opc);
  Type *check_logical_operands(ExprUPtr &lhs, ExprUPtr &rhs, Loc loc,
                               BinaryExpr::OpKind opc);
  Type *check_assignment_operands(Expr *lhs_expr, ExprUPtr &rhs, Loc loc,
                                  Type *compound_type, BinaryExpr::OpKind opc);
  Type *check_compare_operands(ExprUPtr &lhs, ExprUPtr &rhs, Loc loc,
                               BinaryExpr::OpKind opc);
  // ExprUPtr create_materialize_temporary_expr(*args);
  ExprUPtr imp_cast_expr_to_type(ExprUPtr e, Type *ty, CastExpr::CastKind kind,
                                 ValueKind vk = ValueKind::PRVALUE);
  UPtr<BinaryExpr> build_bin_op(Scope *scope, Loc tok_loc,
                                BinaryExpr::OpKind opc, ExprUPtr lhs,
                                ExprUPtr rhs);
  Type *check_address_of_operand(ExprUPtr &e, Loc op_loc);
  Type *check_indirection_operand(ExprUPtr &e, ValueKind &vk, Loc op_loc);
  Type *check_increment_decrement_operand(ExprUPtr &e, ValueKind &vk,
                                          Loc op_loc, bool is_inc,
                                          bool is_prefix);
  UPtr<UnaryExpr> build_unary_op(Scope *scope, Loc op_loc,
                                 UnaryExpr::OpKind opc, ExprUPtr arg);
  UPtr<BinaryExpr> act_on_bin_op(Scope *scope, Loc tok_loc, Tok kind,
                                 ExprUPtr lhs, ExprUPtr rhs);
  UPtr<UnaryExpr> act_on_unary_op(Scope *scope, Loc op_loc, Tok op,
                                  ExprUPtr arg);
  UPtr<UnaryExpr> act_on_postfix_unary_op(Scope *scope, Loc op_loc, Tok op,
                                          ExprUPtr arg);
  FieldDecl const *lookup_field_in_struct(StructDecl const *struct_type,
                                          IdentInfo const *name);
  FunctionDecl const *lookup_method_in_struct(StructDecl const *struct_type,
                                              IdentInfo const *name);
  ExprUPtr build_member_reference_expr(ExprUPtr base, Type *base_type,
                                       Loc oploc, bool is_arrow, Loc name_loc,
                                       IdentInfo *name_id);
  ExprUPtr act_on_member_access_expr(Scope *scope, ExprUPtr base, Loc oploc,
                                     Tok opkind, Loc name_loc,
                                     IdentInfo *name_id);
  ExprUPtr create_builtin_array_subscript_expr(ExprUPtr base, Loc lloc,
                                               ExprUPtr idx, Loc rloc);
  ExprUPtr default_argument_promotion(ExprUPtr arg);
  ExprUPtr build_call_expr(Scope *scope, ExprUPtr fn, Loc lparen_loc,
                           std::vector<ExprUPtr> &&arg_exprs, Loc rparen_loc);
  ExprUPtr act_on_call_expr(Scope *scope, ExprUPtr fn, Loc lparen_loc,
                            std::vector<ExprUPtr> &&arg_exprs, Loc rparen_loc);
  ExprUPtr act_on_array_subscript_expr(Scope *scope, ExprUPtr base, Loc lbloc,
                                       ExprUPtr arg, Loc rbloc);
  ExprUPtr perform_implicit_conversion(ExprUPtr f, Type *to_type,
                                       struct ConversionSequence *ics);
  ExprUPtr check_boolean_condition(ExprUPtr cond_expr,
                                   bool is_constexpr = false);
  Type *check_conditional_operands(ExprUPtr &cond, ExprUPtr &lhs, ExprUPtr &rhs,
                                   ValueKind &vk, Loc question_loc);
  UPtr<ConditionalExpr> act_on_conditional_op(Loc question_loc, Loc colon_loc,
                                              ExprUPtr cond_expr, ExprUPtr lhs,
                                              ExprUPtr rhs);
  ExprUPtr act_on_numeric_constant(Token tok);
  UPtr<CharLiteral> act_on_character_constant(Token tok);
  UPtr<BoolLiteral> act_on_bool_literal(Loc op_loc, Tok kind);
  UPtr<StringLiteral>
  act_on_string_literal(std::vector<Token> const &string_toks);
  UPtr<DeclRefExpr> build_decl_ref_expr(ValueDecl *d, Type *ty, ValueKind vk,
                                        IdentInfo *ii, Loc nameloc,
                                        StructDecl *struct_scope);
  ExprUPtr act_on_id_expression(Scope *scope, StructDecl *struct_scope,
                                IdentInfo *ident, Loc ident_loc);
  UPtr<ParenExpr> act_on_paren_expr(Loc lp_loc, Loc rp_loc, ExprUPtr e);
  UPtr<RecoveryExpr> create_recovery_expr(Loc begin, Loc end,
                                          std::vector<ExprUPtr> &&sub_exprs,
                                          Type *t = nullptr);
  void check_completed_expr(Expr *e, Loc check_loc, bool is_constexpr);
  ExprUPtr maybe_create_expr_with_cleanups(ExprUPtr sub_expr);
  ExprUPtr act_on_finish_full_expr(ExprUPtr fe, Loc cc, bool discarded_value,
                                   bool is_constexpr = false);
  ExprUPtr ignored_value_conversions(ExprUPtr e);
  ExprUPtr act_on_explicit_cast(Type *ty, ExprUPtr e, Loc sl, Loc el);
  UPtr<NullptrLiteral> act_on_nullptr_literal(Loc loc);
  UPtr<VAArgExpr> act_on_vaarg_expr(Type *ty, Loc sl, Loc el);
  UPtr<VAArgsExpr> act_on_vaargs_expr(Loc loc);
  UPtr<SizeofExpr> act_on_sizeof_expr(Type *ty, ExprUPtr expr, Loc sl, Loc el);
  StructDecl *act_on_scoped_identifier(Scope *scope, IdentInfo *ii, Loc loc);
  struct ConversionSequence try_implicit_conversion(Expr *f, Type *to_type,
                                                    bool is_explicit = false);
  struct ConversionSequence try_contextually_convert_to_bool(Expr *f);
  ExprUPtr perform_contextually_convert_to_bool(ExprUPtr f);
  i64 eval_integer_constexpr(Expr *e);
};

#endif // SEMA_SEMA_HPP_INCLUDED
