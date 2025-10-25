#ifndef AST_NODES_EXPR_HPP_INCLUDED
#define AST_NODES_EXPR_HPP_INCLUDED

#include "ast/nodes/decl.hpp"
#include "lexer/ident.hpp"
#include "lexer/loc.hpp"
#include "lexer/tok.hpp"
#include "stmt.hpp"
#include "type.hpp"
#include <memory>
#include <span>

enum class ValueKind {
  PRVALUE,
  LVALUE,
  XVALUE,
};

struct Expr : public Stmt {
  Type *type;
  ValueKind vk;

  Expr(Kind k, Type *type, ValueKind vk) : Stmt(k), type(type), vk(vk) {}

  static bool is_class(Kind k) { return k >= DECLREF_EXPR; }

  Expr *ignore_imp_casts();
  Expr const *ignore_imp_casts() const { return const_cast<Expr*>(this)->ignore_imp_casts(); }
  Expr *ignore_parens();
  Expr const *ignore_parens() const { return const_cast<Expr*>(this)->ignore_parens(); }
  Expr *ignore_paren_imp_casts();
  Expr const *ignore_paren_imp_casts() const { return const_cast<Expr*>(this)->ignore_paren_imp_casts(); }

  bool is_lvalue() const { return vk == ValueKind::LVALUE; }
  bool is_prvalue() const { return vk == ValueKind::PRVALUE; }
  bool is_xvalue() const { return vk == ValueKind::XVALUE; }
  bool is_glvalue() const { return vk != ValueKind::PRVALUE; }

  enum LValueClassification {
    LV_Valid,
    LV_NotObjectType,
    LV_IncompleteVoidType,
    LV_DuplicateVectorComponents,
    LV_InvalidExpression,
    LV_InvalidMessageExpression,
    LV_MemberFunction,
    LV_SubObjCPropertySetting,
    LV_ClassTemporary,
    LV_ArrayTemporary
  };
  LValueClassification classify_lvalue() const;

  enum isModifiableLvalueResult {
    MLV_Valid,
    MLV_NotObjectType,
    MLV_IncompleteVoidType,
    MLV_DuplicateVectorComponents,
    MLV_InvalidExpression,
    MLV_LValueCast,           // Specialized form of MLV_InvalidExpression.
    MLV_IncompleteType,
    MLV_ConstQualified,
    MLV_ConstQualifiedField,
    MLV_ConstAddrSpace,
    MLV_ArrayType,
    MLV_NoSetterProperty,
    MLV_MemberFunction,
    MLV_SubObjCPropertySetting,
    MLV_InvalidMessageExpression,
    MLV_ClassTemporary,
    MLV_ArrayTemporary
  };
  isModifiableLvalueResult is_modifiable_lvalue(Loc *loc = nullptr) const;

  struct Classification {
    /// The various classification results. Most of these mean prvalue.
    enum Kinds : u16 {
      CL_LValue,
      CL_XValue,
      CL_Function, // Functions cannot be lvalues in C.
      CL_Void, // Void cannot be an lvalue in C.
      CL_AddressableVoid, // Void expression whose address can be taken in C.
      CL_DuplicateVectorComponents, // A vector shuffle with dupes.
      CL_MemberFunction, // An expression referring to a member function
      CL_SubObjCPropertySetting,
      CL_ClassTemporary, // A temporary of class type, or subobject thereof.
      CL_ArrayTemporary, // A temporary of array type.
      CL_ObjCMessageRValue, // ObjC message is an rvalue
      CL_PRValue // A prvalue for any other reason, of any other type
    };
    /// The results of modification testing.
    enum ModifiableType : u16 {
      CM_Untested, // testModifiable was false.
      CM_Modifiable,
      CM_RValue, // Not modifiable because it's an rvalue
      CM_Function, // Not modifiable because it's a function; C++ only
      CM_LValueCast, // Same as CM_RValue, but indicates GCC cast-as-lvalue ext
      CM_NoSetterProperty,// Implicit assignment to ObjC property without setter
      CM_ConstQualified,
      CM_ConstQualifiedField,
      CM_ConstAddrSpace,
      CM_ArrayType,
      CM_IncompleteType
    };

    Kinds kind;
    ModifiableType modifiable;

    explicit Classification(Kinds k, ModifiableType m)
      : kind(k), modifiable(m) {}
    Classification() {}

    bool is_lvalue() const { return kind == CL_LValue; }
    bool is_xvalue() const { return kind == CL_XValue; }
    bool is_glvalue() const { return kind <= CL_XValue; }
    bool is_prvalue() const { return kind >= CL_Function; }
    bool is_rvalue() const { return kind >= CL_XValue; }
    bool is_modifiable() const { return modifiable == CM_Modifiable; }

    static Classification make_simple_lvalue() {
      return Classification(CL_LValue, CM_Modifiable);
    }
  };

  Classification classify() const {
    return classify_impl(nullptr);
  }
  Classification classify_modifiable(Loc &loc) const {
    return classify_impl(&loc);
  }

private:
  Classification classify_impl(Loc *loc) const;
};

using ExprUPtr = std::unique_ptr<Expr>;

struct ValueDecl;

struct DeclRefExpr : public Expr {
  ValueDecl const *decl;
  IdentInfo const *ident;
  Loc ident_loc;

  DeclRefExpr(ValueDecl const *d, IdentInfo const *i, Loc l, Type *type, ValueKind vk)
      : Expr(DECLREF_EXPR, type, vk), decl(d), ident(i), ident_loc(l) {}

  Loc get_start_loc() const override { return ident_loc; }
  Loc get_end_loc() const override { return ident_loc; }

  static bool is_class(Kind k) { return k == DECLREF_EXPR; }
};

struct IntegerLiteral : public Expr {
  u64 value;
  Loc loc;

  IntegerLiteral(u64 v, Loc l, Type *type)
      : Expr(INTEGER_LITERAL, type, ValueKind::PRVALUE), value(v), loc(l) {}

  Loc get_start_loc() const override { return loc; }
  Loc get_end_loc() const override { return loc; }
  static bool is_class(Kind k) { return k == INTEGER_LITERAL; }
};

struct FloatingLiteral : public Expr {
  f64 value;
  Loc loc;

  FloatingLiteral(f64 v, Loc l, Type *type)
      : Expr(FLOATING_LITERAL, type, ValueKind::PRVALUE), value(v), loc(l) {}

  Loc get_start_loc() const override { return loc; }
  Loc get_end_loc() const override { return loc; }
  static bool is_class(Kind k) { return k == FLOATING_LITERAL; }
};

struct BoolLiteral : public Expr {
  bool value;
  Loc loc;

  BoolLiteral(bool v, Loc l, Type *type)
      : Expr(BOOL_LITERAL, type, ValueKind::PRVALUE), value(v), loc(l) {}

  Loc get_start_loc() const override { return loc; }
  Loc get_end_loc() const override { return loc; }
  static bool is_class(Kind k) { return k == BOOL_LITERAL; }
};

struct CharLiteral : public Expr {
  i64 value;
  Loc loc;

  CharLiteral(i64 v, Loc l, Type *type)
      : Expr(CHAR_LITERAL, type, ValueKind::PRVALUE), value(v), loc(l) {}

  Loc get_start_loc() const override { return loc; }
  Loc get_end_loc() const override { return loc; }
  static bool is_class(Kind k) { return k == CHAR_LITERAL; }
};

struct NullptrLiteral : public Expr {
  Loc loc;

  NullptrLiteral(Loc l, Type *type)
      : Expr(NULLPTR_LITERAL, type, ValueKind::PRVALUE), loc(l) {}

  Loc get_start_loc() const override { return loc; }
  Loc get_end_loc() const override { return loc; }
  static bool is_class(Kind k) { return k == NULLPTR_LITERAL; }
};

struct StringLiteral : public Expr {
  std::string value;
  std::vector<Loc> locs;

  StringLiteral(std::string const &value, std::span<Loc> locs, Type *type)
      : Expr(STRING_LITERAL, type, ValueKind::LVALUE), value(value),
        locs(locs.begin(), locs.end()) {}

  Loc get_start_loc() const override { return locs.front(); }
  Loc get_end_loc() const override { return locs.back(); }
  static bool is_class(Kind k) { return k == STRING_LITERAL; }
};

struct ParenExpr : public Expr {
  ExprUPtr val;
  Loc lp;
  Loc rp;

  ParenExpr(ExprUPtr val, Loc lp, Loc rp)
      : Expr(PAREN_EXPR, val->type, val->vk), val(std::move(val)), lp(lp),
        rp(rp) {}

  Loc get_start_loc() const override { return lp; }
  Loc get_end_loc() const override { return rp; }
  static bool is_class(Kind k) { return k == PAREN_EXPR; }
};

struct UnaryExpr : public Expr {
  enum OpKind {
    POSTINC,
    POSTDEC,
    PREINC,
    PREDEC,
    ADDROF,
    DEREF,
    PLUS,
    MINUS,
    NOT,
    LNOT,
  };

  static OpKind opc_from_tok(Tok t);
  static cstr opc_name(OpKind k);

  OpKind opc;
  Loc oploc;
  ExprUPtr arg;

  UnaryExpr(ExprUPtr arg, OpKind opc, Loc oploc, Type *ty,
            ValueKind vk)
      : Expr(UNARY_EXPR, ty, vk), opc(opc), oploc(oploc), arg(std::move(arg)) {}

  bool is_postfix() const { return opc >= POSTINC && opc <= POSTDEC; }

  Loc get_start_loc() const override;
  Loc get_end_loc() const override;
  static bool is_class(Kind k) { return k == UNARY_EXPR; }
};

struct BinaryExpr : public Expr {
  enum OpKind {
    MUL,
    DIV,
    REM,
    ADD,
    SUB,
    SHL,
    SHR,
    AND,
    XOR,
    OR,
    LT,
    GT,
    LE,
    GE,
    EQ,
    NE,
    LAND,
    LOR,
    ASSIGN,
    MULASSIGN,
    DIVASSIGN,
    REMASSIGN,
    ADDASSIGN,
    SUBASSIGN,
    SHLASSIGN,
    SHRASSIGN,
    ANDASSIGN,
    XORASSIGN,
    ORASSIGN,
  };

  static OpKind opc_from_tok(Tok t);
  static cstr opc_name(OpKind k);

  OpKind opc;
  Loc oploc;
  ExprUPtr lhs;
  ExprUPtr rhs;

  BinaryExpr(ExprUPtr lhs, ExprUPtr rhs, OpKind opc,
             Loc oploc, Type *ty, ValueKind vk)
      : Expr(BINARY_EXPR, ty, vk), opc(opc), oploc(oploc), lhs(std::move(lhs)),
        rhs(std::move(rhs)) {}

  bool is_assignment_op() const { return opc >= ASSIGN; }

  Loc get_start_loc() const override { return lhs->get_start_loc(); }
  Loc get_end_loc() const override { return rhs->get_end_loc(); }
  static bool is_class(Kind k) {
    return k >= BINARY_EXPR && k <= COMPOUND_ASSIGN_EXPR;
  }
};

struct CompoundAssignExpr : public BinaryExpr {
  CompoundAssignExpr(ExprUPtr lhs, ExprUPtr rhs,
                     OpKind opc, Loc oploc, Type *ty, ValueKind vk)
      : BinaryExpr(std::move(lhs), std::move(rhs), opc, oploc, ty, vk) {
    kind = COMPOUND_ASSIGN_EXPR;
  }
  static bool is_class(Kind k) { return k == COMPOUND_ASSIGN_EXPR; }
};

struct CallExpr : public Expr {
  Loc rp;
  ExprUPtr fn;
  std::vector<ExprUPtr> args;

  CallExpr(ExprUPtr fn, std::vector<ExprUPtr> &&args,
           Loc rp, Type *ty, ValueKind vk)
      : Expr(CALL_EXPR, ty, vk), rp(rp), fn(std::move(fn)),
        args(std::move(args)) {}
  Loc get_start_loc() const override { return fn->get_start_loc(); }
  Loc get_end_loc() const override { return rp; }
  static bool is_class(Kind k) {
    return k >= CALL_EXPR && k <= INIT_CALL_EXPR;
  }
};

struct MethodCallExpr : public CallExpr {
  MethodCallExpr(ExprUPtr fn,
                 std::vector<ExprUPtr> &&args, Loc rp, Type *ty,
                 ValueKind vk)
      : CallExpr(std::move(fn), std::move(args), rp, ty, vk) {
    kind = METHOD_CALL_EXPR;
  }
  static bool is_class(Kind k) { return k == METHOD_CALL_EXPR; }
};

struct InitCallExpr : public CallExpr {
  InitCallExpr(ExprUPtr fn, std::vector<ExprUPtr> &&args, Loc rp, Type *ty) 
    : CallExpr(std::move(fn), std::move(args), rp, ty, ValueKind::PRVALUE) {
      kind = INIT_CALL_EXPR;
    }
  static bool is_class(Kind k) { return k == INIT_CALL_EXPR; }
};

struct SizeofExpr : public Expr {
  ExprUPtr expr;
  Type *ty_of_sizeof;
  Loc kw_loc;
  Loc rp;

  SizeofExpr(Type *ty_of_sizeof, ExprUPtr expr, Loc kw, Loc rp,
             Type *ty)
      : Expr(SIZEOF_EXPR, ty, ValueKind::PRVALUE), expr(std::move(expr)),
        ty_of_sizeof(ty_of_sizeof), kw_loc(kw), rp(rp) {}
  Loc get_start_loc() const override { return kw_loc; }
  Loc get_end_loc() const override { return rp; }
  static bool is_class(Kind k) { return k == SIZEOF_EXPR; }
};

struct MemberExpr : public Expr {
  ExprUPtr base;
  const struct FieldDecl *field_decl;
  IdentInfo *name;
  Loc idloc;
  bool is_arrow;

  MemberExpr(ExprUPtr base, IdentInfo *name, Loc idloc, const struct FieldDecl *d,
             bool is_arrow, Type *ty, ValueKind vk)
      : Expr(MEMBER_EXPR, ty, vk), base(std::move(base)), field_decl(d), name(name),
        idloc(idloc), is_arrow(is_arrow) {}
  Loc get_start_loc() const override { return base->get_start_loc(); }
  Loc get_end_loc() const override { return idloc; }
  static bool is_class(Kind k) { return k >= MEMBER_EXPR && k <= METHOD_EXPR; }
};

struct MethodExpr : public MemberExpr {
  const struct FunctionDecl *method_func;

  MethodExpr(ExprUPtr base, IdentInfo *name,
             const struct FunctionDecl *m, Loc idloc, bool is_arrow, Type *ty,
             ValueKind vk)
      : MemberExpr(std::move(base), name, idloc, nullptr, is_arrow, ty, vk),
        method_func(m) {
    kind = METHOD_EXPR;
  }
  static bool is_class(Kind k) { return k == METHOD_EXPR; }
};

struct ArraySubscriptExpr : public Expr {
  ExprUPtr lhs;
  ExprUPtr rhs;
  Loc rb;

  ArraySubscriptExpr(ExprUPtr lhs, ExprUPtr rhs,
                     Loc rb, Type *ty, ValueKind vk)
      : Expr(ARRAY_SUBSCRIPT_EXPR, ty, vk), lhs(std::move(lhs)),
        rhs(std::move(rhs)), rb(rb) {}
  Loc get_start_loc() const override { return lhs->get_start_loc(); }
  Loc get_end_loc() const override { return rb; }
  static bool is_class(Kind k) { return k == ARRAY_SUBSCRIPT_EXPR; }
};

struct ConditionalExpr : public Expr {
  ExprUPtr cond;
  ExprUPtr true_expr;
  ExprUPtr false_expr;
  Loc ql;
  Loc cl;

  ConditionalExpr(ExprUPtr cond, ExprUPtr true_expr,
                  ExprUPtr false_expr, Loc ql, Loc cl, Type *ty,
                  ValueKind vk)
      : Expr(CONDITIONAL_EXPR, ty, vk), cond(std::move(cond)),
        true_expr(std::move(true_expr)), false_expr(std::move(false_expr)),
        ql(ql), cl(cl) {}
  Loc get_start_loc() const override { return cond->get_start_loc(); }
  Loc get_end_loc() const override { return false_expr->get_end_loc(); }
  static bool is_class(Kind k) { return k == CONDITIONAL_EXPR; }
};

struct RecoveryExpr : public Expr {
  std::vector<ExprUPtr> sub_exprs;
  LocRge rge;

  RecoveryExpr(std::vector<ExprUPtr> &&sub_exprs, LocRge rge,
               Type *ty, ValueKind vk)
      : Expr(RECOVERY_EXPR, ty, vk), sub_exprs(std::move(sub_exprs)), rge(rge) {
  }

  Loc get_start_loc() const override { return rge.start; }
  Loc get_end_loc() const override { return rge.end; }
  static bool is_class(Kind k) { return k == RECOVERY_EXPR; }
};

struct CastExpr : public Expr {
  enum CastKind {
    NOOP,
    TO_VOID,
    LVALUE_TO_RVALUE,
    POINTER_TO_BOOLEAN,
    POINTER_TO_INTEGRAL,
    INTEGRAL_TO_BOOLEAN,
    INTEGRAL_TO_POINTER,
    INTEGRAL_TO_FLOATING,
    INTEGRAL_CAST,
    FLOATING_TO_BOOLEAN,
    FLOATING_TO_INTEGRAL,
    FLOATING_CAST,
    ARRAY_TO_POINTER_DECAY,
    FUNCTION_TO_POINTER_DECAY,
    NULL_TO_POINTER,
  };

  static cstr kind_to_str(CastKind k);

  ExprUPtr op;
  CastKind cast_kind;

  CastExpr(Kind k, ExprUPtr op, CastKind ck, Type *ty,
           ValueKind vk)
      : Expr(k, ty, vk), op(std::move(op)), cast_kind(ck) {}

  static bool is_class(Kind k) {
    return k >= IMPLICIT_CAST_EXPR && k <= EXPLICIT_CAST_EXPR;
  }
};

struct ImplicitCastExpr : public CastExpr {
  ImplicitCastExpr(ExprUPtr op, CastKind ck, Type *ty,
                   ValueKind vk)
      : CastExpr(IMPLICIT_CAST_EXPR, std::move(op), ck, ty, vk) {}
  Loc get_start_loc() const override { return op->get_start_loc(); }
  Loc get_end_loc() const override { return op->get_end_loc(); }
  static bool is_class(Kind k) { return k == IMPLICIT_CAST_EXPR; }
};

struct ExplicitCastExpr : public CastExpr {
  Loc kw_loc;
  Loc rp;

  ExplicitCastExpr(ExprUPtr op, CastKind ck, Loc kw, Loc rp,
                   Type *ty, ValueKind vk)
      : CastExpr(EXPLICIT_CAST_EXPR, std::move(op), ck, ty, vk), kw_loc(kw),
        rp(rp) {}
  Loc get_start_loc() const override { return kw_loc; }
  Loc get_end_loc() const override { return rp; }
  static bool is_class(Kind k) { return k == EXPLICIT_CAST_EXPR; }
};

struct VAArgExpr : public Expr {
  LocRge rge;

  VAArgExpr(LocRge rge, Type *ty)
      : Expr(VAARG_EXPR, ty, ValueKind::PRVALUE), rge(rge) {}
  Loc get_start_loc() const override { return rge.start; }
  Loc get_end_loc() const override { return rge.end; }
  static bool is_class(Kind k) { return k == VAARG_EXPR; }
};

struct VAArgsExpr : public Expr {
  Loc l;

  VAArgsExpr(Loc l, Type *ty) : Expr(VAARGS_EXPR, ty, ValueKind::PRVALUE), l(l) {}
  Loc get_start_loc() const override { return l; }
  Loc get_end_loc() const override { return l; }
  static bool is_class(Kind k) { return k == VAARGS_EXPR; }
};

#endif // AST_NODES_EXPR_HPP_INCLUDED
