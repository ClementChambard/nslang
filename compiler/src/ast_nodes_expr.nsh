#include ast_nodes_stmt.nsh
#include loc.nsh
#include str.nsh
#include tok.nsh

struct Type;
struct ValueDecl;
struct IdentInfo;
struct FnDecl;

enum ValueKind {
  VK_PRVALUE,
  VK_LVALUE,
  VK_XVALUE,
};

struct Expr {
  super base_stmt: Stmt;
  value_kind: ValueKind;
  ty: Type*;
};

// lib fn Expr::ignore_expr_nodes(self: Expr*, ignore_parens_single_step: void(void));

struct DeclRefExpr {
  super base: Expr;
  decl: ValueDecl*;
  ii: IdentInfo*;
  loc: Loc;
  qualifier: void*;
};

lib fn DeclRefExpr::new(nns: void*, d: ValueDecl*, name: CStr, ii: IdentInfo*, nameloc: Loc, ty: Type*, vk: ValueKind) -> DeclRefExpr*;

struct IntegerLiteral {
  super base: Expr;
  value: i64;
  loc: Loc;
};

lib fn IntegerLiteral::new(v: i64, ty: Type*, l: Loc) -> IntegerLiteral*;

struct BoolLiteral {
  super base: Expr;
  value: bool;
  loc: Loc;
};

lib fn BoolLiteral::new(v: bool, ty: Type*, l: Loc) -> BoolLiteral*;

struct StringLiteral {
  super base: Expr;
  value: CStr;
  locs: Loc*;
  locs_count: i64;
  str_kind: void*;
};

lib fn StringLiteral::new(v: CStr, locs: Loc*, locs_count: i64, kind: void*, ty: Type*) -> StringLiteral*;

struct ParenExpr {
  super base: Expr;
  l: Loc;
  r: Loc;
  val: Expr*;
};

lib fn ParenExpr::new(l: Loc, r: Loc, val: Expr*) -> ParenExpr*;

enum UnaryOperatorKind {
  UOK_POSTINC,
  UOK_POSTDEC,
  UOK_PREINC,
  UOK_PREDEC,
  UOK_ADDROF,
  UOK_DEREF,
  UOK_PLUS,
  UOK_MINUS,
  UOK_NOT,
  UOK_LNOT,
};

lib fn unary_operator_kind_from_tok(t: Tok) -> UnaryOperatorKind;
lib fn unary_operator_kind_str(k: UnaryOperatorKind) -> CStr;

struct UnaryExpr {
  super base: Expr;
  opc: UnaryOperatorKind;
  loc: Loc;
  arg: Expr*;
};

lib fn UnaryExpr::new(arg: Expr*, opc: UnaryOperatorKind, ty: Type*, vk: ValueKind, l: Loc) -> UnaryExpr*;
lib fn UnaryExpr::is_postfix(self: UnaryExpr*) -> bool;

enum BinaryOperatorKind {
    BOK_MUL,
    BOK_DIV,
    BOK_REM,
    BOK_ADD,
    BOK_SUB,
    BOK_SHL,
    BOK_SHR,
    BOK_LT,
    BOK_GT,
    BOK_LE,
    BOK_GE,
    BOK_EQ,
    BOK_NE,
    BOK_AND,
    BOK_XOR,
    BOK_OR,
    BOK_LAND,
    BOK_LOR,
    BOK_ASSIGN,
    BOK_MULASSIGN,
    BOK_DIVASSIGN,
    BOK_REMASSIGN,
    BOK_ADDASSIGN,
    BOK_SUBASSIGN,
    BOK_SHLASSIGN,
    BOK_SHRASSIGN,
    BOK_ANDASSIGN,
    BOK_XORASSIGN,
    BOK_ORASSIGN,
};

lib fn binary_operator_kind_from_tok(t: Tok) -> BinaryOperatorKind;
lib fn binary_operator_kind_str(k: BinaryOperatorKind) -> CStr;

struct BinaryExpr {
  super base: Expr;
  opc: BinaryOperatorKind;
  op_loc: Loc;
  lhs: Expr*;
  rhs: Expr*;
};

lib fn BinaryExpr::new(lhs: Expr*, rhs: Expr*, opc: BinaryOperatorKind, ty: Type*, vk: ValueKind, op_loc: Loc) -> BinaryExpr*;

struct CompoundAssignExpr { super bin_expr: BinaryExpr; };

lib fn CompoundAssignExpr::new(lhs: Expr*, rhs: Expr*, opc: BinaryOperatorKind, ty: Type*, vk: ValueKind, op_loc: Loc) -> CompoundAssignExpr*;

struct CallExpr {
  super base: Expr;
  rp_loc: Loc;
  func: Expr*;
  args: Expr**;
  args_count: i64;
};

lib fn CallExpr::new(func: Expr*, args: Expr**, args_count: i64, ty: Type*, vk: ValueKind, rp_loc: Loc) -> CallExpr*;

struct MethodCallExpr { super call_expr: Expr; };

lib fn MethodCallExpr::new(method: Expr*, args: Expr**, args_count: i64, ty: Type*, vk: ValueKind, rp_loc: Loc) -> MethodCallExpr*;

struct SizeofExpr {
  super base: Expr;
  sizeof_loc: Loc;
  rp_loc: Loc;
  expr: Expr*;
  ty_of_sizeof: Type*;
};

lib fn SizeofExpr::new(ty: Type*, expr: Expr*, sl: Loc, rl: Loc) -> SizeofExpr*;

struct MemberExpr {
  super base: Expr;
  lhs: Expr*;
  field_offset: i64;
  name: CStr;
  is_arrow: bool;
  oploc: Loc;
};

lib fn MemberExpr::new(lhs: Expr*, is_arrow: bool, oploc: Loc, name: CStr, ty: Type*, vk: ValueKind, field_offset: i64) -> MemberExpr*;

struct MethodExpr {
  super base: Expr;
  self_object: Expr*;
  method_func: FnDecl*;
  is_arrow: bool;
  oploc: Loc;
};

lib fn MethodExpr::new(self_object: Expr*, is_arrow: bool, oploc: Loc, method: FnDecl*) -> MethodExpr*;

struct ArraySubscriptExpr {
  super base: Expr;
  lhs: Expr*;
  rhs: Expr*;
  rb_loc: Loc;
};

lib fn ArraySubscriptExpr::new(lhs: Expr*, rhs: Expr*, t: Type*, vk: ValueKind, rb_loc: Loc) -> ArraySubscriptExpr*;

struct ConditionalExpr {
  super base: Expr;
  cond: Expr*;
  question_loc: Loc;
  lhs: Expr*;
  colon_loc: Loc;
  rhs: Expr*;
};

lib fn ConditionalExpr::new(cond: Expr*, qloc: Loc, lhs: Expr*, cloc: Loc, rhs: Expr*, t: Type*, vk: ValueKind) -> ConditionalExpr*;

struct RecoveryExpr {
  super base: Expr;
  begin_loc: Loc;
  end_loc: Loc;
  sub: Expr**;
  sub_cnt: i64;
};

lib fn RecoveryExpr::new(t: Type*, bl: Loc, el: Loc, sub: Expr**, sub_cnt: i64) -> RecoveryExpr*;

struct BuiltinExpr {
  super base: Expr;
  builtin_name: CStr;
  builtin_loc: Loc;
  args: Expr**;
  args_count: i64;
  rparen_loc: Loc;
};

lib fn BuiltinExpr::new(builtin: CStr, loc: Loc, args: Expr**, args_count: i64, rp: Loc) -> BuiltinExpr*;

enum CastKind {
  CK_NOOP,
  CK_TO_VOID,
  CK_LVALUE_TO_RVALUE,
  CK_POINTER_TO_BOOLEAN,
  CK_INTEGRAL_TO_BOOLEAN,
  CK_INTEGRAL_CAST,
  CK_ARRAY_TO_POINTER_DECAY,
  CK_FUNCTION_TO_POINTER_DECAY
};

lib fn cast_kind_to_string(k: CastKind) -> CStr;

struct CastExpr {
  super base: Expr;
  expr: Expr*;
  cast_kind: CastKind;
};

lib fn CastExpr::new(ty: Type*, vk: ValueKind, kind: CastKind, expr: Expr*) -> CastExpr*;

struct ImplicitCastExpr { super cast_expr: CastExpr; };

lib fn ImplicitCastExpr::new(ty: Type*, vk: ValueKind, kind: CastKind, expr: Expr*) -> ImplicitCastExpr*;

struct VAArgExpr {
  super base: Expr;
  s: Loc;
  e: Loc;
};

lib fn VAArgExpr::new(ty: Type*, s: Loc, e: Loc) -> VAArgExpr*;

lib fn ignore_parens_single_step(e: Expr*) -> Expr*;
lib fn ignore_expr_nodes(e: Expr*, args: void*) -> Expr*;
