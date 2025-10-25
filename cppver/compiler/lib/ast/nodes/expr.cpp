#include "expr.hpp"
#include "lexer/tok.hpp"
#include <cassert>

// clang-format off
UnaryExpr::OpKind UnaryExpr::opc_from_tok(Tok t) {
  switch (t) {
  case tok::PLUSPLUS: return PREINC;
  case tok::MINUSMINUS: return PREDEC;
  case tok::AMP: return ADDROF;
  case tok::STAR: return DEREF;
  case tok::PLUS: return PLUS;
  case tok::MINUS: return MINUS;
  case tok::TILDE: return NOT;
  case tok::EXCLAIM: return LNOT;
  default:
    assert(false && "Unknown unary operator token");
  }
}

cstr UnaryExpr::opc_name(OpKind k) {
  switch (k) {
  case POSTINC: return "post ++";
  case POSTDEC: return "post --";
  case PREINC: return "++";
  case PREDEC: return "--";
  case ADDROF: return "&";
  case DEREF: return "*";
  case PLUS: return "+";
  case MINUS: return "-";
  case NOT: return "~";
  case LNOT: return "!";
  }
  return "";
}

BinaryExpr::OpKind BinaryExpr::opc_from_tok(Tok t) {
  switch (t) {
    case tok::STAR: return MUL;
    case tok::SLASH: return DIV;
    case tok::PERCENT: return REM;
    case tok::PLUS: return ADD;
    case tok::MINUS: return SUB;
    case tok::LESSLESS: return SHL;
    case tok::GREATERGREATER: return SHR;
    case tok::AMP: return AND;
    case tok::CARET: return XOR;
    case tok::PIPE: return OR;
    case tok::LESS: return LT;
    case tok::GREATER: return GT;
    case tok::LESSEQUAL: return LE;
    case tok::GREATEREQUAL: return GE;
    case tok::EQUALEQUAL: return EQ;
    case tok::EXCLAIMEQUAL: return NE;
    case tok::AMPAMP: return LAND;
    case tok::PIPEPIPE: return LOR;
    case tok::EQUAL: return ASSIGN;
    case tok::STAREQUAL: return MULASSIGN;
    case tok::SLASHEQUAL: return DIVASSIGN;
    case tok::PERCENTEQUAL: return REMASSIGN;
    case tok::PLUSEQUAL: return ADDASSIGN;
    case tok::MINUSEQUAL: return SUBASSIGN;
    case tok::LESSLESSEQUAL: return SHLASSIGN;
    case tok::GREATERGREATEREQUAL: return SHRASSIGN;
    case tok::AMPEQUAL: return ANDASSIGN;
    case tok::CARETEQUAL: return XORASSIGN;
    case tok::PIPEEQUAL: return ORASSIGN;
    default:
      assert(false && "Unknown binary operator token");
  }
}

cstr BinaryExpr::opc_name(OpKind k) {
  switch (k) {
  case MUL: return "*";
  case DIV: return "/";
  case REM: return "%";
  case ADD: return "+";
  case SUB: return "-";
  case SHL: return "<<";
  case SHR: return ">>";
  case AND: return "&";
  case XOR: return "^";
  case OR: return "|";
  case LT: return "<";
  case GT: return ">";
  case LE: return "<=";
  case GE: return ">=";
  case EQ: return "==";
  case NE: return "!=";
  case LAND: return "&&";
  case LOR: return "||";
  case ASSIGN: return "=";
  case MULASSIGN: return "*=";
  case DIVASSIGN: return "/=";
  case REMASSIGN: return "%=";
  case ADDASSIGN: return "+=";
  case SUBASSIGN: return "-=";
  case SHLASSIGN: return "<<=";
  case SHRASSIGN: return ">>=";
  case ANDASSIGN: return "&=";
  case XORASSIGN: return "^=";
  case ORASSIGN: return "|=";
  }
  return "";
}

cstr CastExpr::kind_to_str(CastKind k) {
  switch (k) {
  case NOOP: return "Noop";
  case TO_VOID: return "ToVoid";
  case POINTER_TO_BOOLEAN: return "PointerToBoolean";
  case POINTER_TO_INTEGRAL: return "PointerToIntegral";
  case INTEGRAL_TO_BOOLEAN: return "IntegralToBoolean";
  case INTEGRAL_TO_POINTER: return "IntegralToPointer";
  case INTEGRAL_TO_FLOATING: return "IntegralToFloating";
  case INTEGRAL_CAST: return "IntegralCast";
  case FLOATING_TO_BOOLEAN: return "FloatingToBoolean";
  case FLOATING_TO_INTEGRAL: return "FloatingToIntegral";
  case FLOATING_CAST: return "FloatingCast";
  case LVALUE_TO_RVALUE: return "LValueToRValue";
  case ARRAY_TO_POINTER_DECAY: return "ArrayToPointerDecay";
  case FUNCTION_TO_POINTER_DECAY: return "FunctionToPointerDecay";
  case NULL_TO_POINTER: return "NullToPointer";
  }
  return "";
}
// clang-format on

Loc UnaryExpr::get_start_loc() const {
  return is_postfix() ? arg->get_start_loc() : oploc;
}

Loc UnaryExpr::get_end_loc() const {
  return is_postfix() ? oploc : arg->get_end_loc();
}

namespace detail {
/// Given an expression E and functions Fn_1,...,Fn_n : Expr * -> Expr *,
/// Return Fn_n(...(Fn_1(E)))
inline Expr *IgnoreExprNodesImpl(Expr *E) { return E; }
template <typename FnTy, typename... FnTys>
Expr *IgnoreExprNodesImpl(Expr *E, FnTy &&Fn, FnTys &&... Fns) {
  return IgnoreExprNodesImpl(std::forward<FnTy>(Fn)(E),
                             std::forward<FnTys>(Fns)...);
}
}

inline Expr *IgnoreImplicitCastsSingleStep(Expr *E) {
  if (auto *ICE = E->dyn_cast<ImplicitCastExpr>())
    return ICE->op.get();
  // if (auto *FE = dyn_cast<FullExpr>(E)) return FE->getSubExpr();
  return E;
}

inline Expr *IgnoreParensSingleStep(Expr *E) {
  if (auto *PE = E->dyn_cast<ParenExpr>())
    return PE->val.get();
  return E;
}

template <typename... FnTys> Expr *ignore_expr_nodes(Expr *e, FnTys &&... fns) {
  Expr *last_e = nullptr;
  while (e != last_e) {
    last_e = e;
    e = detail::IgnoreExprNodesImpl(e, std::forward<FnTys>(fns)...);
  }
  return e;
}

Expr *Expr::ignore_imp_casts() {
  return ignore_expr_nodes(this, IgnoreImplicitCastsSingleStep);
}

Expr *Expr::ignore_parens() {
  return ignore_expr_nodes(this, IgnoreParensSingleStep);
}

Expr *Expr::ignore_paren_imp_casts() {
  return ignore_expr_nodes(this, IgnoreParensSingleStep,
                         IgnoreImplicitCastsSingleStep);
}
