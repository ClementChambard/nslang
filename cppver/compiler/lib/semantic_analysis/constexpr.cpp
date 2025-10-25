#include "ast/context.hpp"
#include "ast/nodes/decl.hpp"
#include "ast/nodes/expr.hpp"
#include <cassert>


std::pair<i64, bool> eval_integer_constexpr(ASTContext &ctx, Expr const *expr) {
  if (auto *e = expr->dyn_cast<ParenExpr>()) {
    return eval_integer_constexpr(ctx, e->val.get());
  } else if (auto *_ = expr->dyn_cast<StringLiteral>()) {
    return {0, false};
  } else if (auto *e = expr->dyn_cast<IntegerLiteral>()) {
    return {e->value, true};
  } else if (auto *e = expr->dyn_cast<FloatingLiteral>()) {
    return {e->value, true};
  } else if (auto *e = expr->dyn_cast<CharLiteral>()) {
    return {e->value, true};
  } else if (auto *_ = expr->dyn_cast<NullptrLiteral>()) {
    return {0, true};
  } else if (auto *e = expr->dyn_cast<BoolLiteral>()) {
    return {e->value, true};
  } else if (auto *e = expr->dyn_cast<SizeofExpr>()) {
    return {ctx.get_type_size(e->ty_of_sizeof), true};
  } else if (auto *e = expr->dyn_cast<CastExpr>()) {
    // TODO: maybe some cast kinds do stuff ?
    return eval_integer_constexpr(ctx, e->op.get());
  } else if (auto *e = expr->dyn_cast<DeclRefExpr>()) {
    if (auto *d = e->decl->dyn_cast<EnumVariantDecl>()) {
      return {d->value, true};
    }
    return {0, false};
  } else if (auto *e = expr->dyn_cast<BinaryExpr>()) {
    auto [lhs, lok] = eval_integer_constexpr(ctx, e->lhs.get());
    auto [rhs, rok] = eval_integer_constexpr(ctx, e->rhs.get());
    if (!rok || !lok) return {0, false};
    switch (e->opc) {
    case BinaryExpr::MUL:
      return {lhs * rhs, true};
    case BinaryExpr::DIV:
      return {lhs / rhs, true};
    case BinaryExpr::REM:
      return {lhs % rhs, true};
    case BinaryExpr::ADD:
      return {lhs + rhs, true};
    case BinaryExpr::SUB:
      return {lhs - rhs, true};
    case BinaryExpr::SHL:
      return {lhs << rhs, true};
    case BinaryExpr::SHR:
      return {lhs >> rhs, true};
    case BinaryExpr::LT:
      return {lhs < rhs, true};
    case BinaryExpr::GT:
      return {lhs > rhs, true};
    case BinaryExpr::LE:
      return {lhs <= rhs, true};
    case BinaryExpr::GE:
      return {lhs >= rhs, true};
    case BinaryExpr::EQ:
      return {lhs == rhs, true};
    case BinaryExpr::NE:
      return {lhs != rhs, true};
    case BinaryExpr::AND:
      return {lhs & rhs, true};
    case BinaryExpr::OR:
      return {lhs | rhs, true};
    case BinaryExpr::XOR:
      return {lhs ^ rhs, true};
    case BinaryExpr::LAND:
      return {lhs && rhs, true};
    case BinaryExpr::LOR:
      return {lhs || rhs, true};
    default:
      return {0, false};
    }
  } else if (auto *e = expr->dyn_cast<UnaryExpr>()) {
    auto [arg, ok] = eval_integer_constexpr(ctx, e->arg.get());
    if (!ok) return {0, false};
    switch (e->opc) {
    case UnaryExpr::PLUS:
      return {arg, true};
    case UnaryExpr::MINUS:
      return {-arg, true};
    case UnaryExpr::NOT:
      return {~arg, true};
    case UnaryExpr::LNOT:
      return {!arg, true};
    default:
      return {0, false};
    }
  } else if (auto *e = expr->dyn_cast<ConditionalExpr>()) {
    auto [arg, ok] = eval_integer_constexpr(ctx, e->cond.get());
    if (!ok) return {0, false};
    if (arg) {
      return eval_integer_constexpr(ctx, e->true_expr.get());
    } else {
      return eval_integer_constexpr(ctx, e->false_expr.get());
    }
  } else {
    return {0, false};
  }
}
