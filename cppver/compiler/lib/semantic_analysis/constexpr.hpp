#ifndef SEMA_CONSTEXPR_HPP_INCLUDED
#define SEMA_CONSTEXPR_HPP_INCLUDED

#include "defines.hpp"
#include "ast/context.hpp"
#include <utility>

std::pair<i64, bool> eval_integer_constexpr(ASTContext &ctx, Expr const *expr);

#endif // SEMA_CONSTEXPR_HPP_INCLUDED
