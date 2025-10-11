#include "scope.hpp"
#include "ast/nodes/decl.hpp"
#include <cassert>

Scope::Scope(Scope *parent, ScopeFlags flags) {
  set_flags_pr(parent, flags);
  any_parent = parent;
  this->flags = flags;
  decls_in_scope.clear();
}

void Scope::set_flags_pr(Scope *parent, ScopeFlags flags) {
  any_parent = parent;
  this->flags = flags;

  if (parent && !(flags & SF_FN)) {
    break_parent = parent->break_parent;
    continue_parent = parent->continue_parent;
  } else {
    break_parent = nullptr;
    continue_parent = nullptr;
  }

  if (parent) {
    depth = parent->depth + 1;
    fn_parent = parent->fn_parent;
    block_parent = parent->block_parent;
    decl_parent = parent->decl_parent;
    if (!(flags & (SF_FN | SF_CLASS | SF_BLOCK | SF_FUNCTION_PROTO))) {
      this->flags = ScopeFlags(this->flags | parent->flags);
    }
  } else {
    depth = 0;
    decl_parent = nullptr;
    fn_parent = nullptr;
    block_parent = nullptr;
  }

  if (flags & SF_FN)
    fn_parent = this;
  if (flags & SF_BREAK)
    break_parent = this;
  if (flags & SF_CONTINUE)
    continue_parent = this;
  if (flags & SF_BLOCK)
    block_parent = this;
  if (flags & SF_DECL)
    decl_parent = this;
}

bool Scope::is_switch_scope() const {
  Scope const *s = this;
  while (s) {
    if (s->flags & SF_SWITCH)
      return true;
    if (s->flags & (SF_FN | SF_CLASS | SF_BLOCK | SF_FUNCTION_PROTO))
      return false;
    s = s->get_parent();
  }
  return false;
}

bool Scope::contained_in_prototype_scope() const {
  Scope const *s = this;
  while (s) {
    if (s->is_function_proto_scope())
      return true;
    s = s->get_parent();
  }
  return false;
}

void Scope::add_flags(ScopeFlags flags) {
  assert(!(flags & ~(SF_BREAK | SF_CONTINUE)) && "Unsupported scope flags");
  if (flags & SF_BREAK) {
    assert(!(flags & SF_BREAK) && "Already set");
    break_parent = this;
  }
  if (flags & SF_CONTINUE) {
    assert(!(flags & SF_CONTINUE) && "Already set");
    continue_parent = this;
  }
  this->flags = ScopeFlags(this->flags | flags);
}

void Scope::set_is_condition_var_scope(bool val) {
  flags = ScopeFlags((flags & ~SF_CONDITION_VAR) | (val * SF_CONDITION_VAR));
}

NamedDecl *Scope::lookup_named_decl_norec(IdentInfo *decl_name) const {
  for (auto d : decls_in_scope) {
    if (d->name == decl_name)
      return d;
  }
  return nullptr;
}

NamedDecl *Scope::lookup_named_decl(IdentInfo *decl_name) const {
  for (auto d : decls_in_scope) {
    if (d->name == decl_name)
      return d;
  }
  if (any_parent)
    return any_parent->lookup_named_decl(decl_name);
  return nullptr;
}
