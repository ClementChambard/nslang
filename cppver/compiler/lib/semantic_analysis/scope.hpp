#ifndef SEMA_SCOPE_HPP_INCLUDED
#define SEMA_SCOPE_HPP_INCLUDED

#include "ast/nodes/type.hpp"
#include "lexer/ident.hpp"
#include <algorithm>
#include <defines.hpp>
#include <vector>

enum ScopeFlags : u32 {
  SF_NO = 0x000,
  SF_FN = 0x001,
  SF_BREAK = 0x002,
  SF_CONTINUE = 0x004,
  SF_DECL = 0x008,
  SF_CONTROL = 0x010,
  SF_CLASS = 0x020,
  SF_BLOCK = 0x040,
  SF_FUNCTION_PROTO = 0x100,
  SF_FUNCTION_DECL = 0x200,
  SF_SWITCH = 0x1000,
  SF_ENUM = 0x40000,
  SF_COMPOUND_STMT = 0x400000,
  SF_CONDITION_VAR = 0x2000000,
  SF_LAMBDA = 0x8000000,
  SF_TYPE_ALIAS = 0x20000000,
  SF_FRIEND = 0x40000000,
};

struct Scope {
  Scope *any_parent = nullptr;
  ScopeFlags flags = SF_NO;
  u32 depth = 0;
  Scope *fn_parent = nullptr;
  Scope *break_parent = nullptr;
  Scope *continue_parent = nullptr;
  Scope *block_parent = nullptr;
  Scope *decl_parent = nullptr;
  std::vector<NamedDecl *> decls_in_scope{};

  Scope(Scope *parent, ScopeFlags flags);

  void set_flags_pr(Scope *parent, ScopeFlags flags);
  void set_flags(ScopeFlags flags) { set_flags_pr(get_parent(), flags); }
  Scope *get_parent() const { return any_parent; }
  bool is_block_scope() const { return flags & SF_BLOCK; }
  bool is_condition_var_scope() const { return flags & SF_CONDITION_VAR; }
  bool is_function_proto_scope() const { return flags & SF_FUNCTION_PROTO; }
  bool is_function_decl_scope() const { return flags & SF_FUNCTION_DECL; }
  bool is_loop_scope() const {
    return (flags & SF_BREAK) && !(flags & SF_SWITCH);
  }
  bool is_continue_scope() const { return flags & SF_CONTINUE; }
  bool is_compound_stmt_scope() const { return flags & SF_COMPOUND_STMT; }
  bool is_control_scope() const { return flags & SF_CONTROL; }
  bool is_type_alias_scope() const { return flags & SF_TYPE_ALIAS; }
  bool is_friend_scope() const { return flags & SF_FRIEND; }
  bool is_function_scope() const { return flags & SF_FN; }
  bool is_class_scope() const { return flags & SF_CLASS; }
  bool is_switch_scope() const;
  bool contains(Scope const &o) const { return depth < o.depth; }
  bool contained_in_prototype_scope() const;
  void add_flags(ScopeFlags flags);
  void set_is_condition_var_scope(bool val);
  void add_decl(NamedDecl *d) { decls_in_scope.push_back(d); }
  void remove_decl(NamedDecl *d) {
    decls_in_scope.erase(
        std::find(decls_in_scope.begin(), decls_in_scope.end(), d));
  }
  bool contains_decl(NamedDecl *d) const {
    return std::find(decls_in_scope.begin(), decls_in_scope.end(), d) !=
           decls_in_scope.end();
  }
  NamedDecl *lookup_named_decl(IdentInfo *decl_name) const;
  NamedDecl *lookup_named_decl_norec(IdentInfo *decl_name) const;
};

#endif // SEMA_SCOPE_HPP_INCLUDED
