#include darray.nsh
#include str.nsh

struct Decl;
struct NamedDecl;

enum ScopeFlags : i64 {
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
  any_parent: Scope*;
  flags: i64;
  depth: i64;
  prototype_depth: i64;
  prototype_index: i64;
  fn_parent: Scope*;
  break_parent: Scope*;
  continue_parent: Scope*;
  block_parent: Scope*;
  decl_parent: Scope*;
  decls_in_scope: DArray;
  decl_context: void*;
};

lib fn Scope::set_flags_pr(self: Scope*, parent: Scope*, f: i64);
lib fn Scope::set_flags(self: Scope*, f: i64);
lib fn Scope::get_parent(self: Scope*) -> Scope*;
lib fn Scope::is_block_scope(self: Scope*) -> bool;
lib fn Scope::is_condition_var_scope(self: Scope*) -> bool;
lib fn Scope::is_function_proto_scope(self: Scope*) -> bool;
lib fn Scope::is_function_decl_scope(self: Scope*) -> bool;
lib fn Scope::is_switch_scope(self: Scope*) -> bool;
lib fn Scope::is_loop_scope(self: Scope*) -> bool;
lib fn Scope::is_continue_scope(self: Scope*) -> bool;
lib fn Scope::is_compound_stmt_scope(self: Scope*) -> bool;
lib fn Scope::is_control_scope(self: Scope*) -> bool;
lib fn Scope::is_type_alias_scope(self: Scope*) -> bool;
lib fn Scope::is_friend_scope(self: Scope*) -> bool;
lib fn Scope::is_function_scope(self: Scope*) -> bool;
lib fn Scope::is_class_scope(self: Scope*) -> bool;
lib fn Scope::contains(self: Scope*, other: Scope*) -> bool;
lib fn Scope::contained_in_prototype_scope(self: Scope*) -> bool;
lib fn Scope::add_flags(self: Scope*, flags: i64);
lib fn Scope::dump(self: Scope*, out_str: String*);
lib fn Scope::set_is_condition_var_scope(self: Scope*, val: bool);
lib fn Scope::get_next_function_prototype_index(self: Scope*) -> i64;
lib fn Scope::add_decl(self: Scope*, d: Decl*);
lib fn Scope::remove_decl(self: Scope*, d: Decl*);
lib fn Scope::contains_decl(self: Scope*, d: Decl*) -> bool;
lib fn Scope::lookup_named_decl(self: Scope*, decl_name: CStr) -> NamedDecl*;

lib fn Scope::new(parent: Scope*, flags: ScopeFlags) -> Scope*;
lib fn Scope::delete(self: Scope*);
