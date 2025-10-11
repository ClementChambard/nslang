#include "ast/nodes/decl.hpp"
#include "ast/nodes/stmt.hpp"
#include "ast/nodes/type.hpp"
#include "diags/diagnostic.hpp"
#include "lexer/ident.hpp"
#include "lexer/loc.hpp"
#include "sema.hpp"
#include <algorithm>
#include <cassert>
#include <memory>

bool check_fn_decl_compat(NamedDecl *old_decl, IdentInfo *name,
                          std::vector<UPtr<ParamDecl>> const &params,
                          Type *return_type, Loc loc, bool will_have_body,
                          bool is_vararg) {
  auto old_fn = old_decl->dyn_cast<FunctionDecl>();
  if (!old_fn) {
    Diag(diag::ERROR, loc, "name '%s' already used with different type",
         name->name.c_str());
    return true;
  }
  auto old_fnty = old_fn->type->dyn_cast<FunctionType>();
  if (will_have_body && old_fn->body) {
    Diag(diag::ERROR, loc, "function '%s' has already been defined",
         name->name.c_str());
    return true;
  }
  if (return_type != old_fnty->result_type) {
    Diag(diag::ERROR, loc,
         "function '%s' already declared with different return type",
         name->name.c_str());
    return true;
  }
  if (params.size() != old_fn->params.size()) {
    Diag(diag::ERROR, loc,
         "function '%s' already declared with different parameters",
         name->name.c_str());
    return true;
  }
  for (u32 i = 0; i < params.size(); i++) {
    if (params[i]->type != old_fn->params[i]->type) {
      Diag(diag::ERROR, loc,
           "function '%s' already declared with different parameters",
           name->name.c_str());
      return true;
    }
  }
  if (is_vararg != old_fnty->function.is_variadic) {
    Diag(diag::ERROR, loc,
         "function '%s' already declared with different parameters",
         name->name.c_str());
    return true;
  }
  return false;
}

UPtr<FunctionDecl> Sema::act_on_fn_decl(Scope *scope, IdentInfo *name,
                                        std::vector<UPtr<ParamDecl>> &params,
                                        Type *return_type, Loc fn_loc,
                                        Loc semi_loc, bool is_vararg, StructDecl *struct_scope) {
  NamedDecl *old_decl = nullptr;
  if (struct_scope) {
    // TODO: better use struct_scope
    for (auto m : struct_scope->methods) {
      if (m->name == name) {
        old_decl = m;
        break;
      }
    }
  } else {
    old_decl = scope->lookup_named_decl(name);
  }
  bool is_lib = false;
  if (old_decl) {
    if (check_fn_decl_compat(old_decl, name, params, return_type, fn_loc, false,
                             is_vararg)) {
      return nullptr;
    }
    if (struct_scope) {
      struct_scope->methods.erase(std::find(struct_scope->methods.begin(), struct_scope->methods.end(), old_decl));
    } else {
      scope->remove_decl(old_decl);
    }
    is_lib = old_decl->is_lib;
  }
  std::vector<Type *> arg_types;
  arg_types.reserve(params.size());
  for (auto &p : params) {
    arg_types.push_back(p->type);
  }
  auto ty = ctx.get_function_type(return_type, arg_types, is_vararg);
  auto decl =
      std::make_unique<FunctionDecl>(LocRge{fn_loc, semi_loc}, name, ty);
  decl->params = std::move(params);
  decl->is_lib = is_lib;
  if (struct_scope) {
    struct_scope->methods.push_back(decl.get());
    decl->struct_scope = struct_scope;
  } else {
    scope->add_decl(decl.get());
  }
  return decl;
}

UPtr<FunctionDecl> Sema::act_on_start_fn_definition(
    Scope *scope, IdentInfo *name, std::vector<UPtr<ParamDecl>> &params,
    Type *return_type, Loc fn_loc, bool is_vararg, StructDecl *struct_scope) {
  NamedDecl *old_decl = nullptr;
  if (struct_scope) {
    // TODO: better use struct_scope
    for (auto m : struct_scope->methods) {
      if (m->name == name) {
        old_decl = m;
        break;
      }
    }
  } else {
    old_decl = scope->lookup_named_decl(name);
  }
  bool is_lib = false;
  if (old_decl) {
    if (check_fn_decl_compat(old_decl, name, params, return_type, fn_loc, true,
                             is_vararg)) {
      return nullptr;
    }
    if (struct_scope) {
      struct_scope->methods.erase(std::find(struct_scope->methods.begin(), struct_scope->methods.end(), old_decl));
    } else {
      scope->remove_decl(old_decl);
    }
    is_lib = old_decl->is_lib;
  }
  std::vector<Type *> arg_types;
  arg_types.reserve(params.size());
  for (auto &p : params) {
    arg_types.push_back(p->type);
  }
  auto ty = ctx.get_function_type(return_type, arg_types, is_vararg);
  auto decl =
      std::make_unique<FunctionDecl>(LocRge{fn_loc, LOC_INVALID}, name, ty);
  decl->params = std::move(params);
  decl->is_lib = is_lib;
  if (struct_scope) {
    struct_scope->methods.push_back(decl.get());
    decl->struct_scope = struct_scope;
  } else {
    scope->add_decl(decl.get());
  }
  cur_fn_decl = decl.get();
  return decl;
}

UPtr<FunctionDecl> Sema::act_on_end_fn_definition(UPtr<FunctionDecl> decl,
                                                  UPtr<CompoundStmt> body) {
  decl->src_range.end = body->get_end_loc();
  decl->body = std::move(body);
  cur_fn_decl = nullptr;
  return decl;
}

std::pair<std::string, StructType *>
Sema::act_on_method_decl_name(Scope *scope, std::string struct_name, Loc loc,
                              std::string method_name, Loc name_loc) {
  (void)name_loc;
  std::string full_name = struct_name + "::" + method_name;
  auto maybe_struct_decl =
      scope->lookup_named_decl(IdentInfo::find(struct_name));
  StructDecl *sd;
  if (maybe_struct_decl && (sd = maybe_struct_decl->dyn_cast<StructDecl>())) {
    return {full_name, ctx.get_declared_type(sd)->dyn_cast<StructType>()};
  } else {
    Diag(diag::ERROR, loc, "'%s' does not name a type", struct_name.c_str());
    return {full_name, nullptr};
  }
}

void Sema::act_on_start_of_translation_unit() {}
void Sema::act_on_end_of_translation_unit() {}

DeclUPtr Sema::act_on_lib_decl(DeclUPtr decl, Loc lib_loc) {
  if (decl) {
    decl->is_lib = true;
    decl->src_range.start = lib_loc;
  }
  return decl;
}

void Sema::act_on_enum_variant_decl(Scope *scope, Loc start_loc, Loc end_loc,
                                    IdentInfo *name, ExprUPtr e, EnumDecl *decl,
                                    i64 val) {
  // TODO: check name availability
  auto ty = ctx.get_declared_type(decl);
  auto vdecl = std::make_unique<EnumVariantDecl>(
      LocRge{start_loc, end_loc}, name, ty, std::move(e), val,
      ty->is_unsigned_integer_or_enumeration_type());
  scope->add_decl(vdecl.get());
  decl->variants.push_back(std::move(vdecl));
}

UPtr<ParamDecl> Sema::act_on_param_decl(Scope *scope, Loc id_loc, Loc end_loc,
                                        IdentInfo *name, Type *type) {
  // TODO: check name availability
  auto decl =
      std::make_unique<ParamDecl>(LocRge{id_loc, end_loc}, id_loc, name, type);
  scope->add_decl(decl.get());
  return decl;
}

UPtr<VarDecl> Sema::act_on_var_decl(Scope *scope, Loc kw_loc, Loc id_loc,
                                    Loc end_loc, IdentInfo *name, Type *type, bool global) {
  // TODO: check name availability
  auto decl =
      std::make_unique<VarDecl>(LocRge{kw_loc, end_loc}, id_loc, name, type);
  decl->is_global = global;
  scope->add_decl(decl.get());
  return decl;
}

void Sema::act_on_field_decl(StructDecl *scope, Loc id_loc, Loc end_loc,
                             IdentInfo *name, Type *type) {
  // TODO: check name availability
  auto decl = std::make_unique<FieldDecl>(LocRge{id_loc, end_loc}, name, type);
  decl->parent = scope;
  scope->fields.push_back(std::move(decl));
}

UPtr<EnumDecl> Sema::act_on_enum_decl(Scope *scope, Loc kw_loc, Loc end_loc,
                                      IdentInfo *name, Type *aliased_type) {
  // TODO: redeclaration, name availability ...
  assert(name != nullptr);
  bool scoped = aliased_type == nullptr;
  if (aliased_type && !aliased_type->is_integral_type()) {
    Diag(diag::ERROR, kw_loc, "enum underlying type is not integral");
    aliased_type = nullptr;
  }
  auto decl = std::make_unique<EnumDecl>(LocRge(kw_loc, end_loc), name, scoped);
  decl->int_ty = aliased_type;
  decl->is_complete = false;


  auto existing_decl = scope->lookup_named_decl_norec(name);

  // name not in use
  if (!existing_decl) {
    (void)ctx.get_declared_type(decl.get());
    scope->add_decl(decl.get());
    return decl;
  }

  // name in use but not an enum
  if (!existing_decl->isa<EnumDecl>()) {
    Diag(diag::ERROR, kw_loc, "'%s' already defined not as enum",
         name->name.c_str());
    Diag(diag::NOTE, existing_decl->get_loc(), "defined here")
        << existing_decl->src_range;
    (void)ctx.get_declared_type(decl.get());
    return decl;
  }

  auto *old_decl = existing_decl->dyn_cast<EnumDecl>();

  // different underlying types
  if (old_decl->int_ty != aliased_type) {
    Diag(diag::ERROR, kw_loc, "enum '%s' already defined with different int type", name->name.c_str());
    Diag(diag::NOTE, existing_decl->get_loc(), "defined here")
        << existing_decl->src_range;
    (void)ctx.get_declared_type(decl.get());
    return decl;
  }

  ctx.set_declared_type(ctx.get_declared_type(existing_decl), decl.get());
  return decl;
}

UPtr<EnumDecl> Sema::act_on_start_enum_decl(Scope *scope, Loc kw_loc,
                                            IdentInfo *name,
                                            Type *aliased_type) {
  static u32 UNAMED_ENUM_CNT = 0;
  // TODO: redeclaration, name availability ...
  bool annonymous = false;
  if (name == nullptr) {
    // TODO: better way to have annonymous enums
    name = IdentInfo::find(std::string("__annonymous_enum_decl_") +
                           std::to_string(UNAMED_ENUM_CNT++));
    annonymous = true;
  }
  bool scoped = aliased_type == nullptr;
  if (aliased_type && !aliased_type->is_integral_type()) {
    Diag(diag::ERROR, kw_loc, "enum underlying type is not integral");
    aliased_type = nullptr;
  }
  auto decl =
      std::make_unique<EnumDecl>(LocRge(kw_loc, LOC_INVALID), name, scoped);
  decl->int_ty = aliased_type;
  decl->is_complete = true;

  auto existing_decl = scope->lookup_named_decl_norec(name);

  // name not in use
  if (!existing_decl) {
    (void)ctx.get_declared_type(decl.get());
    if (!annonymous) scope->add_decl(decl.get());
    return decl;
  }

  assert(!annonymous && "can't have annonymous enum in scope");

  // name in use but not a struct
  if (!existing_decl->isa<EnumDecl>()) {
    Diag(diag::ERROR, kw_loc, "'%s' already defined not as enum",
         name->name.c_str());
    Diag(diag::NOTE, existing_decl->get_loc(), "defined here")
        << existing_decl->src_range;
    (void)ctx.get_declared_type(decl.get());
    return decl;
  }

  auto *old_decl = existing_decl->dyn_cast<EnumDecl>();
  auto ty = ctx.get_declared_type(decl.get())->dyn_cast<EnumType>();
  ctx.set_declared_type(ty, decl.get());

  // different underlying types
  if (old_decl->int_ty != aliased_type) {
    Diag(diag::ERROR, kw_loc, "enum '%s' already defined with different int type", name->name.c_str());
    Diag(diag::NOTE, existing_decl->get_loc(), "defined here")
        << existing_decl->src_range;
    (void)ctx.get_declared_type(decl.get());
    return decl;
  }

  if (old_decl->is_complete) {
    Diag(diag::ERROR, kw_loc, "enum '%s' was already defined",
         name->name.c_str());
    Diag(diag::NOTE, existing_decl->get_loc(), "defined here")
        << existing_decl->src_range;
    return decl;
  }

  ty->decl = decl.get();
  scope->remove_decl(old_decl);
  scope->add_decl(decl.get());
  return decl;
}

UPtr<EnumDecl> Sema::act_on_end_enum_decl(UPtr<EnumDecl> decl, Loc end_loc) {
  decl->src_range.end = end_loc;
  return decl;
}

UPtr<AliasDecl> Sema::act_on_alias_decl(Scope *scope, Loc kw_loc, Loc end_loc,
                                        IdentInfo *name, Type *aliased_type) {
  assert(aliased_type);
  auto decl =
      std::make_unique<AliasDecl>(LocRge(kw_loc, end_loc), name, aliased_type);

  auto existing_decl = scope->lookup_named_decl_norec(name);

  if (!existing_decl) {
    (void)ctx.get_declared_type(decl.get()); // < ensure type exists
    scope->add_decl(decl.get());
    return decl;
  }

  // name in use but not an alias
  if (!existing_decl->isa<AliasDecl>()) {
    Diag(diag::ERROR, kw_loc, "'%s' already defined not as type alias",
         name->name.c_str());
    Diag(diag::NOTE, existing_decl->get_loc(), "defined here")
        << existing_decl->src_range;
    (void)ctx.get_declared_type(decl.get());
    return decl;
  }

  auto ty = ctx.get_declared_type(existing_decl);
  auto ad = existing_decl->dyn_cast<AliasDecl>();
  if (ad->underlying_type != aliased_type) {
    Diag(diag::ERROR, kw_loc,
         "type alias '%s' already defined with different type",
         name->name.c_str());
    Diag(diag::NOTE, existing_decl->get_loc(), "defined here")
        << existing_decl->src_range;
    (void)ctx.get_declared_type(decl.get());
    return decl;
  }

  ctx.set_declared_type(ty, decl.get());
  return decl;
}

UPtr<StructDecl> Sema::act_on_struct_decl(Scope *scope, Loc kw_loc, Loc id_loc,
                                          IdentInfo *type_name) {
  auto decl = std::make_unique<StructDecl>(LocRge{kw_loc, id_loc}, type_name);
  decl->is_complete = false;

  auto existing_decl = scope->lookup_named_decl_norec(type_name);

  // name not in use
  if (!existing_decl) {
    (void)ctx.get_declared_type(decl.get());
    scope->add_decl(decl.get());
    return decl;
  }

  // name in use but not a struct
  if (!existing_decl->isa<StructDecl>()) {
    Diag(diag::ERROR, id_loc, "'%s' already defined not as struct",
         type_name->name.c_str());
    Diag(diag::NOTE, existing_decl->get_loc(), "defined here")
        << existing_decl->src_range;
    (void)ctx.get_declared_type(decl.get());
    return decl;
  }

  ctx.set_declared_type(ctx.get_declared_type(existing_decl), decl.get());
  return decl;
}

UPtr<StructDecl> Sema::act_on_start_struct_decl(Scope *scope, Loc kw_loc,
                                                Loc id_loc,
                                                IdentInfo *type_name) {
  auto decl =
      std::make_unique<StructDecl>(LocRge{kw_loc, LOC_INVALID}, type_name);
  decl->is_complete = true;

  auto existing_decl = scope->lookup_named_decl_norec(type_name);

  // name not in use
  if (!existing_decl) {
    (void)ctx.get_declared_type(decl.get());
    scope->add_decl(decl.get());
    return decl;
  }

  // name in use but not a struct
  if (!existing_decl->isa<StructDecl>()) {
    Diag(diag::ERROR, id_loc, "'%s' already defined not as struct",
         type_name->name.c_str());
    Diag(diag::NOTE, existing_decl->get_loc(), "defined here")
        << existing_decl->src_range;
    (void)ctx.get_declared_type(decl.get());
    return decl;
  }

  auto *old_decl = existing_decl->dyn_cast<StructDecl>();
  auto ty = ctx.get_declared_type(decl.get())->dyn_cast<StructType>();
  ctx.set_declared_type(ty, decl.get());

  if (old_decl->is_complete) {
    Diag(diag::ERROR, id_loc, "struct '%s' was already defined",
         type_name->name.c_str());
    Diag(diag::NOTE, existing_decl->get_loc(), "defined here")
        << existing_decl->src_range;
    return decl;
  }

  ty->decl = decl.get();
  scope->remove_decl(old_decl);
  scope->add_decl(decl.get());
  return decl;
}

UPtr<StructDecl> Sema::act_on_end_struct_decl(UPtr<StructDecl> decl,
                                              Loc end_loc) {
  if (decl->has_super && decl->fields.size() == 0) {
    Diag(diag::ERROR, decl->src_range.start,
         "'super' identifier not used on first field of struct");
    decl->has_super = false;
  }
  decl->src_range.end = end_loc;
  return decl;
}
