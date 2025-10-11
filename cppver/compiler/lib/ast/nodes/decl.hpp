#ifndef AST_NODES_DECL_HPP_INCLUDED
#define AST_NODES_DECL_HPP_INCLUDED

#include "expr.hpp"
#include "lexer/ident.hpp"
#include "lexer/loc.hpp"
#include "stmt.hpp"
#include "type.hpp"
#include <memory>

struct Decl {
  enum Kind {
    TRANSLATION_UNIT,
    ALIAS_DECL,
    ENUM_DECL,
    STRUCT_DECL,
    VAR_DECL,
    PARAM_DECL,
    FIELD_DECL,
    FUNCTION_DECL,
    ENUM_VARIANT_DECL,
  };

  u8 decl_kind : 7;
  u8 is_lib : 1;
  // used, referenced, ...
  LocRge src_range;

  Decl(Kind k, LocRge rge) : decl_kind(k), is_lib(false), src_range(rge) {}
  virtual ~Decl() {}

  virtual Loc get_loc() const { return src_range.start; }

  template <typename T> bool isa() const {
    return T::is_class(Kind(decl_kind));
  }
  template <typename T> const T *dyn_cast() const {
    return const_cast<Decl *>(this)->dyn_cast<T>();
  }
  template <typename T> T *dyn_cast() {
    if (isa<T>())
      return reinterpret_cast<T *>(this);
    return nullptr;
  }

  static bool is_class(Kind) { return true; }
};

struct TranslationUnitDecl : public Decl {
  std::vector<std::unique_ptr<Decl>> decls;

  TranslationUnitDecl() : Decl(TRANSLATION_UNIT, {}) {}

  void add_decl(DeclUPtr d);

  static bool is_class(Kind k) { return k == TRANSLATION_UNIT; }
};

struct NamedDecl : public Decl {
  IdentInfo *name;

  NamedDecl(Kind k, LocRge rge, IdentInfo *name) : Decl(k, rge), name(name) {}

  std::string const &get_name() const { return name->name; }

  static bool is_class(Kind k) {
    return k >= ALIAS_DECL && k <= ENUM_VARIANT_DECL;
  }
};

struct ValueDecl : public NamedDecl {
  Type *type;

  ValueDecl(Kind k, LocRge rge, IdentInfo *name, Type *t)
      : NamedDecl(k, rge, name), type(t) {}

  static bool is_class(Kind k) {
    return k >= VAR_DECL && k <= ENUM_VARIANT_DECL;
  }
};

struct VarDecl : public ValueDecl {
  Loc id_loc;
  bool is_global = false;

  VarDecl(LocRge rge, Loc id_loc, IdentInfo *name, Type *t)
      : ValueDecl(VAR_DECL, rge, name, t), id_loc(id_loc) {}
  VarDecl(Kind k, LocRge rge, Loc id_loc, IdentInfo *name, Type *t)
      : ValueDecl(k, rge, name, t), id_loc(id_loc) {}

  Loc get_loc() const override { return id_loc; }

  static bool is_class(Kind k) { return k >= VAR_DECL && k <= PARAM_DECL; }
};

struct ParamDecl : public VarDecl {
  ParamDecl(LocRge rge, Loc id_loc, IdentInfo *name, Type *t)
      : VarDecl(PARAM_DECL, rge, id_loc, name, t) {}
  static bool is_class(Kind k) { return k == PARAM_DECL; }
};

struct FunctionDecl : public ValueDecl {
  std::vector<std::unique_ptr<ParamDecl>> params;
  std::unique_ptr<Stmt> body = nullptr;
  StructDecl *struct_scope = nullptr;

  FunctionDecl(LocRge rge, IdentInfo *name, Type *t)
      : ValueDecl(FUNCTION_DECL, rge, name, t) {}

  LocRge get_param_range() const { return params.size() ? LocRge{params[0]->src_range.start, params.back()->src_range.end} : LocRge{0, 0}; }

  inline std::string get_mangled_name() const;

  static bool is_class(Kind k) { return k == FUNCTION_DECL; }
};

struct FieldDecl : public ValueDecl {
  struct StructDecl *parent = nullptr;
  FieldDecl(LocRge rge, IdentInfo *name, Type *t)
      : ValueDecl(FIELD_DECL, rge, name, t) {}
  static bool is_class(Kind k) { return k == FIELD_DECL; }
};

struct EnumVariantDecl : public ValueDecl {
  std::unique_ptr<Expr> value_expr;
  i64 value;
  bool is_unsigned;

  EnumVariantDecl(LocRge rge, IdentInfo *id, Type *t, std::unique_ptr<Expr> e,
                  i64 value, bool is_unsigned)
      : ValueDecl(ENUM_VARIANT_DECL, rge, id, t), value_expr(std::move(e)),
        value(value), is_unsigned(is_unsigned) {}
  static bool is_class(Kind k) { return k == ENUM_VARIANT_DECL; }
};

struct TypeDecl : public NamedDecl {
  Type *type_for_decl = nullptr;

  TypeDecl(Kind k, LocRge rge, IdentInfo *id) : NamedDecl(k, rge, id) {}
  static bool is_class(Kind k) { return k >= ALIAS_DECL && k <= STRUCT_DECL; }
};

struct AliasDecl : public TypeDecl {
  Type *underlying_type;

  AliasDecl(LocRge rge, IdentInfo *id, Type *underlying)
      : TypeDecl(ALIAS_DECL, rge, id), underlying_type(underlying) {}
  static bool is_class(Kind k) { return k == ALIAS_DECL; }
};

struct TagDecl : public TypeDecl {
  bool is_complete = false;
  TagDecl(Kind k, LocRge rge, IdentInfo *id) : TypeDecl(k, rge, id) {}
  static bool is_class(Kind k) { return k >= ENUM_DECL && k <= STRUCT_DECL; }
};

struct EnumDecl : public TagDecl {
  bool is_scoped;
  Type *int_ty = nullptr;
  std::vector<std::unique_ptr<EnumVariantDecl>> variants;
  EnumDecl(LocRge rge, IdentInfo *id, bool scoped)
      : TagDecl(ENUM_DECL, rge, id), is_scoped(scoped) {}
  static bool is_class(Kind k) { return k == ENUM_DECL; }
};

struct StructDecl : public TagDecl {
  bool has_super = false;
  std::vector<std::unique_ptr<FieldDecl>> fields;
  std::vector<FunctionDecl*> methods;
  StructDecl(LocRge rge, IdentInfo *id) : TagDecl(STRUCT_DECL, rge, id) {}
  static bool is_class(Kind k) { return k == STRUCT_DECL; }
};

inline std::string FunctionDecl::get_mangled_name() const { 
  if (!struct_scope) return get_name(); 
  return struct_scope->get_name() + "__" + get_name(); 
}

#endif // AST_NODES_DECL_HPP_INCLUDED
