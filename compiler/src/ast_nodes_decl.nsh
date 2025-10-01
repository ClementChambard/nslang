#include darray.nsh
#include loc.nsh
#include str.nsh

struct Type;

enum DeclKind {
  DECLKIND_NONE,
  DECLKIND_TU,
  DECLKIND_NAMED,
  DECLKIND_VALUE,
  DECLKIND_ENUMVARIANT,
  DECLKIND_FIELD,
  DECLKIND_VAR,
  DECLKIND_PARAM,
  DECLKIND_FN,
  DECLKIND_TYPE,
  DECLKIND_ENUM,
  DECLKIND_STRUCT,
};

struct Decl {
  kind: DeclKind;
  src_range: LocRge; // default: (0, 0)
  is_lib: bool;      // default: false
};

lib fn Decl::ast_free(self: Decl*);

struct TranslationUnitDecl {
  super base: Decl;
  decls: Decl**;
  decls_count: i64;
};

lib fn TranslationUnitDecl::new(decls: Decl**, decls_count: i64) -> TranslationUnitDecl*;

struct NamedDecl {
  super decl_base: Decl;
  name: CStr; // default: nullptr
};
lib fn Decl::is_named(self: Decl*) -> bool;

struct ValueDecl {
  super named_decl_base: NamedDecl;
  ty: Type*;  // default: nullptr (void)
};
lib fn Decl::is_value(self: Decl*) -> bool;

struct TypeDecl {
  super named_decl_base: NamedDecl;
  ty: Type*; // default: nullptr (void)
};
lib fn Decl::is_type(self: Decl*) -> bool;

lib fn TypeDecl::new(sl: Loc, el: Loc, name: CStr, ty: Type*) -> TypeDecl*;

struct EnumVariantDecl {
  super value_decl_base: ValueDecl;
  val: i64;
};

lib fn EnumVariantDecl::new(sl: Loc, el: Loc, name: CStr, ty: Type*, val: i64) -> EnumVariantDecl*;

struct EnumDecl {
  super type_decl_base: TypeDecl;
  variants: EnumVariantDecl**;
  variants_count: i64;
};

lib fn EnumDecl::new(sl: Loc, el: Loc, name: CStr, ty: Type*, variants: EnumVariantDecl**, variants_count: i64) -> EnumDecl*;

struct FieldDecl { super base: ValueDecl; };
struct VarDecl { super base: ValueDecl; };
struct ParamDecl { super base: ValueDecl; };

lib fn FieldDecl::new(sl: Loc, el: Loc, name: CStr, ty: Type*) -> FieldDecl*;
lib fn VarDecl::new(sl: Loc, el: Loc, name: CStr, ty: Type*) -> VarDecl*;
lib fn ParamDecl::new(sl: Loc, el: Loc, name: CStr, ty: Type*) -> ParamDecl*;

struct StructDecl {
  super type_decl_base: TypeDecl;
  fields: DArray;
  first_field_is_super: bool;
};

lib fn StructDecl::new(sl: Loc, el: Loc, name: CStr, ty: Type*) -> StructDecl*;

struct CompoundStmt;

struct FnDecl {
  super value_decl_base: ValueDecl;
  param_decls: ParamDecl**;
  param_decls_count: i64;
  is_vararg: bool;
  body: CompoundStmt*;
};

lib fn FnDecl::new(sl: Loc, el: Loc, name: CStr, param_decls: ParamDecl**, param_decls_count: i64, return_type: Type*, is_vararg: bool) -> FnDecl*;
lib fn FnDecl::set_body(self: FnDecl*, body: CompoundStmt*);
lib fn FnDecl::get_params_range(self: FnDecl*, out: LocRge*);
