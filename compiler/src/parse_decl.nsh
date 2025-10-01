#include ast_nodes_decl.nsh

enum DeclaratorContext {
    DC_FILE,
    DC_PROTOTYPE,
    DC_KNR_TYPE_LIST,
    DC_TYPE_NAME,
    DC_FUNCTIONAL_CAST,
    DC_MEMBER,
    DC_BLOCK,
    DC_FOR_INIT,
    DC_SELECTION_INIT,
    DC_CONDITION,
    DC_TEMPLATE_PARAM,
    DC_CXX_NEW,
    DC_CXX_CATCH,
    DC_BLOCK_LITERAL,
    DC_LAMBDA_EXPR,
    DC_LAMBDA_EXPR_PARAMETER,
    DC_CONVERSION_ID,
    DC_TRAILING_RETURN,
    DC_TRAILING_RETURN_VAR,
    DC_ALIAS_DECL,
    DC_ALIAS_TEMPLATE,
    DC_REQUIRES_EXPR,
    DC_ASSOCIATION,
};

struct StructType;

lib fn parse_translation_unit() -> TranslationUnitDecl*;
lib fn parse_top_level_decl() -> Decl*;
lib fn parse_decl(decl_ctx: DeclaratorContext, decl_end: Loc*) -> Decl*;
lib fn parse_enum_decl() -> EnumDecl*;
lib fn parse_param_decl() -> ParamDecl*;
lib fn parse_var_decl() -> VarDecl*;
lib fn parse_fn_decl() -> FnDecl*;
lib fn parse_type_alias_decl() -> TypeDecl*;
lib fn parse_struct_decl() -> StructDecl*;
lib fn parse_struct_decl_inner(cur_type: StructType*, cur_decl: StructDecl*) -> StructDecl*;
lib fn parse_field_decl() -> FieldDecl*;
