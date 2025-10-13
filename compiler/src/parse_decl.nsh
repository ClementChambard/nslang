#include ast_nodes_decl.nsh
#include parser.nsh

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

lib fn Parser::parse_translation_unit(self: Parser*) -> TranslationUnitDecl*;
lib fn Parser::parse_top_level_decl(self: Parser*) -> Decl*;
lib fn Parser::parse_decl(self: Parser*, decl_ctx: DeclaratorContext, decl_end: Loc*) -> Decl*;
lib fn Parser::parse_enum_decl(self: Parser*) -> EnumDecl*;
lib fn Parser::parse_param_decl(self: Parser*) -> ParamDecl*;
lib fn Parser::parse_var_decl(self: Parser*) -> VarDecl*;
lib fn Parser::parse_fn_decl(self: Parser*) -> FnDecl*;
lib fn Parser::parse_type_alias_decl(self: Parser*) -> TypeDecl*;
lib fn Parser::parse_struct_decl(self: Parser*) -> StructDecl*;
lib fn Parser::parse_struct_decl_inner(self: Parser*, cur_type: StructType*, cur_decl: StructDecl*) -> StructDecl*;
lib fn Parser::parse_field_decl(self: Parser*) -> FieldDecl*;
