#include "ast/nodes/decl.hpp"
#include "ast/nodes/stmt.hpp"
#include "diags/diagnostic.hpp"
#include "lexer/ident.hpp"
#include "lexer/loc.hpp"
#include "lexer/tok.hpp"
#include "parser.hpp"
#include "semantic_analysis/scope.hpp"
#include "semantic_analysis/sema.hpp"
#include <cassert>
#include <memory>
#include <vector>

// TranslationUnitDecl ::= List<Decl>
//                       ;
UPtr<TranslationUnitDecl> Parser::parse_translation_unit() {
  auto res = std::make_unique<TranslationUnitDecl>();
  sema.act_on_start_of_translation_unit();
  while (tok.kind != tok::END_OF_FILE) {
    auto d = parse_top_level_decl();
    if (d) {
      res->add_decl(std::move(d));
    }
  }
  sema.act_on_end_of_translation_unit();
  return res;
}

UPtr<Decl> Parser::parse_top_level_decl() { return parse_decl(false); }

// Decl ::= 'lib' Decl
//        | FnDecl
//        | VarDecl
//        | AliasDecl
//        | StructDecl
//        | EnumDecl
//        ;
UPtr<Decl> Parser::parse_decl(bool in_func, Loc *decl_end) {
  UPtr<Decl> decl = nullptr;
  if (tok.kind == tok::KW_FN) {
    if (in_func) {
      Diag(diag::ERROR, tok.loc, "can't declare a function in function body");
      skip_until(tok::SEMI, tok::LBRACE, true);
      if (tok.kind == tok::SEMI)
        consume_token();
      else {
        consume_brace();
        skip_until(tok::RBRACE);
      }
    }
    decl = parse_fn_decl();
  } else if (tok.kind == tok::KW_LIB) {
    if (in_func) {
      Diag(diag::ERROR, tok.loc, "can't declare a lib decl in function body");
      consume_token();
      return parse_decl(in_func, decl_end);
    }
    auto lib_loc = consume_token();
    decl = parse_decl(in_func, decl_end);
    decl = sema.act_on_lib_decl(std::move(decl), lib_loc);
  } else if (tok.kind == tok::KW_LET) {
    decl = parse_var_decl(in_func);
  } else if (tok.kind == tok::KW_TYPE) {
    decl = parse_type_alias_decl();
  } else if (tok.kind == tok::KW_STRUCT) {
    decl = parse_struct_decl();
  } else if (tok.kind == tok::KW_ENUM) {
    decl = parse_enum_decl();
  } else {
    Diag(diag::ERROR, tok.loc, "unexpected token '%s'",
         tok::get_name(tok.kind));
    consume_any_token();
  }
  if (decl && decl_end)
    *decl_end = decl->src_range.end;
  return decl;
}

// EnumDecl ::= 'enum' id ':' Type ';'
//            | 'enum' id ';'
//            | 'enum' id ':' Type '{' LSepList<EnumVariantDecl, ','> '}' ';'
//            | 'enum' id '{' LSepList<EnumVariantDecl, ','> '}' ';'
//            | 'enum' ':' Type '{' LSepList<EnumVariantDecl, ','> '}' ';'
//            | 'enum' '{' LSepList<EnumVariantDecl, ','> '}' ';'
UPtr<EnumDecl> Parser::parse_enum_decl() {
  assert(tok.kind == tok::KW_ENUM);
  auto kw_loc = consume_token();
  IdentInfo *name = nullptr;
  Type *aliased_type = nullptr;
  if (tok.kind == tok::IDENT) {
    name = tok.ident();
    consume_token();
  }
  if (tok.kind == tok::COLON) {
    consume_token();
    aliased_type = parse_type();
  }
  if (tok.kind == tok::SEMI) {
    auto semi_loc = consume_token();
    if (!name) {
      Diag(diag::WARNING, kw_loc, "forward declaring empty enum");
      return nullptr;
    }
    return sema.act_on_enum_decl(cur_scope, kw_loc, semi_loc, name,
                                 aliased_type);
  }
  expect_and_consume(tok::LBRACE);
  auto decl =
      sema.act_on_start_enum_decl(cur_scope, kw_loc, name, aliased_type);
  i64 cur_val = 0;
  while (tok.kind != tok::RBRACE) {
    parse_enum_variant_decl(decl.get(), cur_val);
    if (tok.kind == tok::COMMA) {
      consume_token();
    } else {
      break;
    }
  }
  expect_and_consume(tok::RBRACE);
  decl = sema.act_on_end_enum_decl(std::move(decl), tok.loc);
  expect_and_consume_semi("Expected ';' after 'enum'");
  return decl;
}

// EnumVariantDecl ::= id
//                   | id '=' Expr(integer, constexpr)
void Parser::parse_enum_variant_decl(EnumDecl *decl, i64 &cur_val) {
  if (tok.kind != tok::IDENT) {
    Diag(diag::ERROR, tok.loc, "expected identifier as declaration name");
    skip_until(tok::COMMA, tok::RBRACE, false, true);
    return;
  }
  auto name = tok.ident();
  auto start_loc = consume_token();
  auto end_loc = start_loc;
  ExprUPtr e = nullptr;
  if (tok.kind == tok::EQUAL) {
    consume_token();
    // TODO: error checking, if parse_integer_constexpr fails ?
    auto [val, ce] = parse_integer_constexpr();
    cur_val = val;
    end_loc = ce->get_end_loc();
    e = std::move(ce);
  }
  sema.act_on_enum_variant_decl(cur_scope, start_loc, end_loc, name,
                                std::move(e), decl, cur_val++);
}

// parses =>  id ':' Type
bool parse_end_var_decl_common(Parser *p, IdentInfo *&ii, Type *&ty,
                               Loc &id_loc, Loc &colon_loc, Loc &end_loc,
                               std::span<Tok> skip_until_toks) {
  if (p->tok.kind != tok::IDENT) {
    Diag(diag::ERROR, p->tok.loc, "expected identifier as declaration name");
    p->skip_until(skip_until_toks, false, true);
    return false;
  }
  ii = p->tok.ident();
  id_loc = p->consume_token();
  if (p->tok.kind != tok::COLON) {
    Diag(diag::ERROR, id_loc, "expected ': Type' after declaration name");
    p->skip_until(skip_until_toks, false, true);
    return false;
  }
  colon_loc = p->consume_token();
  ty = p->parse_type();
  if (ty == nullptr) {
    Diag(diag::ERROR, colon_loc, "expected 'Type' after ':'");
    p->skip_until(skip_until_toks, false, true);
    return false;
  }
  end_loc = p->prev_tok_location;
  return true;
}

// ParamDecl ::= id ':' Type
UPtr<ParamDecl> Parser::parse_param_decl() {
  IdentInfo *ii;
  Type *ty;
  Loc id_loc, colon_loc, end_loc;
  Tok until[] = {tok::COMMA, tok::RPAREN};
  if (!parse_end_var_decl_common(this, ii, ty, id_loc, colon_loc, end_loc,
                                 until)) {
    return nullptr;
  }
  return sema.act_on_param_decl(cur_scope, id_loc, end_loc, ii, ty);
}

// VarDecl ::= 'let' id ':' Type ';'
UPtr<VarDecl> Parser::parse_var_decl(bool in_func) {
  assert(tok.kind == tok::KW_LET);
  auto kw_loc = consume_token();

  if (tok.kind != tok::IDENT) {
    Diag(diag::ERROR, tok.loc, "expected identifier as declaration name");
    skip_until(tok::SEMI, tok::RBRACE, false, true);
    try_consume_token(tok::SEMI);
    return nullptr;
  }
  IdentInfo *ii = tok.ident();
  Loc id_loc = consume_token();

  Type *ty = nullptr;
  if (tok.kind != tok::COLON && tok.kind != tok::EQUAL) {
    Diag(diag::ERROR, id_loc, "expected ': Type' after declaration name");
    skip_until(tok::SEMI, tok::RBRACE, false, true);
    try_consume_token(tok::SEMI);
    return nullptr;
  }
  if (tok.kind == tok::COLON) {
    Loc colon_loc = consume_token();
    ty = parse_type();
    if (ty == nullptr) {
      Diag(diag::ERROR, colon_loc, "expected 'Type' after ':'");
      skip_until(tok::SEMI, tok::RBRACE, false, true);
      try_consume_token(tok::SEMI);
      return nullptr;
    }
  }

  Loc end_loc = prev_tok_location;

  // TODO: check errors better
  ExprUPtr initializer;
  if (in_func && tok.kind == tok::EQUAL) {
    if (ty && ty->is_structure_type()) {
      Diag(diag::UNIMPLEMENTED, end_loc, "TODO: implement this edge case...");
      return nullptr;
    }
    consume_token();

    if (!ty && tok.kind == tok::IDENT && is_start_of_type()) {
      Token ident = tok;
      IdentInfo *ty_ii = tok.ident();
      Loc ty_loc = consume_token();
      auto nd = cur_scope->lookup_named_decl(ty_ii);
      auto sd = nd ? nd->dyn_cast<StructDecl>() : nullptr;
      if (!sd) {
        Diag(diag::ERROR, ty_loc, "expected struct type name after '='");
        return nullptr;
      }

      Token coloncolon = tok;
      expect_and_consume(tok::COLONCOLON);
      if (tok.kind != tok::IDENT) {
        Diag(diag::ERROR, tok.loc, "expected identifier after '::'");
        return nullptr;
      }

      auto method = sema.lookup_method_in_struct(sd, tok.ident());
      if (!method || !method->has_init_ident) {
        unconsume_token(coloncolon);
        unconsume_token(ident);
      } else {
        Loc mloc = consume_token();
        expect_and_consume(tok::LPAREN);
        std::vector<std::unique_ptr<Expr>> args;
        parse_expression_list(args);
        end_loc = tok.loc;
        expect_and_consume(tok::RPAREN);
        expect_and_consume_semi("expected ';' at end of var decl");
        return sema.act_on_var_decl_init_method(cur_scope, kw_loc, id_loc, end_loc, mloc, ii, sd,
            method, std::move(args));
      }
    }

    initializer = parse_expr();
    if (!ty) ty = initializer->type;

    assert(!ty->is_void_type()); // TODO: < better check
  } 

  expect_and_consume_semi("expected ';' at end of var decl");
  return sema.act_on_var_decl(cur_scope, kw_loc, id_loc, end_loc, ii, ty,
                              !in_func, std::move(initializer));
}

// FieldDecl ::= id ':' Type ';'
void Parser::parse_field_decl(StructDecl *my_struct) {
  IdentInfo *ii;
  Type *ty;
  Loc id_loc, colon_loc, end_loc;
  Tok until[] = {tok::SEMI, tok::RBRACE};
  if (!parse_end_var_decl_common(this, ii, ty, id_loc, colon_loc, end_loc,
                                 until)) {
    try_consume_token(tok::SEMI);
    return;
  }
  expect_and_consume_semi("expected ';' at end of var decl");
  sema.act_on_field_decl(my_struct, id_loc, end_loc, ii, ty);
}

UPtr<FunctionDecl> Parser::parse_fn_decl() {
  assert(tok.kind == tok::KW_FN);
  auto start_loc = consume_token();
  if (tok.kind != tok::IDENT) {
    Diag(diag::ERROR, tok.loc, "expected identifier as declaration name");
    skip_until(tok::COMMA, tok::SEMI, false, true);
    return nullptr;
  }

  IdentInfo *fn_name = tok.ident();
  StructDecl *struct_scope = nullptr;
  auto name_loc = consume_token();
  if (tok.kind == tok::COLONCOLON) {
    struct_scope = sema.act_on_scoped_identifier(cur_scope, fn_name, name_loc);
    consume_token();
    // TODO: !!! do correctly !!!
    assert(tok.kind == tok::IDENT);
    fn_name = tok.ident();
    // check that fn_name was only declared once in scope
    name_loc = consume_token();
  }

  expect_and_consume(tok::LPAREN);

  enter_scope(ScopeFlags(SF_FN | SF_DECL | SF_COMPOUND_STMT));

  std::vector<UPtr<ParamDecl>> params;
  bool is_vararg = false;
  if (tok.kind != tok::RPAREN) {
    while (true) {
      if (tok.kind == tok::ELLIPSIS) {
        is_vararg = true;
        Loc sl = consume_token();
        if (tok.kind == tok::STAR) {
          Loc el = consume_token();
          params.push_back(sema.act_on_valist_param_decl(sl, el));
        }
      } else {
        auto param = parse_param_decl();
        if (param)
          params.push_back(std::move(param));
        else
          skip_until(tok::COMMA, tok::RPAREN, false, true);
      }
      if (tok.kind != tok::COMMA)
        break;
      if (is_vararg) {
        Diag(diag::ERROR, tok.loc, "additional param decl after '...'");
        skip_until(tok::RPAREN, false, true);
        break;
      }
      consume_token();
    }
  }
  expect_and_consume(tok::RPAREN);

  bool has_init = false;
  if (struct_scope && tok.kind == tok::IDENT && tok.ident() == IdentInfo::find("init")) {
    consume_token();
    has_init = true;
  }

  Type *return_type;
  if (tok.kind == tok::ARROW) {
    consume_token();
    return_type = parse_type();
  } else {
    return_type = sema.ctx.void_ty;
  }

  if (tok.kind == tok::SEMI) {
    exit_scope();
    auto semi_loc = consume_token();
    auto decl =
        sema.act_on_fn_decl(cur_scope, fn_name, params, return_type, start_loc,
                            semi_loc, is_vararg, struct_scope, has_init);
    return decl;
  }

  auto decl = sema.act_on_start_fn_definition(
      cur_scope->get_parent(), fn_name, params, return_type, start_loc,
      is_vararg, struct_scope, has_init);
  if (!decl) {
    consume_brace();
    skip_until(tok::RBRACE);
    exit_scope();
    return nullptr;
  }

  auto fn_body = parse_compound_stmt_body();

  exit_scope();

  return sema.act_on_end_fn_definition(std::move(decl), std::move(fn_body));
}

UPtr<AliasDecl> Parser::parse_type_alias_decl() {
  assert(tok.kind == tok::KW_TYPE);
  auto kw_loc = consume_token();
  if (tok.kind != tok::IDENT) {
    Diag(diag::ERROR, tok.loc, "expected identifier as declaration name");
    skip_until(tok::SEMI);
    return nullptr;
  }
  auto name = tok.ident();
  consume_token();
  expect_and_consume(tok::EQUAL);
  auto aliased_type = parse_type();
  auto decl = sema.act_on_alias_decl(cur_scope, kw_loc, prev_tok_location, name,
                                     aliased_type);
  expect_and_consume_semi("expected ';' after 'type'");
  return decl;
}

UPtr<StructDecl> Parser::parse_struct_decl() {
  assert(tok.kind == tok::KW_STRUCT);
  auto start_loc = consume_token();
  if (tok.kind != tok::IDENT) {
    Diag(diag::ERROR, tok.loc, "expected identifier as declaration name");
    skip_until(Tok::SEMI);
    return nullptr;
  }
  auto type_name = tok.ident();
  auto ident_loc = consume_token();
  if (tok.kind == tok::SEMI) {
    consume_token();
    return sema.act_on_struct_decl(cur_scope, start_loc, ident_loc, type_name);
  }
  expect_and_consume(tok::LBRACE);
  auto decl =
      sema.act_on_start_struct_decl(cur_scope, start_loc, ident_loc, type_name);
  if (tok.kind == tok::IDENT) {
    if (tok.ident()->name == "super") {
      decl->has_super = true; // < should be in sema ?
      consume_token();
    }
  }
  while (tok.kind != tok::RBRACE) {
    parse_field_decl(decl.get());
  }
  decl = sema.act_on_end_struct_decl(std::move(decl), consume_brace());
  expect_and_consume_semi("expected ';' after 'struct'");
  return decl;
}
