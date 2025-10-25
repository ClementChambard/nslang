#include "lexer/file.hpp"
#include "lexer/file_lexer.hpp"
#include "lexer/ident.hpp"
#include <catch2/catch_test_macros.hpp>

#include <cstdio>
#include <lexer/lexer.hpp>
#include <lexer/tok.hpp>

TEST_CASE("token kinds") {
  SECTION("tok::is_keyword") {
    CHECK(tok::is_keyword(tok::KW_I8));
    CHECK(tok::is_keyword(tok::KW_IF));
    CHECK(tok::is_keyword(tok::KW_WHILE));
    CHECK(tok::is_keyword(tok::KW_VOID));
    CHECK(!tok::is_keyword(tok::NUM));
    CHECK(!tok::is_keyword(tok::LPAREN));
  }
  SECTION("tok::is_builtin_type") {
    CHECK(tok::is_builtin_type(tok::KW_VOID));
    CHECK(!tok::is_builtin_type(tok::NUM));
    CHECK(!tok::is_builtin_type(tok::RPAREN));
    CHECK(tok::is_builtin_type(tok::KW_I64));
  }
}

TEST_CASE("idents") {
  IdentInfo::create_ident_info_list();
  SECTION("ident infos for builtin idents give correct token kind") {
    CHECK(IdentInfo::find("fn")->token_kind == tok::KW_FN);
    CHECK(IdentInfo::find("void")->token_kind == tok::KW_VOID);
    CHECK(IdentInfo::find("let")->token_kind == tok::KW_LET);
  }
  SECTION("ident infos for new idents can be created with correct token kind") {
    CHECK(IdentInfo::find("test_ident")->token_kind == tok::IDENT);
    CHECK(IdentInfo::find("test_ident2")->token_kind == tok::IDENT);
    CHECK(IdentInfo::find("test_ident")->token_kind == tok::IDENT);
  }
  SECTION("ident infos have correct name") {
    CHECK(IdentInfo::find("aaa")->name == "aaa");
    CHECK(IdentInfo::find("askljaa")->name == "askljaa");
    CHECK(IdentInfo::find("_akls")->name == "_akls");
    CHECK(IdentInfo::find("u8as")->name == "u8as");
  }
}

// TODO: how to get the file no matter where it is ?
#define INPUT_FILES_LOC                                                        \
  "/home/clement/dev/nslang/cppver/tests/lexer/input_files/"

TEST_CASE("comments are ignored") {
  IdentInfo::create_ident_info_list();
  FileLexer lexer(INPUT_FILES_LOC "comments_are_ignored.ns");
  CHECK(lexer.lex().kind == tok::END_OF_FILE);
}

TEST_CASE("lex identifiers") {
  IdentInfo::create_ident_info_list();
  FileLexer lexer(INPUT_FILES_LOC "identifiers.ns");
  // simple ident
  auto tok = lexer.lex();
  printf("ident: '%s'\n", tok.ident()->name.c_str());
  REQUIRE(tok.kind == tok::IDENT);
  CHECK(tok.ident() == IdentInfo::find("a"));
  // all allowed chars
  tok = lexer.lex();
  REQUIRE(tok.kind == tok::IDENT);
  printf("ident: '%s'\n", tok.ident()->name.c_str());
  CHECK(tok.ident() == IdentInfo::find("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"));
  // start with underscore
  tok = lexer.lex();
  REQUIRE(tok.kind == tok::IDENT);
  CHECK(tok.ident() == IdentInfo::find("_aaaa"));
  // start with uppercase letter
  tok = lexer.lex();
  REQUIRE(tok.kind == tok::IDENT);
  CHECK(tok.ident() == IdentInfo::find("Aaaaa"));
  // just underscores
  tok = lexer.lex();
  REQUIRE(tok.kind == tok::IDENT);
  CHECK(tok.ident() == IdentInfo::find("_____"));
  // keyword
  tok = lexer.lex();
  CHECK(tok.kind == tok::KW_FN);
  // can't start with a number
  tok = lexer.lex();
  CHECK(tok.kind != tok::IDENT);
}

TEST_CASE("lex literals") {
  FileLexer lexer(INPUT_FILES_LOC "literals.ns");
  CHECK(lexer.lex().kind == tok::NUM);
  CHECK(lexer.lex().kind == tok::NUM);
  CHECK(lexer.lex().kind == tok::NUM);
  CHECK(lexer.lex().kind == tok::NUM);
  CHECK(lexer.lex().kind == tok::NUM);

  CHECK(lexer.lex().kind == tok::STR);
  CHECK(lexer.lex().kind == tok::STR);
  CHECK(lexer.lex().kind == tok::STR);

  CHECK(lexer.lex().kind == tok::CHR);
  CHECK(lexer.lex().kind == tok::CHR);
  CHECK(lexer.lex().kind == tok::CHR);
}

TEST_CASE("lex each token once") {
  IdentInfo::create_ident_info_list();
  FileLexer lexer(INPUT_FILES_LOC "each_token_once.ns");
  // IDENTIFIER
  CHECK(lexer.lex().kind == tok::IDENT);

  // CONSTANTS
  CHECK(lexer.lex().kind == tok::NUM);
  CHECK(lexer.lex().kind == tok::STR);
  CHECK(lexer.lex().kind == tok::CHR);

  // PUNCTUATION
  CHECK(lexer.lex().kind == tok::LPAREN);
  CHECK(lexer.lex().kind == tok::RPAREN);
  CHECK(lexer.lex().kind == tok::LBRACE);
  CHECK(lexer.lex().kind == tok::RBRACE);
  CHECK(lexer.lex().kind == tok::LSQUARE);
  CHECK(lexer.lex().kind == tok::RSQUARE);
  CHECK(lexer.lex().kind == tok::SEMI);
  CHECK(lexer.lex().kind == tok::COLON);
  CHECK(lexer.lex().kind == tok::COLONCOLON);
  CHECK(lexer.lex().kind == tok::PERIOD);
  CHECK(lexer.lex().kind == tok::ARROW);
  CHECK(lexer.lex().kind == tok::ELLIPSIS);
  CHECK(lexer.lex().kind == tok::GREATER);
  CHECK(lexer.lex().kind == tok::GREATERGREATER);
  CHECK(lexer.lex().kind == tok::COMMA);
  CHECK(lexer.lex().kind == tok::EQUAL);
  CHECK(lexer.lex().kind == tok::EXCLAIM);
  CHECK(lexer.lex().kind == tok::STAREQUAL);
  CHECK(lexer.lex().kind == tok::SLASHEQUAL);
  CHECK(lexer.lex().kind == tok::PERCENTEQUAL);
  CHECK(lexer.lex().kind == tok::PLUSEQUAL);
  CHECK(lexer.lex().kind == tok::MINUSEQUAL);
  CHECK(lexer.lex().kind == tok::LESSLESSEQUAL);
  CHECK(lexer.lex().kind == tok::GREATERGREATEREQUAL);
  CHECK(lexer.lex().kind == tok::AMPEQUAL);
  CHECK(lexer.lex().kind == tok::CARETEQUAL);
  CHECK(lexer.lex().kind == tok::PIPEEQUAL);
  CHECK(lexer.lex().kind == tok::QUESTION);
  CHECK(lexer.lex().kind == tok::PIPEPIPE);
  CHECK(lexer.lex().kind == tok::AMPAMP);
  CHECK(lexer.lex().kind == tok::PIPE);
  CHECK(lexer.lex().kind == tok::TILDE);
  CHECK(lexer.lex().kind == tok::CARET);
  CHECK(lexer.lex().kind == tok::AMP);
  CHECK(lexer.lex().kind == tok::EXCLAIMEQUAL);
  CHECK(lexer.lex().kind == tok::EQUALEQUAL);
  CHECK(lexer.lex().kind == tok::LESSEQUAL);
  CHECK(lexer.lex().kind == tok::LESS);
  CHECK(lexer.lex().kind == tok::GREATEREQUAL);
  CHECK(lexer.lex().kind == tok::LESSLESS);
  CHECK(lexer.lex().kind == tok::PLUS);
  CHECK(lexer.lex().kind == tok::PLUSPLUS);
  CHECK(lexer.lex().kind == tok::MINUS);
  CHECK(lexer.lex().kind == tok::MINUSMINUS);
  CHECK(lexer.lex().kind == tok::PERCENT);
  CHECK(lexer.lex().kind == tok::SLASH);
  CHECK(lexer.lex().kind == tok::STAR);

  // KEYWORDS
  CHECK(lexer.lex().kind == tok::KW_FN);
  CHECK(lexer.lex().kind == tok::KW_LET);
  CHECK(lexer.lex().kind == tok::KW_LIB);
  CHECK(lexer.lex().kind == tok::KW_TYPE);
  CHECK(lexer.lex().kind == tok::KW_STRUCT);
  CHECK(lexer.lex().kind == tok::KW_ENUM);
  CHECK(lexer.lex().kind == tok::KW_I8);
  CHECK(lexer.lex().kind == tok::KW_I16);
  CHECK(lexer.lex().kind == tok::KW_I32);
  CHECK(lexer.lex().kind == tok::KW_I64);
  CHECK(lexer.lex().kind == tok::KW_U8);
  CHECK(lexer.lex().kind == tok::KW_U16);
  CHECK(lexer.lex().kind == tok::KW_U32);
  CHECK(lexer.lex().kind == tok::KW_U64);
  CHECK(lexer.lex().kind == tok::KW_F32);
  CHECK(lexer.lex().kind == tok::KW_F64);
  CHECK(lexer.lex().kind == tok::KW_BOOL);
  CHECK(lexer.lex().kind == tok::KW_VOID);
  CHECK(lexer.lex().kind == tok::KW_SIZEOF);
  CHECK(lexer.lex().kind == tok::KW_CAST);
  CHECK(lexer.lex().kind == tok::KW_IF);
  CHECK(lexer.lex().kind == tok::KW_ELSE);
  CHECK(lexer.lex().kind == tok::KW_TRUE);
  CHECK(lexer.lex().kind == tok::KW_FALSE);
  CHECK(lexer.lex().kind == tok::KW_NULLPTR);
  CHECK(lexer.lex().kind == tok::KW_VAARG);
  CHECK(lexer.lex().kind == tok::KW_VAARGS);
  CHECK(lexer.lex().kind == tok::KW_CASE);
  CHECK(lexer.lex().kind == tok::KW_DEFAULT);
  CHECK(lexer.lex().kind == tok::KW_SWITCH);
  CHECK(lexer.lex().kind == tok::KW_WHILE);
  CHECK(lexer.lex().kind == tok::KW_DO);
  CHECK(lexer.lex().kind == tok::KW_FOR);
  CHECK(lexer.lex().kind == tok::KW_CONTINUE);
  CHECK(lexer.lex().kind == tok::KW_BREAK);
  CHECK(lexer.lex().kind == tok::KW_RETURN);
 
  CHECK(lexer.lex().kind == tok::END_OF_FILE);
}

TEST_CASE("include") {
  SECTION("relative includes") {
    Lexer lexer;
    lexer.enter_source_file(INPUT_FILES_LOC "includes.ns");
    auto tok0 = lexer.lex();
    auto tok1 = lexer.lex();
    auto tok2 = lexer.lex();
    auto tok3 = lexer.lex();
    REQUIRE(lexer.lex().kind == tok::END_OF_FILE);
    REQUIRE(tok0.kind == tok::IDENT);
    REQUIRE(tok1.kind == tok::IDENT);
    REQUIRE(tok2.kind == tok::IDENT);
    REQUIRE(tok3.kind == tok::IDENT);
    CHECK(tok0.ident() == IdentInfo::find("test0"));
    CHECK(tok1.ident() == IdentInfo::find("test1"));
    CHECK(tok2.ident() == IdentInfo::find("test2"));
    CHECK(tok3.ident() == IdentInfo::find("test3"));
  }

  SECTION("include path") {
    Lexer lexer;
    lexer.add_include_path(INPUT_FILES_LOC "subdir2");
    lexer.enter_source_file(INPUT_FILES_LOC "includepath.ns");
    auto tok0 = lexer.lex();
    REQUIRE(lexer.lex().kind == tok::END_OF_FILE);
    REQUIRE(tok0.kind == tok::IDENT);
    CHECK(tok0.ident() == IdentInfo::find("test2"));
  }

}

// TODO: 
//   - test locations ?
//   - test diagnostics ?
