#include token.nsh
#include loc.nsh
#include sema_scope.nsh
#include lexer.nsh

struct TranslationUnitDecl;

struct Parser {
  lexer: Lexer*;
  tok: Token*;
  prev_tok_location: Loc;
  paren_count: i64;
  brace_count: i64;
  bracket_count: i64;
  cur_scope: Scope*;
};

lib fn Parser::new(lexer: Lexer*) -> Parser*;
lib fn Parser::delete(self: Parser*);

lib fn Parser::unconsume_token(self: Parser*, consumed: Token*);
lib fn Parser::consume_token(self: Parser*) -> Loc;
lib fn Parser::try_consume_token(self: Parser*, expected: Tok, loc_ptr: Loc*) -> bool;
lib fn Parser::consume_any_token(self: Parser*) -> Loc;
lib fn Parser::is_token_paren(self: Parser*) -> bool;
lib fn Parser::is_token_brace(self: Parser*) -> bool;
lib fn Parser::is_token_bracket(self: Parser*) -> bool;
lib fn Parser::is_token_special(self: Parser*) -> bool;
lib fn Parser::consume_paren(self: Parser*) -> Loc;
lib fn Parser::consume_brace(self: Parser*) -> Loc;
lib fn Parser::consume_bracket(self: Parser*) -> Loc;
lib fn Parser::expect_and_consume(self: Parser*, expected: Tok, diag_id: CStr, msg: CStr) -> bool; 
lib fn Parser::next_token(self: Parser*) -> Token*;
lib fn Parser::expect_and_consume_semi(self: Parser*, diag_id: CStr, token_used: CStr) -> bool;
lib fn Parser::skip_until(self: Parser*, until_toks: Tok*, until_toks_count: i64, stop_at_semi: bool /* = false */, stop_before_match: bool /* = false */) -> bool;
lib fn Parser::enter_scope(self: Parser*, scope_flags: ScopeFlags);
lib fn Parser::exit_scope(self: Parser*);
lib fn Parser::parse(self: Parser*) -> TranslationUnitDecl*;

lib fn is_common_typo(expected: Tok, actual: Tok) -> bool;
