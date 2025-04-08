from lex import Lexer, Token, Loc, LocPtr, Tok
from semantic_analysis import Scope, ScopeFlags
from utils.diagnostic import diag, Diag

CUR_PARSER = None

class Parser:
    lexer: Lexer
    tok: Token
    prev_tok_location: Loc
    paren_count: int
    brace_count: int
    bracket_count: int
    cur_scope: Scope

    def __init__(self, lexer: Lexer):
        self.lexer = lexer
        self.tok = lexer.lex()
        self.prev_tok_location = 0
        self.paren_count = 0
        self.brace_count = 0
        self.bracket_count = 0
        self.cur_scope = Scope(None, 0)
        global CUR_PARSER
        CUR_PARSER = self

    def unconsume_token(self, consumed: Token):
        next_tok = self.tok
        self.lexer.enter_token(consumed, True)
        self.tok = self.lexer.lex()
        self.lexer.enter_token(next_tok, True)

    def consume_token(self) -> Loc:
        assert not self.is_token_special(), "Should consume special tokens with consume_*_token"
        self.prev_tok_location = self.tok.loc
        self.tok = self.lexer.lex()
        return self.prev_tok_location

    def try_consume_token(self, expected: Tok, loc_ptr: LocPtr = None) -> bool:
        if self.tok.ty != expected:
            return False
        loc = self.consume_token()
        if loc_ptr is not None:
            loc_ptr.value = loc
        return True

    def consume_any_token(self) -> Loc:
        if self.is_token_paren():
            return self.consume_paren()
        if self.is_token_brace():
            return self.consume_brace()
        if self.is_token_bracket():
            return self.consume_bracket()
        return self.consume_token()

    def is_token_paren(self) -> bool:
        return self.tok.ty == Tok.LPAREN or self.tok.ty == Tok.RPAREN

    def is_token_brace(self) -> bool:
        return self.tok.ty == Tok.LBRACE or self.tok.ty == Tok.RBRACE

    def is_token_bracket(self) -> bool:
        return self.tok.ty == Tok.LSQUARE or self.tok.ty == Tok.RSQUARE

    def is_token_special(self) -> bool:
        return self.is_token_paren() or self.is_token_brace() or self.is_token_bracket()

    def consume_paren(self) -> Loc:
        assert self.is_token_paren(), "Wrong consume method"
        if self.tok.ty == Tok.LPAREN:
            self.paren_count += 1
        elif self.paren_count > 0:
            # self.angle_brackets.clear()
            self.paren_count -= 1
        self.prev_tok_location = self.tok.loc
        self.tok = self.lexer.lex()
        return self.prev_tok_location

    def consume_brace(self) -> Loc:
        assert self.is_token_brace(), "Wrong consume method"
        if self.tok.ty == Tok.LBRACE:
            self.brace_count += 1
        elif self.brace_count > 0:
            # self.angle_brackets.clear()
            self.brace_count -= 1
        self.prev_tok_location = self.tok.loc
        self.tok = self.lexer.lex()
        return self.prev_tok_location

    def consume_bracket(self) -> Loc:
        assert self.is_token_bracket(), "Wrong consume method"
        if self.tok.ty == Tok.LSQUARE:
            self.bracket_count += 1
        elif self.bracket_count > 0:
            # self.angle_brackets.clear()
            self.bracket_count -= 1
        self.prev_tok_location = self.tok.loc
        self.tok = self.lexer.lex()
        return self.prev_tok_location

    def expect_and_consume(self, expected: Tok, diag_id: str = "expected {}", msg: str = "") -> bool:
        if self.tok.ty == expected:
            self.consume_any_token()
            return False
        if is_common_typo(expected, self.tok.ty):
            loc = self.tok.loc
            # fixit with replacement ...
            diag(loc, diag_id + " " + msg, Diag.ERROR)
            self.consume_any_token()
            return False
        # fixit ...
        diag_msg = ""
        if diag_id == "expected {}":
            diag_msg = diag_id.format(expected)
        elif diag_id == "expected {} after {}":
            diag_msg = diag_id.format(expected, msg)
        else:
            diag_msg = diag_id.format(msg)
        loc = self.tok.loc # should get loc of end of previous token
        diag(loc, diag_msg, Diag.ERROR)
        return True

    def expect_and_consume_semi(self, diag_id: str = "expected ';'", token_used: str = "") -> bool:
        if self.try_consume_token(Tok.SEMI):
            return False
        if self.tok.ty in [Tok.RPAREN, Tok.RSQUARE] and self.next_token().ty == Tok.SEMI:
            diag(self.tok.loc, f"extraneous '{self.tok.ty}' before ';'", Diag.ERROR) #fixit
            self.consume_any_token()
            self.consume_token()
            return False
        return self.expect_and_consume(Tok.SEMI, diag_id, token_used)

    def skip_until(self, *until_toks, **options):
        toks = list(until_toks)
        stop_before_match = ("stop_before_match" in options.keys()) and options[
            "stop_before_match"
        ]
        stop_at_semi = ("stop_at_semi" in options.keys()) and options["stop_at_semi"]
        is_first_token_skipped = True
        while True:
            for i in range(len(toks)):
                if self.tok.ty == toks[i]:
                    if not stop_before_match:
                        self.consume_any_token()
                    return True
            if len(toks) == 1 and toks[0] == Tok.EOF and not stop_at_semi:
                while self.tok.ty != Tok.EOF:
                    self.consume_any_token()
                return True
            match self.tok.ty:
                case Tok.EOF:
                    return False
                case Tok.LPAREN:
                    self.consume_paren()
                    self.skip_until(Tok.RPAREN)
                case Tok.LSQUARE:
                    self.consume_bracket()
                    self.skip_until(Tok.RSQUARE)
                case Tok.LBRACE:
                    self.consume_brace()
                    self.skip_until(Tok.RBRACE)
                case Tok.QUESTION:
                    self.consume_token()
                    self.skip_until(
                        Tok.COLON,
                        stop_at_semi=True,
                        stop_before_match=stop_before_match,
                    )
                case Tok.RPAREN:
                    if self.paren_count > 0 and not is_first_token_skipped:
                        return False
                    self.consume_paren()
                case Tok.RSQUARE:
                    if self.bracket_count > 0 and not is_first_token_skipped:
                        return False
                    self.consume_bracket()
                case Tok.RBRACE:
                    if self.brace_count > 0 and not is_first_token_skipped:
                        return False
                    self.consume_brace()
                case Tok.SEMI:
                    if stop_at_semi:
                        return False
                    self.consume_token()
                case _:
                    self.consume_any_token()
            is_first_token_skipped = False

    def enter_scope(self, scope_flags: ScopeFlags):
        self.cur_scope = Scope(self.cur_scope, scope_flags)

    def exit_scope(self):
        assert self.cur_scope is not None
        # actions.act_on_pop_scope(self.tok.loc, self.cur_scope)
        old_scope = self.cur_scope
        assert old_scope.parent_scope is not None
        self.cur_scope = old_scope.parent_scope

    def parse(self) -> []:
        from .decl import parse_translation_unit
        return parse_translation_unit()


def is_common_typo(expected: Tok, actual: Tok) -> bool:
    match expected:
        case Tok.SEMI:
            return actual == Tok.COLON or actual == Tok.COMMA
        case _:
            return False

def parser() -> Parser:
    assert CUR_PARSER is not None
    return CUR_PARSER
