from dataclasses import dataclass
from typing import List
from . import FileLexer, Token, IdentInfo, Tok, Loc

@dataclass
class Lexer:
    cur_lexer: FileLexer | None
    lexer_stack: List[FileLexer]
    cached_tokens: List[Token]
    include_paths: List[str]
    already_included_files: List[str]

    def __init__(self):
        self.cur_lexer = None
        self.lexer_stack = []
        self.cached_tokens = []
        self.include_paths = []
        self.already_included_files = []

    def add_include_paths(self, paths):
        self.include_paths += paths

    def enter_token(self, token: Token, a: bool):
        _ = a # UNUSED
        self.cached_tokens.append(token)

    def enter_source_file(self, filename: str):
        if self.cur_lexer is not None:
            self.lexer_stack.append(self.cur_lexer)
        self.cur_lexer = FileLexer(filename, self)

    def end_source_file(self, token: Token):
        if self.cur_lexer is None:
            return
        if len(self.lexer_stack) == 0:
            self.cur_lexer.construct_token(token, self.cur_lexer.pos + 1, Tok.EOF)
            return
        self.cur_lexer = self.lexer_stack[-1]
        self.lexer_stack.pop()
        tmp_tok = self.cur_lexer.lex()
        token.copy_from(tmp_tok)

    def find_file_to_include(self, filename: str, loc: Loc) -> str:
        from utils.diagnostic import diag, Diag
        import os.path
        if os.path.isfile(filename):
            return filename
        cur_filename = self.cur_lexer.f.filename if self.cur_lexer is not None else ""
        # get dir name of filename
        if '/' in cur_filename:
            # TODO: what if two consecutive slashes in filepath
            path = cur_filename.split('/')
            dir_path = '/'.join(path[:-1])
            f = dir_path + '/' + filename
            if os.path.isfile(f):
                return f
        for ip in self.include_paths:
            f = ip + "/" + filename
            if os.path.isfile(f):
                return f
        diag(loc, f"include file not found: '{filename}'", Diag.ERROR)
        return "/dev/null"

    def handle_directive(self, token: Token):
        assert self.cur_lexer is not None
        dir_kind = self.cur_lexer.read_to_whitespace()
        assert dir_kind == "include", f"Only include directive supported (not {dir_kind})"
        include_file = self.cur_lexer.read_to_end_of_line().strip()
        include_file = self.find_file_to_include(include_file, token.loc)
        if include_file in self.already_included_files:
            include_file = "/dev/null"
        else:
            self.already_included_files.append(include_file)
        self.lexer_stack.append(self.cur_lexer)
        self.cur_lexer = FileLexer(include_file, self)
        token.copy_from(self.lex())

    def handle_ident(self, token: Token, ident_info: IdentInfo):
        _, _ = token, ident_info # UNUSED
        pass

    def lex(self) -> Token:
        if len(self.cached_tokens) != 0:
            token = self.cached_tokens[0]
            self.cached_tokens = self.cached_tokens[1:]
            return token
        if self.cur_lexer is None:
            assert False, "Nothing to lex"
        return self.cur_lexer.lex()
