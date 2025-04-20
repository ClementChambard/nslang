from dataclasses import dataclass
from . import OpenedFile, Token, Tok, IdentInfo
from .lex_utils import ident_continue


@dataclass
class FileLexer:
    f: OpenedFile
    pos: int
    the_lexer: "Lexer | None"

    def __init__(self, filename: str, the_lexer: "Lexer | None" = None):
        f = OpenedFile.find(filename)
        if f is None:
            self.f = OpenedFile(filename)
        else:
            self.f = f
        self.pos = 0
        self.the_lexer = the_lexer

    def construct_token(self, token: Token, end_pos: int, ty: Tok):
        token.ty = ty
        token.loc = self.f.pos_offset + self.pos
        token.value = self.f.source[self.pos:end_pos]
        self.pos = end_pos

    def handle_end_of_file(self, token: Token, cur_pos: int):
        if self.the_lexer is None:
            self.construct_token(token, cur_pos, Tok.EOF)
            return
        self.the_lexer.end_source_file(token)

    def read_to_whitespace(self):
        out = ""
        while not self.f.source[self.pos].isspace():
            out += self.f.source[self.pos]
            self.pos += 1
        return out

    def read_to_end_of_line(self):
        out = ""
        while not self.f.source[self.pos] in ["\n", "\r"]:
            out += self.f.source[self.pos]
            self.pos += 1
        last = self.f.source[self.pos]
        self.pos += 1
        if last == "\n" and self.f.source[self.pos] == "\r":
            self.pos += 1
        elif last == "\r" and self.f.source[self.pos] == "\n":
            self.pos += 1
        return out

    def handle_directive(self, token: Token):
        if self.the_lexer is None:
            self.construct_token(token, self.pos, Tok.EOF)
            return
        self.the_lexer.handle_directive(token)

    def lex_ident(self, token: Token, cur_pos: int):
        c = self.f.source[cur_pos]
        while ident_continue(c):
            cur_pos += 1
            c = self.f.source[cur_pos]
        ident_info = IdentInfo.find(self.f.source[self.pos:cur_pos])
        if self.the_lexer is not None and ident_info.needs_handling():
            self.the_lexer.handle_ident(token, ident_info)
        self.construct_token(token, cur_pos, ident_info.ty)
        token.value = ident_info

    def lex_number(self, token: Token, cur_pos: int):
        while True:
            c = self.f.source[cur_pos]
            if c == "e" and (self.f.source[cur_pos + 1] == "+" or self.f.source[cur_pos + 1] == "-"):
                cur_pos += 2
            elif c == "'" and ident_continue(self.f.source[cur_pos + 1]):
                cur_pos += 2
            elif ident_continue(c) or c == ".":
                cur_pos += 1
            else:
                break
        self.construct_token(token, cur_pos, Tok.NUM)

    def skip_to_end_of_line_comment(self, cur_pos: int):
        while True:
            c = self.f.source[cur_pos]
            if c == "\0" or c == "\n" or c == "\r":
                return cur_pos
            cur_pos += 1

    def skip_to_end_of_multiline_comment(self, cur_pos: int):
        from utils.diagnostic import diag, Diag
        while True:
            c = self.f.source[cur_pos]
            if c == "\0":
                if cur_pos + 1 != self.f.src_len:
                    diag(self.f.pos_offset + cur_pos, "NULL in file", Diag.WARNING)
                    cur_pos += 1
                    continue
                diag(self.f.pos_offset + cur_pos, "unterminated multiline comment.", Diag.ERROR)
                return cur_pos
            elif c == "*" and self.f.source[cur_pos + 1] == "/":
                return cur_pos + 2
            cur_pos += 1

    def lex_char_literal(self, token: Token, cur_pos: int):
        c = self.f.source[cur_pos]
        while c != "'":
            if c == "\\":
                cur_pos += 1
            cur_pos += 1
            c = self.f.source[cur_pos]
        self.construct_token(token, cur_pos + 1, Tok.CHR)

    def lex_str_literal(self, token: Token, cur_pos: int):
        c = self.f.source[cur_pos]
        while c != "\"":
            if c == "\\":
                cur_pos += 1
            cur_pos += 1
            c = self.f.source[cur_pos]
        self.construct_token(token, cur_pos + 1, Tok.STR)

    def lex_internal(self, token: Token):
        from utils.diagnostic import diag, Diag
        while self.f.source[self.pos].isspace():
            self.pos += 1
        c = self.f.source[self.pos]
        cur_pos = self.pos + 1
        if c == "\0":
            if cur_pos == self.f.src_len:
                self.handle_end_of_file(token, cur_pos)
            else:
                diag(self.pos + self.f.pos_offset, "NULL in file", Diag.WARNING)
                self.pos = cur_pos
                self.lex_internal(token)
        elif c == "#":
            token.loc = self.f.pos_offset + self.pos
            self.pos += 1
            self.handle_directive(token)
        elif c <= "9" and c >= "0":
            self.lex_number(token, cur_pos)
        elif (c <= "z" and c >= "a") or (c <= "Z" and c >= "A") or c == "_":
            self.lex_ident(token, cur_pos)
        elif c == "(":
            self.construct_token(token, cur_pos, Tok.LPAREN)
        elif c == ")":
            self.construct_token(token, cur_pos, Tok.RPAREN)
        elif c == "{":
            self.construct_token(token, cur_pos, Tok.LBRACE)
        elif c == "}":
            self.construct_token(token, cur_pos, Tok.RBRACE)
        elif c == "[":
            self.construct_token(token, cur_pos, Tok.LSQUARE)
        elif c == "]":
            self.construct_token(token, cur_pos, Tok.RSQUARE)
        elif c == ";":
            self.construct_token(token, cur_pos, Tok.SEMI)
        elif c == ":":
            c = self.f.source[cur_pos]
            if c == ":":
                self.construct_token(token, cur_pos + 1, Tok.COLONCOLON)
            else:
                self.construct_token(token, cur_pos, Tok.COLON)
        elif c == ",":
            self.construct_token(token, cur_pos, Tok.COMMA)
        elif c == "?":
            self.construct_token(token, cur_pos, Tok.QUESTION)
        elif c == "~":
            self.construct_token(token, cur_pos, Tok.TILDE)
        elif c == "<":
            c = self.f.source[cur_pos]
            if c == "<":
                c = self.f.source[cur_pos + 1]
                if c == "=":
                    self.construct_token(token, cur_pos + 2, Tok.LESSLESSEQUAL)
                else:
                    self.construct_token(token, cur_pos + 1, Tok.LESSLESS)
            elif c == "=":
                self.construct_token(token, cur_pos + 1, Tok.LESSEQUAL)
            else:
                self.construct_token(token, cur_pos, Tok.LESS)
        elif c == ">":
            c = self.f.source[cur_pos]
            if c == ">":
                c = self.f.source[cur_pos + 1]
                if c == "=":
                    self.construct_token(token, cur_pos + 2, Tok.GREATERGREATEREQUAL)
                else:
                    self.construct_token(token, cur_pos + 1, Tok.GREATERGREATER)
            elif c == "=":
                self.construct_token(token, cur_pos + 1, Tok.GREATEREQUAL)
            else:
                self.construct_token(token, cur_pos, Tok.GREATER)
        elif c == "=":
            c = self.f.source[cur_pos]
            if c == "=":
                self.construct_token(token, cur_pos + 1, Tok.EQUALEQUAL)
            else:
                self.construct_token(token, cur_pos, Tok.EQUAL)
        elif c == "+":
            c = self.f.source[cur_pos]
            if c == "+":
                self.construct_token(token, cur_pos + 1, Tok.PLUSPLUS)
            elif c == "=":
                self.construct_token(token, cur_pos + 1, Tok.PLUSEQUAL)
            else:
                self.construct_token(token, cur_pos, Tok.PLUS)
        elif c == "-":
            c = self.f.source[cur_pos]
            if c == "-":
                self.construct_token(token, cur_pos + 1, Tok.MINUSMINUS)
            elif c == "=":
                self.construct_token(token, cur_pos + 1, Tok.MINUSEQUAL)
            elif c == ">":
                self.construct_token(token, cur_pos + 1, Tok.ARROW)
            else:
                self.construct_token(token, cur_pos, Tok.MINUS)
        elif c == "*":
            c = self.f.source[cur_pos]
            if c == "=":
                self.construct_token(token, cur_pos + 1, Tok.STAREQUAL)
            else:
                self.construct_token(token, cur_pos, Tok.STAR)
        elif c == "/":
            c = self.f.source[cur_pos]
            if c == "/":
                self.pos = self.skip_to_end_of_line_comment(cur_pos + 1)
                self.lex_internal(token)
            elif c == "*":
                self.pos = self.skip_to_end_of_multiline_comment(cur_pos + 1)
                self.lex_internal(token)
            elif c == "=":
                self.construct_token(token, cur_pos + 1, Tok.SLASHEQUAL)
            else:
                self.construct_token(token, cur_pos, Tok.SLASH)
        elif c == "%":
            c = self.f.source[cur_pos]
            if c == "=":
                self.construct_token(token, cur_pos + 1, Tok.PERCENTEQUAL)
            else:
                self.construct_token(token, cur_pos, Tok.PERCENT)
        elif c == "!":
            c = self.f.source[cur_pos]
            if c == "=":
                self.construct_token(token, cur_pos + 1, Tok.EXCLAIMEQUAL)
            else:
                self.construct_token(token, cur_pos, Tok.EXCLAIM)
        elif c == "^":
            c = self.f.source[cur_pos]
            if c == "=":
                self.construct_token(token, cur_pos + 1, Tok.CARETEQUAL)
            else:
                self.construct_token(token, cur_pos, Tok.CARET)
        elif c == "&":
            c = self.f.source[cur_pos]
            if c == "&":
                self.construct_token(token, cur_pos + 1, Tok.AMPAMP)
            elif c == "=":
                self.construct_token(token, cur_pos + 1, Tok.AMPEQUAL)
            else:
                self.construct_token(token, cur_pos, Tok.AMP)
        elif c == "|":
            c = self.f.source[cur_pos]
            if c == "|":
                self.construct_token(token, cur_pos + 1, Tok.PIPEPIPE)
            elif c == "=":
                self.construct_token(token, cur_pos + 1, Tok.PIPEEQUAL)
            else:
                self.construct_token(token, cur_pos, Tok.PIPE)
        elif c == ".":
            c = self.f.source[cur_pos]
            if c <= "9" and c >= "0":
                self.lex_number(token, cur_pos + 1)
            else:
                if self.f.source[cur_pos] == "." and self.f.source[cur_pos + 1] == ".":
                    self.construct_token(token, cur_pos + 2, Tok.ELLIPSIS)
                else:
                    self.construct_token(token, cur_pos, Tok.PERIOD)
        elif c == "'":
            self.lex_char_literal(token, cur_pos)
        elif c == "\"":
            self.lex_str_literal(token, cur_pos)
        else:
            diag(self.pos + self.f.pos_offset, f"Unknown character {c} in source file", Diag.ERROR)
            self.pos = cur_pos
            self.lex_internal(token)

    def lex(self) -> Token:
        token = Token()
        self.lex_internal(token)
        return token
