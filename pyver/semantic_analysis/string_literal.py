from typing import List, Tuple
from lex import Tok, Token, Loc, LOC_INVALID
from utils.diagnostic import diag, Diag
from utils import HEX_DIGITS, OCTAL_DIGITS, hex_digit_value


def get_char_width(kind: Tok) -> int:
    match kind:
        case Tok.CHR | Tok.STR:
            return 8
        case _:
            assert False, "unreachable"


class StringLiteralParser:
    max_token_length: int
    size_bound: int
    char_byte_width: int
    kind: Tok
    result_buf: bytes
    result_off: int
    eval_method: bool
    had_error: bool

    def get_string(self) -> bytes:
        return self.result_buf[: self.get_string_length()]

    def get_string_length(self) -> int:
        return self.result_off

    def get_num_string_chars(self) -> int:
        return self.get_string_length() // self.char_byte_width

    def diagnose_lexing_error(self, loc: Loc):
        self.had_error = True
        diag(loc, "failure when lexing a string literal", Diag.ERROR)

    def encode_char_into_string(self, c: str):
        self.result_off += 1
        self.result_buf += bytes(c, encoding="utf-8")

    def encode_ucn_escape(self, aaaa):
        # TODO:
        _ = aaaa  # UNUSED
        pass

    def copy_string_fragment(self, tok: Token, s: str):
        # TODO:
        _ = tok  # UNUSED
        for c in s:
            self.encode_char_into_string(c)
        return False

    def parse(self, string_toks: List[Token]):
        if len(string_toks) == 0 or len(string_toks[0].value_str()) < 2:
            return self.diagnose_lexing_error(LOC_INVALID)

        self.max_token_length = len(string_toks[0].value_str())
        self.size_bound = len(string_toks[0].value_str()) - 2
        self.had_error = False

        self.kind = Tok.STR

        for tok in string_toks:
            tok_value = tok.value_str()
            if len(tok_value) < 2:
                return self.diagnose_lexing_error(tok.loc)

            self.size_bound += len(tok_value) - 2
            if len(tok_value) > self.max_token_length:
                self.max_token_length = len(tok_value)

            if not self.eval_method and tok.ty != Tok.STR:
                # prefix_end_loc = Lexer::AdvanceToTokenCharacter(Tok.getLocation(), getEncodingPrefixLen(Tok.getKind()), SM, Features);
                # CharSourceRange Range = CharSourceRange::getCharRange({Tok.getLocation(), PrefixEndLoc});
                # StringRef Prefix(SM.getCharacterData(Tok.getLocation()), getEncodingPrefixLen(Tok.getKind()));
                diag(
                    tok.loc,
                    "encoding prefix '{prefix}' on an unevaluated string literal has no effect",
                    Diag.ERROR,
                )  # << Prefix << FixItHint::CreateRemoval(Range);
                self.had_error = True
            # else change kind for concat

        self.size_bound += 1
        self.char_byte_width = get_char_width(self.kind)
        assert (self.char_byte_width & 7) == 0, (
            "Assumes character size is byte multiple"
        )
        self.char_byte_width //= 8
        self.size_bound *= self.char_byte_width
        self.result_buf = b""  # b"\0" * self.size_bound
        self.result_off = 0
        for i in range(len(string_toks)):
            this_token_val = string_toks[i].value_str()
            # TODO: different kinds of strings
            if this_token_val[0] != '"' or this_token_val[-1] != '"':
                return self.diagnose_lexing_error(string_toks[i].loc)
            this_token_val = this_token_val[1:-1]
            while len(this_token_val) > 0:
                if this_token_val[0] != "\\":
                    count = 0
                    while count < len(this_token_val) and this_token_val[count] != "\\":
                        count += 1
                    if self.copy_string_fragment(
                        string_toks[i], this_token_val[:count]
                    ):
                        self.had_error = True
                    this_token_val = this_token_val[count:]
                    continue
                if (
                    this_token_val[1] == "u"
                    or this_token_val[1] == "U"
                    or this_token_val[1] == "N"
                ):
                    self.encode_ucn_escape(0000)
                    continue
                result_char, to_skip = self.process_char_escape(
                    this_token_val, string_toks[i].loc
                )
                this_token_val = this_token_val[to_skip:]
                self.encode_char_into_string(result_char)

        self.encode_char_into_string("\0")

    def process_char_escape(self, buf: str, loc: Loc) -> Tuple[str, Loc]:
        cur_loc = 0
        # escape_begin_loc = cur_loc
        delimited = False
        end_delimiter_found = False
        cur_loc += 1
        result_char = buf[cur_loc]
        cur_loc += 1
        # escape = result_char
        match result_char:
            case "\\" | "'" | '"' | "?":
                pass
            case "a":
                result_char = chr(7)
            case "b":
                result_char = chr(8)
            case "e":
                # diag(loc, "diag::ext_nonstandard_escape e", Diag.WARNING)
                result_char = chr(27)
            case "E":
                # diag(loc, "diag::ext_nonstandard_escape E", Diag.WARNING)
                result_char = chr(27)
            case "f":
                result_char = chr(12)
            case "n":
                result_char = chr(10)
            case "r":
                result_char = chr(13)
            case "t":
                result_char = chr(9)
            case "v":
                result_char = chr(11)
            case "x":
                result_char = 0
                if buf[cur_loc:] != "" and buf[cur_loc] == "{":
                    delimited = True
                    cur_loc += 1
                    if buf[cur_loc] == "}":
                        self.had_error = True
                        diag(loc, "diag::err_delimited_escape_empty", Diag.ERROR)
                elif buf[cur_loc:] == "" or buf[cur_loc] not in HEX_DIGITS:
                    self.had_error = True
                    diag(loc, "diag::err_hex_escape_no_digits x", Diag.ERROR)
                    return chr(0), 2
                overflow = False
                while buf[cur_loc:] != "":
                    if delimited and buf[cur_loc] == "}":
                        cur_loc += 1
                        end_delimiter_found = True
                        break
                    if buf[cur_loc] not in HEX_DIGITS:
                        if not delimited:
                            break
                        self.had_error = True
                        diag(
                            loc,
                            "diag::err_delimited_escape_invalid << StringRef(cur_loc, 1)",
                            Diag.ERROR,
                        )
                        continue
                    if result_char & 0xF0000000:
                        overflow = True
                    result_char <<= 4
                    result_char |= hex_digit_value(buf[cur_loc])
                    cur_loc += 1
                # Check overflow depending on char width
                if result_char >> 8 != 0:  # use char_width variable
                    overflow = True
                    result_char &= ~0 >> (32 - 8)  # use char_width variable
                if not self.had_error and overflow:
                    self.had_error = True
                    diag(loc, "diag::err_escape_too_large 0", Diag.ERROR)
                result_char = chr(result_char)
            case "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7":
                cur_loc -= 1
                result_char = 0
                num_digits = 0
                while (
                    buf[cur_loc:] != ""
                    and num_digits < 3
                    and buf[cur_loc] in OCTAL_DIGITS
                ):
                    result_char <<= 3
                    result_char |= ord(buf[cur_loc]) - ord("0")
                    cur_loc += 1
                    num_digits += 1
                # Check overflow depending on char width
                if result_char >> 8 != 0:  # use char_width variable
                    self.had_error = True
                    result_char &= ~0 >> (32 - 8)  # use char_width variable
                    diag(loc, "diag::err_escape_too_large 1", Diag.ERROR)
                result_char = chr(result_char)
            case "o":
                overflow = False
                if buf[cur_loc:] == "" or buf[cur_loc] != "{":
                    self.had_error = True
                    diag(loc, "diag::err_hex_escape_no_digits x", Diag.ERROR)
                    return chr(0), 2
                result_char = 0
                delimited = True
                cur_loc += 1
                if buf[cur_loc] == "}":
                    self.had_error = True
                    diag(loc, "diag::err_delimited_escape_empty", Diag.ERROR)
                while buf[cur_loc:] != "":
                    if buf[cur_loc] == "}":
                        end_delimiter_found = True
                        cur_loc += 1
                        break
                    if buf[cur_loc] not in OCTAL_DIGITS:
                        self.had_error = True
                        diag(
                            loc,
                            "diag::err_delimited_escape_invalid << StringRef(cur_loc, 1)",
                            Diag.ERROR,
                        )
                        cur_loc += 1
                        continue
                    if result_char & 0xE0000000:
                        overflow = True
                    result_char <<= 3
                    result_char |= ord(buf[cur_loc]) - ord("0")
                    cur_loc += 1
                # Check overflow depending on char width
                if not self.had_error and overflow or (result_char >> 8) != 0:
                    self.had_error = True
                    result_char &= ~0 >> (32 - 8)  # use char_width variable
                    diag(loc, "diag::err_escape_too_large 1", Diag.ERROR)
                result_char = chr(result_char)
            case "(" | "{" | "[" | "%":
                # diag(loc, diag::ext_nonstandard_escape << std::string(1, result_char), Diag.EXT)
                pass
            case _:
                if result_char.isprintable():
                    diag(
                        loc,
                        "diag::ext_unknown_escape << std::string(1, result_char)",
                        Diag.ERROR,
                    )
                else:
                    diag(
                        loc,
                        'diag::ext_unknown_escape << "x" + utohexstr(result_char)',
                        Diag.ERROR,
                    )

        if delimited and not end_delimiter_found:
            diag(loc, "err_expected '}'", Diag.ERROR)
        # if (EvalMethod == StringLiteralEvalMethod::Unevaluated && !IsEscapeValidInUnevaluatedStringLiteral(Escape)) { Diag(Diags, Features, Loc, ThisTokBegin, EscapeBegin, ThisTokBuf, diag::err_unevaluated_string_invalid_escape_sequence) << StringRef(EscapeBegin, ThisTokBuf - EscapeBegin); HadError = true; }

        return result_char, cur_loc

    def __init__(self, string_toks: List[Token], eval_method: bool = True):
        self.max_token_length = 0
        self.size_bound = 0
        self.char_byte_width = 0
        self.kind = Tok.UNKNOWN
        self.result_off = 0
        self.result_buf = b""
        self.eval_method = eval_method
        self.had_error = False

        self.parse(string_toks)
