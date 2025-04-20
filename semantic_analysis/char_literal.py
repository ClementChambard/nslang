from ns_ast.nodes import Type
from lex import Token, Tok, Loc
from utils import HEX_DIGITS, OCTAL_DIGITS, hex_digit_value


class CharLiteralParser:
    res: int
    ty: Type
    had_error: bool

    def process_char_escape(self, buf: str, loc: Loc) -> str:
        from utils.diagnostic import diag, Diag

        delimited = False
        end_delimiter_found = False
        result_char = buf[0]
        cur_loc = 1
        match result_char:
            case "\\" | "'" | '"' | "?":
                result_char = ord(result_char)
            case "a":
                result_char = 7
            case "b":
                result_char = 8
            case "e":
                # diag(loc, "diag::ext_nonstandard_escape e", Diag.WARNING)
                result_char = 27
            case "E":
                # diag(loc, "diag::ext_nonstandard_escape E", Diag.WARNING)
                result_char = 27
            case "f":
                result_char = 12
            case "n":
                result_char = 10
            case "r":
                result_char = 13
            case "t":
                result_char = 9
            case "v":
                result_char = 11
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
                    return buf[1:]
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
            case "o":
                overflow = False
                if buf[cur_loc:] == "" or buf[cur_loc] != "{":
                    self.had_error = True
                    diag(loc, "diag::err_hex_escape_no_digits x", Diag.ERROR)
                    return buf[1:]
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
            case "(" | "{" | "[" | "%":
                # diag(loc, diag::ext_nonstandard_escape << std::string(1, result_char), Diag.EXT)
                result_char = ord(result_char)
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
                result_char = ord(result_char)

        if delimited and not end_delimiter_found:
            diag(loc, "err_expected '}'", Diag.ERROR)
        # if (EvalMethod == StringLiteralEvalMethod::Unevaluated && !IsEscapeValidInUnevaluatedStringLiteral(Escape)) { Diag(Diags, Features, Loc, ThisTokBegin, EscapeBegin, ThisTokBuf, diag::err_unevaluated_string_invalid_escape_sequence) << StringRef(EscapeBegin, ThisTokBuf - EscapeBegin); HadError = true; }

        self.res = result_char
        return buf[cur_loc:]

    def parse(self, tok: Token):
        from utils.diagnostic import diag, Diag

        assert tok.ty == Tok.CHR
        val = tok.value_str()
        assert val.startswith("'") and val.endswith("'")
        val = val[1:-1]
        assert len(val) > 0
        if val[0] != "\\":
            if len(val) > 1:
                diag(tok.loc, "Char constant too long", Diag.ERROR)
                self.had_error = True
            self.res = ord(val[0])
            return
        val = val[1:]
        if val[0] == "u" or val[0] == "U" or val[0] == "N":
            assert False, "TODO: ucn escape"
        remaining_val = self.process_char_escape(val, tok.loc)
        if len(remaining_val) > 0:
            diag(tok.loc, "Char constant too long", Diag.ERROR)
            self.had_error = True

    def __init__(self, tok: Token):
        from semantic_analysis import TYPES

        self.res = 0
        self.ty = TYPES["i8"]
        self.had_error = False
        self.parse(tok)
