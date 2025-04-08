from typing import Self, Tuple
from lex import Loc, LocRge, LOC_INVALID, OpenedFile, Token
import enum


def fatal_error(msg):
    print(f"\x1b[1;31mfatal error: \x1b[0;1m{msg}\x1b[0m")
    exit(1)


class Diag(enum.Enum):
    WARNING = enum.auto()
    ERROR = enum.auto()
    NOTE = enum.auto()
    UNIMPLEMENTED = enum.auto()

_HAD_ERRORS: bool = False
_LAST_LOC: Loc = 0
_LAST_LEVEL: Diag

class StyleRange:
    color: str
    start: int
    end: int

    def __init__(self, color, start, end):
        self.color = color
        self.start = start
        self.end = end

class LineRange:
    line_no: int
    start_col: int
    end_col: int

    def __init__(self, line: int, col: int = 1, end: int = 100000000):
        self.line_no = line
        self.start_col = col - 1
        self.end_col = end - 1


class DiagRenderer:
    def print_level_color(os: str, level: Diag, bold: bool) -> str:
        os += "\x1b["
        if bold:
            os += "1;"
        match level:
            case Diag.WARNING:
                os += "35m"
            case Diag.NOTE:
                os += "34m"
            case Diag.ERROR:
                os += "31m"
            case Diag.UNIMPLEMENTED:
                os += "31m"
            case _:
                os += "37m"
        return os

    def print_diagnostic_level(os: str, level: Diag, show_colors: bool) -> str:
        if show_colors:
            os = DiagRenderer.print_level_color(os, level, True)
        match level:
            case Diag.WARNING:
                os += "warning: "
            case Diag.NOTE:
                os += "note: "
            case Diag.ERROR:
                os += "error: "
            case Diag.UNIMPLEMENTED:
                os += "unimplemented: "
            case _:
                os += "UNKDIAG: "
        if show_colors:
            os += "\x1b[0m"
        return os

    def print_diagnostic_message(os: str, is_supplemental: bool, message: str, current_column: int, columns: int, show_colors: bool) -> str:
        bold = False
        if show_colors and not is_supplemental:
            os += "\x1b[1m"
            bold = True

        if columns != 0:
            os = print_word_wrapped(os, message, columns, current_column, bold)
        else:
            os += message

        if show_colors:
            os += "\x1b[0m"

        return os + "\n"

    def emit_filename(filename: str) -> str:
        print(filename, end="")

    def highlight_range(r, caret_line: str, src_line) -> str:
        start_col_no = r.start_col


        # while start_col_no < len(src_line) and (src_line[start_col_no] == ' ' or src_line[start_col_no] == '\t'):
        #     if src_line[start_col_no] == '\t':
        #         start_col_no += 3
        #     start_col_no += 1

        end_col_no = min(r.end_col, len(src_line))

        # while end_col_no != 0 and (src_line[end_col_no - 1] == ' ' or src_line[end_col_no - 1] == '\t'):
        #     if src_line[end_col_no - 1] == '\t':
        #         end_col_no += 3
        #     end_col_no -= 1

        if start_col_no > end_col_no:
            return;

        # start_col_no = map.byte_to_containing_column(start_col_no);
        # end_col_no = map.byte_to_containing_column(end_col_no);

        assert start_col_no <= end_col_no, "Invalid range!"

        if len(caret_line) < end_col_no:
            caret_line += (" " * (end_col_no - len(caret_line)))

        return caret_line[:start_col_no] + ("~" * (end_col_no + 1 - start_col_no)) + caret_line[end_col_no + 1:]

    def highlight_lines(file_data: str, start_line_number: int, end_line_number: int, show_colors: bool, cur_file: OpenedFile) -> []:
        assert start_line_number <= end_line_number
        snippet_ranges = [[]] * (end_line_number - start_line_number + 1)
        return snippet_ranges

    def prepare_and_filter_ranges(ranges: [], lines, cur_file: OpenedFile) -> []:
        line_ranges = []
        for r in ranges:
            if r[0] == 0 or r[1] == 0:
                continue
            fb, begin_line_no, begin_col_no = OpenedFile.get_loc(r[0])
            fe, end_line_no, end_col_no = OpenedFile.get_loc(r[1])
            if begin_line_no > lines[1] or fb != cur_file.filename:
                continue
            if end_line_no < lines[0] or fe != cur_file.filename:
                continue
            if begin_line_no == end_line_no:
                line_ranges.append(LineRange(begin_line_no, begin_col_no, end_col_no))
                continue
            line_ranges.append(LineRange(begin_line_no, begin_col_no))
            for s in range(begin_line_no + 1, end_line_no):
                line_ranges.append(LineRange(s))
            line_ranges.append(LineRange(end_line_no, 1, end_col_no))
        return line_ranges

    def emit_snippet_and_caret(loc, level, ranges, hints):
        assert loc != 0, "must have a valid source location here"
        os = ""

        if loc == _LAST_LOC and len(ranges) == 0 and len(hints) == 0 and (_LAST_LEVEL != Diag.NOTE or level == _LAST_LEVEL):
            return

        cur_file = OpenedFile.find_by_loc(loc)

        if cur_file is None:
            return

        buf_data = cur_file.source

        buf_start_off = 0
        buf_end_off = cur_file.src_len

        _, caret_line_no, caret_col_no = OpenedFile.get_loc(loc)

        MAX_LINE_LENGTH_TO_PRINT = 4096;

        if caret_col_no > MAX_LINE_LENGTH_TO_PRINT:
            return;

        max_lines = 16;

        lines = (caret_line_no, caret_line_no);
        display_line_no = caret_line_no

        for r in ranges:
            _, l0, _ = OpenedFile.get_loc(r[0])
            _, l1, _ = OpenedFile.get_loc(r[1])
            lines = (min(lines[0], l0), max(lines[1], l1))
            display_line_no = min(display_line_no, l0)

        max_line_no_display_width = max(4, DiagRenderer.get_num_display_width(display_line_no + max_lines))

        source_styles = DiagRenderer.highlight_lines(buf_data, lines[0], lines[1], True, cur_file)
        line_ranges = DiagRenderer.prepare_and_filter_ranges(ranges, lines, cur_file)

        for line_no in range(lines[0], lines[1] + 1):
            line_start = buf_data[cur_file.line_offset(line_no):]
            if len(line_start) == 0:
                break

            line_len = 0
            while len(line_start) != line_len and line_start[line_len] != "\n" and line_start[line_len] != "\r":
                line_len += 1
            if line_len > MAX_LINE_LENGTH_TO_PRINT:
                return
            source_line = line_start[:line_len]

            while len(source_line) > 0 and source_line[-1] == '\0' and (line_no != caret_line_no or len(source_line) > caret_col_no):
                source_line.pop()

            caret_line = ""

            for lr in line_ranges:
                if lr.line_no == line_no:
                    caret_line = DiagRenderer.highlight_range(lr, caret_line, line_start[:line_len])

            if caret_line_no == line_no:
                col = caret_col_no - 1
                caret_line += " " * max(col + 1, len(caret_line))
                caret_line = caret_line[:col] + "^" + caret_line[col+1:]

            # fixit_insertion_line = build_fixit_insertion_line(cur_file, line_no, source_col_map, hints)

            DiagRenderer.emit_snippet(source_line, max_line_no_display_width, line_no, display_line_no, source_styles[line_no - lines[0]])

            if len(caret_line) > 0:
                os += " " * (max_line_no_display_width + 2)
                os += "| \x1b[1;32m"
                os += caret_line
                os += "\n\x1b[0m"
                print(os, end="")
                os = ""

            # if !fixit_insertion_line.is_empty() {
            #     os.push_str(&" ".repeat(max_line_no_display_width + 2));
            #     os.push_str("| \x1b[1;32m");
            #     os.push_str(&fixit_insertion_line);
            #     os.push_str("\n\x1b[0m");
            # }

            display_line_no += 1;

        print(os, end="")
        DiagRenderer.emit_parseable_fixits(hints);

    def get_num_display_width(n: int) -> int:
        l = 1;
        m = 10;
        while m <= n:
            l += 1;
            if l == 9:
                break;
            m *= 10;
        return l;

    def printable_text_for_char(c: str, cur_col: int) -> Tuple[str, bool]:
        assert len(c) == 1
        if c == "\t":
            return ((4 - (cur_col % 4)) * " ", True)
        if c.isprintable():
            return (c, True)
        return (f"<U+{ord(c)}>", False)

    def emit_snippet(src_line: str, max_line_no_display_width: int, line_no: int, display_line_no: int, styles: [StyleRange]):
        os = ""

        if max_line_no_display_width > 0:
            line_no_display_width = DiagRenderer.get_num_display_width(display_line_no)
            os += " " * (max_line_no_display_width - line_no_display_width + 1)
            os += f"{display_line_no} | "

        print_reversed = False
        current_color = None
        i = 0
        for ci, c in enumerate(src_line):
            s, was_printable = DiagRenderer.printable_text_for_char(c, i)
            i += len(s)

            if was_printable == print_reversed:
                print_reversed = not print_reversed
                if print_reversed:
                    os += "\x1b[7m"
                else:
                    os += "\x1b[0m"
                    current_color = None;


            char_styles_here = [s for s in styles if s.start <= ci and s.end > ci]
            if len(char_styles_here) > 0:
                char_style = char_styles_here[0]
                if current_color != char_style.color:
                    os += char_style.color
                    current_color = char_style.color
            elif current_color is not None:
                current_color = None
                os += "\x1b[0m"

            os += s

        os += "\x1b[0m\n"
        print(os, end="");

    def emit_parseable_fixits(hints: []):
        # TODO:
        pass

    def emit_diagnostic_message(loc: Loc, level: Diag, message: str):
        os = ""
        if loc != 0:
            DiagRenderer.emit_diagnostic_loc(loc, level)

        os += "\x1b[0m"
        start_of_location_info = len(os)
        os = DiagRenderer.print_diagnostic_level(os, level, True)
        length = len(os)
        os = DiagRenderer.print_diagnostic_message(os, level == Diag.NOTE, message, length - start_of_location_info, 0, True)
        print(os, end="")

    def emit_diagnostic_loc(loc: Loc, level: Diag):
        os = ""
        f, l, c = OpenedFile.get_loc(loc)
        os = DiagRenderer.print_level_color(os, level, True)
        print(os, end="")
        DiagRenderer.emit_filename(f)
        os += f":{l}"
        if c != 0:
            os += f":{c}"
        os += ": "
        print(os, end="")

    def emit_code_context(loc: Loc, level: Diag, ranges: [LocRge], hints: []):
        DiagRenderer.emit_snippet_and_caret(loc, level, ranges, hints)

    # include / import / module loc

def reset_errors():
    global _HAD_ERRORS
    _HAD_ERRORS = False

def compilation_had_errors() -> bool:
    return _HAD_ERRORS

def diag(loc_at_diag, msg, kind, ranges = [], hints = []):
    global _HAD_ERRORS, _LAST_LOC, _LAST_LEVEL
    DiagRenderer.emit_diagnostic_message(loc_at_diag, kind, msg)
    DiagRenderer.emit_code_context(loc_at_diag, kind, ranges, hints)
    _LAST_LOC = loc_at_diag
    _LAST_LEVEL = kind
    if kind == Diag.ERROR:
        _HAD_ERRORS = True
