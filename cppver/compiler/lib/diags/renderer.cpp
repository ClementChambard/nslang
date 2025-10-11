#include "renderer.hpp"
#include "lexer/file.hpp"
#include "lexer/loc.hpp"
#include <cstdio>
#include <cstring>
#include <string>
#include <string_view>

extern Loc g_LAST_LOC;
extern diag::Diag g_LAST_LEVEL;

void diag::renderer::level_color(std::string &s, diag::Diag level, bool bold) {
  s += "\x1b[";
  if (bold)
    s += "1;";
  switch (level) {
  case diag::ERROR:
  case diag::BUG:
  case diag::UNIMPLEMENTED:
    s += "31m";
    break;
  case diag::NOTE:
    s += "34m";
    break;
  case diag::WARNING:
    s += "35m";
    break;
  default:
    s += "37m";
  }
}

void diag::renderer::diagnostic_level(std::string &s, diag::Diag level,
                                      bool show_colors) {
  if (show_colors)
    level_color(s, level, true);
  switch (level) {
  case diag::ERROR:
    s += "error: ";
    break;
  case diag::BUG:
    s += "bug: ";
    break;
  case diag::NOTE:
    s += "note: ";
    break;
  case diag::WARNING:
    s += "warning: ";
    break;
  case diag::UNIMPLEMENTED:
    s += "unimplemented: ";
    break;
  default:
    s += "diag: ";
  }
  if (show_colors)
    s += "\x1b[0m";
}

void diag::renderer::diagnostic_message(std::string &s, bool is_supplemental,
                                        std::string const &message, u32,
                                        u32 columns, bool show_colors) {
  // bool bold = false;
  if (show_colors && !is_supplemental) {
    s += "\x1b[1m";
    // bold = true;
  }

  if (columns != 0) {
    // print_word_wrapped(s, message, columns, current_column, bold);
    s += message;
  } else {
    s += message;
  }

  if (show_colors) {
    s += "\x1b[0m";
  }

  s += '\n';
}

void diag::renderer::emit_diagnostic_message(diag::Diag level, Loc loc,
                                             std::string const &message) {
  if (loc != LOC_INVALID) {
    emit_diagnostic_loc(level, loc);
  }
  std::string out = "\x1b[0m";
  diagnostic_level(out, level, true);
  diagnostic_message(out, level == diag::NOTE, message, 0, 0, true);
  printf("%s", out.c_str());
}

void diag::renderer::emit_diagnostic_loc(diag::Diag level, Loc loc) {
  DecomposedLoc l = OpenedFile::get_loc(loc);
  std::string out;
  level_color(out, level, true);
  filename(out, l.file_name);
  out += ':' + std::to_string(l.line) + ':' + std::to_string(l.col) + ':';
  printf("%s", out.c_str());
}

void emit_snippet_and_caret(diag::Diag level, Loc loc,
                            std::vector<LocRge> const &ranges);

void diag::renderer::emit_code_context(diag::Diag level, Loc loc,
                                       std::vector<LocRge> const &ranges) {
  if (loc == LOC_INVALID)
    return;
  emit_snippet_and_caret(level, loc, ranges);
}

bool make_caret_line(std::string &out, std::string_view line, u32 cur_line,
                     std::vector<LocRge> const &ranges) {
  for (u32 i = 0; i < line.length(); i++)
    out.push_back(' ');
  bool res = true;
  u32 i = 0;
  while (i < ranges.size()) {
    DecomposedLoc dloc;
    dloc = OpenedFile::get_loc(ranges[i].start);
    u32 start_line = dloc.line;
    u32 start_col = dloc.col;
    dloc = OpenedFile::get_loc(ranges[i].end);
    u32 end_line = dloc.line;
    u32 end_col = dloc.col;
    if (start_line == cur_line) {
      u32 count;
      if (end_line == cur_line)
        count = end_col - start_col + 1;
      else
        count = line.length() - start_col + 1;
      memset(&out.data()[start_col - 1], '~', count);
      res = false;
    } else if (end_line == cur_line) {
      memset(out.data(), '~', end_col);
      res = false;
    } else if (start_line > cur_line) {
    } else if (end_line < cur_line) {
    } else {
      memset(out.data(), '~', line.length());
      res = false;
      break;
    }
    i += 1;
  }
  return res;
}

u32 get_num_display_width(u32 n) {
  u32 l;
  u32 m;
  l = 1;
  m = 10;
  while (m <= n) {
    l += 1;
    if (l == 9)
      break;
    m = m * 10;
  }
  return l;
}

void push_snippet(std::string &out, std::string_view src_line,
                  u32 max_line_no_display_width, u32 line_no, void *styles) {
  (void)styles;
  if (max_line_no_display_width > 0) {
    for (u32 i = 0;
         i < max_line_no_display_width - get_num_display_width(line_no) + 1;
         i++)
      out += ' ';
    out += std::to_string(line_no) + " | ";
  }
  out += src_line;
  out += '\n';
}

u32 *prepare_and_filter_ranges(std::vector<LocRge> const & /*ranges*/,
                               u32 /*line_start*/, u32 /*line_end*/,
                               OpenedFile * /*cur_file*/) {
  return nullptr;
}

void emit_snippet_and_caret(diag::Diag level, Loc loc,
                            std::vector<LocRge> const &ranges) {
  std::string out;
  if (loc == g_LAST_LOC) {
    if (ranges.size() == 0) { // fixits too
      if (g_LAST_LEVEL != diag::NOTE)
        return;
      if (g_LAST_LEVEL == level)
        return;
    }
  }

  OpenedFile *cur_file = OpenedFile::find_by_loc(loc);
  if (cur_file == nullptr)
    return;

  std::string_view buf_data;
  buf_data = cur_file->source;

  DecomposedLoc dloc = OpenedFile::get_loc(loc);

  if (dloc.col > 4096)
    return;

  u32 lines_start = dloc.line;
  u32 lines_end = dloc.line;
  u32 display_line_no;

  for (auto const &range : ranges) {
    if (range.start == LOC_INVALID || range.end == LOC_INVALID) 
      continue;
    DecomposedLoc r_dloc = OpenedFile::get_loc(range.start);
    if (r_dloc.line < lines_start)
      lines_start = r_dloc.line;
    r_dloc = OpenedFile::get_loc(range.end);
    if (r_dloc.line > lines_end)
      lines_end = r_dloc.line;
  }
  display_line_no = lines_start;

  u32 max_line_no_display_width = get_num_display_width(display_line_no + 16);
  if (max_line_no_display_width < 4)
    max_line_no_display_width = 4;

  // source_styles = DiagRenderer.highlight_lines(buf_data, lines[0], lines[1],
  // True, cur_file)

  std::string caret_line;

  while (display_line_no <= lines_end) {
    u32 line_offset;
    line_offset = cur_file->line_offset(display_line_no);
    std::string_view line;
    line = buf_data.substr(line_offset);
    if (line.length() == 0)
      break;
    line_offset = 0;
    while (true) {
      if (line.length() == line_offset)
        break;
      if (line[line_offset] == '\n')
        break;
      if (line[line_offset] == '\r')
        break;
      line_offset += 1;
    }
    if (line_offset > 4096)
      return; // Line too long
    line = line.substr(0, line_offset);

    caret_line.clear();

    bool caret_line_is_empty =
        make_caret_line(caret_line, line, display_line_no, ranges);

    if (dloc.line == display_line_no) {
      caret_line[dloc.col - 1] = '^';
      caret_line_is_empty = false;
    }

    push_snippet(out, line, max_line_no_display_width, display_line_no,
                 nullptr); // source_styles[display_line_no - lines[0]]

    if (!caret_line_is_empty) {
      for (u32 i = 0; i < max_line_no_display_width; i++)
        out += ' ';
      out += "  | \x1b[1;32m" + caret_line + "\n\x1b[0m";
    }

    // fixit_insertion_line = build_fixit_insertion_line(cur_file,
    // display_line_no, source_col_map, hints) if
    // !fixit_insertion_line.is_empty() {
    //     os.push_str(&" ".repeat(max_line_no_display_width + 2));
    //     os.push_str("| \x1b[1;32m");
    //     os.push_str(&fixit_insertion_line);
    //     os.push_str("\n\x1b[0m");
    // }

    display_line_no += 1;
  }

  printf("%s", out.c_str());
  // emit_parseable_fixits(hint_count, hints);
}
