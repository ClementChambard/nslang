#ifndef DIAGS_RENDERER_HPP_INCLUDED
#define DIAGS_RENDERER_HPP_INCLUDED

#include "diags/diagnostic.hpp"
#include "lexer/loc.hpp"
#include <string>
namespace diag::renderer {

void level_color(std::string &s, diag::Diag level, bool bold);
void diagnostic_level(std::string &s, diag::Diag level, bool show_colors);
void diagnostic_message(std::string &s, bool is_supplemental,
                        std::string const &message, u32 current_column,
                        u32 columns, bool show_colors);
inline void filename(std::string &s, std::string const &filename) {
  s += filename;
}
void emit_diagnostic_message(diag::Diag level, Loc loc,
                             std::string const &message);
void emit_diagnostic_loc(diag::Diag level, Loc loc);
void emit_code_context(diag::Diag level, Loc loc,
                       std::vector<LocRge> const &ranges); // TODO: fixits

} // namespace diag::renderer

#endif // DIAGS_RENDERER_HPP_INCLUDED
