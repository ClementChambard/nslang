#ifndef DIAGS_DIAGNOSTIC_HPP_INCLUDED
#define DIAGS_DIAGNOSTIC_HPP_INCLUDED

#include "lexer/loc.hpp"
#include <string>
#include <vector>

namespace diag {

enum Diag { WARNING, ERROR, NOTE, UNIMPLEMENTED, BUG };

bool had_errors();
void reset_errors();
void fatal_error(std::string const &message);

} // namespace diag

struct DiagnosticBuilder {
  std::string message;
  std::vector<LocRge> ranges;
  // fixits
  Loc loc;
  diag::Diag level;

  DiagnosticBuilder() = default;
  ~DiagnosticBuilder();

  DiagnosticBuilder &operator<<(LocRge range);
  DiagnosticBuilder &operator<<(Loc loc);
  // TODO: more
};

DiagnosticBuilder Diag(diag::Diag level, Loc loc, char const *message, ...);
DiagnosticBuilder Diag(diag::Diag level, Loc loc, std::string const &message);

#endif // DIAGS_DIAGNOSTIC_HPP_INCLUDED
