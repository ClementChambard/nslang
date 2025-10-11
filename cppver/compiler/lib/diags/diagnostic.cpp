#include "diagnostic.hpp"
#include <cstdarg>
#include <cstdio>

bool g_HAD_ERRORS = false;

bool diag::had_errors() { return g_HAD_ERRORS; }
void diag::reset_errors() { g_HAD_ERRORS = false; }
void diag::fatal_error(std::string const &message) {
  printf("\x1b[1;31mfatal error: \x1b[0;1m%s\x1b[0m\n", message.c_str());
}

void emit_diagnostic(DiagnosticBuilder *);

DiagnosticBuilder::~DiagnosticBuilder() { emit_diagnostic(this); }

DiagnosticBuilder &DiagnosticBuilder::operator<<(LocRge range) {
  ranges.push_back(range);
  return *this;
}

DiagnosticBuilder &DiagnosticBuilder::operator<<(Loc loc) {
  ranges.push_back({loc, loc});
  return *this;
}

DiagnosticBuilder Diag(diag::Diag level, Loc loc, char const *message, ...) {
  va_list args;
  va_start(args, message);
  static constexpr u32 MESSAGE_LENGTH = 2048;
  char message_buf[MESSAGE_LENGTH];
  vsnprintf(message_buf, MESSAGE_LENGTH, message, args);
  return Diag(level, loc, std::string(message_buf));
}

DiagnosticBuilder Diag(diag::Diag level, Loc loc, std::string const &message) {
  DiagnosticBuilder b;
  b.level = level;
  b.message = message;
  b.loc = loc;
  return b;
}
