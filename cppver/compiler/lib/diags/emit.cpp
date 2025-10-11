#include "diagnostic.hpp"
#include "diags/renderer.hpp"

Loc g_LAST_LOC = LOC_INVALID;
diag::Diag g_LAST_LEVEL = diag::ERROR;
extern bool g_HAD_ERRORS;

void emit_diagnostic(DiagnosticBuilder *builder) {
  diag::renderer::emit_diagnostic_message(builder->level, builder->loc,
                                          builder->message);
  diag::renderer::emit_code_context(builder->level, builder->loc,
                                    builder->ranges);
  g_LAST_LOC = builder->loc;
  g_LAST_LEVEL = builder->level;

  if (builder->level == diag::ERROR)
    g_HAD_ERRORS = true;
}
