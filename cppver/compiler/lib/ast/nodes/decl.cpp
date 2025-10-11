#include "decl.hpp"

void TranslationUnitDecl::add_decl(DeclUPtr d) {
  if (src_range.start == LOC_INVALID) src_range.start = d->src_range.start;
  src_range.end = d->src_range.end;
  decls.push_back(std::move(d));
}
