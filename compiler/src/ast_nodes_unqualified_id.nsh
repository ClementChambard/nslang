#include ident_info.nsh
#include loc.nsh

enum UnqualifiedIdKind {
  UIDK_IDENTIFIER,
  // operator function id
  // conversion function id
  // literal operator id
  // constructor name
  // constructor template id
  // destructor name
  // template id
  // implicit self param
  // deduction guide name
};

struct UnqualifiedId {
  kind: UnqualifiedIdKind;
  value: IdentInfo*;
  start_location: Loc;
  end_location: Loc;
};

lib fn UnqualifiedId::new() -> UnqualifiedId*;
lib fn UnqualifiedId::is_valid(self: UnqualifiedId*) -> bool;
lib fn UnqualifiedId::set_identifier(self: UnqualifiedId*, i: IdentInfo*, idloc: Loc);
lib fn UnqualifiedId::get_range(self: UnqualifiedId*, out: LocRge*);
