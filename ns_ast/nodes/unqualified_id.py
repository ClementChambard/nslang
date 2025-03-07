from utils.my_enum import Enum, ENUM_INIT, ENUM_N
from dataclasses import dataclass
from lex import Loc, IdentInfo, LocRge

class UnqualifiedIdKind(Enum):
    IDENTIFIER = ENUM_INIT()
    # operator function id
    # conversion function id
    # literal operator id
    # constructor name
    # constructor template id
    # destructor name
    # template id
    # implicit self param
    # deduction guide name

@dataclass
class UnqualifiedId:
    kind: UnqualifiedIdKind
    value: IdentInfo | None # | ...
    start_location: Loc
    end_location: Loc

    def __init__(self):
        self.kind = UnqualifiedIdKind.IDENTIFIER
        self.value = None

    def is_valid(self) -> bool:
        return start_location != 0

    def set_identifier(self, i: IdentInfo, idloc: Loc):
        self.kind = UnqualifiedIdKind.IDENTIFIER
        self.value = i
        self.start_location = idloc
        self.end_location = idloc

    def get_range(self) -> LocRge:
        return (self.start_location, self.end_location)
