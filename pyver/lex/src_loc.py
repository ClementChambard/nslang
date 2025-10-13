from typing import Tuple
from dataclasses import dataclass

type Loc = int
type LocRge = Tuple[Loc, Loc]
LOC_INVALID: Loc = 0

@dataclass
class LocRef:
    value: Loc

type LocPtr = LocRef | None
