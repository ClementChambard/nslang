from .string_literal import StringLiteralParser
from .scope import Scope, ScopeFlags
from ns_ast.nodes import BuiltinType, BuiltinTypeKind

def tmp_int_parse(val: str) -> int:
    assert len(val) > 0
    if len(val) == 1:
        return int(val)
    if len(val) == 2 and val[0] == "0":
        return int(val[1:], 8)
    if len(val) > 2 and val[0] == "0":
        if val[1] == "x":
            return int(val[2:], 16)
        if val[1] == "b":
            return int(val[2:], 2)
        return int(val[1:], 8)
    return int(val)

TYPES = {}

TYPES["i64"] = BuiltinType(BuiltinTypeKind.I64)
TYPES["bool"] = BuiltinType(BuiltinTypeKind.BOOL)
TYPES["i8"] = BuiltinType(BuiltinTypeKind.I8)

