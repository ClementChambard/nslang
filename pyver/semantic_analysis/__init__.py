from .string_literal import StringLiteralParser
from .number_literal import NumLiteralParser
from .char_literal import CharLiteralParser
from .scope import Scope, ScopeFlags
from .constexpr import eval_const_expr
from ns_ast.nodes import BuiltinType, BuiltinTypeKind

TYPES = {}

TYPES["bool"] = BuiltinType(BuiltinTypeKind.BOOL)
TYPES["i8"] = BuiltinType(BuiltinTypeKind.I8)
TYPES["i16"] = BuiltinType(BuiltinTypeKind.I16)
TYPES["i32"] = BuiltinType(BuiltinTypeKind.I32)
TYPES["i64"] = BuiltinType(BuiltinTypeKind.I64)
TYPES["u8"] = BuiltinType(BuiltinTypeKind.U8)
TYPES["u16"] = BuiltinType(BuiltinTypeKind.U16)
TYPES["u32"] = BuiltinType(BuiltinTypeKind.U32)
TYPES["u64"] = BuiltinType(BuiltinTypeKind.U64)

__all__ = [
    "TYPES",
    "StringLiteralParser",
    "NumLiteralParser",
    "CharLiteralParser",
    "Scope",
    "ScopeFlags",
    "eval_const_expr",
]
