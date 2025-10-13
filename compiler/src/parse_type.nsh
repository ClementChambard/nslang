#include parser.nsh

struct Type;

lib fn Parser::parse_type(self: Parser *) -> Type*;
lib fn Parser::is_start_of_type(self: Parser *) -> bool;
lib fn Parser::following_is_type(self: Parser *, ctx: i64) -> bool;
