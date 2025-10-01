struct Type;

lib fn parse_type() -> Type*;
lib fn is_start_of_type() -> bool;
lib fn following_is_type(ctx: i64) -> bool;
