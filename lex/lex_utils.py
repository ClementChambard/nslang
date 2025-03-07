
def ident_continue(c: str):
    return (
        (c <= "z" and c >= "a")
        or (c <= "Z" and c >= "A")
        or (c <= "9" and c >= "0")
        or c == "_"
    )
