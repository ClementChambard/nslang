OCTAL_DIGITS = ["0", "1", "2", "3", "4", "5", "6", "7"]
DECIMAL_DIGITS = OCTAL_DIGITS + ["8", "9"]
HEX_DIGITS = DECIMAL_DIGITS + ["a", "b", "c", "d", "e", "f", "A", "B", "C", "D", "E", "F"]

def hex_digit_value(digit: str) -> int:
    assert digit in HEX_DIGITS
    if digit in DECIMAL_DIGITS:
        return ord(digit) - ord("0")
    return 10 + ord(digit.lower()) - ord("a")
