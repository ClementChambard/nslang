from enum import Enum

ENUM_CUR_VAL = 0


def ENUM_INIT() -> int:
    global ENUM_CUR_VAL
    ENUM_CUR_VAL = 0
    return 0


def ENUM_N() -> int:
    global ENUM_CUR_VAL
    ENUM_CUR_VAL += 1
    return ENUM_CUR_VAL


def ENUM_SAME() -> int:
    return ENUM_CUR_VAL - 1
