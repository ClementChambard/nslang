from .x86_ir import X86_Register, X86_Size


class Abi:
    PARAM_INT_MAX_REGISTER = 6

    @staticmethod
    def param_int_register(i: int, size: int | X86_Size = 8) -> X86_Register:
        assert i < Abi.PARAM_INT_MAX_REGISTER
        register_names = ["di", "si", "d", "c", "r8", "r9"]
        name = register_names[i]
        return X86_Register(size, name)
