from . import FullIr

def print_ir(ir: FullIr):
    for key, value in ir.functions.items():
        if len(value.instructions) == 0:
            if value.is_lib:
                print(f"lib {key}")
            continue
        i = ["", "lib "][value.is_lib]
        print(f"{i}{key}: ({value.stack_frame}) ({value.returns_value})")
        for i in value.instructions:
            print(i)
    for i, g in enumerate(ir.globs):
        print(f"global_{i}:")
        print(f"    {g.data}")
