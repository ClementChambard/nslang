import subprocess

def create_object_from_asm(code, outputfilename, nasm_args):
    with open("out.s", "w") as f:
        f.write(code)

    cmd =["nasm", "-f", "elf64", "out.s", "-o", outputfilename] + nasm_args
    subprocess.run(cmd)
    subprocess.run(["rm", "out.s"])

def link_executable(input_files, outputfilename="a.out"):
    subprocess.run(["ld", "-o", outputfilename] + input_files)
    subprocess.run(["rm"] + input_files)
