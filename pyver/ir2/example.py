from ir2.data.types import IrTypeInt
from ir2.data.value import ConstantInt
from ir2.gen_func import ir2_func
from ir2.optim.simplifycfg import SimplifycfgPass
from ir2.optim.sroa import SroaPass
from ns_ast.nodes.decl import (
    FnDecl,
    StructDecl,
    TranslationUnitDecl,
    VarDecl,
)
from ns_ast.printer import print_ast


def ir2_globalvar(d: VarDecl):
    assert False, "unimplemented"


def ir2_from_ast(ast: TranslationUnitDecl):
    out_funcs = []
    out_globs = []
    for d in ast.decls:
        if isinstance(d, FnDecl):
            out_funcs.append(ir2_func(d))
        if isinstance(d, StructDecl):
            assert False, "struct not implemented"
        if isinstance(d, VarDecl):
            out_globs.append(ir2_globalvar(d))

    return out_funcs


def example():
    from lex import Lexer
    from parse import Parser

    lexer = Lexer()

    lexer.enter_source_file("/home/clement/dev/nslang/ir2/tst_ir2.ns")

    parser = Parser(lexer)

    ast = parser.parse()

    print_ast(ast)

    ir2 = ir2_from_ast(ast)

    # print("\n".join([a.to_txt() for a in ir2]))

    SimplifycfgPass().run(ir2)
    SroaPass().run(ir2)

    print("\n".join([a.to_txt() for a in ir2]))
