# def tests():
#     from ir2.data import (
#         IrBinOpKind,
#         IrBlock,
#         IrConstant,
#         IrFunction,
#         IrInstrBinOp,
#         IrInstrBranch,
#         IrInstrRet,
#         IrLinkage,
#         IrTranslationUnit,
#         IrTypeInt,
#         IrValue,
#     )
#     from ir2.print_ir import print_ir
#     param_0 = IrValue(IrTypeInt(64), "arg0")
#     res_value = IrValue(IrTypeInt(64), "res")
#     br_value = IrValue(IrTypeInt(1), "2")
#
#     entry_block = IrBlock("entry", [])
#     after_block = IrBlock(
#         "if_after",
#         [
#             IrInstrBinOp(
#                 res_value,
#                 IrBinOpKind.ADD,
#                 IrConstant(IrTypeInt(64), 10),
#                 param_0,
#             ),
#             IrInstrRet(res_value),
#         ],
#     )
#     entry_block.instrs.append(IrInstrBranch(after_block, br_value, entry_block))
#
#     ir = IrTranslationUnit(
#         [
#             IrFunction(
#                 "test",
#                 IrLinkage.PUBLIC,
#                 [param_0],
#                 IrTypeInt(64),
#                 [entry_block, after_block],
#                 False,
#             )
#         ]
#     )
#     print_ir(ir)


def tests():
    from codegen.x86_64.code2 import compile_ir
    from lex import Lexer
    from parse import Parser
    from ir import generate_ir
    from utils.diagnostic import compilation_had_errors

    lexer = Lexer()
    lexer.add_include_paths(["/home/clement/dev/nslang/stdlib/include"])
    lexer.enter_source_file("main.ns")
    parser = Parser(lexer)
    ast = parser.parse()
    if compilation_had_errors():
        return
    ir = generate_ir(ast)

    print(compile_ir(ir))


tests()
