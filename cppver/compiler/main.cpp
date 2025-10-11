#include <ast/context.hpp>
#include <ast/print.hpp>
#include <codegen/context.hpp>
#include <lexer/ident.hpp>
#include <lexer/lexer.hpp>
#include <llvm/IR/LLVMContext.h>
#include <parse/parser.hpp>
#include <semantic_analysis/sema.hpp>

int main(int argc, char **argv) {

  std::string file_name;
  if (argc == 1)
    file_name = "/home/clement/dev/nslang/compiler/src/darray.ns";
  else
    file_name = argv[1];

  Lexer lexer;
  IdentInfo::create_ident_info_list();
  lexer.add_include_path("/home/clement/dev/nslang/stdlib/include");
  lexer.enter_source_file(file_name);

  ASTContext ctx;
  Sema sema(ctx);
  Parser parser(lexer, sema);

  auto tu = parser.parse();

  // print_ast(tu.get());

  CGContext cgctx(file_name, ctx);

  cgctx.gen_tu(tu.get());

  cgctx.module.print(llvm::outs(), nullptr);

  return 0;
}
