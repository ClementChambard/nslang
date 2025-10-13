#include <clap.hpp>
#include <ast/context.hpp>
#include <ast/print.hpp>
#include <codegen/context.hpp>
#include <filesystem>
#include <lexer/ident.hpp>
#include <lexer/lexer.hpp>
#include <parse/parser.hpp>
#include <semantic_analysis/sema.hpp>
#include <backend/backend.hpp>
#include <opts.hpp>

void compile_one_file(Options const & opts, std::string const &input_file, std::string const &output_file) {
  Lexer lexer;
  for (auto const &ip : opts.include_path)
    lexer.add_include_path(ip);
  lexer.enter_source_file(input_file);

  ASTContext ctx;
  Sema sema(ctx);
  Parser parser(lexer, sema);

  auto tu = parser.parse();

  if (opts.mode == Mode::PRINT_AST) {
    print_ast(tu.get());
    return;
  }

  CGContext cg(input_file, ctx);

  cg.gen_tu(tu.get());

  run_backend(opts, cg, output_file);
}


int main(int argc, char **argv) {
  IdentInfo::create_ident_info_list();

  auto args = Clap::parse(argc, argv).check();

  for (auto [i, o] : args.files_to_compile) {
    compile_one_file(args.opts, i, o);
  }

  if (args.opts.mode == Mode::COMPILE_AND_LINK) {
    link_files(args.opts, args.files_to_link, args.program_name);
  }

  for (auto &f : args.files_to_remove) {
    std::filesystem::remove(f);
  }

  return 0;
}
