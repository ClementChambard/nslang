#include "clap.hpp"
#include "defines.hpp"
#include "opts.hpp"
#include <cassert>
#include <cstdio>
#include <cstring>
#include <filesystem>

[[noreturn]] void fatal_error(std::string const &msg) {
  std::fprintf(stderr, "\x1b[1;31mfatal error: \x1b[0;1m%s\x1b[0m\n", msg.c_str());
  std::exit(1);
}

Clap Clap::parse(int argc, char **argv) {
  // exe
  argv++, argc--;

  Clap out;
  
  while (argc > 0) {
    if (std::strcmp(*argv, "-o") == 0) {
      if (out.output_filename) fatal_error("'-o' already set");
      argv++, argc--;
      if (argc == 0) fatal_error("missing filename after '-o'");
      out.output_filename = *argv;
    } else if (std::strcmp(*argv, "-c") == 0) {
      out.compile_only = true;
    } else if (std::strcmp(*argv, "-ast") == 0) {
      out.print_ast = true;
    } else if (std::strcmp(*argv, "-asm") == 0) {
      out.print_asm = true;
    } else if (std::strcmp(*argv, "-llvm") == 0) {
      out.print_llvm = true;
    } else if (std::strcmp(*argv, "-llvm-opt") == 0) {
      out.print_opt_llvm = true;
    } else if (std::strcmp(*argv, "-nostdlib") == 0) {
      out.no_stdlib = true;
    } else if (std::strcmp(*argv, "-noruntime") == 0) {
      out.no_runtime = true;
    } else if (std::memcmp(*argv, "-I", 2) == 0) {
      out.included_dirs.push_back(*argv + 2);
    } else if (std::memcmp(*argv, "-L", 2) == 0) {
      out.linked_libs.push_back(*argv + 2);
    } else {
      out.supplied_filenames.push_back(*argv);
    }
    argv++, argc--;
  }

  return out;
}

Mode get_mode(Clap const &c) {
  i32 set = 0;
  Mode mode = Mode::COMPILE_AND_LINK;
  if (c.print_ast) mode = Mode::PRINT_AST, set++;
  if (c.print_llvm) mode = Mode::PRINT_LLVM, set++;
  if (c.print_opt_llvm) mode = Mode::PRINT_OPT_LLVM, set++;
  if (c.print_asm) mode = Mode::PRINT_ASM, set++;
  if (c.compile_only) mode = Mode::COMPILE_ONLY, set++;

  if (set > 1) {
    fatal_error("only one of [-c, -asm, -ast, -llvm, -llvm-opt] can be set at a time");
  }

  return mode;
}

#define STDLIB_LOC "/home/clement/dev/nslang/stdlib/"

bool is_linktime_file(std::string const &filename) {
  return filename.ends_with(".a") || filename.ends_with(".o");
}

CheckedClap Clap::check() const {
  CheckedClap out;

  out.opts.mode = get_mode(*this);
  if (!no_stdlib) {
    out.opts.link_libraries.push_back(STDLIB_LOC "bin/stdlib.a");
    out.opts.include_path.push_back(STDLIB_LOC "include");
  }
  if (!no_runtime) {
    out.opts.link_libraries.push_back(STDLIB_LOC "bin/runtime.a");
  }

  out.opts.include_path.insert(out.opts.include_path.end(), included_dirs.begin(), included_dirs.end());
  out.opts.link_libraries.insert(out.opts.link_libraries.end(), linked_libs.begin(), included_dirs.end());

  if (supplied_filenames.size() == 0) fatal_error("no input file");

  if (supplied_filenames.size() > 1 && out.opts.mode != Mode::COMPILE_AND_LINK && output_filename)  {
    fatal_error("cannot supply output filename with multiple files");
  }

  if (out.opts.mode != Mode::COMPILE_AND_LINK && out.opts.mode != Mode::COMPILE_ONLY && output_filename) {
    fatal_error("cannot supply output filename with one of [-asm, -ast, -llvm, -llvm-opt]");
  }

  if (out.opts.mode == Mode::COMPILE_ONLY && output_filename) {
    assert(supplied_filenames.size() == 1);
    if (is_linktime_file(supplied_filenames[0])) fatal_error("file " + supplied_filenames[0] + " is already compiled");
    out.files_to_compile.emplace_back(supplied_filenames[0], *output_filename);
  } else {
    for (auto & f : supplied_filenames) {
      if (is_linktime_file(f)) {
        if (out.opts.mode != Mode::COMPILE_AND_LINK) fatal_error("file " + f + " is already compiled");
        out.files_to_link.push_back(f);
      } else {
        auto out_fn = std::filesystem::path(f).replace_extension("o").string();
        out.files_to_compile.emplace_back(f, out_fn);
        if (out.opts.mode != Mode::COMPILE_ONLY) out.files_to_remove.push_back(out_fn);
      }
    }
  }

  if (out.opts.mode == Mode::COMPILE_AND_LINK) {
    for (auto &[_, o] : out.files_to_compile) {
      out.files_to_link.push_back(o);
    }
    if (output_filename) out.program_name = *output_filename;
    else out.program_name = "a.out";
  }

  return out;
}
