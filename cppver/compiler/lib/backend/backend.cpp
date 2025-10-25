#include "backend.hpp"
#include "codegen/context.hpp"
#include "opts.hpp"

#include <filesystem>
#include <fstream>
#include <iostream>
#include <llvm/Support/raw_ostream.h>
#include <string>
#include <sys/wait.h>

// TODO: correct backend

void subprocess_run(std::span<char const *> args) {
  auto pid = fork();
  if (pid == 0) {
    char const *args_array[64]; // max 64 arg. is this ok ?
    std::memcpy(args_array, args.data(), args.size() * sizeof(args[0]));
    args_array[args.size()] = nullptr;
    execvp(args[0], const_cast<char *const *>(args_array));
  } else {
    waitpid(pid, nullptr, 0);
  }
}

void print_and_remove_file(std::string const &filename) {
  std::ifstream s(filename);
  std::cout << s.rdbuf();
  std::filesystem::remove(filename);
}

void run_backend(Options const &opts, CGContext &ctx, std::string const &output_file_name) {
  std::string f_tmp_no = output_file_name + ".tmp-no.ll";
  std::string f_tmp = output_file_name + ".tmp.ll";
  std::string f_s = output_file_name + ".tmp.s";

  std::error_code ec;
  llvm::raw_fd_ostream out(f_tmp_no, ec);

  ctx.module.print(out, nullptr);

  out.close();

  char const * const opt_strs[] = {"-O0", "-O1", "-O2", "-O3"};
  char const *ar[] = {"opt", f_tmp_no.c_str(), "-S", "-o", f_tmp.c_str(), opt_strs[opts.opt_level]};
  subprocess_run(ar);

  std::filesystem::remove(f_tmp_no);

  if (opts.mode == Mode::PRINT_LLVM) {
    return print_and_remove_file(f_tmp);
  }

  char const *ar2[] = {"llc", f_tmp.c_str(), "--relocation-model=pic", "-o", f_s.c_str()};
  subprocess_run(ar2);
  
  std::filesystem::remove(f_tmp);

  if (opts.mode == Mode::PRINT_ASM) {
    return print_and_remove_file(f_s);
  }


  char const *ar3[] = {"as", f_s.c_str(), "-o", output_file_name.c_str()};
  subprocess_run(ar3);

  std::filesystem::remove(f_s);

}

void link_files(Options const &opts, std::span<std::string> files_to_link, std::string const &out) {
  std::vector<std::string> link_libs_args = {"ld", "-o", out};
  link_libs_args.insert(link_libs_args.end(), files_to_link.begin(), files_to_link.end());

  // libs
  for (auto &l : opts.link_libraries) {
    // auto xt = std::filesystem::path(l).extension();
    // if (xt == "a" || xt == "so") {
      link_libs_args.push_back(l);
    //} else {
    //  link_libs_args.push_back("-l" + l); // TODO: is this correct ?
    //}
  }

  std::vector<char const *> cstr_args;
  for (auto &s : link_libs_args) cstr_args.push_back(s.c_str());

  // TODO: why is that ? can this be determined at runtime ?
  cstr_args.push_back("--dynamic-linker=/lib64/ld-linux-x86-64.so.2");
  cstr_args.push_back("-pie");

  subprocess_run(cstr_args);
}
