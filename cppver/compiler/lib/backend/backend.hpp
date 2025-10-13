#ifndef BACKEND_BACKEND_HPP_INCLUDED
#define BACKEND_BACKEND_HPP_INCLUDED

#include "codegen/context.hpp"
#include "opts.hpp"
#include <string>

void run_backend(Options const &opts, CGContext &ctx, std::string const &output_file_name = "");

void link_files(Options const &opts, std::span<std::string> files_to_link, std::string const &out);

#endif // !BACKEND_BACKEND_HPP_INCLUDED
