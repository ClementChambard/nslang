#include "ident.hpp"
#include <memory>
#include <unordered_map>

static std::unordered_map<std::string, std::unique_ptr<IdentInfo>> ident_map;

void IdentInfo::create_ident_info_list() {
  static bool was_init = false;
  if (was_init)
    return;
  ident_map["fn"] = std::make_unique<IdentInfo>("fn", tok::KW_FN);
  ident_map["let"] = std::make_unique<IdentInfo>("let", tok::KW_LET);
  ident_map["lib"] = std::make_unique<IdentInfo>("lib", tok::KW_LIB);
  ident_map["type"] = std::make_unique<IdentInfo>("type", tok::KW_TYPE);
  ident_map["struct"] = std::make_unique<IdentInfo>("struct", tok::KW_STRUCT);
  ident_map["enum"] = std::make_unique<IdentInfo>("enum", tok::KW_ENUM);
  ident_map["i8"] = std::make_unique<IdentInfo>("i8", tok::KW_I8);
  ident_map["i16"] = std::make_unique<IdentInfo>("i16", tok::KW_I16);
  ident_map["i32"] = std::make_unique<IdentInfo>("i32", tok::KW_I32);
  ident_map["i64"] = std::make_unique<IdentInfo>("i64", tok::KW_I64);
  ident_map["u8"] = std::make_unique<IdentInfo>("u8", tok::KW_U8);
  ident_map["u16"] = std::make_unique<IdentInfo>("u16", tok::KW_U16);
  ident_map["u32"] = std::make_unique<IdentInfo>("u32", tok::KW_U32);
  ident_map["u64"] = std::make_unique<IdentInfo>("u64", tok::KW_U64);
  ident_map["f32"] = std::make_unique<IdentInfo>("f32", tok::KW_F32);
  ident_map["f64"] = std::make_unique<IdentInfo>("f64", tok::KW_F64);
  ident_map["bool"] = std::make_unique<IdentInfo>("bool", tok::KW_BOOL);
  ident_map["void"] = std::make_unique<IdentInfo>("void", tok::KW_VOID);
  ident_map["sizeof"] = std::make_unique<IdentInfo>("sizeof", tok::KW_SIZEOF);
  ident_map["cast"] = std::make_unique<IdentInfo>("cast", tok::KW_CAST);
  ident_map["if"] = std::make_unique<IdentInfo>("if", tok::KW_IF);
  ident_map["else"] = std::make_unique<IdentInfo>("else", tok::KW_ELSE);
  ident_map["true"] = std::make_unique<IdentInfo>("true", tok::KW_TRUE);
  ident_map["false"] = std::make_unique<IdentInfo>("false", tok::KW_FALSE);
  ident_map["nullptr"] =
      std::make_unique<IdentInfo>("nullptr", tok::KW_NULLPTR);
  ident_map["vaarg"] = std::make_unique<IdentInfo>("vaarg", tok::KW_VAARG);
  ident_map["vaargs"] = std::make_unique<IdentInfo>("vaargs", tok::KW_VAARGS);
  ident_map["case"] = std::make_unique<IdentInfo>("case", tok::KW_CASE);
  ident_map["default"] =
      std::make_unique<IdentInfo>("default", tok::KW_DEFAULT);
  ident_map["switch"] = std::make_unique<IdentInfo>("switch", tok::KW_SWITCH);
  ident_map["while"] = std::make_unique<IdentInfo>("while", tok::KW_WHILE);
  ident_map["do"] = std::make_unique<IdentInfo>("do", tok::KW_DO);
  ident_map["for"] = std::make_unique<IdentInfo>("for", tok::KW_FOR);
  ident_map["continue"] =
      std::make_unique<IdentInfo>("continue", tok::KW_CONTINUE);
  ident_map["break"] = std::make_unique<IdentInfo>("break", tok::KW_BREAK);
  ident_map["return"] = std::make_unique<IdentInfo>("return", tok::KW_RETURN);
  was_init = true;
}

IdentInfo *IdentInfo::find(std::string const &name) {
  if (ident_map.contains(std::string(name))) {
    return ident_map[name].get();
  }
  auto ident = std::make_unique<IdentInfo>(name);
  auto res = ident.get();
  ident_map[name] = std::move(ident);
  return res;
}
