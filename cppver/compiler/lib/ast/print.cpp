
#include "print.hpp"
#include "ast/nodes/decl.hpp"
#include "ast/nodes/expr.hpp"
#include "ast/nodes/stmt.hpp"
#include "defines.hpp"
#include "lexer/file.hpp"
#include <cstdio>
#include <string>

struct COL {
  static cstr RESET;
  static cstr DECL;
  static cstr ATTR;
  static cstr STMT;
  static cstr CMT;
  static cstr TYPE;
  static cstr ADDR;
  static cstr LOC;
  static cstr VK;
  static cstr OK;
  static cstr ERROR;
  static cstr NULLTXT;
  static cstr UNDERSERIALID;
  static cstr CAST;
  static cstr VALUE;
  static cstr DECL_NAME;
  static cstr INDENT;
};

cstr COL::RESET = "\x1b[0m";
cstr COL::DECL = "\x1b[1;32m";          // Green Bold
cstr COL::ATTR = "\x1b[1;34m";          // Blue  Bold
cstr COL::STMT = "\x1b[1;35m";          // Magenta Bold
cstr COL::CMT = "\x1b[34m";             // Blue
cstr COL::TYPE = "\x1b[32m";            // Green
cstr COL::ADDR = "\x1b[33m";            // Yellow
cstr COL::LOC = "\x1b[33m";             // Yellow
cstr COL::VK = "\x1b[36m";              // Cyan
cstr COL::OK = "\x1b[36m";              // Cyan
cstr COL::ERROR = "\x1b[1;31m";         // Red   Bold
cstr COL::NULLTXT = "\x1b[34m";         // Blue
cstr COL::UNDERSERIALID = "\x1b[1;32m"; // Green Bold
cstr COL::CAST = "\x1b[31m";            // Red
cstr COL::VALUE = "\x1b[1;36m";         // Cyan  Bold
cstr COL::DECL_NAME = "\x1b[1;36m";     // Cyan  Bold
cstr COL::INDENT = "\x1b[34m";          // Blue

static cstr last_filename = nullptr;
static u32 last_line_num = 0;

void print_location(Loc loc) {
  auto [f, l, c] = OpenedFile::get_loc(loc);
  if (f != last_filename) {
    fprintf(stderr, "%s%s:%d:%d%s", COL::LOC, f, l, c, COL::RESET);
    last_filename = f;
  } else if (l != last_line_num) {
    fprintf(stderr, "%sline:%d:%d%s", COL::LOC, l, c, COL::RESET);
    last_line_num = l;
  } else {
    fprintf(stderr, "%scol:%d%s", COL::LOC, c, COL::RESET);
  }
}

void print_loc_range(LocRge rge) {
  fprintf(stderr, " <");
  print_location(rge.start);
  if (rge.start != rge.end) {
    fprintf(stderr, ", ");
    print_location(rge.end);
  }
  fprintf(stderr, ">");
}

void print_named_decl_name(NamedDecl const *nd) {
  if (nd->name)
    fprintf(stderr, " %s%s%s", COL::DECL_NAME, nd->get_name().c_str(),
            COL::RESET);
}

void print_fn_decl_name(FunctionDecl const *fd) {
  if (!fd->name)
    return;
  auto struct_name =
      fd->struct_scope ? fd->struct_scope->name->name + "::" : "";
  fprintf(stderr, " %s%s%s%s", COL::DECL_NAME, struct_name.c_str(),
          fd->get_name().c_str(), COL::RESET);
}

void print_type_inner(Type const *t) {
  if (auto *bt = t->dyn_cast<BuiltinType>()) {
    fprintf(stderr, "%s", bt->get_name());
  } else if (auto *pt = t->dyn_cast<PointerType>()) {
    print_type_inner(pt->pointee_type);
    if (!pt->pointee_type->is_pointer_type())
      fprintf(stderr, " ");
    fprintf(stderr, "*");
  } else if (auto *pt = t->dyn_cast<ArrayType>()) {
    print_type_inner(pt->element_type);
    fprintf(stderr, "[%lld]", pt->element_count);
  } else if (auto *at = t->dyn_cast<AliasType>()) {
    fprintf(stderr, "type %s", at->decl->get_name().c_str());
  } else if (auto *at = t->dyn_cast<EnumType>()) {
    fprintf(stderr, "enum %s", at->decl->get_name().c_str());
  } else if (auto *at = t->dyn_cast<StructType>()) {
    fprintf(stderr, "struct %s", at->decl->get_name().c_str());
  } else if (auto *ft = t->dyn_cast<FunctionType>()) {
    print_type_inner(ft->result_type);
    fprintf(stderr, "(");
    u32 i = 0;
    for (i = 0; i < ft->param_types.size(); i++) {
      if (i != 0) {
        fprintf(stderr, ", ");
      }
      print_type_inner(ft->param_types[i]);
    }
    if (ft->function.variadic) {
      if (i)
        fprintf(stderr, ", ");
      fprintf(stderr, "...");
      if (ft->function.variadic == Type::FunctionTypeBits::VALIST_IN_ARGS)
        fprintf(stderr, "*");
    }
    fprintf(stderr, ")");
  }
}

void print_type(Type const *t) {
  fprintf(stderr, " %s", COL::TYPE);
  print_type_inner(t);
  fprintf(stderr, "%s", COL::RESET);
}

void print_indent(std::vector<int> const &indent) {
  if (indent.size() == 0)
    return;
  fprintf(stderr, "%s", COL::INDENT);
  for (u32 i = 0; i < indent.size() - 1; i++) {
    fprintf(stderr, "%s", indent[i] ? "│  " : "   ");
  }
  fprintf(stderr, "%s", indent.back() ? "├─ " : "╰─ ");
  fprintf(stderr, "%s", COL::RESET);
}

std::string format_string_literal(std::string const &s) {
  std::string out;
  out.reserve(s.size());
  for (auto c : s) {
    if (c == '\n')
      out += "\\n";
    else if (c == '\r')
      out += "\\r";
    else if (c == '\t')
      out += "\\t";
    else if (c == '\"')
      out += "\\\"";
    else if (c == '\\')
      out += "\\\\";
    else
      out += c;
    // TODO: all escape sequences
  }
  return out;
}

void print_bare_decl(Decl const *decl) {
  fprintf(stderr, "%s", COL::DECL);
  if (decl->isa<TranslationUnitDecl>())
    fprintf(stderr, "TranslationUnitDecl");
  else if (decl->isa<AliasDecl>())
    fprintf(stderr, "AliasDecl");
  else if (decl->isa<EnumDecl>())
    fprintf(stderr, "EnumDecl");
  else if (decl->isa<StructDecl>())
    fprintf(stderr, "StructDecl");
  else if (decl->isa<ParamDecl>())
    fprintf(stderr, "ParamDecl");
  else if (decl->isa<VarDecl>())
    fprintf(stderr, "VarDecl");
  else if (decl->isa<FieldDecl>())
    fprintf(stderr, "FieldDecl");
  else if (decl->isa<FunctionDecl>())
    fprintf(stderr, "FunctionDecl");
  else if (decl->isa<EnumVariantDecl>())
    fprintf(stderr, "EnumVariantDecl");
  fprintf(stderr, "%s", COL::RESET);

  if (auto *fd = decl->dyn_cast<FunctionDecl>()) {
    print_fn_decl_name(fd);
    print_type(fd->type);
  } else if (auto *vd = decl->dyn_cast<ValueDecl>()) {
    print_named_decl_name(vd);
    print_type(vd->type);
  } else if (auto *vd = decl->dyn_cast<TypeDecl>()) {
    print_named_decl_name(vd);
  }
}

void print_decl(Decl const *decl) {
  fprintf(stderr, "%s", COL::DECL);
  if (decl->isa<TranslationUnitDecl>())
    fprintf(stderr, "TranslationUnitDecl");
  else if (decl->isa<AliasDecl>())
    fprintf(stderr, "AliasDecl");
  else if (decl->isa<EnumDecl>())
    fprintf(stderr, "EnumDecl");
  else if (decl->isa<StructDecl>())
    fprintf(stderr, "StructDecl");
  else if (decl->isa<ParamDecl>())
    fprintf(stderr, "ParamDecl");
  else if (decl->isa<VarDecl>())
    fprintf(stderr, "VarDecl");
  else if (decl->isa<FieldDecl>())
    fprintf(stderr, "FieldDecl");
  else if (decl->isa<FunctionDecl>())
    fprintf(stderr, "FunctionDecl");
  else if (decl->isa<EnumVariantDecl>())
    fprintf(stderr, "EnumVariantDecl");
  fprintf(stderr, "%s", COL::RESET);
  print_loc_range(decl->src_range);
  fprintf(stderr, " ");
  print_location(decl->get_loc());

  if (auto *fd = decl->dyn_cast<FunctionDecl>()) {
    print_fn_decl_name(fd);
    print_type(fd->type);
    if (fd->has_init_ident) {
      fprintf(stderr, " init");
    }
  } else if (auto *evd = decl->dyn_cast<EnumVariantDecl>()) {
    print_named_decl_name(evd);
    fprintf(stderr, " = %lld", evd->value);
  } else if (auto *vd = decl->dyn_cast<ValueDecl>()) {
    print_named_decl_name(vd);
    print_type(vd->type);
  } else if (auto *vd = decl->dyn_cast<TypeDecl>()) {
    print_named_decl_name(vd);
  }
}

void print_stmt(Stmt const *stmt) {
  fprintf(stderr, "%s", COL::STMT);
  switch (stmt->kind) {
  case Stmt::DECL_STMT:
    fprintf(stderr, "DeclStmt");
    break;
  case Stmt::NULL_STMT:
    fprintf(stderr, "NullStmt");
    break;
  case Stmt::COMPOUND_STMT:
    fprintf(stderr, "CompoundStmt");
    break;
  case Stmt::CASE_STMT:
    fprintf(stderr, "CaseStmt");
    break;
  case Stmt::DEFAULT_STMT:
    fprintf(stderr, "DefaultStmt");
    break;
  case Stmt::IF_STMT:
    fprintf(stderr, "IfStmt");
    break;
  case Stmt::SWITCH_STMT:
    fprintf(stderr, "SwitchStmt");
    break;
  case Stmt::WHILE_STMT:
    fprintf(stderr, "WhileStmt");
    break;
  case Stmt::DO_STMT:
    fprintf(stderr, "DoStmt");
    break;
  case Stmt::FOR_STMT:
    fprintf(stderr, "ForStmt");
    break;
  case Stmt::CONTINUE_STMT:
    fprintf(stderr, "ContinueStmt");
    break;
  case Stmt::BREAK_STMT:
    fprintf(stderr, "BreakStmt");
    break;
  case Stmt::RETURN_STMT:
    fprintf(stderr, "ReturnStmt");
    break;
  }
  fprintf(stderr, "%s", COL::RESET);
  print_loc_range(stmt->get_range());
}

void print_expr(Expr const *expr) {
  fprintf(stderr, "%s", COL::STMT);
  switch (expr->kind) {
  case Stmt::DECLREF_EXPR:
    fprintf(stderr, "DeclRefExpr");
    break;
  case Stmt::INTEGER_LITERAL:
    fprintf(stderr, "IntegerLiteral");
    break;
  case Stmt::FLOATING_LITERAL:
    fprintf(stderr, "FloatingLiteral");
    break;
  case Stmt::BOOL_LITERAL:
    fprintf(stderr, "BoolLiteral");
    break;
  case Stmt::STRING_LITERAL:
    fprintf(stderr, "StringLiteral");
    break;
  case Stmt::CHAR_LITERAL:
    fprintf(stderr, "CharLiteral");
    break;
  case Stmt::NULLPTR_LITERAL:
    fprintf(stderr, "NullptrLiteral");
    break;
  case Stmt::PAREN_EXPR:
    fprintf(stderr, "ParenExpr");
    break;
  case Stmt::UNARY_EXPR:
    fprintf(stderr, "UnaryExpr");
    break;
  case Stmt::BINARY_EXPR:
    fprintf(stderr, "BinaryExpr");
    break;
  case Stmt::COMPOUND_ASSIGN_EXPR:
    fprintf(stderr, "CompoundAssignExpr");
    break;
  case Stmt::CALL_EXPR:
    fprintf(stderr, "CallExpr");
    break;
  case Stmt::METHOD_CALL_EXPR:
    fprintf(stderr, "MethodCallExpr");
    break;
  case Stmt::INIT_CALL_EXPR:
    fprintf(stderr, "InitCallExpr");
    break;
  case Stmt::SIZEOF_EXPR:
    fprintf(stderr, "SizeofExpr");
    break;
  case Stmt::MEMBER_EXPR:
    fprintf(stderr, "MemberExpr");
    break;
  case Stmt::METHOD_EXPR:
    fprintf(stderr, "MethodExpr");
    break;
  case Stmt::ARRAY_SUBSCRIPT_EXPR:
    fprintf(stderr, "ArraySubscriptExpr");
    break;
  case Stmt::CONDITIONAL_EXPR:
    fprintf(stderr, "ConditionalExpr");
    break;
  case Stmt::RECOVERY_EXPR:
    fprintf(stderr, "RecoveryExpr");
    break;
  case Stmt::IMPLICIT_CAST_EXPR:
    fprintf(stderr, "ImplicitCastExpr");
    break;
  case Stmt::EXPLICIT_CAST_EXPR:
    fprintf(stderr, "ExplicitCastExpr");
    break;
  case Stmt::VAARG_EXPR:
    fprintf(stderr, "VAArgExpr");
    break;
  case Stmt::VAARGS_EXPR:
    fprintf(stderr, "VAArgsExpr");
    break;
  }
  fprintf(stderr, "%s", COL::RESET);
  print_loc_range(expr->get_range());
  print_type(expr->type);
  if (expr->vk == ValueKind::LVALUE)
    fprintf(stderr, " %slvalue%s", COL::VK, COL::RESET);
  if (expr->vk == ValueKind::XVALUE)
    fprintf(stderr, " %sxvalue%s", COL::VK, COL::RESET);

  if (auto *e = expr->dyn_cast<IntegerLiteral>()) {
    fprintf(stderr, " %s%lld%s", COL::VALUE, e->value, COL::RESET);
  } else if (auto *e = expr->dyn_cast<FloatingLiteral>()) {
    fprintf(stderr, " %s%f%s", COL::VALUE, e->value, COL::RESET);
  } else if (auto *e = expr->dyn_cast<CharLiteral>()) {
    fprintf(stderr, " %s%lld%s", COL::VALUE, e->value, COL::RESET);
  } else if (auto *e = expr->dyn_cast<BoolLiteral>()) {
    fprintf(stderr, " %s%s%s", COL::VALUE, e->value ? "true" : "false",
            COL::RESET);
  } else if (auto *e = expr->dyn_cast<StringLiteral>()) {
    fprintf(stderr, " %s\"%s\"%s", COL::VALUE,
            format_string_literal(e->value).c_str(), COL::RESET);
  } else if (auto *e = expr->dyn_cast<MemberExpr>()) {
    fprintf(stderr, " %s%s", e->is_arrow ? "->" : ".", e->name->name.c_str());
  } else if (auto *e = expr->dyn_cast<CastExpr>()) {
    fprintf(stderr, " <%s%s%s>", COL::CAST, CastExpr::kind_to_str(e->cast_kind),
            COL::RESET);
  } else if (auto *e = expr->dyn_cast<UnaryExpr>()) {
    fprintf(stderr, " '%s'", UnaryExpr::opc_name(e->opc));
  } else if (auto *e = expr->dyn_cast<BinaryExpr>()) {
    fprintf(stderr, " '%s'", BinaryExpr::opc_name(e->opc));
  } else if (auto *e = expr->dyn_cast<DeclRefExpr>()) {
    fprintf(stderr, " ");
    print_bare_decl(e->decl);
  }
}

void print_expr_tree(Expr const *expr, std::vector<int> &indent) {
  if (expr == nullptr)
    return;
  print_indent(indent);
  print_expr(expr);
  fprintf(stderr, "\n");
  indent.push_back(1);

  if (auto *e = expr->dyn_cast<ParenExpr>()) {
    indent.back()--;
    print_expr_tree(e->val.get(), indent);
  } else if (auto *e = expr->dyn_cast<UnaryExpr>()) {
    indent.back()--;
    print_expr_tree(e->arg.get(), indent);
  } else if (auto *e = expr->dyn_cast<CastExpr>()) {
    indent.back()--;
    print_expr_tree(e->op.get(), indent);
  } else if (auto *e = expr->dyn_cast<BinaryExpr>()) {
    print_expr_tree(e->lhs.get(), indent);
    indent.back()--;
    print_expr_tree(e->rhs.get(), indent);
  } else if (auto *e = expr->dyn_cast<ArraySubscriptExpr>()) {
    print_expr_tree(e->lhs.get(), indent);
    indent.back()--;
    print_expr_tree(e->rhs.get(), indent);
  } else if (auto *e = expr->dyn_cast<ConditionalExpr>()) {
    print_expr_tree(e->cond.get(), indent);
    print_expr_tree(e->true_expr.get(), indent);
    indent.back()--;
    print_expr_tree(e->false_expr.get(), indent);
  } else if (auto *e = expr->dyn_cast<MemberExpr>()) {
    indent.back()--;
    print_expr_tree(e->base.get(), indent);
  } else if (auto *e = expr->dyn_cast<CallExpr>()) {
    if (e->args.size() == 0)
      indent.back()--;
    print_expr_tree(e->fn.get(), indent);
    u32 i = 0;
    for (auto &v : e->args) {
      if (++i == e->args.size())
        indent.back()--;
      print_expr_tree(v.get(), indent);
    }
  } else if (auto *e = expr->dyn_cast<RecoveryExpr>()) {
    u32 i = 0;
    for (auto &v : e->sub_exprs) {
      if (++i == e->sub_exprs.size())
        indent.back()--;
      print_expr_tree(v.get(), indent);
    }
  } else if (auto *e = expr->dyn_cast<SizeofExpr>()) {
    indent.back()--;
    if (e->expr)
      print_expr_tree(e->expr.get(), indent);
  }

  indent.pop_back();
}

void print_decl_tree(Decl const *decl, std::vector<int> &indent);

void print_stmt_tree(Stmt const *stmt, std::vector<int> &indent) {
  if (stmt == nullptr)
    return;
  if (auto *expr = stmt->dyn_cast<Expr>())
    return print_expr_tree(expr, indent);
  print_indent(indent);
  print_stmt(stmt);
  fprintf(stderr, "\n");
  indent.push_back(1);

  if (auto *ds = stmt->dyn_cast<DeclStmt>()) {
    indent.back()--;
    print_decl_tree(ds->decl.get(), indent);
  } else if (auto *cs = stmt->dyn_cast<CompoundStmt>()) {
    u32 i = 0;
    for (auto &v : cs->inner) {
      if (++i == cs->inner.size())
        indent.back()--;
      print_stmt_tree(v.get(), indent);
    }
  } else if (auto *s = stmt->dyn_cast<CaseStmt>()) {
    indent.back()--;
    print_stmt_tree(s->sub_stmt.get(), indent);
  } else if (auto *s = stmt->dyn_cast<DefaultStmt>()) {
    indent.back()--;
    print_stmt_tree(s->sub_stmt.get(), indent);
  } else if (auto *s = stmt->dyn_cast<IfStmt>()) {
    print_expr_tree(s->cond.get(), indent);
    if (s->else_stmt) {
      print_stmt_tree(s->then_stmt.get(), indent);
      indent.back()--;
      print_stmt_tree(s->else_stmt.get(), indent);
    } else {
      indent.back()--;
      print_stmt_tree(s->then_stmt.get(), indent);
    }
  } else if (auto *s = stmt->dyn_cast<WhileStmt>()) {
    print_expr_tree(s->cond.get(), indent);
    indent.back()--;
    print_stmt_tree(s->body.get(), indent);
  } else if (auto *s = stmt->dyn_cast<ForStmt>()) {
    if (s->init_stmt) {
      print_stmt_tree(s->init_stmt.get(), indent);
    }
    if (s->cond) {
      print_expr_tree(s->cond.get(), indent);
    }
    if (s->latch) {
      print_expr_tree(s->latch.get(), indent);
    }
    indent.back()--;
    print_stmt_tree(s->body.get(), indent);
  } else if (auto *s = stmt->dyn_cast<SwitchStmt>()) {
    print_expr_tree(s->cond.get(), indent);
    indent.back()--;
    print_stmt_tree(s->body.get(), indent);
  } else if (auto *s = stmt->dyn_cast<DoStmt>()) {
    print_stmt_tree(s->body.get(), indent);
    indent.back()--;
    print_expr_tree(s->cond.get(), indent);
  } else if (auto *s = stmt->dyn_cast<ReturnStmt>()) {
    indent.back()--;
    print_expr_tree(s->return_value.get(), indent);
  }

  indent.pop_back();
}

void print_decl_tree(Decl const *decl, std::vector<int> &indent) {
  if (decl == nullptr)
    return;
  print_indent(indent);
  print_decl(decl);
  fprintf(stderr, "\n");

  indent.push_back(1);

  if (auto *d = decl->dyn_cast<TranslationUnitDecl>()) {
    u32 i = 0;
    for (auto &v : d->decls) {
      if (++i == d->decls.size())
        indent.back()--;
      print_decl_tree(v.get(), indent);
    }
  } else if (auto *d = decl->dyn_cast<EnumDecl>()) {
    u32 i = 0;
    for (auto &v : d->variants) {
      if (++i == d->variants.size())
        indent.back()--;
      print_decl_tree(v.get(), indent);
    }
  } else if (auto *d = decl->dyn_cast<StructDecl>()) {
    u32 i = 0;
    for (auto &v : d->fields) {
      if (++i == d->fields.size())
        indent.back()--;
      print_decl_tree(v.get(), indent);
    }
  } else if (auto *d = decl->dyn_cast<FunctionDecl>()) {
    u32 i = 0;
    for (auto &v : d->params) {
      if (++i == d->params.size() + u32(d->body != nullptr))
        indent.back()--;
      print_decl_tree(v.get(), indent);
    }
    if (d->body) {
      indent.back()--;
      print_stmt_tree(d->body.get(), indent);
    }
  } else if (auto *d = decl->dyn_cast<VarDecl>()) {
    indent.back()--;
    if (d->initializer)
      print_expr_tree(d->initializer.get(), indent);
  } else if (auto *d = decl->dyn_cast<EnumVariantDecl>()) {
    indent.back()--;
    if (d->value_expr)
      print_expr_tree(d->value_expr.get(), indent);
  }

  indent.pop_back();
}

void print_ast(TranslationUnitDecl const *ast) {
  std::vector<int> indent;
  print_decl_tree(ast, indent);
}
