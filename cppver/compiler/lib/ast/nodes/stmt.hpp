#ifndef AST_NODES_STMT_HPP_INCLUDED
#define AST_NODES_STMT_HPP_INCLUDED

#include "lexer/loc.hpp"
#include <defines.hpp>
#include <memory>
#include <vector>

struct Stmt {
  enum Kind {
    DECL_STMT,
    NULL_STMT,
    COMPOUND_STMT,
    CASE_STMT,
    DEFAULT_STMT,
    IF_STMT,
    SWITCH_STMT,
    WHILE_STMT,
    DO_STMT,
    FOR_STMT,
    CONTINUE_STMT,
    BREAK_STMT,
    RETURN_STMT,
    DECLREF_EXPR,
    INTEGER_LITERAL,
    BOOL_LITERAL,
    STRING_LITERAL,
    CHAR_LITERAL,
    NULLPTR_LITERAL,
    PAREN_EXPR,
    UNARY_EXPR,
    BINARY_EXPR,
    COMPOUND_ASSIGN_EXPR,
    CALL_EXPR,
    METHOD_CALL_EXPR,
    SIZEOF_EXPR,
    MEMBER_EXPR,
    METHOD_EXPR,
    ARRAY_SUBSCRIPT_EXPR,
    CONDITIONAL_EXPR,
    RECOVERY_EXPR,
    IMPLICIT_CAST_EXPR,
    EXPLICIT_CAST_EXPR,
    VAARG_EXPR,
    VAARGS_EXPR,
  };
  union {
    u32 kind : 6;
    struct {
      u32 _ : 6;
      u32 unused : 26;
    } unused;
  };

  Stmt(u32 kind) : kind(kind) {}
  virtual ~Stmt() {}

  static bool is_class(Kind) { return true; }

  virtual Loc get_start_loc() const { return LOC_INVALID; }
  virtual Loc get_end_loc() const { return LOC_INVALID; }
  LocRge get_range() const { return {get_start_loc(), get_end_loc()}; }

  template <typename T> const T *dyn_cast() const {
    return const_cast<Stmt *>(this)->dyn_cast<T>();
  }
  template <typename T> T *dyn_cast() {
    if (isa<T>())
      return reinterpret_cast<T *>(this);
    return nullptr;
  }
  template <typename T> bool isa() const { return T::is_class(Kind(kind)); }
};

struct Decl;
struct Expr;

using StmtUPtr = std::unique_ptr<Stmt>;
using ExprUPtr = std::unique_ptr<Expr>;
using DeclUPtr = std::unique_ptr<Decl>;

struct DeclStmt : public Stmt {
  DeclUPtr decl;
  Loc start_loc;
  Loc end_loc;

  DeclStmt(DeclUPtr decl, Loc start_loc, Loc end_loc);

  Loc get_start_loc() const override { return start_loc; }
  Loc get_end_loc() const override { return end_loc; }

  static bool is_class(Kind k) { return k == DECL_STMT; }
};

struct NullStmt : public Stmt {
  Loc semi_loc;

  NullStmt(Loc semi_loc) : Stmt(NULL_STMT), semi_loc(semi_loc) {}

  Loc get_start_loc() const override { return semi_loc; }
  Loc get_end_loc() const override { return semi_loc; }

  static bool is_class(Kind k) { return k == NULL_STMT; }
};

struct CompoundStmt : public Stmt {
  std::vector<StmtUPtr> inner;
  Loc lbrace_loc;
  Loc rbrace_loc;

  CompoundStmt(std::vector<StmtUPtr> &&inner, Loc lb, Loc rb)
      : Stmt(COMPOUND_STMT), inner(std::move(inner)), lbrace_loc(lb),
        rbrace_loc(rb) {}

  Loc get_start_loc() const override { return lbrace_loc; }
  Loc get_end_loc() const override { return rbrace_loc; }

  static bool is_class(Kind k) { return k == COMPOUND_STMT; }
};

struct SwitchCase : public Stmt {
  SwitchCase *next_switch_case = nullptr;
  Loc keyword_loc;
  Loc colon_loc;

  SwitchCase(Kind k, Loc kl, Loc cl)
      : Stmt(k), keyword_loc(kl), colon_loc(cl) {}

  Loc get_start_loc() const override { return keyword_loc; }

  static bool is_class(Kind k);
};

struct CaseStmt : public SwitchCase {
  StmtUPtr sub_stmt;
  i64 case_val;

  CaseStmt(StmtUPtr sub_stmt, Loc kl, Loc cl, i64 case_val)
      : SwitchCase(CASE_STMT, kl, cl), sub_stmt(std::move(sub_stmt)),
        case_val(case_val) {}

  Loc get_end_loc() const override;

  static bool is_class(Kind k) { return k == CASE_STMT; }
};

struct DefaultStmt : public SwitchCase {
  StmtUPtr sub_stmt;

  DefaultStmt(StmtUPtr sub_stmt, Loc kl, Loc cl)
      : SwitchCase(CASE_STMT, kl, cl), sub_stmt(std::move(sub_stmt)) {}

  Loc get_end_loc() const override { return sub_stmt->get_end_loc(); }

  static bool is_class(Kind k) { return k == DEFAULT_STMT; }
};

inline bool SwitchCase::is_class(Kind k) {
  return CaseStmt::is_class(k) || DefaultStmt::is_class(k);
}

struct IfStmt : public Stmt {
  ExprUPtr cond;
  StmtUPtr then_stmt;
  StmtUPtr else_stmt;
  Loc if_loc;
  Loc else_loc;
  Loc lparen_loc;
  Loc rparen_loc;

  IfStmt(ExprUPtr cond, StmtUPtr ts, Loc il, Loc lp, Loc rp);
  IfStmt(ExprUPtr cond, StmtUPtr ts, StmtUPtr es, Loc il, Loc el, Loc lp,
         Loc rp);

  Loc get_start_loc() const override { return if_loc; }
  Loc get_end_loc() const override;

  static bool is_class(Kind k) { return k == IF_STMT; }
};

struct SwitchStmt : public Stmt {
  ExprUPtr cond;
  StmtUPtr body;
  SwitchCase *first_case = nullptr;
  Loc switch_loc;
  Loc lparen_loc;
  Loc rparen_loc;

  SwitchStmt(ExprUPtr cond, StmtUPtr body, Loc sl, Loc lp, Loc rp);

  Loc get_start_loc() const override { return switch_loc; }
  Loc get_end_loc() const override { return body->get_end_loc(); }

  static bool is_class(Kind k) { return k == SWITCH_STMT; }
};

struct WhileStmt : public Stmt {
  ExprUPtr cond;
  StmtUPtr body;
  Loc while_loc;
  Loc lparen_loc;
  Loc rparen_loc;

  WhileStmt(ExprUPtr cond, StmtUPtr body, Loc wl, Loc lp, Loc rp);

  Loc get_start_loc() const override { return while_loc; }
  Loc get_end_loc() const override { return body->get_end_loc(); }

  static bool is_class(Kind k) { return k == WHILE_STMT; }
};

struct DoStmt : public Stmt {
  ExprUPtr cond;
  StmtUPtr body;
  Loc do_loc;
  Loc while_loc;
  Loc rparen_loc;

  DoStmt(ExprUPtr cond, StmtUPtr body, Loc dl, Loc wl, Loc rp);

  Loc get_start_loc() const override { return do_loc; }
  Loc get_end_loc() const override { return rparen_loc; }

  static bool is_class(Kind k) { return k == DO_STMT; }
};

struct ForStmt : public Stmt {
  // TODO:
  ForStmt() : Stmt(FOR_STMT) {}

  static bool is_class(Kind k) { return k == FOR_STMT; }
};

struct ContinueStmt : public Stmt {
  Loc continue_loc;

  ContinueStmt(Loc cl) : Stmt(CONTINUE_STMT), continue_loc(cl) {}

  Loc get_start_loc() const override { return continue_loc; }
  Loc get_end_loc() const override { return continue_loc; }

  static bool is_class(Kind k) { return k == CONTINUE_STMT; }
};

struct BreakStmt : public Stmt {
  Loc break_loc;

  BreakStmt(Loc cl) : Stmt(BREAK_STMT), break_loc(cl) {}

  Loc get_start_loc() const override { return break_loc; }
  Loc get_end_loc() const override { return break_loc; }

  static bool is_class(Kind k) { return k == BREAK_STMT; }
};

struct ReturnStmt : public Stmt {
  ExprUPtr return_value;
  Loc return_loc;

  ReturnStmt(ExprUPtr return_value, Loc rl);

  Loc get_start_loc() const override { return return_loc; }
  Loc get_end_loc() const override;

  static bool is_class(Kind k) { return k == RETURN_STMT; }
};

#endif // AST_NODES_STMT_HPP_INCLUDED
