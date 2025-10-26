#include "stmt.hpp"
#include "decl.hpp"
#include "expr.hpp"

DeclStmt::DeclStmt(DeclUPtr decl, Loc start_loc, Loc end_loc)
    : Stmt(DECL_STMT), decl(std::move(decl)), start_loc(start_loc),
      end_loc(end_loc) {}

IfStmt::IfStmt(ExprUPtr cond, StmtUPtr ts, Loc il, Loc lp, Loc rp)
    : Stmt(IF_STMT), cond(std::move(cond)), then_stmt(std::move(ts)),
      else_stmt(nullptr), if_loc(il), else_loc(LOC_INVALID), lparen_loc(lp),
      rparen_loc(rp) {}

IfStmt::IfStmt(ExprUPtr cond, StmtUPtr ts, StmtUPtr es, Loc il, Loc el, Loc lp,
               Loc rp)
    : Stmt(IF_STMT), cond(std::move(cond)), then_stmt(std::move(ts)),
      else_stmt(std::move(es)), if_loc(il), else_loc(el), lparen_loc(lp),
      rparen_loc(rp) {}

Loc IfStmt::get_end_loc() const {
  return else_stmt ? else_stmt->get_end_loc() : then_stmt->get_end_loc();
}

SwitchStmt::SwitchStmt(ExprUPtr cond, StmtUPtr body, Loc sl, Loc lp, Loc rp)
    : Stmt(SWITCH_STMT), cond(std::move(cond)), body(std::move(body)),
      switch_loc(sl), lparen_loc(lp), rparen_loc(rp) {}

WhileStmt::WhileStmt(ExprUPtr cond, StmtUPtr body, Loc wl, Loc lp, Loc rp)
    : Stmt(WHILE_STMT), cond(std::move(cond)), body(std::move(body)),
      while_loc(wl), lparen_loc(lp), rparen_loc(rp) {}

DoStmt::DoStmt(ExprUPtr cond, StmtUPtr body, Loc dl, Loc wl, Loc rp)
    : Stmt(DO_STMT), cond(std::move(cond)), body(std::move(body)), do_loc(dl),
      while_loc(wl), rparen_loc(rp) {}

ForStmt::ForStmt(StmtUPtr init_stmt, ExprUPtr cond, ExprUPtr latch, StmtUPtr body, Loc fl, Loc lp, Loc rp) 
    : Stmt(FOR_STMT), 
    init_stmt(std::move(init_stmt)), 
    cond(std::move(cond)), 
    latch(std::move(latch)), 
    body(std::move(body)),
    for_loc(fl), lparen_loc(lp), rparen_loc(rp) {}

ReturnStmt::ReturnStmt(ExprUPtr return_value, Loc rl)
    : Stmt(RETURN_STMT), return_value(std::move(return_value)), return_loc(rl) {
}

Loc ReturnStmt::get_end_loc() const {
  return return_value ? return_value->get_end_loc() : return_loc;
}
