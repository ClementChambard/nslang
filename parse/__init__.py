from semantic_analysis import Scope, ScopeFlags, TYPES, actions, eval_const_expr

from typing import Callable, Tuple

from ns_ast.nodes import *
from lex import Lexer, Tok, Token, LocPtr, LocRef
from utils.my_enum import Enum, ENUM_INIT, ENUM_N
from utils.diagnostic import diag, Diag
from .prec import Prec


class ParsedStmtContext(Enum):
    ALLOW_DECLARATION_IN_C = 1
    IN_STMT_EXPR = 4
    SUB_STMT = 0
    COMPOUND = 1
    def in_stmt_expr(self) -> bool:
        return (int(self) & 4) != 0


class DeclaratorContext(Enum):
    FILE = ENUM_INIT()                      # File scope declaration.
    PROTOTYPE = ENUM_N()                    # Within a function prototype.
    KNR_TYPE_LIST = ENUM_N()                # K&R type definition list for formals.
    TYPE_NAME = ENUM_N()                    # Abstract declarator for types.
    FUNCTIONAL_CAST = ENUM_N()              # Type in a C++ functional cast expression.
    MEMBER = ENUM_N()                       # Struct/Union field.
    BLOCK = ENUM_N()                        # Declaration within a block in a function.
    FOR_INIT = ENUM_N()                     # Declaration within first part of a for loop.
    SELECTION_INIT = ENUM_N()               # Declaration within optional init stmt of if/switch.
    CONDITION = ENUM_N()                    # Condition declaration in a C++ if/switch/while/for.
    TEMPLATE_PARAM = ENUM_N()               # Within a template parameter list.
    CXX_NEW = ENUM_N()                      # C++ new-expression.
    CXX_CATCH = ENUM_N()                    # C++ catch exception-declaration
    BLOCK_LITERAL = ENUM_N()                # Block literal declarator.
    LAMBDA_EXPR = ENUM_N()                  # Lambda-expression declarator.
    LAMBDA_EXPR_PARAMETER = ENUM_N()        # Lambda-expression parameter declarator.
    CONVERSION_ID = ENUM_N()                # C++ conversion-type-id.
    TRAILING_RETURN = ENUM_N()              # C++11 trailing-type-specifier.
    TRAILING_RETURN_VAR = ENUM_N()          # C++11 trailing-type-specifier for variable.
    ALIAS_DECL = ENUM_N()                   # C++11 alias-declaration.
    ALIAS_TEMPLATE = ENUM_N()               # C++11 alias-declaration template.
    REQUIRES_EXPR = ENUM_N()                # C++2a requires-expression.
    ASSOCIATION = ENUM_N()                  # C11 _Generic selection expression association.




class CastParseKind(Enum):
    ANY = ENUM_INIT()
    UNARY = ENUM_N()
    PRIMARY = ENUM_N()


class Parser:
    lexer: Lexer
    tok: Token
    prev_tok_location: Loc
    saved_type: None
    greater_than_is_operator: bool
    paren_count: int
    brace_count: int
    bracket_count: int
    cur_scope: Scope

    def __init__(self, lexer: Lexer):
        self.lexer = lexer
        self.tok = lexer.lex()
        self.prev_tok_location = 0
        self.saved_type = None
        self.greater_than_is_operator = True
        self.paren_count = 0
        self.brace_count = 0
        self.bracket_count = 0
        self.cur_scope = Scope(None, 0)

    def is_fold_operator_l(self, level: Prec) -> bool:
        return level.value > Prec.UNKNOWN.value and level != Prec.COND

    def is_fold_operator(self, kind: Tok) -> bool:
        return self.is_fold_operator_l(Prec.from_bin_op(kind, self.greater_than_is_operator))

    def unconsume_token(self, consumed: Token):
        next_tok = self.tok
        self.lexer.enter_token(consumed, True)
        self.tok = self.lexer.lex()
        self.lexer.enter_token(next_tok, True)

    def consume_token(self) -> Loc:
        assert (
            not self.is_token_special()
        ), "Should consume special tokens with consume_*_token"
        self.prev_tok_location = self.tok.loc
        self.tok = self.lexer.lex()
        return self.prev_tok_location

    def try_consume_token(self, expected: Tok, loc_ptr: LocPtr = None) -> bool:
        if self.tok.ty != expected:
            return False
        loc = self.consume_token()
        if loc_ptr is not None:
            loc_ptr.value = loc
        return True

    def consume_any_token(self) -> Loc:
        if self.is_token_paren():
            return self.consume_paren()
        if self.is_token_brace():
            return self.consume_brace()
        if self.is_token_bracket():
            return self.consume_bracket()
        return self.consume_token()

    def is_token_paren(self) -> bool:
        return self.tok.ty == Tok.LPAREN or self.tok.ty == Tok.RPAREN

    def is_token_brace(self) -> bool:
        return self.tok.ty == Tok.LBRACE or self.tok.ty == Tok.RBRACE

    def is_token_bracket(self) -> bool:
        return self.tok.ty == Tok.LSQUARE or self.tok.ty == Tok.RSQUARE

    def is_token_special(self) -> bool:
        return self.is_token_paren() or self.is_token_brace() or self.is_token_bracket()

    def consume_paren(self) -> Loc:
        assert self.is_token_paren(), "Wrong consume method"
        if self.tok.ty == Tok.LPAREN:
            self.paren_count += 1
        elif self.paren_count > 0:
            # self.angle_brackets.clear()
            self.paren_count -= 1
        self.prev_tok_location = self.tok.loc
        self.tok = self.lexer.lex()
        return self.prev_tok_location

    def consume_brace(self) -> Loc:
        assert self.is_token_brace(), "Wrong consume method"
        if self.tok.ty == Tok.LBRACE:
            self.brace_count += 1
        elif self.brace_count > 0:
            # self.angle_brackets.clear()
            self.brace_count -= 1
        self.prev_tok_location = self.tok.loc
        self.tok = self.lexer.lex()
        return self.prev_tok_location

    def consume_bracket(self) -> Loc:
        assert self.is_token_bracket(), "Wrong consume method"
        if self.tok.ty == Tok.LSQUARE:
            self.bracket_count += 1
        elif self.bracket_count > 0:
            # self.angle_brackets.clear()
            self.bracket_count -= 1
        self.prev_tok_location = self.tok.loc
        self.tok = self.lexer.lex()
        return self.prev_tok_location

    @staticmethod
    def is_common_typo(expected: Tok, actual: Tok) -> bool:
        match expected:
            case Tok.SEMI:
                return actual == Tok.COLON or actual == Tok.COMMA
            case _:
                return False

    def expect_and_consume(self, expected: Tok, diag_id: str = "expected {}", msg: str = "") -> bool:
        if self.tok.ty == expected:
            self.consume_any_token()
            return False
        if Parser.is_common_typo(expected, self.tok.ty):
            loc = self.tok.loc
            # fixit with replacement ...
            diag(loc, diag_id + " " + msg, Diag.ERROR)
            self.consume_any_token()
            return False
        # fixit ...
        diag_msg = ""
        if diag_id == "expected {}":
            diag_msg = diag_id.format(expected)
        elif diag_id == "expected {} after {}":
            diag_msg = diag_id.format(expected, msg)
        else:
            diag_msg = diag_id.format(msg)
        loc = self.tok.loc # should get loc of end of previous token
        diag(loc, diag_msg, Diag.ERROR)
        return True

    def expect_and_consume_semi(self, diag_id, token_used = "") -> bool:
        if self.try_consume_token(Tok.SEMI):
            return False
        if self.tok.ty in [Tok.RPAREN, Tok.RSQUARE] and self.next_token().ty == Tok.SEMI:
            diag(self.tok.loc, f"extraneous '{self.tok.ty}' before ';'", Diag.ERROR) #fixit
            self.consume_any_token()
            self.consume_token()
            return False
        return self.expect_and_consume(Tok.SEMI, diag_id, token_used)

    def skip_until(self, *args, **kwargs):
        toks = list(args)
        stop_before_match = ("stop_before_match" in kwargs.keys()) and kwargs[
            "stop_before_match"
        ]
        stop_at_semi = ("stop_at_semi" in kwargs.keys()) and kwargs["stop_at_semi"]
        is_first_token_skipped = True
        while True:
            for i in range(len(toks)):
                if self.tok.ty == toks[i]:
                    if not stop_before_match:
                        self.consume_any_token()
                    return True
            if len(toks) == 1 and toks[0] == Tok.EOF and not stop_at_semi:
                while self.tok.ty != Tok.EOF:
                    self.consume_any_token()
                return True
            match self.tok.ty:
                case Tok.EOF:
                    return False
                case Tok.LPAREN:
                    self.consume_paren()
                    self.skip_until(Tok.RPAREN)
                case Tok.LSQUARE:
                    self.consume_bracket()
                    self.skip_until(Tok.RSQUARE)
                case Tok.LBRACE:
                    self.consume_brace()
                    self.skip_until(Tok.RBRACE)
                case Tok.QUESTION:
                    self.consume_token()
                    self.skip_until(
                        Tok.COLON,
                        stop_at_semi=True,
                        stop_before_match=stop_before_match,
                    )
                case Tok.RPAREN:
                    if self.paren_count > 0 and not is_first_token_skipped:
                        return False
                    self.consume_paren()
                case Tok.RSQUARE:
                    if self.bracket_count > 0 and not is_first_token_skipped:
                        return False
                    self.consume_bracket()
                case Tok.RBRACE:
                    if self.brace_count > 0 and not is_first_token_skipped:
                        return False
                    self.consume_brace()
                case Tok.SEMI:
                    if stop_at_semi:
                        return False
                    self.consume_token()
                case _:
                    self.consume_any_token()
            is_first_token_skipped = False

    def get_cur_scope(self) -> None:
        return self.cur_scope

    def enter_scope(self, scope_flags: ScopeFlags):
        self.cur_scope = Scope(self.get_cur_scope(), scope_flags)

    def exit_scope(self):
        assert self.get_cur_scope() is not None
        # actions.act_on_pop_scope(self.tok.loc, self.get_cur_scope())
        old_scope = self.get_cur_scope()
        self.cur_scope = old_scope.parent_scope

    def check_potential_angle_bracket_delimiter(self, s: Tok) -> bool:
        return False

    def is_not_expression_start(self) -> bool:
        return True

    def parse_rhs_of_binary_expr(self, lhs: Expr | None, prec: Prec) -> Expr | None:
        next_tok_prec = Prec.from_bin_op(self.tok.ty, self.greater_than_is_operator)
        colon_loc = 0
        while True:
            if next_tok_prec.value < prec.value:
                return lhs
            op_token = self.tok
            self.consume_token()
            if (op_token.ty == Tok.COMMA or op_token.ty == Tok.GREATER or op_token.ty == Tok.GREATERGREATER) and self.check_potential_angle_bracket_delimiter(op_token):
                return None
            if op_token.ty == Tok.COMMA and self.is_not_expression_start():
                self.lexer.enter_token(self.tok, True)
                self.tok = op_token
                return lhs
            if self.is_fold_operator_l(next_tok_prec) and self.tok.ty == Tok.ELLIPSIS:
                self.lexer.enter_token(self.tok, True)
                self.tok = op_token
                return lhs
            ternary_middle = None
            if next_tok_prec == Prec.COND:
                if self.tok.ty == Tok.LBRACE:
                    # brace_loc = self.tok.loc
                    ternary_middle = self.parse_brace_initializer()
                    if ternary_middle is not None:
                        diag(op_token.loc,"initializer list cannot be used on the left hand side of operator ':'", Diag.ERROR) # actions.get_expr_range(ternary_middle)
                        ternary_middle = None
                elif self.tok.ty != Tok.COLON:
                    # x = ColonProtectionRAIIObject(self)
                    ternary_middle = self.parse_expr()
                else:
                    ternary_middle = None
                    diag(op_token.loc, "ext_gnu_conditional_expr", Diag.WARNING)
                if ternary_middle is None:
                    actions.correct_delayed_typos_in_expr(lhs)
                    lhs = None
                    ternary_middle = None
                colon_loc = LocRef(colon_loc)
                if not self.try_consume_token(Tok.COLON, colon_loc):
                    diag(op_token.loc, "expected ':'", Diag.ERROR)
                    diag(op_token.loc, "to match this '?'", Diag.NOTE)
                colon_loc = colon_loc.value
            rhs: Expr | None = None
            rhs_is_init_list = False
            if self.tok.ty == Tok.LBRACE:
                self.parse_brace_initializer()
                rhs_is_init_list = True
            elif next_tok_prec.value <= Prec.COND.value:
                rhs = self.parse_assignment_expr()
            else:
                rhs = self.parse_cast_expr(CastParseKind.ANY)
            if rhs is None:
                actions.correct_delayed_typos_in_expr(lhs)
                if ternary_middle is not None:
                    ternary_middle = actions.correct_delayed_typos_in_expr(ternary_middle)
                lhs = None
            this_prec = next_tok_prec
            next_tok_prec = Prec.from_bin_op(self.tok.ty, self.greater_than_is_operator)
            is_right_assoc = this_prec == Prec.COND or this_prec == Prec.ASSIGN
            if this_prec.value < next_tok_prec.value or (this_prec == next_tok_prec and is_right_assoc):
                if rhs is not None and rhs_is_init_list:
                    diag(self.tok.loc, f"initializer list cannot be used on the left hand side of operator '{op_token.ty}'", Diag.ERROR) # actions.get_expr_range(rhs)
                    rhs = None
                rhs = self.parse_rhs_of_binary_expr(rhs, Prec(this_prec.value + int(not is_right_assoc)))
                rhs_is_init_list = False
                if rhs is None:
                    actions.correct_delayed_typos_in_expr(lhs)
                    if ternary_middle is not None:
                        ternary_middle = actions.correct_delayed_typos_in_expr(ternary_middle)
                    lhs = None
                next_tok_prec = Prec.from_bin_op(self.tok.ty, self.greater_than_is_operator)
            if rhs is not None and rhs_is_init_list:
                if this_prec == Prec.ASSIGN:
                    pass
                elif colon_loc != 0:
                    diag(colon_loc, f"initializer list cannot be used on the right hand side of operator ':'", Diag.ERROR) # actions.get_expr_range(rhs)
                    lhs = None
                else:
                    diag(self.tok.loc, f"initializer list cannot be used on the right hand side of operator '{op_token.ty}'", Diag.ERROR)  # get_spelling # actions.get_expr_range(rhs)
                    lhs = None
            orig_lhs = lhs
            if lhs is not None:
                if ternary_middle is None:
                    if (not self.greater_than_is_operator and op_token.ty == Tok.GREATERGREATER):
                        self.suggest_parentheses(op_token.loc, "warn_cxx11_right_shift_in_template_arg", (actions.get_expr_range(lhs)[0], actions.get_expr_range(rhs)[1]))
                    bin_op = actions.act_on_bin_op(self.get_cur_scope(), op_token.loc, op_token.ty, lhs, rhs)
                    if bin_op is None:
                        bin_op = actions.create_recovery_expr(lhs.get_range()[0], rhs.get_range()[1], [lhs, rhs])
                    lhs = bin_op
                else:
                    cond_op = actions.act_on_conditional_op(op_token.loc, colon_loc, lhs, ternary_middle, rhs)
                    if cond_op is None:
                        args = []
                        if ternary_middle is not None:
                            args = [lhs, ternary_middle, rhs]
                        else:
                            args = [lhs, rhs]
                        cond_op = actions.create_recovery_expr(lhs.get_range()[0], rhs.get_range()[1], args)
                    lhs = cond_op
            if lhs is None:
                actions.correct_delayed_typos_in_expr(orig_lhs)
                actions.correct_delayed_typos_in_expr(ternary_middle)
                actions.correct_delayed_typos_in_expr(rhs)

    def parse_expression_list(self, exprs: [Expr], expression_starts: Callable[None, []] | None = None, fail_immediately_on_invalid_expr: bool = False, early_typo_correction: bool = False) -> bool:
        saw_error = False
        while True:
            if expression_starts is not None:
                expression_starts()

            expr = None
            if self.tok.ty == Tok.LBRACE:
                # diag::warn_cxx98_compat_generalized_initializer_lists
                expr = self.parse_brace_initializer()
            else:
                expr = self.parse_assignment_expr()

            if early_typo_correction:
                expr = actions.correct_delayed_typos_in_expr(expr)

            if self.tok.ty == Tok.ELLIPSIS:
                expr = actions.act_on_pack_expansion(expr, self.consume_token())

            if expr is None:
                saw_error = True
                if fail_immediately_on_invalid_expr:
                    break
                self.skip_until(Tok.COMMA, Tok.RPAREN, stop_before_match = True)
            else:
                exprs.append(expr)

            if self.tok.ty != Tok.COMMA:
                break

            comma = self.tok
            self.consume_token()
            self.check_potential_angle_bracket_delimiter(comma)
        if saw_error:
            for e in exprs:
                expr = actions.correct_delayed_typos_in_expr(e)
                if expr is not None:
                    e = expr
        return saw_error


    def parse_postfix_expression_suffix(self, lhs: Expr | None) -> Expr | None:
        loc = 0
        while True:
            match self.tok.ty:
                case Tok.PLUSPLUS | Tok.MINUSMINUS:
                    if lhs is not None:
                        arg = lhs
                        lhs = actions.act_on_postfix_unary_op(self.get_cur_scope(), self.tok.loc, self.tok.ty, arg)
                        if lhs is None:
                            lhs = actions.create_recovery_expr(arg.get_begin_loc(), self.tok.loc, arg)
                    self.consume_token()
                case Tok.LSQUARE:
                    loc = self.consume_bracket()
                    arg_exprs = []
                    has_error = False

                    if self.parse_expression_list(arg_exprs):
                        lhs = actions.correct_delayed_typos_in_expr(lhs)
                        has_error = True

                    rloc = self.tok.loc
                    lhs = actions.correct_delayed_typos_in_expr(lhs)

                    if lhs is not None and not has_error and self.tok.ty == Tok.RSQUARE:
                        lhs = actions.act_on_array_subscript_expr(self.get_cur_scope(), lhs, loc, arg_exprs, rloc);
                    else:
                        lhs = None
                    self.consume_bracket()
                case Tok.LPAREN:
                    loc = self.consume_paren()
                    arg_exprs = []

                    if self.tok.ty != Tok.RPAREN:
                        if self.parse_expression_list(arg_exprs):
                            actions.correct_delayed_typos_in_expr(lhs)
                            lhs = None
                        elif lhs is None:
                            for e in arg_exprs:
                                actions.correct_delayed_typos_in_expr(e)

                    if lhs is None:
                        self.skip_until(Tok.RPAREN, stop_at_semi = True)
                    elif self.tok.ty != Tok.RPAREN:
                        had_delayed_typo = False
                        if actions.correct_delayed_typos_in_expr(lhs) != lhs:
                            had_delayed_typo = True
                        for e in arg_exprs:
                            if actions.correct_delayed_typos_in_expr(e) != e:
                                had_delayed_typo = True
                        if had_delayed_typo:
                            self.skip_until(Tok.RPAREN, stop_at_semi = True)
                        else:
                            self.consume_paren()
                        lhs = None
                    else:
                        fn = lhs
                        rpar_loc = self.tok.loc
                        lhs = actions.act_on_call_expr(self.get_cur_scope(), fn, loc, arg_exprs, rpar_loc)
                        if lhs is None:
                            arg_exprs = [fn] + arg_exprs
                            lhs = actions.create_recovery_expr(fn.get_range()[0], rpar_loc, arg_exprs)
                        self.consume_paren()
                case Tok.ARROW | Tok.PERIOD:
                    opkind = self.tok.ty
                    oploc = self.consume_token()

                    ss = None # CXXScopeSpec
                    object_type = None # ParsedType

                    orig_lhs = lhs

                    #if lhs is not None:
                    #    base = orig_lhs
                    #    base_type = base.ty # .get_type_ptr_or_null()
                    #
                    #    lhs = actions.act_on_start_cxx_member_reference(self.get_cur_scope(), base, oploc, opkind, None, False);
                    #
                    #    if lhs is None:
                    #        break
                    #    self.parse_optional_cxx_scope_specifier(ss, object_type, lhs and lhs.contains_errors(), False, &False)
                    #    if ss.is_not_empty() object_type = None

                    name = UnqualifiedId()

                    if self.parse_unqualified_id(name):
                        actions.correct_delayed_typos_in_expr(lhs)
                        lhs = None

                    if lhs is not None:
                        lhs = actions.act_on_member_access_expr(self.get_cur_scope(), lhs, oploc, opkind, ss, name)
                    if lhs is None and orig_lhs is not None and name is not None:
                        lhs = actions.create_recovery_expr(orig_lhs.get_range()[0], name.get_range()[1], [orig_lhs])
                case _:
                    return lhs

        return lhs

    def parse_unqualified_id(self, out: UnqualifiedId) -> bool:
        if self.tok.ty != Tok.IDENT:
            # kw_operator
            # tilde (for destructor name)
            # transform_type_trait => parse ident
            diag(self.tok.loc, "Expected unqualified identifier", Diag.ERROR)
            return True
        idinf = self.tok.value
        idloc = self.consume_token()
        out.set_identifier(idinf, idloc)
        return False

    def parse_builtin_expression(self) -> Expr | None:
        assert self.tok.ty.is_builtin(), "not a builtin expression"
        builtin_tok = self.tok
        self.consume_token()
        if self.tok.ty != Tok.LPAREN:
            return None
        self.consume_paren()
        if self.tok.ty == Tok.RPAREN:
            rparen_loc = self.consume_paren()
            return actions.act_on_builtin_expr(builtin_tok, rparen_loc, [])
        args = []
        while True:
            arg = self.parse_assignment_expr()
            assert (arg is not None), "TODO: handle errors: invalid expr in builtin arg list"
            args.append(arg)
            if self.tok.ty != Tok.COMMA:
                break
            self.consume_token()
        assert self.tok.ty == Tok.RPAREN, "TODO: handle errors: missing ')'"
        rparen_loc = self.consume_paren()
        return actions.act_on_builtin_expr(builtin_tok, rparen_loc, args)

    def following_is_type(self, ctx: int) -> bool:
        # TODO:
        return False

    def parse_paren_expression(self, expr_type, stop_if_cast_expr, is_type_cast, cast_ty, rparen_loc) -> Tuple[Expr | None, int, Type, Loc]:
        assert self.tok.ty == Tok.LPAREN, "Not a paren expr!"
        # ColonProtectionRAIIObject ColonProtection(*this, false);
        open_loc = self.consume_paren()
        result = None
        cast_ty = None
        if expr_type >= 3 and self.following_is_type(0): # TypeInParens
            ty = self.parse_type()
            assert self.tok.ty == Tok.RPAREN, "Expected RPAREN after type"
            rparen_loc = self.consume_paren()
            # ColonProtection.restore();
            if self.tok.ty == Tok.LBRACE:
                expr_type = 3;
                return self.parse_compound_literal_expression(ty, open_loc, rparen_loc), expr_type, cast_ty, rparen_loc

            if expr_type == 4:
                # if type is invalid: return ExprError
                if stop_if_cast_expr:
                    cast_ty = ty
                    return None, expr_type, cast_ty, rparen_loc
                result = self.parse_cast_expr(CastParseKind.ANY, False, is_type_cast)
                if result is not None:
                    result, cast_ty = actions.act_on_cast_expr(self.get_cur_scope(), open_loc, ty, cast_ty, rparen_loc, result)
                return result, expr_type, cast_ty, rparen_loc
            diag(self.tok.loc, "expected '{' in compound literal", Diag.ERROR)
            return None, expr_type, cast_ty, rparen_loc
        elif expr_type >= 1 and self.tok.ty == Tok.ELLIPSIS and self.is_fold_operator(next_token().ty):
            expr_type = 1
            return self.parse_fold_expression(None, T), expr_type, cast_ty, rparen_loc
        elif is_type_cast:
            arg_exprs = []
            if not self.parse_simple_expression_list(arg_exprs):
                if expr_type >= 1 and len(arg_exprs) == 1 and self.is_fold_operator(self.tok.ty) and next_token().ty == Tok.ELLIPSIS:
                    expr_type = 1;
                    return self.parse_fold_expression(arg_exprs[0], T), expr_type, cast_ty, rparen_loc
                expr_type = 0
                result = actions.act_on_paren_list_expr(open_loc, tok.loc, arg_exprs);
        else:
            result = self.parse_expr(); # MaybeTypeCast
            if expr_type >= 1 and self.is_fold_operator(self.tok.ty) and next_token().ty == Tok.ELLIPSIS:
                expr_type = 1
                return self.parse_fold_expression(result, T), expr_type, cast_ty, rparen_loc
            expr_type = 0
            if result is not None and self.tok.ty == Tok.RPAREN:
                result = actions.act_on_paren_expr(open_loc, self.tok.loc, result);
        if result is None:
            self.skip_until(Tok.RPAREN, stop_at_semi=True)
            return None, expr_type, cast_ty, rparen_loc
        assert self.tok.ty == Tok.RPAREN, "expected RPAREN"
        rparen_loc = self.consume_paren()
        return result, expr_type, cast_ty, rparen_loc


    def parse_bool_literal(self) -> Expr:
        kind = self.tok.ty
        return actions.act_on_bool_literal(self.consume_token(), kind)

    # enum ParenParseOption {
    #   SimpleExpr,      // Only parse '(' expression ')'
    #   FoldExpr,        // Also allow fold-expression <anything>
    #   CompoundStmt,    // Also allow '(' compound-statement ')'
    #   CompoundLiteral, // Also allow '(' type-name ')' '{' ... '}'
    #   CastExpr         // Also allow '(' type-name ')' <anything>
    # };

    def parse_string_literal_expression(self, unevaluated: bool = False) -> Expr:
        assert self.tok.ty == Tok.STR, "Not a string-literal-like token!"
        string_toks = []
        while self.tok.ty == Tok.STR:
            string_toks.append(self.tok)
            self.consume_any_token()
        if unevaluated:
            return actions.act_on_unevaluated_string_literal(string_toks)
        return actions.act_on_string_literal(string_toks)

    def is_start_of_type(self):
        if self.tok.ty.is_builtin_type():
            return True
        if self.tok.ty == Tok.KW_VOID:
            return True
        if self.tok.ty == Tok.IDENT:
            type_name = self.tok.value.val
            lookup_decl = self.cur_scope.lookup_named_decl(type_name)
            return lookup_decl is not None and isinstance(lookup_decl, TypeDecl)
        return False

    def parse_cast_expr_inner(self, parse_kind: CastParseKind, is_address_of_operand, is_type_cast: bool) -> Tuple[Expr | None, bool]:
        res = None
        saved_kind = self.tok.ty
        not_cast_expr = False
        allow_suffix = True

        if saved_kind == Tok.LPAREN:
            paren_expr_type = 1
            if parse_kind == CastParseKind.UNARY or parse_kind == CastParseKind.ANY:
                paren_expr_type = 4
            cast_ty = None
            rparen_loc = 0
            res, paren_expr_type, cast_ty, rparen_loc = self.parse_paren_expression(paren_expr_type, False, is_type_cast, cast_ty, rparen_loc)
            if paren_expr_type == 4:
                return res, not_cast_expr
        elif saved_kind == Tok.NUM:  # CPP or binary_data
            res = actions.act_on_numeric_constant(self.tok, self.get_cur_scope())
            self.consume_token()
        elif saved_kind == Tok.KW_TRUE or saved_kind == Tok.KW_FALSE:
            res = self.parse_bool_literal()
        elif saved_kind == Tok.KW_NULLPTR:
            res = actions.act_on_nullptr_literal(self.consume_token())
        # CPP decltype
        elif saved_kind == Tok.IDENT:
            ii = self.tok.value
            iloc = self.consume_token()

            if is_address_of_operand and self.tok.ty in [Tok.LSQUARE, Tok.LPAREN, Tok.PERIOD, Tok.ARROW, Tok.PLUSPLUS, Tok.MINUSMINUS]:
                is_address_of_operand = False

            name = UnqualifiedId()
            ss = None
            replacement = Token()

            name.set_identifier(ii, iloc);

            res = actions.act_on_id_expression(self.get_cur_scope(), ss, name, self.tok.ty == Tok.LPAREN, is_address_of_operand
                                               #, &Validator
                                               , False, replacement if self.tok.ty == Tok.RPAREN else None)
            if res is None:
                self.unconsume_token(replacement)
                return self.parse_cast_expr(parse_kind, is_address_of_operand, is_type_cast), not_cast_expr
            # pack indexing
        elif saved_kind == Tok.CHR:
            res = actions.act_on_character_constant(self.tok, self.get_cur_scope())
            self.consume_token()
        elif saved_kind == Tok.STR:
            res = self.parse_string_literal_expression()
        elif saved_kind.is_builtin():
            return self.parse_builtin_expression(), not_cast_expr
        elif saved_kind == Tok.PLUSPLUS or saved_kind == Tok.MINUSMINUS:
            saved_tok2 = self.tok
            self.consume_token()
            res, not_cast_expr = self.parse_cast_expr_inner(CastParseKind.UNARY, False, False)
            if not_cast_expr:
                assert res is None
                self.unconsume_token(saved_tok2)
                return None, not_cast_expr
            if res is not None:
                arg = res
                res = actions.act_on_unary_op(self.get_cur_scope(), saved_tok2.loc, saved_kind, arg)
                if res is None:
                    res = actions.create_recovery_expr(saved_tok2.loc, arg.src_range[1], arg)
            return res, not_cast_expr
        elif saved_kind == Tok.AMP:
            saved_loc = self.consume_token()
            res = self.parse_cast_expr(CastParseKind.ANY, True)
            if res is not None:
                arg = res
                res = actions.act_on_unary_op(self.get_cur_scope(), saved_loc, saved_kind, arg)
                if res is None:
                    res = actions.create_recovery_expr(self.tok.loc, arg.src_range[1], arg)
            return res, not_cast_expr
        elif saved_kind in [Tok.STAR, Tok.PLUS, Tok.MINUS, Tok.TILDE, Tok.EXCLAIM]:
            saved_loc = self.consume_token()
            res = self.parse_cast_expr(CastParseKind.ANY)
            if res is not None:
                arg = res
                res = actions.act_on_unary_op(self.get_cur_scope(), saved_loc, saved_kind, arg, is_address_of_operand)
                if res is None:
                    res = actions.create_recovery_expr(self.tok.loc, arg.src_range[1], arg)
            return res, not_cast_expr
        elif saved_kind == Tok.KW_CAST:
            start_loc = self.consume_token()
            assert self.tok.ty == Tok.LESS
            self.consume_token()
            ty = self.parse_type()
            assert self.tok.ty == Tok.GREATER
            self.consume_token()
            assert self.tok.ty == Tok.LPAREN
            self.consume_paren()
            expr = self.parse_expr()
            assert self.tok.ty == Tok.RPAREN
            end_loc = self.consume_paren()
            res = actions.act_on_explicit_cast(ty, expr, start_loc, end_loc)
            return res, not_cast_expr
        elif saved_kind == Tok.KW_SIZEOF:
            start_loc = self.consume_token()
            assert self.tok.ty == Tok.LPAREN
            self.consume_paren()
            ty, expr = None, None
            if self.is_start_of_type():
                ty = self.parse_type()
            else:
                expr = self.parse_expr()
                ty = expr.ty
            assert ty is not None
            assert self.tok.ty == Tok.RPAREN
            end_loc = self.consume_paren()
            res = SizeofExpr(ty, expr, start_loc, end_loc)
            return res, not_cast_expr
        # CPP co_await, __extension__, alignof, ...
        # CPP casts
        # CPP typeid
        # CPP this
        # CPP type-construct-expr
        # CPP kw_operator -> idexpr
        # CPP new/delete expr
        # ...

        if parse_kind == CastParseKind.PRIMARY:
            return res, not_cast_expr

        if not allow_suffix:
            if res is None:
                return None, not_cast_expr
            if (
                self.tok.ty == Tok.LSQUARE
                or self.tok.ty == Tok.LPAREN
                or self.tok.ty == Tok.PLUSPLUS
                or self.tok.ty == Tok.MINUSMINUS
            ):
                if self.tok.is_at_start_of_line():
                    return res, not_cast_expr
            elif self.tok.ty != Tok.PERIOD and self.tok.ty != Tok.ARROW:
                return res, not_cast_expr
            diag(self.tok.loc, "expression cannot be followed by a postfix {self.tok.ty} operator; add parentheses", Diag.ERROR)

        return self.parse_postfix_expression_suffix(res), not_cast_expr

    def parse_cast_expr(self, parse_kind: CastParseKind, is_address_of_operand: bool = False, is_type_cast: bool = False) -> Expr | None:
        res, not_cast_expr = self.parse_cast_expr_inner(parse_kind, is_address_of_operand, is_type_cast)
        if not_cast_expr:
            diag(self.tok.loc, "expected expression", Diag.ERROR)
        return res

    def parse_assignment_expr(self, is_type_cast: bool = False) -> Expr | None:
        # CPP: 'throw' -> return self.parse_throw_expression()
        # CPP: 'co_yield' -> return self.parse_coyield_expression()

        lhs = self.parse_cast_expr(CastParseKind.ANY, False, is_type_cast)
        return self.parse_rhs_of_binary_expr(lhs, Prec.ASSIGN)

    def parse_expr(self, is_type_cast: bool = False) -> Expr | None:
        lhs = self.parse_assignment_expr(is_type_cast)
        return self.parse_rhs_of_binary_expr(lhs, Prec.COMMA)

    def parse_type(self) -> Type | None:
        cur_type = None
        while True:
            if cur_type is None:
                if self.tok.ty.is_builtin_type():
                    cur_type = BuiltinType.get_from_tok(self.tok)
                    self.consume_token()
                    continue
                if self.tok.ty == Tok.KW_VOID:
                    cur_type = Type()
                    self.consume_token()
                    continue
                if self.tok.ty == Tok.IDENT:
                    type_name = self.tok.value.val
                    type_loc = self.consume_token()
                    lookup_decl = self.cur_scope.lookup_named_decl(type_name)
                    if lookup_decl is None or not isinstance(lookup_decl, TypeDecl):
                        diag(type_loc, f"'{type_name}' is not a type name", Diag.ERROR)
                        return None
                    cur_type = lookup_decl.ty
                    if isinstance(cur_type, AliasType) or isinstance(cur_type, EnumType) and cur_type.aliased_type is not None:
                        cur_type = cur_type.get_aliased_type()
                    continue
                return cur_type
            if self.tok.ty == Tok.STAR:
                cur_type = PointerType(cur_type)
                self.consume_token()
                continue
            if self.tok.ty == Tok.LSQUARE:
                self.consume_bracket()
                count, _ = self.parse_integer_constexpr()
                self.expect_and_consume(Tok.RSQUARE)
                cur_type = ArrayType(cur_type, count)
                continue
            break
        # ActOnTypeName -> TODO:
        return cur_type

    def parse_if_stmt(self, trailing_else_loc: LocPtr):
        assert self.tok.ty == Tok.KW_IF, "Not an if stmt!"
        if_loc = self.consume_token()

        if self.tok.ty != Tok.LPAREN:
            diag(self.tok.loc, "expected '(' after 'if'", Diag.ERROR)
            self.skip_until(Tok.SEMI)
            return None

        self.enter_scope(ScopeFlags.DECL | ScopeFlags.CONTROL)

        lparen_loc = self.consume_paren()
        start = self.tok.loc
        cond = self.parse_expr()

        if cond is None and self.tok.ty != Tok.RPAREN:
            self.skip_until(Tok.SEMI);
            if self.tok.ty != Tok.RPAREN:
                self.exit_scope()
                return None

        if cond is None:
            cond_expr = actions.create_recovery_expr(start, start if self.tok.loc == start else self.prev_tok_location, [])
            cond = actions.act_on_condition(self.get_cur_scope(), loc, cond_expr)

        if self.tok.ty != Tok.RPAREN:
            assert False, "Expected RPAREN"

        rparen_loc = self.consume_paren()

        while self.tok.ty == Tok.RPAREN:
            diag(self.tok.loc, "extraneous ')' after condition, expected a statement", Diag.ERROR) # FixItHint::CreateRemoval(Tok.getLocation());
            self.consume_paren();

        is_braced = self.tok.ty == Tok.LBRACE
        if is_braced:
            self.enter_scope(ScopeFlags.DECL)

        then_stmt_loc = self.tok.loc
        inner_statement_trailing_else_loc = LocRef(0)
        then_stmt = self.parse_stmt(ParsedStmtContext.SUB_STMT, inner_statement_trailing_else_loc);

        if is_braced:
            self.exit_scope()

        else_loc = 0
        else_stmt_loc = 0
        else_stmt = None

        if self.tok.ty == Tok.KW_ELSE:
            if trailing_else_loc is not None:
                trailing_else_loc.value = self.tok.loc

            else_loc = self.consume_token()

            is_braced = self.tok.ty == Tok.LBRACE
            if is_braced:
                self.enter_scope(ScopeFlags.DECL)

            else_stmt_loc = self.tok.loc
            else_stmt = self.parse_stmt(ParsedStmtContext.SUB_STMT, None);

            if is_braced:
                self.exit_scope()
        elif inner_statement_trailing_else_loc.value != 0:
            diag(inner_statement_trailing_else_loc.value, "add explicit braces to avoid dangling else", Diag.WARNING)

        self.exit_scope()

        if then_stmt is None and else_stmt is None:
            return None

        if then_stmt is None:
            then_stmt = actions.act_on_null_stmt(then_stmt_loc)
        if else_stmt is None:
            else_stmt = actions.act_on_null_stmt(else_stmt_loc)

        return actions.act_on_if_stmt(if_loc, lparen_loc, cond, rparen_loc, then_stmt, else_loc, else_stmt);

    def parse_case_stmt(self, stmt_ctx: ParsedStmtContext, missing_case: bool = False, expr: Expr | None = None):
        assert missing_case or self.tok.ty == Tok.KW_CASE, "Not a case stmt!"
        top_level_case = None
        deepest_parsed_case_stmt = None
        colon_loc = LocRef(0)
        while missing_case or self.tok.ty == Tok.KW_CASE:
            case_loc = self.consume_token() if not missing_case else expr.get_range()[0]
            colon_loc.value = 0

            lhs = None
            if not missing_case:
                lhs = self.parse_expr() # self.parse_case_expression(case_loc);
                if lhs is None:
                    if not self.skip_until(Tok.COLON, Tok.RBRACE, stop_at_semi = True, stop_before_match = True):
                        return None
            else:
                lhs = expr;
                missing_case = False;

            if self.try_consume_token(Tok.COLON, colon_loc):
                pass
            elif self.try_consume_token(Tok.SEMI, colon_loc) or self.try_consume_token(Tok.COLONCOLON, colon_loc):
                diag(colon_loc.value, "expected ':' after 'case'", Diag.ERROR) # FixItHint::CreateReplacement(ColonLoc, ":");
            else:
                expected_loc = self.prev_tok_location # get_loc_for_end_of_token
                diag(expected_loc, "expected ':' after 'case'", Diag.ERROR) # FixItHint::CreateInsertion(ExpectedLoc, ":");
                colon_loc.value = expected_loc

            case_stmt = actions.act_on_case_stmt(case_loc, lhs, colon_loc.value);
            if case_stmt is None:
                if top_level_case is None:
                    return self.parse_stmt(stmt_ctx, None)
            else:
                next_deepest = case_stmt
                if top_level_case is None:
                    top_level_case = case_stmt
                else:
                    actions.act_on_case_stmt_body(deepest_parsed_case_stmt, case_stmt)
                deepest_parsed_case_stmt = next_deepest

        sub_stmt = None

        if self.tok.ty == Tok.RBRACE:
            sub_stmt = actions.act_on_null_stmt(colon_loc.value)
        else:
            sub_stmt = self.parse_stmt(stmt_ctx, None)

        if deepest_parsed_case_stmt is not None:
            if sub_stmt is None:
                sub_stmt = actions.act_on_null_stmt(0)
            #self.diagnose_label_followed_by_decl(sub_stmt)
            actions.act_on_case_stmt_body(deepest_parsed_case_stmt, sub_stmt)

        return top_level_case

    def parse_default_stmt(self, stmt_ctx: ParsedStmtContext):
        assert self.tok.ty == Tok.KW_DEFAULT, "Not a default stmt!"
        default_loc = self.consume_token()
        colon_loc = LocRef(0)
        if self.try_consume_token(Tok.COLON, colon_loc):
            pass
        elif self.try_consume_token(Tok.SEMI, colon_loc):
            diag(colon_loc.value, "expected ':' after 'default'", Diag.ERROR) # FixItHint::CreateReplacement(ColonLoc, ":");
        else:
            expected_loc = self.prev_tok_location # get_loc_for_end_of_token
            diag(expected_loc, "expected ':' after 'default'", Diag.ERROR) # FixItHint::CreateInsertion(ExpectedLoc, ":");
            colon_loc.value = expected_loc
        sub_stmt = None
        if self.tok.ty == Tok.RBRACE:
            sub_stmt = actions.act_on_null_stmt(colon_loc.value)
        else:
            sub_stmt = self.parse_stmt(stmt_ctx, None)

        if sub_stmt is None:
            sub_stmt = actions.act_on_null_stmt(colon_loc.value)

        #self.diagnose_label_followed_by_decl(sub_stmt)
        return actions.act_on_default_stmt(default_loc, colon_loc.value, sub_stmt, self.get_cur_scope())

    def parse_switch_stmt(self, trailing_else_loc: LocPtr):
        assert self.tok.ty == Tok.KW_SWITCH, "Not a switch stmt!"
        switch_loc = self.consume_token()

        if self.tok.ty != Tok.LPAREN:
            diag(self.tok.loc, "expected '(' after 'switch'", Diag.ERROR)
            self.skip_until(Tok.SEMI)
            return None

        self.enter_scope(ScopeFlags.SWITCH | ScopeFlags.DECL | ScopeFlags.CONTROL)

        lparen_loc = self.consume_paren()
        start = self.tok.loc
        cond = self.parse_expr()

        if cond is None and self.tok.ty != Tok.RPAREN:
            self.skip_until(Tok.SEMI);
            if self.tok.ty != Tok.RPAREN:
                self.exit_scope()
                return None

        if cond is None:
            cond_expr = actions.create_recovery_expr(start, start if self.tok.loc == start else self.prev_tok_location, [])
            cond = actions.act_on_condition(self.get_cur_scope(), loc, cond_expr)

        if self.tok.ty != Tok.RPAREN:
            assert False, "Expected RPAREN"

        rparen_loc = self.consume_paren()

        while self.tok.ty == Tok.RPAREN:
            diag(self.tok.loc, "extraneous ')' after condition, expected a statement", Diag.ERROR) # FixItHint::CreateRemoval(Tok.getLocation());
            self.consume_paren();

        switch = actions.act_on_start_of_switch_stmt(switch_loc, lparen_loc, cond, rparen_loc)
        if switch is None:
            if self.tok.ty == Tok.LBRACE:
                self.consume_brace()
                self.skip_until(Tok.RBRACE)
            else:
                self.skip_until(Tok.SEMI)
            return switch

        self.get_cur_scope().flags |= ScopeFlags.BREAK

        has_lbrace = self.tok.ty == Tok.LBRACE
        if has_lbrace:
            self.enter_scope(ScopeFlags.DECL)

        body = self.parse_stmt(ParsedStmtContext.SUB_STMT, trailing_else_loc)

        if has_lbrace:
            self.exit_scope()

        self.exit_scope()

        return actions.act_on_finish_switch_stmt(switch_loc, switch, body);

    def parse_while_stmt(self, trailing_else_loc: LocPtr):
        assert self.tok.ty == Tok.KW_WHILE, "Not a while stmt!"
        while_loc = self.consume_token()
        if self.tok.ty != Tok.LPAREN:
            diag(self.tok.loc, "expected '(' after 'while'", Diag.ERROR)
            self.skip_until(Tok.SEMI)
            return None

        self.enter_scope(ScopeFlags.BREAK | ScopeFlags.CONTINUE | ScopeFlags.DECL | ScopeFlags.CONTROL)

        lparen_loc = self.consume_paren()
        start = self.tok.loc
        cond = self.parse_expr()

        if cond is None and self.tok.ty != Tok.RPAREN:
            self.skip_until(Tok.SEMI);
            if self.tok.ty != Tok.RPAREN:
                self.exit_scope()
                return None

        if cond is None:
            cond_expr = actions.create_recovery_expr(start, start if self.tok.loc == start else self.prev_tok_location, [])
            cond = actions.act_on_condition(self.get_cur_scope(), loc, cond_expr)

        if self.tok.ty != Tok.RPAREN:
            assert False, "Expected RPAREN"

        rparen_loc = self.consume_paren()

        has_brace = self.tok.ty == Tok.LBRACE
        if has_brace:
            self.enter_scope(ScopeFlags.DECL)

        body = self.parse_stmt(ParsedStmtContext.SUB_STMT, trailing_else_loc)

        if has_brace:
            self.exit_scope()
        self.exit_scope()

        if cond is None or body is None:
            return None

        return actions.act_on_while_stmt(while_loc, lparen_loc, cond, rparen_loc, body)

    def parse_do_stmt(self):
        assert self.tok.ty == Tok.KW_DO, "Not a do stmt!"
        do_loc = self.consume_token()

        self.enter_scope(ScopeFlags.BREAK | ScopeFlags.CONTINUE | ScopeFlags.DECL)

        has_lbrace = self.tok.ty == Tok.LBRACE
        if has_lbrace:
            self.enter_scope(ScopeFlags.DECL)

        body = self.parse_stmt(ParsedStmtContext.SUB_STMT, None)

        if has_lbrace:
            self.exit_scope()

        if self.tok.ty != Tok.KW_WHILE:
            if body is not None:
                diag(self.tok.loc, "expected 'while' in do/while loop", Diag.ERROR)
                diag(do_loc, "to match this 'do'", Diag.NOTE)
                self.skip_until(Tok.SEMI, stop_before_match=True)
            return None

        while_loc = self.consume_token()

        if self.tok.ty != Tok.LPAREN:
            diag(self.tok.loc, "expected '(' after do/while", Diag.ERROR)
            self.skip_until(Tok.SEMI, stop_before_match=True)
            return None

        lparen_loc = self.consume_paren()

        start = self.tok.loc

        cond = self.parse_expr()

        if cond is not None:
            cond = actions.correct_delayed_typos_in_expr(cond, None, True)
        else:
            if self.tok.ty not in [Tok.RPAREN, Tok.RSQUARE, Tok.RBRACE]:
                self.skip_until(Tok.SEMI)
            cond = actions.create_recovery_expr(start, start if start == self.tok.loc else self.prev_tok_loc, [], TYPES["bool"])
        rparen_loc = self.consume_paren()

        self.exit_scope()

        if cond is None or body is None:
            return None

        return actions.act_on_do_stmt(do_loc, body, while_loc, lparen_loc, cond, rparen_loc)

    def parse_for_stmt(self):
        pass

    def parse_continue_stmt(self):
        continue_loc = self.consume_token()
        return actions.act_on_continue_stmt(continue_loc, self.get_cur_scope())

    def parse_break_stmt(self):
        break_loc = self.consume_token()
        return actions.act_on_break_stmt(break_loc, self.get_cur_scope())

    def parse_return_stmt(self):
        assert self.tok.ty == Tok.KW_RETURN, "not a return stmt!"
        return_loc = self.consume_token()
        r = None
        if self.tok.ty != Tok.SEMI:
            if self.tok.ty == Tok.LBRACE:
                r = self.parse_initializer()
            else:
                r = self.parse_expr()
            if r is None:
                self.skip_until(Tok.RBRACE, stop_at_semi=True, stop_before_match=True)
                return None
        return actions.act_on_return_stmt(return_loc, r, self.get_cur_scope())

    def parse_expr_stmt(self, stmt_ctx: ParsedStmtContext):
        old_token = self.tok
        expr_stmt_tok_loc = self.tok.loc

        expr = self.parse_expr()
        if expr is None:
            self.skip_until(Tok.RBRACE, stop_at_semi = True, stop_before_match = True)
            if self.tok.ty == Tok.SEMI:
                self.consume_token()
            return actions.act_on_expr_stmt_error()

        if self.tok.ty == Tok.COLON and self.get_cur_scope().is_switch_scope() and actions.check_case_expression(expr):
            diag(old_token.loc, "expected 'case' keyword before expression") # FixItHint::CreateInsertion(OldToken.getLocation(), "case ");
            return self.parse_case_stmt(stmt_ctx, True, expr)

        self.expect_and_consume_semi("expected ';' after expression")

        is_stmt_expr_result = False
        # if stmt_ctx.in_stmt_expr():
        #     look_ahead = 0
        #     while self.get_look_ahead_token(look_ahead).ty == Tok.SEMI:
        #         look_ahead += 1
        #     in_stmt_expr_result = self.get_look_ahead_token(look_ahead).ty == Tok.RBRACE and self.get_look_ahead_token(look_ahead + 1).ty == Tok.RPAREN

        if is_stmt_expr_result:
            expr = actions.act_on_stmt_expr_result(expr);
        return actions.act_on_expr_stmt(expr, not is_stmt_expr_result);

    def parse_compound_stmt_body(self, is_stmt_expr: bool) -> Stmt:
        if self.tok.ty != Tok.LBRACE:
            return None
        open_loc = self.consume_brace()
        # actions.push_compound_scope(is_stmt_expr)
        stmts = []
        sub_stmt_ctx = ParsedStmtContext.COMPOUND
        if is_stmt_expr:
            sub_stmt_ctx |= ParsedStmtContext.IN_STMT_EXPR

        while self.tok.ty not in [Tok.RBRACE, Tok.EOF]:
            r = self.parse_stmt(sub_stmt_ctx)
            if r is not None:
                stmts.append(r)

        close_loc = self.consume_brace()
        out = actions.act_on_compound_stmt(open_loc, close_loc, stmts, is_stmt_expr);
        # actions.pop_compound_scope()
        return out

    def parse_compound_stmt(self, is_stmt_expr: bool = False, scope_flags: ScopeFlags = ScopeFlags.DECL | ScopeFlags.COMPOUND_STMT) -> Stmt:
        assert self.tok.ty == Tok.LBRACE, "not a compound stmt!"
        self.enter_scope(scope_flags);
        out = self.parse_compound_stmt_body(is_stmt_expr)
        self.exit_scope()
        return out

    def parse_stmt(self, stmt_ctx: ParsedStmtContext, trailing_else_loc: LocPtr = None):
        semi_error = ""
        res = None
        # if self.tok.ty == Tok.IDENT:
        #     if colon after, parse label statement
        #     next_tok = self.next_token()
        #     if next_tok.ty != Tok.COLONCOLON:
        #         typo correct ident
        #         if it is now a keyword, retry
        match self.tok.ty:
            case Tok.KW_CASE:
                return self.parse_case_stmt(stmt_ctx)
            case Tok.KW_DEFAULT:
                return self.parse_default_stmt(stmt_ctx)
            case Tok.LBRACE:
                return self.parse_compound_stmt()
            case Tok.SEMI:
                return actions.act_on_null_stmt(self.consume_token()) # Tok.has_leading_empty_macro()
            case Tok.KW_IF:
                return self.parse_if_stmt(trailing_else_loc);
            case Tok.KW_SWITCH:
                return self.parse_switch_stmt(trailing_else_loc);
            case Tok.KW_WHILE:
                return self.parse_while_stmt(trailing_else_loc);
            case Tok.KW_DO:
                res = self.parse_do_stmt();
                semi_error = "do/while";
            case Tok.KW_FOR:
                return self.parse_for_stmt(trailing_else_loc);
            # goto
            case Tok.KW_CONTINUE:
                res = self.parse_continue_stmt();
                semi_error = "continue";
            case Tok.KW_BREAK:
                res = self.parse_break_stmt();
                semi_error = "break";
            case Tok.KW_RETURN:
                res = self.parse_return_stmt();
                semi_error = "return";
            case _:
                if self.is_declaration_statement():
                    decl_start = self.tok.loc
                    decl_end = LocRef(0)
                    decl = self.parse_decl(DeclaratorContext.BLOCK, decl_end)
                    return actions.act_on_decl_stmt(decl, decl_start, decl_end.value)
                if self.tok.ty == Tok.RBRACE:
                    diag(self.tok.loc, "expected statement", Diag.ERROR)
                    return None
                return self.parse_expr_stmt(stmt_ctx)
        if not self.try_consume_token(Tok.SEMI) and res is not None:
            self.expect_and_consume(Tok.SEMI, "expected ';' after {} statement", semi_error)
            self.skip_until(Tok.RBRACE, stop_at_semi = True, stop_before_match = True)
        return res

    def parse_end_var_decl_common(self) -> (str, Type, (Loc, Loc)):
        assert self.tok.ty == Tok.IDENT, "Expected identifier in var decl"
        ident = self.tok.value.val
        ident_loc = self.consume_token()
        assert self.tok.ty == Tok.COLON, "Expected ':' after ident in var decl"
        self.consume_token()
        ty = self.parse_type()
        assert ty is not None, "Expected Type after ':'"
        return (ident, ty, (ident_loc, self.prev_tok_location))

    def parse_param_decl(self):
        name, ty, rge = self.parse_end_var_decl_common()
        out_decl = ParamDecl(rge, name, ty)
        self.cur_scope.decls_in_scope.append(out_decl)
        return out_decl

    def parse_var_decl(self):
        assert self.tok.ty == Tok.KW_LET, "Not a var decl"
        start_loc = self.consume_token()
        name, ty, (_, end_loc) = self.parse_end_var_decl_common()
        # TODO: initializer
        assert self.tok.ty == Tok.SEMI, "Expected semicolon at end of var decl"
        self.consume_token()
        out_decl = VarDecl((start_loc, end_loc), name, ty)
        self.cur_scope.decls_in_scope.append(out_decl)
        return out_decl

    def parse_fn_decl(self):
        assert self.tok.ty == Tok.KW_FN, "Not a function decl"
        start_loc = self.consume_token()
        assert self.tok.ty == Tok.IDENT

        self.enter_scope(ScopeFlags.FN | ScopeFlags.DECL | ScopeFlags.COMPOUND_STMT)

        fn_name = self.tok.value.val
        self.consume_token()
        assert self.tok.ty == Tok.LPAREN
        self.consume_paren()
        params = []
        if self.tok.ty != Tok.RPAREN:
            while True:
                params.append(self.parse_param_decl())
                if self.tok.ty != Tok.COMMA:
                    break
                self.consume_token()
        assert self.tok.ty == Tok.RPAREN, "TODO: handle errors: missing ')'"
        self.consume_paren()
        return_type = None
        if self.tok.ty == Tok.ARROW:
            self.consume_token()
            return_type = self.parse_type()

        if self.tok.ty == Tok.SEMI:
            self.exit_scope()
            semi_loc = self.consume_token()
            return actions.act_on_fn_decl(self.cur_scope, fn_name, params, return_type, start_loc, semi_loc)

        decl = actions.act_on_start_fn_definition(self.cur_scope.parent_scope, fn_name, params, return_type, start_loc)
        if decl is None:
            self.consume_brace()
            self.skip_until(Tok.RBRACE)
            return None

        fn_body = self.parse_compound_stmt_body(False)

        self.exit_scope()

        return actions.act_on_end_fn_definition(decl, fn_body)

    def parse_type_alias_decl(self) -> TypeDecl:
        assert self.tok.ty == Tok.KW_TYPE, "Not a type alias decl"
        start_loc = self.consume_token()
        assert self.tok.ty == Tok.IDENT
        type_name = self.tok.value.val
        # lookup name availability
        self.consume_token()
        assert self.tok.ty == Tok.EQUAL
        self.consume_token()
        aliased_type = self.parse_type()
        assert self.tok.ty == Tok.SEMI
        end_loc = self.consume_token()
        decl = TypeDecl((start_loc, end_loc), type_name, AliasType(aliased_type))
        self.cur_scope.add_decl(decl)
        return decl

    def parse_field_decl(self) -> FieldDecl:
        name, ty, (start_loc, _) = self.parse_end_var_decl_common()
        if self.tok.ty != Tok.SEMI:
            diag(self.tok.loc, "ASSERT FALSE", Diag.ERROR)
            assert False, "Expected semicolon at end of field decl"
        end_loc = self.consume_token()
        decl = FieldDecl((start_loc, end_loc), name, ty)
        return decl

    def parse_struct_decl_inner(self, cur_type, cur_decl) -> TypeDecl:
        assert self.tok.ty == Tok.LBRACE
        self.consume_brace()
        fields = []
        while self.tok.ty != Tok.RBRACE:
            fields.append(self.parse_field_decl())
        self.consume_brace()
        assert self.tok.ty == Tok.SEMI
        cur_type.fields = fields
        cur_type.is_incomplete = False
        end_loc = self.consume_token()
        cur_decl.src_range = (cur_decl.src_range[0], end_loc)
        return cur_decl

    def parse_struct_decl(self) -> TypeDecl:
        assert self.tok.ty == Tok.KW_STRUCT, "Not a struct decl"
        start_loc = self.consume_token()
        assert self.tok.ty == Tok.IDENT
        type_name = self.tok.value.val
        cur_decl = self.cur_scope.lookup_named_decl(type_name)
        if cur_decl is not None and (not isinstance(cur_decl, TypeDecl) or not isinstance(cur_decl.ty, StructType)):
            diag(self.tok.loc, f"name '{type_name}' is already in use", Diag.ERROR)
            diag(cur_decl.get_range()[0], f"defined here", Diag.NOTE, [cur_decl.get_range()])
            self.skip_until(Tok.SEMI)
            return None
        cur_type = None
        if cur_decl is not None:
            cur_type = cur_decl.ty
            if not cur_type.is_incomplete:
                diag(self.tok.loc, f"struct '{type_name}' was already defined", Diag.ERROR)
                self.skip_until(Tok.SEMI)
                return None
            if cur_decl is not None:
                # TODO: get correct scope
                self.cur_scope.remove_decl(cur_decl)
        else:
            cur_type = StructType(type_name, [], True)
            cur_decl = TypeDecl((start_loc, 0), type_name, cur_type)
        self.cur_scope.add_decl(cur_decl)
        self.consume_token()
        # lookup name availability
        if self.tok.ty == Tok.SEMI:
            end_loc = self.consume_token()
            cur_decl.src_range = (decl.src_range[0], end_loc)
            return cur_decl
        return self.parse_struct_decl_inner(cur_type, cur_decl)

    def parse_integer_constexpr(self):
        e = self.parse_expr()
        end_loc = e.get_range()[1]
        if not e.ty.is_integer_type():
            diag(e.get_range()[0], "Expected integer constexpr", Diag.ERROR, [e.get_range()])
            return 0
        return eval_const_expr(e), end_loc

    def parse_enum_variant(self, enum_type, last_val):
        assert self.tok.ty == Tok.IDENT
        name = self.tok.value.val
        start_loc = self.consume_token()
        end_loc = start_loc
        if self.tok.ty == Tok.EQUAL:
            self.consume_token()
            assert self.tok.ty == Tok.NUM
            val = self.tok.value
            last_val, end_loc = self.parse_integer_constexpr()
        allow_more = self.tok.ty == Tok.COMMA
        if allow_more:
            self.consume_token()
        decl = EnumVariantDecl((start_loc, end_loc), name, enum_type, last_val)
        self.cur_scope.add_decl(decl)
        enum_type.variants.append(decl)
        return last_val + 1, allow_more

    def parse_enum_decl(self) -> TypeDecl:
        assert self.tok.ty == Tok.KW_ENUM, "not enum decl"
        start_loc = self.consume_token()
        # TODO: check previous definition
        # TODO: partial definition
        name = None
        if self.tok.ty == Tok.IDENT:
            name = self.tok.value.val
            self.consume_token()
        aliased_type = None
        if self.tok.ty == Tok.COLON:
            self.consume_token()
            aliased_type = self.parse_type()
        enum_type = EnumType(name, aliased_type, [])
        assert self.tok.ty == Tok.LBRACE
        self.consume_brace()
        last_val = 0
        allow_more = True
        while allow_more and self.tok.ty != Tok.RBRACE:
            last_val, allow_more = self.parse_enum_variant(enum_type, last_val)
        assert self.tok.ty == Tok.RBRACE
        self.consume_brace()
        assert self.tok.ty == Tok.SEMI
        end_loc = self.consume_token()
        decl = TypeDecl((start_loc, end_loc), name, enum_type)
        if name is not None:
            self.cur_scope.add_decl(decl)
        return decl

    def parse_decl(self, decl_ctx: DeclaratorContext, decl_end: LocPtr = None):
        filectx = decl_ctx == DeclaratorContext.FILE
        decl = None
        if self.tok.ty == Tok.KW_FN:
            if filectx == False:
                print("Error, can't declare a function in a function body (yet)!")
                exit(1)
            decl = self.parse_fn_decl()
        elif self.tok.ty == Tok.KW_LET:
            # if filectx == True:
            #     print("Error, can't declare a variable in global scope (yet)!")
            #     exit(1)
            decl = self.parse_var_decl()
        elif self.tok.ty == Tok.KW_LIB:
            if filectx == False:
                print("Error, can't declare a lib decl in a function body!")
                exit(1)
            start_loc = self.consume_token()
            decl = self.parse_decl(decl_ctx, decl_end)
            decl.is_lib = True
            decl.src_range = (start_loc, decl.src_range[1])
        elif self.tok.ty == Tok.KW_TYPE:
            decl = self.parse_type_alias_decl()
        elif self.tok.ty == Tok.KW_STRUCT:
            decl = self.parse_struct_decl()
        elif self.tok.ty == Tok.KW_ENUM:
            decl = self.parse_enum_decl()
        else:
            diag(self.tok.loc, f"Unexpected token {self.tok.ty}", Diag.ERROR)
            self.consume_any_token()
        if decl is not None and decl_end is not None:
            decl_end.val = decl.get_range()[1]
        return decl

    def parse_top_level_decl(self):
        result = None
        return self.parse_decl(DeclaratorContext.FILE)

    def parse_translation_unit(self):
        decls = []
        actions.act_on_start_of_translation_unit()
        while self.tok.ty != Tok.EOF:
            decls.append(self.parse_top_level_decl())
        actions.act_on_end_of_translation_unit()
        return decls

    def is_declaration_statement(self, disambiguating_with_expression: bool = False):
        match self.tok.ty:
            case Tok.KW_LET | Tok.KW_FN | Tok.KW_LIB | Tok.KW_TYPE | Tok.KW_STRUCT | Tok.KW_ENUM: # TODO: other kw
                return True
            # case Tok.IDENT | Tok.COLONCOLON:
            #     # TODO:
            #     if (DisambiguatingWithExpression) {
            #       RevertingTentativeParsingAction TPA(*this);
            #       // Parse the C++ scope specifier.
            #       CXXScopeSpec SS;
            #       ParseOptionalCXXScopeSpecifier(SS, /*ObjectType=*/nullptr,
            #                                      /*ObjectHasErrors=*/false,
            #                                      /*EnteringContext=*/true);
            #
            #       switch (Tok.getKind()) {
            #       case tok::identifier: {
            #         IdentifierInfo *II = Tok.getIdentifierInfo();
            #         bool isDeductionGuide = Actions.isDeductionGuideName(
            #             getCurScope(), *II, Tok.getLocation(), SS, /*Template=*/nullptr);
            #         if (Actions.isCurrentClassName(*II, getCurScope(), &SS) ||
            #             isDeductionGuide) {
            #           if (isConstructorDeclarator(
            #                   /*Unqualified=*/SS.isEmpty(), isDeductionGuide,
            #                   /*IsFriend=*/DeclSpec::FriendSpecified::No))
            #             return true;
            #         } else if (SS.isNotEmpty()) {
            #           // If the scope is not empty, it could alternatively be something like
            #           // a typedef or using declaration. That declaration might be private
            #           // in the global context, which would be diagnosed by calling into
            #           // isCXXSimpleDeclaration, but may actually be fine in the context of
            #           // member functions and static variable definitions. Check if the next
            #           // token is also an identifier and assume a declaration.
            #           // We cannot check if the scopes match because the declarations could
            #           // involve namespaces and friend declarations.
            #           if (NextToken().is(tok::identifier))
            #             return true;
            #         }
            #         break;
            #       }
            #       case tok::kw_operator:
            #         return true;
            #       case tok::tilde:
            #         return true;
            #       default:
            #         break;
            #       }
            #     }
            #     [[fallthrough]]
            case _:
                return False # is_cxx_simple_declaration(false)
