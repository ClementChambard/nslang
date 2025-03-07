/**
 * @file Nslang grammar for tree-sitter
 * @author ClementChambard <clement.chambard@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "nslang",

  rules: {
    // TODO: add the actual grammar rules
    source_file: $ => repeat(choice($._top_level_decl, $.include_pp)),
    include_pp: $ => seq('#include', field("file", $.file_name)),
    file_name: $ => /([a-zA-Z0-9_/\.]|\\.)+/,

    // DECLS
    _top_level_decl: $ => choice($.fn_decl, $.lib_decl, $._block_decl),
    lib_decl: $ => seq('lib', field("decl", $._top_level_decl)),
    _block_decl: $ => choice($.var_decl, $.struct_decl, $.enum_decl, $.type_decl),
    fn_decl: $ => seq(
      'fn', 
      field("name", $.ident), 
      '(', 
      field("params", optional($.param_decl_list)), 
      ')', 
      field("return_type", optional($._return_type)), 
      field("body", choice($.compound_stmt, ';'))),
    _return_type: $ => seq('->', $.type),
    param_decl: $ => seq(field("name", $.ident), ':', field("type", $.type)),
    param_decl_list: $ => seq($.param_decl, repeat(seq(',', $.param_decl))),
    var_decl: $ => seq('let', field("name", $.ident), ':', field("type", $.type), ';'),
    struct_decl: $ => seq('struct', field("name", $.ident), field("fields", optional(seq('{', repeat($.field_decl), '}'))), ';'),
    field_decl: $ => seq(field("name", $.ident), ':', field("type", $.type), ';'),
    enum_decl: $ => seq('enum', choice(field("name", $.ident), seq(field("name", $.ident), ':', field("aliased_type", $.type)), seq(':', field("aliased_type", $.type))), '{', field("body", optional($._enum_body)), '}', ';'),
    _enum_body: $ => seq($.enum_variant_decl, repeat(seq(',', $.enum_variant_decl)), optional(',')),
    enum_variant_decl: $ => seq(field("name", $.ident), optional(seq('=', field("value", $.num)))),
    type_decl: $ => seq('type', field("name", $.ident), '=', field("aliased_type", $.type), ';'),

    // TYPES
    type: $ => choice(field("name", $.ident), $.pointer_type, $.array_type, $.builtin_type),
    builtin_type: $ => choice("i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "void", "bool"),
    pointer_type: $ => seq(field("subtype", $.type), '*'),
    array_type: $ => seq(field("subtype", $.type), '[', field("count", $.num), ']'),

    // STMTS
    stmt: $ => choice(
      $.compound_stmt, 
      prec.right(0, seq('if', '(', field("cond", $.expr), ')', field("then_stmt", $.stmt), field("else_stmt", optional(seq('else', $.stmt))))),
      seq('while', '(', field("cond", $.expr), ')', field("while_stmt", $.stmt)),
      seq('do', field("do_stmt", $.stmt), 'while', '(', field("cond", $.expr), ')', ';'),
      seq('break', ';'),
      seq('continue', ';'),
      // TODO: switch, case, default, for
      seq($.expr, ';'),
      seq('return', field("return_value", optional($.expr)), ';'),
      $._block_decl,
      ';'
    ),
    compound_stmt: $ => seq('{', repeat($.stmt), '}'),

    // EXPRS
    expr_list: $ => seq($.expr, repeat(seq(',', $.expr))),
    expr: $ => choice(
      'true',
      'false',
      'nullptr',
      $.ident,
      $.num,
      $.str,
      seq('(', $.expr, ')'),
      prec.right(0, seq($.expr, '=', $.expr)),
      prec.right(0, seq($.expr, '+=', $.expr)),
      prec.right(0, seq($.expr, '-=', $.expr)),
      prec.right(0, seq($.expr, '*=', $.expr)),
      prec.right(0, seq($.expr, '/=', $.expr)),
      prec.right(0, seq($.expr, '%=', $.expr)),
      prec.right(0, seq($.expr, '>>=', $.expr)),
      prec.right(0, seq($.expr, '<<=', $.expr)),
      prec.right(0, seq($.expr, '&=', $.expr)),
      prec.right(0, seq($.expr, '^=', $.expr)),
      prec.right(0, seq($.expr, '|=', $.expr)),
      prec.right(0, seq($.expr, '?', $.expr, ':', $.expr)),
      prec.left(1, seq($.expr, '||', $.expr)),
      prec.left(2, seq($.expr, '&&', $.expr)),
      prec.left(3, seq($.expr, '|', $.expr)),
      prec.left(4, seq($.expr, '^', $.expr)),
      prec.left(5, seq($.expr, '&', $.expr)),
      prec.left(6, seq($.expr, '==', $.expr)),
      prec.left(6, seq($.expr, '!=', $.expr)),
      prec.left(7, seq($.expr, '<', $.expr)),
      prec.left(7, seq($.expr, '<=', $.expr)),
      prec.left(7, seq($.expr, '>', $.expr)),
      prec.left(7, seq($.expr, '>=', $.expr)),
      prec.left(8, seq($.expr, '<<', $.expr)),
      prec.left(8, seq($.expr, '>>', $.expr)),
      prec.left(9, seq($.expr, '+', $.expr)),
      prec.left(9, seq($.expr, '-', $.expr)),
      prec.left(10, seq($.expr, '*', $.expr)),
      prec.left(10, seq($.expr, '/', $.expr)),
      prec.left(10, seq($.expr, '%', $.expr)),
      prec.right(11, seq('++', $.expr)),
      prec.right(11, seq('--', $.expr)),
      prec.right(11, seq('+', $.expr)),
      prec.right(11, seq('-', $.expr)),
      prec.right(11, seq('!', $.expr)),
      prec.right(11, seq('~', $.expr)),
      prec.right(11, seq('*', $.expr)),
      prec.right(11, seq('&', $.expr)),
      prec.left(12, seq($.expr, '++')),
      prec.left(12, seq($.expr, '--')),
      prec.left(12, seq($.expr, '.', $.expr)),
      prec.left(12, seq($.expr, '->', $.expr)),
      prec.left(12, seq('__builtin_syscall', '(', $.expr_list, ')')),
      prec.left(12, seq('sizeof', '(', $.type, ')')),
      prec.left(12, seq($.expr, '(', optional($.expr_list), ')')),
      prec.left(12, seq($.expr, '[', $.expr, ']')),
    ),
    ident: $ => /[a-zA-Z_][a-zA-Z_0-9]*/,
    num: $ => /(0[xX][0-9a-fA-F]+|0[0-7]*|[1-9][0-9]*)([iu](8|16|32|64))?/,
    str: $ => /"([^"\\]|\\.)*"/
  }
});
