/**
 * @file Nslang grammar for tree-sitter
 * @author ClementChambard <clement.chambard@gmail.com>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "nslang",

  extras: $ => [
    $.comment,
    /[\s\p{Zs}\uFEFF\u2028\u2029\u2060\u200B]/, // whitespace
  ],

  word: $ => $.ident,

  rules: {
    source_file: $ => repeat(choice($._top_level_decl, $.include_pp)),
    include_pp: $ => seq('#include', field("file", $.file_name)), // TODO: actually include ?
    file_name: $ => /([a-zA-Z0-9_/\.]|\\.)+/,

    // DECLS
    _top_level_decl: $ => choice($.fn_decl, $.lib_decl, $._block_decl),
    lib_decl: $ => seq('lib', field("decl", $._top_level_decl)),
    _block_decl: $ => choice($.var_decl, $.struct_decl, $.enum_decl, $.type_decl),
    fn_decl: $ => seq(
      'fn', 
      optional(seq(field("struct_scope", $.ident), '::')),
      field("name", $.ident), 
      '(', 
      field("params", optional($.param_decl_list)), 
      ')', 
      optional("init"),
      field("return_type", optional($._return_type)), 
      field("body", choice($.compound_stmt, ';'))),
    _return_type: $ => seq('->', $.type),
    param_decl: $ => choice(seq(field("name", $.ident), ':', field("type", $.type)), '...'),
    param_decl_list: $ => seq($.param_decl, repeat(seq(',', $.param_decl))),
    var_decl: $ => seq('let', field("name", $.ident), 
      optional(seq(':', field("type", $.type))),
      optional(seq('=', $.expr)),
      ';'),
    
    struct_decl: $ => seq('struct', field("name", $.ident), field("fields", optional($.field_decl_list)), ';'),
    field_decl: $ => seq(optional("super"), field("name", $.ident), ':', field("type", $.type), ';'),
    field_decl_list: $ => seq('{', repeat($.field_decl), '}'),
    enum_decl: $ => seq('enum', choice(field("name", $.ident), seq(field("name", $.ident), ':', field("aliased_type", $.type)), seq(':', field("aliased_type", $.type))), '{', field("body", optional($._enum_body)), '}', ';'),
    _enum_body: $ => seq($.enum_variant_decl, repeat(seq(',', $.enum_variant_decl)), optional(',')),
    enum_variant_decl: $ => seq(field("name", $.ident), optional(seq('=', field("value", $.num)))),
    type_decl: $ => seq('type', field("name", $.ident), '=', field("aliased_type", $.type), ';'),

    // TYPES
    type: $ => choice(field("name", $.ident), $.pointer_type, $.array_type, $.builtin_type),
    builtin_type: $ => choice("i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64", "void", "bool"),
    pointer_type: $ => seq(field("subtype", $.type), '*'),
    array_type: $ => seq(field("subtype", $.type), '[', field("count", $.expr), ']'), // constexpr

    // STMTS
    stmt: $ => choice(
      $.compound_stmt, 
      $.if_stmt,
      $.while_stmt,
      $.do_stmt,
      $.switch_stmt,
      seq('break', ';'),
      seq('continue', ';'),
      seq($.expr, ';'),
      // TODO: switch, for
      seq('case', field("val", $.expr), ':'),
      seq('default', ':'),
      seq('return', field("return_value", optional($.expr)), ';'),
      $._block_decl,
      ';'
    ),
    compound_stmt: $ => seq('{', repeat($.stmt), '}'),
    while_stmt: $ => seq('while', '(', field("cond", $.expr), ')', field("body", $.stmt)),
    if_stmt: $ => prec.right(0, seq('if', '(', field("cond", $.expr), ')', field("then_body", $.stmt), field("else_body", optional(seq('else', $.stmt))))),
    do_stmt: $ => seq('do', field("body", $.stmt), 'while', '(', field("cond", $.expr), ')', ';'),
    switch_stmt: $ => seq('switch', '(', field("switch_var", $.expr), ')', field("body", $.stmt)),

    // EXPRS
    expr_list: $ => seq($.expr, repeat(seq(',', $.expr))),
    expr: $ => choice(
      'true',
      'false',
      'nullptr',
      $.ident,
      $.method_ident,
      $.num,
      $.str,
      $.chr,
      seq('(', $.expr, ')'),
      seq('vaarg', '<', $.type, '>'),
      'vaargs',
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
      prec.right(11, seq('cast', '<', $.type, '>', '(', $.expr, ')')),
      prec.right(11, seq('sizeof', '(', $.type, ')')), // TODO: could be type or expr
      prec.left(12, seq($.expr, '++')),
      prec.left(12, seq($.expr, '--')),
      prec.left(12, $.member_expr),
      prec.left(12, $.call_expr),
      prec.left(12, seq($.expr, '[', $.expr, ']')),
    ),
    member_expr: $ => seq($.expr, choice('.', '->'), field("field", $.ident)),
    method_ident: $ => seq(field("type", $.ident), '::', field("name", $.ident)),
    call_expr: $ => seq(field("func", choice($.ident, $.method_ident, $.member_expr)), '(', optional($.expr_list), ')'),

    comment: _ => token(choice(
      seq("//", /[^\r\n\u2028\u2029]*/),
      seq(
        "/*",
        /[^*]*\*+([^/*][^*]*\*+)*/,
        "/",
      ),
    )),

    ident: _ => /[a-zA-Z_][a-zA-Z_0-9]*/,
    num: _ => /(0[xX][0-9a-fA-F]+|0[0-7]*|[1-9][0-9]*|([0-9]*\.[0-9]+|[1-9][0-9]*\.?)([eE][+-]?[0-9]+)?)([iuf](8|16|32|64))?/,
    str: _ => /"([^"\\]|\\.)*"/,
    chr: _ => /'([^'\\]|\\.)*'/,
  }
});
