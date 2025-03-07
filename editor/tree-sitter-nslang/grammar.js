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
    source_file: $ => "hello"
  }
});
