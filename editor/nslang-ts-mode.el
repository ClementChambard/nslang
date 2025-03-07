;;; nslang-ts-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Clement Chambard
;;
;; Author: Clement Chambard <cclems2002@gmail.com>
;; Maintainer: Clement Chambard <cclems2002@gmail.com>
;; Created: March 07, 2025
;; Modified: March 07, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/clement/nslang-ts-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'nslang-mode)
(require 'treesit)

(defconst nslang-ts-types
  `("i8" "i16" "i32" "i64" "u8" "u16" "u32" "u64" "void" "bool"))

(defconst nslang-ts-keywords
  `("if" "else" "while" "do" "sizeof" "switch" "case" "default" "let" "type" "struct" "enum" "lib" "fn" "break" "continue" "return" "for"))

(defvar nslang-ts-operators
  '("=" "-" "*" "/" "+" "%" "~" "|" "&" "^" "<<" ">>" "->"
    "." "<" "<=" ">=" ">" "==" "!=" "!" "&&" "||" "-="
    "+=" "*=" "/=" "%=" "|=" "&=" "^=" ">>=" "<<=" "--" "++"))


(defun nslang-ts-mode-font-lock-settings (mode)
  "Tree sitter font-lock settings."
  (treesit-font-lock-rules
   :default-language mode
   ; :feature 'comment
   ; `((comment) @font-lock-comment-face)

   ; :feature 'preprocessor
   ; `((preproc_directive) @font-lock-preprocessor-face)

   :feature 'constant
   `("true" @font-lock-constant-face
     "false" @font-lock-constant-face
     "nullptr" @font-lock-constant-face)

   :feature 'keyword
   `([,@nslang-ts-keywords] @font-lock-keyword-face)

   :feature 'operator
   `([,@nslang-ts-operators] @font-lock-operator-face
     "!" @font-lock-negation-char-face)

   :feature 'string
   `((str) @font-lock-string-face)

   :feature 'literal
   `((num) @font-lock-number-face)

   ;:feature 'type
   ;`((primitive_type) @font-lock-type-face
   ;  (type_identifier) @font-lock-type-face
   ;  (sized_type_specifier) @font-lock-type-face
   ;  ,@(when (eq mode 'cpp)
   ;      '((type_qualifier) @font-lock-type-face
   ;
   ;        (qualified_identifier
   ;         scope: (namespace_identifier) @font-lock-constant-face)
   ;
   ;        (operator_cast) type: (type_identifier) @font-lock-type-face
   ;
   ;        (namespace_identifier) @font-lock-constant-face))
   ;  [,@c-ts-mode--type-keywords] @font-lock-type-face)

   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :feature 'delimiter
   '((["," ":" ";"]) @font-lock-delimiter-face)))

;;;###autoload
(define-derived-mode nslang-ts-mode nslang-mode "NSLang"
  "Major mode for editing nslang, powered by tree-sitter."
  (when (treesit-ready-p 'nslang)
    (let ((primary-parser (treesit-parser-create 'nslang)))
      (setq-local treesit-primary-parser primary-parser)
      (setq-local treesit-font-lock-settings
                  (nslang-ts-mode-font-lock-settings 'nslang))
      (treesit-major-mode-setup))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ns\\'" . nslang-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nsh\\'" . nslang-mode))

(provide 'nslang-ts-mode)
;;; nslang-ts-mode.el ends here
