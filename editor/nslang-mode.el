;;; nslang-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Clement Chambard
;;
;; Author: Clement Chambard <cclems2002@gmail.com>
;; Maintainer: Clement Chambard <cclems2002@gmail.com>
;; Created: March 05, 2025
;; Modified: March 05, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/clement/nslang-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar nslang-mode-hook nil)

(defvar nslang-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for nslang major mode")

(defconst nslang-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; strings / chars
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\" "\"" table)
    ;; escape char
    (modify-syntax-entry ?\\ "\\" table)
    ;; comments
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?* ". 23b" table)
    (modify-syntax-entry ?\n ">" table)
    ;; punctuation
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?| "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?\; "." table)
    ;; fix underscore in word
    (modify-syntax-entry ?_ "w" table)
    ;; parens
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table in use in `nslang-mode` buffers.")


(defconst nslang-types
  `("i8" "i16" "i32" "i64" "u8" "u16" "u32" "u64" "void" "bool"))

(defconst nslang-keywords
  `("if" "else" "while" "do" "sizeof" "switch" "case" "default" "let" "type" "struct" "enum" "lib" "fn" "break" "continue" "return" "for"))

(defconst nslang-preprocessors
  `("#include"))

(defconst nslang-builtins
  `("__builtin_syscall"))

(defvar nslang-mode-literal-boolean nil)

(defun nslang-mode-generate-literal-boolean ()
  (let ((literal-bool-regexp (regexp-opt `("true" "false") 'words)))
    (setq nslang-mode-literal-boolean
          `(
            ;; highlight
            (,literal-bool-regexp (0 font-lock-constant-face))))))

(defvar nslang-mode-literal-nullptr nil)

(defun nslang-mode-generate-literal-nullptr ()
  (let ((literal-nullptr-regexp (regexp-opt `("nullptr") 'words)))
    (setq nslang-mode-literal-nullptr
          `(
            ;; highlight
            (,literal-nullptr-regexp (0 font-lock-constant-face))))))

(defvar nslang-mode-keywords nil)

(defun nslang-mode-generate-keywords ()
  (let ((types-regexp (regexp-opt nslang-types 'words))
        (preprocessors-regexp (regexp-opt nslang-preprocessors))
        (keywords-regexp (regexp-opt nslang-keywords 'words))
        (builtins-regexp (regexp-opt nslang-builtins 'words)))
    (setq nslang-mode-keywords
          `(
            ;; All kinds of keywords here
            (,types-regexp (0 font-lock-type-face))
            (,builtins-regexp (0 font-lock-builtin-face))
            (,preprocessors-regexp (0 font-lock-preprocessor-face))
            (,keywords-regexp (0 font-lock-keyword-face))))))

(defvar nslang-mode-literal-integer)

(defun nslang-mode-generate-literal-integer ()
  (let* ((integer-suffix-regexp "\\(?:\\(?:i\\|u\\)\\(?:8\\|16\\|32\\|64\\)\\)")
        (literal-hex-regexp (concat "\\<\\(0[xX]\\)\\([0-9a-fA-F]+\\)\\(" integer-suffix-regexp "?\\)\\>"))
        (literal-oct-regexp (concat "\\<\\(0\\)\\([0-7]*\\)\\(" integer-suffix-regexp "?\\)\\>"))
        (literal-dec-regexp (concat "\\<\\([1-9][0-9]*\\)\\(" integer-suffix-regexp "?\\)\\>")))
    (setq nslang-mode-literal-integer
          `(
            ;; All numbers
            (,literal-hex-regexp (1 'font-lock-number-face) (2 'font-lock-number-face) (3 'font-lock-number-face))
            (,literal-oct-regexp (1 'font-lock-number-face) (2 'font-lock-number-face) (3 'font-lock-number-face))
            (,literal-dec-regexp (1 'font-lock-number-face) (2 'font-lock-number-face))))))

(defun nslang-mode-add-keywords (&optional mode)
  "Install keywords"
  (font-lock-add-keywords mode (nslang-mode-generate-keywords) nil)
  (font-lock-add-keywords mode (nslang-mode-generate-literal-boolean) nil)
  (font-lock-add-keywords mode (nslang-mode-generate-literal-integer) nil)
  (font-lock-add-keywords mode (nslang-mode-generate-literal-nullptr) nil)
  ;(font-lock-add-keywords mode (nslang-mode-generate-literal-string) nil)
  )

(defun nslang-mode-remove-keywords (&optional mode)
  "Remove keywords"
  (font-lock-remove-keywords mode nslang-mode-keywords)
  (font-lock-remove-keywords mode nslang-mode-literal-boolean)
  (font-lock-remove-keywords mode nslang-mode-literal-integer)
  (font-lock-remove-keywords mode nslang-mode-literal-nullptr)
  ;(font-lock-remove-keywords mode nslang-mode-literal-string)
  )

;;;###autoload
(define-derived-mode nslang-mode prog-mode "nslang"
  "Major Mode for editing nslang source code."
  :syntax-table nslang-mode-syntax-table
  (nslang-mode-add-keywords)
  (rainbow-delimiters-mode)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ns\\'" . nslang-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nsh\\'" . nslang-mode))

(provide 'nslang-mode)

;;; nslang-mode.el ends here
