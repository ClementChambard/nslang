;;; ts-nslang-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Clement Chambard
;;
;; Author: Clement Chambard <cclems2002@gmail.com>
;; Maintainer: Clement Chambard <cclems2002@gmail.com>
;; Created: March 07, 2025
;; Modified: March 07, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/clement/ts-nslang-mode
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

;;;###autoload
(define-derived-mode ts-nslang-mode nslang-mode "nslang[ts]"
  "Major mode for editing nslang with tree-sitter."
  :syntax-table nslang-mode-syntax-table
  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'nslang)
    (treesit-parser-create 'nslang)))


(provide 'ts-nslang-mode)
;;; ts-nslang-mode.el ends here
