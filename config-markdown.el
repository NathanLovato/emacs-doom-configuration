;;; config-markdown.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Nathan
;;
;; Author: Nathan Lovato <nathan@gdquest.com>
;; Maintainer: Nathan Lovato <nathan@gdquest.com>
;; Created: December 26, 2020
;; Modified: December 26, 2020
;; Version: 0.0.1
;; Keywords:
;; Package-Requires: ((emacs 27.1.0))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Configuration for markdown.
;;
;;; Code:

;; Don't spell-check in the following faces.
(setf (alist-get 'markdown-mode +spell-excluded-faces-alist)
      '(markdown-code-face
        markdown-reference-face
        markdown-link-face
        markdown-url-face
        markdown-markup-face
        markdown-html-attr-value-face
        markdown-html-attr-name-face
        markdown-html-tag-name-face))

(map! :after markdown-mode
      :map evil-markdown-mode-map
      :i "M-b" 'evil-backward-word-begin)

(map! :map evil-markdown-mode-map
      :i "M-C-`" #'markdown-insert-code)

(provide 'config-markdown)
;;; config-markdown.el ends here
