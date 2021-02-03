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
      :when (featurep! :editor evil +everywhere)
      :map evil-markdown-mode-map
      :i "M-b" nil)

(map! :after markdown-mode
      :map markdown-mode-map
      :localleader
      (:prefix ("i" . "insert")
       :desc "<hr>" "-" #'markdown-insert-hr
       :desc "Heading 1" "1" #'markdown-insert-header-atx-1
       :desc "Heading 2" "2" #'markdown-insert-header-atx-2
       :desc "Heading 3" "3" #'markdown-insert-header-atx-3
       :desc "Heading 4" "4" #'markdown-insert-header-atx-4
       :desc "Heading 5" "5" #'markdown-insert-header-atx-5
       :desc "Heading 6" "6" #'markdown-insert-header-atx-6
       :desc "Code block" "C" #'markdown-insert-gfm-code-block
       :desc "Pre region" "P" #'markdown-pre-region
       :desc "Blockquote region" "Q" #'markdown-blockquote-region
       :desc "Checkbox" "[" #'markdown-insert-gfm-checkbox
       :desc "Bold" "b" #'markdown-insert-bold
       :desc "Inline code" "c" #'markdown-insert-code
       :desc "Italic" "e" #'markdown-insert-italic
       :desc "Footnote" "f" #'markdown-insert-footnote
       :desc "Header dwim" "h" #'markdown-insert-header-dwim
       :desc "Italic" "i" #'markdown-insert-italic
       :desc "Kbd" "k" #'markdown-insert-kbd
       :desc "Link" "l" #'markdown-insert-link
       :desc "Pre" "p" #'markdown-insert-pre
       :desc "New blockquote" "q" #'markdown-insert-blockquote
       :desc "Strike through" "s" #'markdown-insert-strike-through
       :desc "Table" "t" #'markdown-insert-table
       :desc "Wiki link" "w" #'markdown-insert-wiki-link))

(map! :map evil-markdown-mode-map
      :i "M-C-`" #'markdown-insert-code)

(provide 'config-markdown)
;;; config-markdown.el ends here
