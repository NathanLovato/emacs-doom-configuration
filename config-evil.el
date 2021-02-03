;;; config-evil.el --- Evil-mode configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Nathan
;;
;; Author: Nathan <http://github/gdquest>
;; Maintainer: Nathan <gdquest@pop-os>
;; Created: December 29, 2020
;; Modified: December 29, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/gdquest/config-evil
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Evil-mode configuration
;;
;;; Code:

;; Evil line text object, from https://github.com/emacsorphanage/evil-textobj-line/blob/master/evil-textobj-line.el.
(defun evil-line-range (count beg end type &optional inclusive)
  (if inclusive
      (evil-range (line-beginning-position) (line-end-position))
    (let ((start (save-excursion
                   (back-to-indentation)
                   (point)))
          (end (save-excursion
                 (goto-char (line-end-position))
                 (skip-syntax-backward " " (line-beginning-position))
                 (point))))
      (evil-range start end))))

;; Markdown code fence text object.
(defun evil-markdown-code-fence-range (count beg end type &optional inner)
  (let
      ((start (save-excursion
                (search-backward "```")
                (if inner
                    (forward-line))
                (point)))
       (end (save-excursion
              (search-forward "```")
              (if inner
                  (progn
                    (forward-line -1)
                    (end-of-line)))
              (point))))
    (evil-range start end)))

;; Registering text objects and keyboard shortcuts.
(evil-define-text-object evil-a-line (count &optional beg end type)
  (evil-line-range count beg end type t))
(evil-define-text-object evil-inner-line (count &optional beg end type)
  (evil-line-range count beg end type))
(evil-define-text-object evil-a-markdown-code-fence (count &optional beg end type)
  (evil-markdown-code-fence-range count beg end type))
(evil-define-text-object evil-inner-markdown-code-fence (count &optional beg end type)
  (evil-markdown-code-fence-range count beg end type t))

;; keymaps
(map! :map global-map
      :g "C-;" 'evil-multiedit-match-all)
(define-key evil-outer-text-objects-map "l" 'evil-a-line)
(define-key evil-inner-text-objects-map "l" 'evil-inner-line)
(define-key evil-inner-text-objects-map (kbd "M-f") 'evil-inner-markdown-code-fence)
(define-key evil-outer-text-objects-map (kbd "M-f") 'evil-a-markdown-code-fence)

(setq evil-undo-system 'undo-redo)

;; Move recenter-window to C-k to free C-l for delete-forward-char in evil insert mode
(global-unset-key (kbd "C-l"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "M-s"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-j"))
(global-set-key (kbd "C-k") 'recenter-top-bottom)

;; Insert mode keyboard shortcuts
(evil-global-set-key 'normal (kbd "M-s") 'evil-substitute)
(evil-global-set-key 'insert (kbd "C-h") 'delete-backward-char)
(evil-global-set-key 'insert (kbd "C-l") 'delete-forward-char)
(evil-global-set-key 'insert (kbd "M-p") 'evil-paste-after)
(evil-global-set-key 'insert (kbd "M-P") 'evil-paste-before)
;; Navigation between lines in insert mode
(evil-global-set-key 'insert (kbd "M-l") 'evil-forward-char)
(evil-global-set-key 'insert (kbd "M-h") 'evil-backward-char)
(evil-global-set-key 'insert (kbd "M-k") 'evil-previous-line)
(evil-global-set-key 'insert (kbd "M-j") 'evil-next-line)
(evil-global-set-key 'normal (kbd "M-j") 'default-indent-new-line)

;; Make `:s' use the `g' option by default
(setq evil-ex-substitute-global t)

(provide 'config-evil)
;;; config-evil.el ends here
