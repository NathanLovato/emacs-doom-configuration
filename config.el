;;; .doom.d/config.el -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.doom.d")

(+global-word-wrap-mode)

;; Completion settings
(setq which-key-idle-delay 0.1)
(setq company-idle-delay 0.1
      company-show-numbers t)

(setq display-line-numbers-type 'relative)

;; Base settings
;; Use relative line numbers
(setq-hook! 'text-mode-hook display-line-numbers 'relative)
(setq-hook! 'prog-mode-hook display-line-numbers 'relative)

(setq-default fill-column 100)
(remove-hook 'text-mode-hook #'auto-fill-mode)

;; Programming
(after! git-commit (setq git-commit-summary-max-length 72))
(after! gdscript-mode
  (set-company-backend! 'company-lsp 'company-keywords 'company-capf 'company-yasnippet))
(set-company-backend! 'text-mode 'company-files)

;; Evil line text object, from https://github.com/emacsorphanage/evil-textobj-line/blob/master/evil-textobj-line.el
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

(evil-define-text-object evil-a-line (count &optional beg end type)
  (evil-line-range count beg end type t))
(evil-define-text-object evil-inner-line (count &optional beg end type)
  (evil-line-range count beg end type))

(define-key evil-outer-text-objects-map "l" 'evil-a-line)
(define-key evil-inner-text-objects-map "l" 'evil-inner-line)

;; Temporary workaround issues with the language server
(defun franco/godot-gdscript--lsp-ignore-error (original-function &rest args)
  "Ignore the error message resulting from Godot not replying to the `JSONRPC' request."
  (if (string-equal major-mode "gdscript-mode")
      (let ((json-data (nth 0 args)))
        (if (and (string= (gethash "jsonrpc" json-data "") "2.0")
                 (not (gethash "id" json-data nil))
                 (not (gethash "method" json-data nil)))
            nil
          (apply original-function args)))
    (apply original-function args)))
(after! lsp
  (add-hook 'gdscript-mode-hook #'lsp)
  (setq lsp-prefer-capf t)
  (advice-add #'lsp--get-message-type :around #'franco/godot-gdscript--lsp-ignore-error))

;; for lsp performances
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)

(after! emojify-mode (setq! emojify-emoji-styles '(unicode)))

;; Markdown configuration
;; (setq-hook! 'markdown-mode-hook visual-line-mode t)

;; Org mode configuration
;; (setq-hook! 'org-mode-hook visual-line-mode t)
(after! org (progn
              (require 'config-org)
              (setq org-support-shift-select t)
              (setq org-directory "~/Documents/org/")
              (setq org-default-notes-file (concat org-directory "notes.org"))
              (setq org-agenda-files '("~/Documents/org/calendar.org"
                                       "~/Documents/org/tasks.org"))
              (setq org-projectile-capture-template "* %?\n  %U\n  %i\n  %a")))
(setq ob-mermaid-cli-path "/home/gdquest/.local/bin/node_modules/mermaid.cli/index.bundle.js")

(map! :leader
      :desc "Remove known project" "p x" #'projectile-remove-known-project)
(map! :leader
      :desc "Find directory in project" "p d" #'counsel-projectile-find-dir)
