;;; .doom.d/config.el -*- lexical-binding: t; -*-
(add-to-list 'load-path "~/.doom.d")

(+global-word-wrap-mode)

;; Completion settings
(setq which-key-idle-delay 0.05)
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0
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
(set-company-backend! 'text-mode
  nil 'company-capf 'company-files '(:separate 'company-yasnippet 'company-dabbrev 'company-ispell))
(set-company-backend! 'gdscript-mode
  'company-lsp 'company-tabnine '(:separate 'company-yasnippet 'company-dabbrev 'company-capf) 'company-keywords )
;; (set-company-backend! 'markdown-mode 'company-tabnine)
(set-company-backend! 'rst-mode
  'company-tabnine)
(global-set-key (kbd "M-S") 'company-yasnippet)

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

(evil-define-text-object evil-a-line (count &optional beg end type)
  (evil-line-range count beg end type t))
(evil-define-text-object evil-inner-line (count &optional beg end type)
  (evil-line-range count beg end type))

(evil-define-text-object evil-a-markdown-code-fence (count &optional beg end type)
  (evil-markdown-code-fence-range count beg end type))
(evil-define-text-object evil-inner-markdown-code-fence (count &optional beg end type)
  (evil-markdown-code-fence-range count beg end type t))

(define-key evil-outer-text-objects-map "l" 'evil-a-line)
(define-key evil-inner-text-objects-map "l" 'evil-inner-line)
(define-key evil-inner-text-objects-map (kbd "M-f") 'evil-inner-markdown-code-fence)
(define-key evil-outer-text-objects-map (kbd "M-f") 'evil-a-markdown-code-fence)

(setq evil-undo-system 'undo-redo)
;; Move recenter-window to C-k to free C-l for delete-forward-char in evil insert mode
(global-unset-key (kbd "C-l"))
(global-unset-key (kbd "C-k"))
(global-unset-key (kbd "M-s"))
(global-set-key (kbd "C-k") 'recenter-top-bottom)

;; Insert mode keyboard shortcuts
(evil-global-set-key 'normal (kbd "M-s") 'evil-substitute)
(evil-global-set-key 'insert (kbd "C-h") 'delete-backward-char)
(evil-global-set-key 'insert (kbd "C-l") 'delete-forward-char)
(evil-global-set-key 'insert (kbd "M-p") 'evil-paste-after)
(evil-global-set-key 'insert (kbd "M-P") 'evil-paste-before)

(setq evil-ex-substitute-global t)

(defun lsp--gdscript-ignore-errors (original-function &rest args)
  "Ignore the error message resulting from Godot not replying to the `JSONRPC' request."
  (if (string-equal major-mode "gdscript-mode")
      (let ((json-data (nth 0 args)))
        (if (and (string= (gethash "jsonrpc" json-data "") "2.0")
                 (not (gethash "id" json-data nil))
                 (not (gethash "method" json-data nil)))
            nil ; (message "Method not found")
          (apply original-function args)))
    (apply original-function args)))
(advice-add #'lsp--get-message-type :around #'lsp--gdscript-ignore-errors)
(after! lsp
  (setq lsp-prefer-capf t)
  (setq lsp-idle-delay 0.1)
  (setq lsp-tcp-connection-timeout 0.2))

;; for lsp performances
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)

(after! emojify-mode (setq! emojify-emoji-styles '(unicode)))

;; Org mode configuration
(after! org (require 'config-org))
;; Markdown configuration
(after! markdown-mode (require 'config-markdown))

(setq ob-mermaid-cli-path "/home/gdquest/.local/bin/node_modules/mermaid.cli/index.bundle.js")

(map! :leader :desc "Dired" "-" #'dired-jump)
(map! :leader :desc "Dired in other window" "o o" #'dired-jump-other-window)
(map! :leader :desc "Remove known project" "p x" #'projectile-remove-known-project)
(map! :leader :desc "Find directory in project" "p d" #'counsel-projectile-find-dir)
(map! :leader
      :desc "List bookmarks"
      "b L" #'list-bookmarks
      :leader
      :desc "Save current bookmarks to bookmark file"
      "b w" #'bookmark-save)

(setq! projectile-project-search-path '("~/Projects" "~/Repositories"))
(map! :map global-map
      :g "C-s" 'save-buffer
      :g "C-S-s" 'projectile-save-project-buffers)

(use-package! company-tabnine
  :ensure t)

(yas-global-mode 1)
