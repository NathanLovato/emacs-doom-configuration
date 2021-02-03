;;; .doom.d/config.el -*- lexical-binding: t; -*-
(add-to-list 'load-path "~/.doom.d")

(+global-word-wrap-mode)

(setq-default fill-column 80)
(setq!
 ;; Completion variables
 which-key-idle-delay 0.05
 company-minimum-prefix-length 1
 company-idle-delay 0.0
 company-show-numbers t

 ;; Line numbers
 display-line-numbers-type 'relative
 ;; Projects
 projectile-project-search-path '("~/Projects" "~/Repositories")
 ;; For lsp performance
 read-process-output-max (* 1024 1024)
 gc-cons-threshold 100000000)

;; lsp global settings
(after! lsp
  (setq!
   lsp-prefer-capf t
   lsp-idle-delay 0.1
   lsp-tcp-connection-timeout 0.2))

(yas-global-mode 1)

;; Use relative line numbers
(setq-hook! 'text-mode-hook display-line-numbers 'relative)
(setq-hook! 'prog-mode-hook display-line-numbers 'relative)
(remove-hook 'text-mode-hook #'auto-fill-mode)

;; Programming
(after! git-commit (setq git-commit-summary-max-length 72))
(set-company-backend! 'text-mode
  nil 'company-capf 'company-files '(:separate 'company-yasnippet 'company-dabbrev 'company-ispell))
(set-company-backend! 'gdscript-mode
  'company-lsp 'company-tabnine '(:separate 'company-yasnippet 'company-dabbrev 'company-capf) 'company-keywords )
(set-company-backend! 'rst-mode 'company-tabnine)
(global-set-key (kbd "M-S") 'company-yasnippet)

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

(after! emojify-mode (setq! emojify-emoji-styles '(unicode)))
(setq ob-mermaid-cli-path "/home/gdquest/.local/bin/node_modules/mermaid.cli/index.bundle.js")

;; Keymaps
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
(map! :map global-map
      :g "C-s" 'save-buffer
      :g "C-S-s" 'projectile-save-project-buffers)
(map! :after ivy :map ivy-minibuffer-map "M-m" #'ivy-mark)



;; Loading configuration packages.
(require 'config-org)
(require 'config-markdown)
(require 'config-evil)
