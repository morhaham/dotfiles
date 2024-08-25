;;; General settings
(delete-selection-mode 1)  ; Yank replaces the selected region
(global-display-line-numbers-mode)
(global-auto-revert-mode 1)  ; Auto revert/refresh file when change detected
(tool-bar-mode 0)            ; Hide top toolbar
(setq ring-bell-function 'ignore)  ; Disable beep on C-g (keyboard-quit)
(defun infer-indentation-style ()
  "If our source file use tabs, we use tabs.
if spaces spaces, and if neither, we use the current `indent-tabs-mode`"
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))
(setq-default indent-tabs-mode nil)    ; Use spaces instead of tabs
(infer-indentation-style)
(electric-pair-mode 1)
(setopt sentence-end-double-space nil) ; Fix archaic defaults(default is not recommended anymore)


;; improves performance
;;https://emacs-lsp.github.io/lsp-mode/page/performance
(setq read-process-output-max (* 1024 1024)) ;; 10mb
(setq gc-cons-threshold 100000000)

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
;; taken from emacs-bedrock:
;; https://codeberg.org/ashton314/emacs-bedrock/src/branch/main/init.el
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path(FPATH).
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)

;;; General keybindings
;; set keys for Apple keyboard, for emacs in OS X
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper) ; make Fn key do Hyper
(windmove-default-keybindings 'meta) ; Move through windows with Ctrl-<arrow keys>
(global-set-key (kbd "M-o") 'other-window)
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(global-set-key (kbd "C-x K") 'kill-other-buffers)

;;; Font
(set-frame-font "Iosevka Nerd Font 16" nil t)

;; Packages
;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
(straight-use-package 'use-package)

(use-package move-text
  :straight t
  :init
  (move-text-default-bindings))


(use-package treesit
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ;; More modes defined here...
         )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (bash "https://github.com/tree-sitter/tree-sitter-bash")
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
               (markdown "https://github.com/ikatyang/tree-sitter-markdown")
               (make "https://github.com/alemuller/tree-sitter-make")
               (elisp "https://github.com/Wilfred/tree-sitter-elisp")
               (cmake "https://github.com/uyha/tree-sitter-cmake")
               (c "https://github.com/tree-sitter/tree-sitter-c")
               (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
               (toml "https://github.com/tree-sitter/tree-sitter-toml")
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
               (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package gruber-darker-theme
  :straight (gruber-darker-theme :type git :host github :repo "rexim/gruber-darker-theme")
  :init
  (load-theme 'gruber-darker :no-confirm))

(use-package magit
  :straight t)

(use-package multiple-cursors
  :straight t
  :init
  (global-set-key (kbd "C->") 'mc/mark-next-like-this-word)
  (global-set-key (kbd "C-M->") 'mc/skip-to-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-M-<") 'mc/skip-to-previous-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click))

;; Code Completion
;; (use-package corfu
;;   :ensure t
;;   :init
;;   (global-corfu-mode)
;;   (corfu-popupinfo-mode) ; Popup completion info
;;   )

(use-package yasnippet ;; Snippets
  :straight t
  :config
  (yas-global-mode 1)
  :init
  (global-set-key (kbd "C-c y") 'company-yasnippet))
;; :hook
;; tsx-ts-mode . 'yas-minor-mode)

(use-package yasnippet-snippets         ; Collection of snippets
  :straight t)

(use-package company
  :straight t
  :hook
  (after-init . global-company-mode))

;; Linter
(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error)))

(use-package go-mode
  :straight t
  )

(use-package lsp-ui
  :straight t
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance)
              ("TAB" . 'lsp-ui-doc-focus-frame)
              ("C-c C-a" . 'lsp-ui-flycheck-list))
  :config (setq lsp-ui-doc-enable t
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))

(use-package lsp-eslint
  :demand t
  :after lsp-mode)

;; Use formatting tools like Prettier easily (format on save by default)
(use-package apheleia
  :straight t
  :init
  ;; Enable Apheleia
  (apheleia-global-mode +1)
  ;; Prevent Apheleia from respecting the buffer's indent level
  (setq apheleia-formatters-respect-indent-level nil))

;; Critical package for commenting jsx/tsx
(use-package jtsx
  :straight t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :custom
  (jtsx-enable-jsx-element-tags-auto-sync t)
  :commands jtsx-install-treesit-language
  :hook ((jtsx-jsx-mode . hs-minor-mode)
         (jtsx-tsx-mode . hs-minor-mode)
         (jtsx-typescript-mode . hs-minor-mode)))


(use-package lsp-tailwindcss
  :straight '(lsp-tailwindcss :type git :host github :repo "merrickluo/lsp-tailwindcss")
  :init (setq lsp-tailwindcss-add-on-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode
             clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

;; LSP
(use-package lsp-mode
  :straight t
  :init
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq lsp-use-plists t)
  :hook (
	 (prog-mode . lsp))
  :custom (
 	   (lsp-log-io nil)                     ; IMPORTANT! Use only for debugging! Drastically affects performance
	   (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
	   ;; core
	   (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
	   (lsp-enable-dap-auto-configure t)     ; Debug support
	   (lsp-enable-file-watchers nil)
	   (lsp-enable-indentation t)            ; Fallback to Apheleia formatters
	   (lsp-enable-links nil)                ; No need since we have `browse-url'
	   (lsp-enable-on-type-formatting nil)
           (lsp-typescript-format-enable t)
           (lsp-javascript-format-enable t)
	   (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
	   (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
	   (lsp-enable-text-document-color nil)   ; This is Treesitter's job
	   (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
	   (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
           (lsp-auto-execute-action nil) ;Disable automatic code actions
	   ;; completion
	   (lsp-enable-snippet t)                         ; Important to provide full JSX completion
	   (lsp-completion-show-kind t)                   ; Optional
	   ;; headerline
	   (lsp-headerline-breadcrumb-enable nil)  ; Optional, I like the breadcrumbs
	   (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
	   (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
	   (lsp-headerline-breadcrumb-icons-enable nil)
	   ;; modeline
	   (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
	   (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
	   (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
	   (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
	   (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
	   (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
	   ;; lens
	   (lsp-lens-enable nil)                 ; Optional, I don't need it
	   ;; semantic
	   (lsp-semantic-tokens-enable nil)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
