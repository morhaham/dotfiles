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

;; Language sepecific settings
(setq typescript-ts-mode-indent-offset nil) ;Disable auto indentation(the default conflicts with Prettier)

;; improves performance
;; per https://github.com/emacs-lsp/lsp-mode#performance
;; (setq read-process-output-max (* 10 1024 1024)) ;; 10mb
;; (setq gc-cons-threshold 200000000)

;;; General keybindings
;; set keys for Apple keyboard, for emacs in OS X
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper
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

(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package gruvbox-theme
  :straight t
  :init
  (load-theme 'gruvbox-dark-hard :no-confirm))


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
  (yas-global-mode 1))

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
              ("TAB" . 'lsp-ui-doc-focus-frame))
  :config (setq lsp-ui-doc-enable t
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))

(use-package lsp-eslint
  :demand t
  :after lsp-mode)

(use-package apheleia
  :straight t
  :init
  (apheleia-global-mode +1))

;; LSP
(use-package lsp-mode
  :straight t
  :init
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  :hook (
	 (prog-mode . lsp))
  :custom (
 	   (lsp-log-io t)                     ; IMPORTANT! Use only for debugging! Drastically affects performance
	   (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
	   ;; core
	   (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
	   (lsp-enable-dap-auto-configure t)     ; Debug support
	   (lsp-enable-file-watchers nil)
	   (lsp-enable-indentation nil)          ; I use prettier
	   (lsp-enable-links nil)                ; No need since we have `browse-url'
	   (lsp-enable-on-type-formatting nil)   ; Prettier handles this
           (lsp-typescript-format-enable nil)
           (lsp-javascript-format-enable nil)
	   (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
	   (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
	   (lsp-enable-text-document-color nil)   ; This is Treesitter's job
	   (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
	   (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
	   ;; completion
	   (lsp-enable-snippet t)                         ; Important to provide full JSX completion
	   (lsp-completion-show-kind t)                   ; Optional
	   ;; headerline
	   (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
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

