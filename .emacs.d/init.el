(setq custom-file "~/dotfiles/.emacs.d/emacs-custom.el")
(load custom-file)

;;; General settings
(delete-selection-mode 1)  ; Yank replaces the selected region
(set-fringe-mode 14)
(global-display-line-numbers-mode)
;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setopt ring-bell-function 'ignore)  ; Disable beep on C-g (keyboard-quit)
(setopt tab-width 4)
(setopt winner-mode t) ; Saves window configuration history, undo/redo history with C-c left/right

;; Tab bar mode related
(setopt tab-bar-mode t)
(setopt tab-bar-history-mode t)
(setopt tab-bar-show nil)
(global-set-key (kbd "M-[") 'tab-bar-history-back)
(global-set-key (kbd "M-]") 'tab-bar-history-forward)

;; (setopt pixel-scroll-precision-large-scroll-height 40.0)
(defun infer-indentation-style ()
  "If our source file use tabs, we use tabs.
if spaces spaces, and if neither, we use the current `indent-tabs-mode`"
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setopt indent-tabs-mode nil))
    (if (> tab-count space-count) (setopt indent-tabs-mode t))))
(infer-indentation-style)
(electric-pair-mode 1)
(setopt sentence-end-double-space nil) ; Fix archaic defaults(default is not recommended anymore)

;; Ido mode - commented out since using Consult
;; (ido-mode 1)
;; (setopt ido-enable-flex-matching t)
;; (setopt ido-everywhere t)
;; (setopt ido-use-filename-at-point 'guess)
;; (setopt ido-use-url-at-point nil)

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
;; taken from emacs-bedrock:
;; https://codeberg.org/ashton314/emacs-bedrock/src/branch/main/init.el
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path(FPATH).
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; Remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)

;;; General keybindings
;; Set keys for Apple keyboard, for emacs in OS X
(setopt mac-command-modifier 'meta) ; Make cmd key do Meta
(setopt mac-option-modifier 'super) ; Make opt key do Super
(setopt mac-control-modifier 'control) ; Make Control key do Control
(setopt ns-function-modifier 'hyper) ; Make Fn key do Hyper
(windmove-default-keybindings 'meta) ; Move through windows with Ctrl-<arrow keys>

(global-set-key (kbd "M-o") 'other-window)
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(global-set-key (kbd "C-x K") 'kill-other-buffers)
(global-set-key (kbd "C-x C-g") 'rgrep) ; Open rgrep in minibuffer
(global-set-key (kbd "C-x M-k") 'kill-buffer-and-window) ; Kill the buffer and close the window

(require 'recentf) ; Disabled while using Consult
;; enable recent files mode.
(recentf-mode t)

;;; Font 
;; (set-frame-font "Iosevka Nerd Font 16" t t)
;; (set-frame-font "Monaspace Neon Var 16" nil t)
(set-frame-font "JetBrains Mono 14" nil t)
(setopt line-spacing 0.3)

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

;; Scroll without moving the cursor position
(use-package scroll-page-without-moving-point
  :straight (:host github :repo "tanrax/scroll-page-without-moving-point.el" :files ("scroll-page-without-moving-point.el"))
  :config
  (global-set-key "\C-v" 'scroll-page-without-moving-point-down)
  (global-set-key "\M-v" 'scroll-page-without-moving-point-up)
  :ensure t)

;; Highlights the cursor line on special movements
(use-package beacon
  :straight t
  :init
  (beacon-mode 1))

;; Smooth scroll and focus mode(centers the window)
(use-package sublimity
  :straight t
  :init
  (require 'sublimity-attractive)
  (require 'sublimity-scroll)
  ;; (require 'sublimity-map)
  :config
  (sublimity-mode 1)
  (setq sublimity-attractive-centering-width 180))

(use-package autothemer
  :straight t)

(use-package rose-pine-theme
  :straight (rose-pine-theme :type git :host github :repo "konrad1977/pinerose-emacs")
  :after autothemer
  :init (load-theme 'rose-pine t))

(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :custom
  (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1))
  (indent-bars-highlight-current-depth '(:pattern "." :pad 0.1 :width 0.45))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
									   if_statement with_statement while_statement)))
  ;; Note: wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;				      list list_comprehension
  ;;				      dictionary dictionary_comprehension
  ;;				      parenthesized_expression subscript)))
  :hook ((python-ts-mode yaml-mode js-mode emacs-elisp-mode) . indent-bars-mode))

(use-package jsdoc
  :straight (:host github :repo "isamert/jsdoc.el"))

(use-package exec-path-from-shell
  :straight t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package project)

(use-package vterm
  :straight t)

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package move-text
  :straight t
  :init
  (global-set-key (kbd "s-<up>") 'move-text-up)
  (global-set-key (kbd "s-<down>") 'move-text-down)
  (defun indent-region-advice (&rest ignored)
    (let ((deactivate deactivate-mark))
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (line-beginning-position) (line-end-position)))
      (setq deactivate-mark deactivate)))
  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice))

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

(use-package pug-mode
  :straight t)
;; :config
;; ((add-to-list 'auto-mode-alist '("\\.pug\\'" . pug-mode))
;;  add-to-list 'auto-mode-alist '("\\.\\'" . pug-mode)))

;; (use-package gruber-darker-theme
;;   :straight (gruber-darker-theme :type git :host github :repo "rexim/gruber-darker-theme")
;;   :init
;;   (load-theme 'gruber-darker :no-confirm))

(use-package wgrep
  :straight t
  :bind (("C-x C-m" . wgrep-change-to-wgrep-mode)))

(use-package magit
  :straight t)

(use-package multiple-cursors
  :straight t
  :init
  (global-set-key (kbd "C->") 'mc/mark-next-like-this-word)
  (global-set-key (kbd "C-M->") 'mc/skip-to-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this-word)
  (global-set-key (kbd "C-M-<") 'mc/skip-to-previous-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c C-c") 'mc/edit-lines)
  (global-set-key (kbd "C-s-<mouse-1>") 'mc/add-cursor-on-click))

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
;; tsx-ts-mode . 'yas-minor-mode))

(use-package yasnippet-snippets         ; Collection of snippets
  :straight t)

(use-package company
  :straight t
  :config
  (setq company-backends '((company-capf company-yasnippet)))
  :hook
  (after-init . global-company-mode))

;; Linter
(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error)))

(use-package go-mode
  :straight t
  :config
  (setq go-ts-mode-indent-offset 4))

(use-package lsp-ui
  :straight t
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance)
              ("M-i" . 'lsp-ui-doc-focus-frame)
              ("C-c C-a" . 'lsp-ui-flycheck-list)) ;Show workspace diagnostics
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-cursor nil)      ; Don't show doc when cursor is over symbol - too distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-doc-position 'at-point))

(use-package lsp-eslint
  :demand t
  :after lsp-mode)

;; Use formatting tools like Prettier easily (format on save by default)
(use-package apheleia
  :straight t
  :hook (after-init . apheleia-global-mode)
  :config
  (setq apheleia-log-debug-info t)
  ;; Use 'ruff' instead of 'black'. Remove 'ruff-isort' when 'ruff format'
  ;; supports it.
  ;; Check - https://docs.astral.sh/ruff/formatter/#sorting-imports
  ;; https://github.com/astral-sh/ruff/issues/8232
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))


;; Critical package for commenting jsx/tsx
(use-package jtsx
  :straight t
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :custom
  (typescript-ts-mode-indent-offset 4)
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
	       ;; Core
           (lsp-warn-no-matched-clients nil)
	       (lsp-eldoc-enable-hover nil)          ; Hide signature information in the echo area
	       (lsp-enable-dap-auto-configure t)     ; Debug support
	       (lsp-enable-file-watchers nil)
	       (lsp-enable-indentation nil)
	       (lsp-enable-links nil)                ; No need since we have `browse-url'
	       (lsp-enable-on-type-formatting nil)   ; This is Apheleia job
	       (lsp-enable-suggest-server-download nil) ; Useful prompt to download LSP providers
	       (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
	       (lsp-enable-text-document-color nil)   ; This is Treesitter's job
           (lsp-auto-execute-action nil) ;Disable automatic code actions
	       ;; Completion
	       (lsp-enable-snippet t)                         ; Important to provide full JSX completion
	       (lsp-completion-show-kind t)                   ; Optional
	       ;; Headerline
	       (lsp-headerline-breadcrumb-enable nil)
	       (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
	       (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
	       (lsp-headerline-breadcrumb-icons-enable nil)
	       ;; Modeline
	       (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
	       (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
	       (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
	       (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
	       (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
	       ;; lens
	       (lsp-lens-enable nil)                 ; Optional, I don't need it
	       ;; semantic
	       (lsp-semantic-tokens-enable nil)
           ;; Javascript/Typescript
           (lsp-typescript-format-enable nil)
           (lsp-javascript-format-enable nil)
           ;; Python
           (lsp-pylsp-plugins-pydocstyle-enabled nil)
           (lsp-pylsp-plugins-flake8-enabled nil)
           (lsp-pylsp-configuration-sources nil)))

(use-package vertico ; Vertical completion UI
  :straight t
  :init (vertico-mode t))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :straight t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package embark
  :straight t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless flex))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult-flycheck
  :straight t)

(use-package consult
  :straight t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )
