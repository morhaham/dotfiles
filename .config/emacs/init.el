;; -*- lexical-binding: t; -*-
(setq custom-file "~/dotfiles/.config/emacs/emacs-custom.el")
(load custom-file)

;;; General settings
(setq read-process-output-max (* 1024 1024)) ; 1MB buffer size for fast LSP data transfers
(setq gc-cons-threshold (* 100 1024 1024))   ; Raise garbage collector threshold to speed up operations
;; Force Emacs to insert spaces instead of raw tab characters
(setq-default indent-tabs-mode nil)
;; Match Prettier's 2-space indentation width
(setq-default tab-width 2)

;; Set keys for Apple keyboard, for emacs in OS X
(setopt mac-command-modifier 'meta) ; Make cmd key do Meta
(setopt mac-option-modifier 'super) ; Make opt key do Super
(setopt mac-control-modifier 'control) ; Make Control key do Control
(setopt ns-function-modifier 'hyper) ; Make Fn key do Hyper
(windmove-default-keybindings 'meta) ; Move through windows with Ctrl-<arrow keys>

;; Ensure auto-save directory exists
(let ((autosave-dir (expand-file-name "auto-saves/" user-emacs-directory)))
  (unless (file-directory-p autosave-dir)
    (make-directory autosave-dir t)))

;; Redirect all auto-save files into ~/.config/emacs/auto-saves/
(setq auto-save-file-name-transforms 
      `((".*" ,"~/.config/emacs/auto-saves/" t)))

(delete-selection-mode 1) ; Yank replaces the selected region
(set-fringe-style 0) ; Fringes are the little gutters on the left and right sides of each window
(global-display-line-numbers-mode)
(setopt auto-revert-avoid-polling t) ; Automatically reread from disk if the underlying file changes
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setopt ring-bell-function 'ignore) ; Disable beep on C-g (keyboard-quit)
(setopt tab-width 4)
(setopt winner-mode t) ; Saves window configuration history, undo/redo history with C-c left/right
(setopt hl-line-mode t)
(setopt cursor-type 'bar)
;; Tab bar mode related
(setopt tab-bar-mode t)
(setopt tab-bar-show nil)

;; Font
(set-face-attribute 'default nil
                    :font (font-spec :family "FiraCode Nerd Font"
                                     :style "Retina"
                                     :size 13.0)) 
(set-face-attribute 'italic nil
                    :font (font-spec :family "VictorMono Nerd Font"
                                     :style "Medium Italic"))
(set-face-attribute 'bold nil
                    :font (font-spec :family "FiraCode Nerd Font"
                                     :style "Bold"))
(set-face-attribute 'bold-italic nil
                    :font (font-spec :family "VictorMono Nerd Font"
                                     :style "Medium Italic"))
(setopt line-spacing 0.3)

;;; General keybindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(global-set-key (kbd "C-x K") 'kill-other-buffers)
(global-set-key (kbd "C-x M-k") 'kill-buffer-and-window) ; Kill the buffer and close the window

;;; Pakcages
;; Elpaca package manager https://github.com/progfolio/elpaca
(setq elpaca-lock-file (expand-file-name "elpaca/lockfile.el" user-emacs-directory))
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; Tree-sitter grammers auto installer
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt) 
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Git
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; LSP
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (((tsx-ts-mode typescript-ts-mode python-ts-mode) . lsp-deferred))
  :commands (lsp lsp-deferred)
  :custom
  ;; Optimization tweaks for high-performance Node servers
  (lsp-use-plists t)
  (lsp-log-io nil)                ; Disable logging to improve memory usage
  (lsp-idle-delay 0.1)            ; Faster response times for autocomplete
  (lsp-headerline-breadcrumb-enable nil) ; Cleans up the top menu bar
  (lsp-completion-provider :none)
  (lsp-clients-typescript-prefer-use-project-ts-server t)
  ;; (lsp-typescript-server 'ts-ls)  ; Tells lsp-mode to use typescript-language-server

  (lsp-tailwindcss-add-on-mode t) ; Activates Tailwind as a concurrent secondary server
  :config
  ;; Native support for multi-root projects
  (setq lsp-auto-guess-root t))

;; 2. Dedicated UI & completion layers
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package yasnippet ;; Snippets
  :ensure t
  :config
  (yas-global-mode 1)
  :init
  (global-set-key (kbd "C-c y") 'company-yasnippet))

;; 3. ESLint configuration matching layer
(use-package lsp-eslint
  :ensure nil ; Part of lsp-mode
  :after lsp-mode)

(use-package go-mode
  :straight t
  :config
  (setq go-ts-mode-indent-offset 4))

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

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)                 ; show completions automatically as you type
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)          ; start after 1 character
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)         ; docs popup next to the candidate
  (corfu-history-mode))

;; Flexible matching: type "bg red" to find bg-red-500, etc.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil))

;; Recommended by the Corfu docs for Eglot: stops stale cached results
;; when the server returns a different list as you keep typing.
(use-package cape
  :ensure t
  :config
  (with-eval-after-load 'eglot
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)))


(use-package autothemer
  :ensure t
  :init
  (require 'json))


(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-dream t))

(use-package highlight-indent-guides
  :ensure t
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package consult
  :ensure t
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
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-flycheck
  :ensure t)

(use-package vertico ; Vertical completion UI
  :ensure t
  :init (vertico-mode t))

(use-package marginalia
  :ensure t
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


(use-package multiple-cursors
  :ensure t
  :init
  (global-set-key (kbd "C->") 'mc/mark-next-like-this-word)
  (global-set-key (kbd "C-M->") 'mc/skip-to-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this-word)
  (global-set-key (kbd "C-M-<") 'mc/skip-to-previous-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-c C-c") 'mc/edit-lines)
  (global-set-key (kbd "C-s-<mouse-1>") 'mc/add-cursor-on-click))

;; allows to 
(use-package wgrep
  :ensure t
  :bind (("C-x C-m" . wgrep-change-to-wgrep-mode)))

;; Terminal related
(use-package vterm
  :ensure t
  :config
  (require 'project)

  (defun my/vterm-buffer-name-from-project ()
    "Return a vterm buffer name based on the current project."
    (let* ((project (project-current))
           (name (if project
                     (file-name-nondirectory (directory-file-name (project-root project)))
                   "vterm")))
      (generate-new-buffer-name (format "*vterm: %s*" name))))

  (defun my/vterm-new-buffer-in-project ()
    "Open a new vterm buffer named after the current project."
    (interactive)
    (let ((default-directory (or (when-let ((proj (project-current)))
                                   (project-root proj))
                                 default-directory)))
      (vterm (my/vterm-buffer-name-from-project))))

  (defun my/vterm-vertical-split ()
    "Open a new vterm in a vertical split, using the current project root."
    (interactive)
    (split-window-right)
    (other-window 1)
    (my/vterm-new-buffer-in-project))

  (defun my/vterm-horizontal-split ()
    "Open a new vterm in a horizontal split, using the current project root."
    (interactive)
    (split-window-below)
    (other-window 1)
    (my/vterm-new-buffer-in-project))

  ;; Keybindings
  (global-set-key (kbd "C-c t") #'my/vterm-new-buffer-in-project)
  (global-set-key (kbd "C-c v") #'my/vterm-vertical-split)
  (global-set-key (kbd "C-c s") #'my/vterm-horizontal-split))

(use-package beacon
  :ensure t
  :init
  (beacon-mode 1))
