;;; General settings
(delete-selection-mode 1)  ; Yank replaces the selected region
(global-display-line-numbers-mode)
(global-auto-revert-mode 1)  ; Auto revert/refresh file when change detected
(tool-bar-mode 0)
(setq ring-bell-function 'ignore)  ; Disable beep on C-g (keyboard-quit)

;;; General keybindings
;; set keys for Apple keyboard, for emacs in OS X
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;; Font
(set-frame-font "Iosevka Nerd Font 16" nil t)

;;; Packages
(setq package-list '(lsp-mode flycheck use-package company treesit-auto dap-mode
			      ligature modus-themes multiple-cursors magit gruvbox-theme
			      go-mode))

;; List the repositories containing packages
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
                      
;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; Theme
(load-theme 'gruvbox-dark-hard :no-confirm)

(eval-when-compile (require 'use-package))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package company
  :hook
  (after-init . global-company-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package go-mode)

(use-package lsp-mode
  :init
  ;; Set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; Replace XXX-mode with concrete major-mode (e.g. python-mode)
         ;; (typescript-ts-mode . lsp))
	 ;; (go-mode . lsp-deferred)
	 (prog-mode . lsp-deferred))
  :commands lsp)

(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package multiple-cursors
  :init
  (global-set-key (kbd "C->") 'mc/mark-next-like-this-word)
  (global-set-key (kbd "C-M->") 'mc/skip-to-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-M-<") 'mc/skip-to-previous-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(exec-path-from-shell go-mode flycheck gruvbox-theme magit company treesit-auto multiple-cursors modus-themes ligature darcula-theme dap-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
