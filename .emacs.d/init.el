
;; General settings
(delete-selection-mode 1)
(global-display-line-numbers-mode)
(global-auto-revert-mode 1) ;; auto revert/refresh file when change detected
(tool-bar-mode 0)


;; Font
(set-frame-font "Iosevka Nerd Font 16" nil t)

;; Theme
(load-theme 'modus-vivendi :no-confirm)
;; Packages
(setq package-list '(lsp-mode use-package company treesit-auto dap-mode ligature modus-themes multiple-cursors magit))

; list the repositories containing packages
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))
                      
; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(eval-when-compile  (require 'use-package))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package comapny
 :hook
  (after-init . global-company-mode))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;; (typescript-ts-mode . lsp))
	 (prog-mode . lsp))
  :commands lsp)

(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit company treesit-auto multiple-cursors modus-themes ligature darcula-theme dap-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
