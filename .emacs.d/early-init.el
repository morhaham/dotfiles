(setenv "LSP_USE_PLISTS" "true")
;; improves performance
;;https://emacs-lsp.github.io/lsp-mode/page/performance
(setopt read-process-output-max (* 1024 1024)) ;; 10mb
(setopt gc-cons-threshold 100000000)
(setopt frame-resize-pixelwise t)
(add-to-list 'default-frame-alist '(undecorated . t))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
