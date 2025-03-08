(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(dap-debug-template-configurations
   '(("Launch Executable" :type "go" :request "launch" :name
	  "Launch Executable" :mode "exec" :program nil :args nil :env nil)
	 ("Launch File" :type "go" :request "launch" :name "Launch File"
	  :mode "auto" :program nil :buildFlags nil :args nil :env nil)
	 ("dlv Go: Attach to running process" :type "go" :request "attach"
	  :name "Attach to running process" :mode "auto" :debugServer
	  "4004")
	 ("Attach to running process" :type "go" :request "attach" :name
	  "Attach to running process" :mode "auto" :debugServer 4004)
	 ("Go: Attach with Custom Port" :type "go" :request "attach" :name
	  "Attach with Custom Port" :mode "auto" :host "127.0.0.1" :port
	  4004)
	 ("Python :: Run pytest (at point)" :type "python" :args ""
	  :program nil :module "pytest" :request "launch" :name
	  "Python :: Run pytest (at point)")
	 ("Python :: Run pytest (buffer)" :type "python" :args "" :cwd nil
	  :program nil :module "pytest" :request "launch" :name
	  "Python :: Run pytest (buffer)")
	 ("Python :: Run file from project directory" :name
	  "Python :: Run file from project directory" :type "python" :args
	  "" :cwd "${workspaceFolder}" :module nil :program nil :request
	  "launch")
	 ("Python :: Run file (buffer)" :type "python" :args "" :cwd nil
	  :module nil :program nil :request "launch" :name
	  "Python :: Run file (buffer)")
	 ("Python :: Attach to running process" :type "python" :request
	  "attach" :processId "${command:pickProcess}" :name
	  "Python :: Attach to running process")
	 ("Go Dlv Test Current Subtest Configuration" :type "go" :request
	  "launch" :name "Test subtest" :mode "test" :program nil :args
	  nil :env nil)
	 ("Go Dlv Test Current Function Configuration" :type "go" :request
	  "launch" :name "Test function" :mode "test" :program nil :args
	  nil :env nil)
	 ("Go Dlv Remote Debug" :type "go" :request "attach" :name
	  "Dlv Remote Debug" :mode "remote")
	 ("Go Dlv Launch Executable Configuration" :type "go" :request
	  "launch" :name "Launch Executable" :mode "exec" :program nil
	  :args nil :env nil)
	 ("Go Dlv Attach Configuration" :type "go" :request "attach" :name
	  "Attach to running process" :mode "auto")
	 ("Go Dlv Launch File Configuration" :type "go" :request "launch"
	  :name "Launch File" :mode "auto" :program nil :buildFlags nil
	  :args nil :env nil)))
 '(dired-use-ls-dired nil)
 '(flycheck-clang-args nil)
 '(json-ts-mode-indent-offset 4)
 '(pug-tab-width 4)
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
