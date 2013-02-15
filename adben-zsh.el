;;; use shell-script-mode for zsh when file ends in .zsh or .zsh-theme or has #compdef at start
(autoload 'shell-script-mode "shell-script-mode" "shell-script-mode for zsh" t)
(setq shell-file-name "zsh") ;;use zsh for shell (might change to bash? naah...)
(add-to-list 'auto-mode-alist '("\\.zsh-theme$" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))
(add-to-list 'interpreter-mode-alist '("compdef" . shell-script-mode))
