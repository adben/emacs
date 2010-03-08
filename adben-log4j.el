;;; use log4j-mode when file ends in .log or has log4j at start
(autoload 'log4j-mode "log4j-mode" "Groovy editing mode." t)
(add-to-list 'auto-mode-alist '("\.log$" . log4j-mode))
(add-to-list 'interpreter-mode-alist '("log4j" . log4j-mode)
;;ToDo buffer-font
             )
