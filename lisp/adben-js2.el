(require 'js2-mode)
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(setq js2-highlight-level 3)
;;Paredit for parentensis matching 
(define-key js-mode-map "{" 'paredit-open-curly)
(define-key js-mode-map "}" 'paredit-close-curly-and-newline)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

