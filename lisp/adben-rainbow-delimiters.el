;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; toward balanced and colorful delimiters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elpa/rainbow-delimiters-20120428.45/")
(when (require 'rainbow-delimiters nil 'noerror) 
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode))

(provide 'adben-rainbow-delimiters)
