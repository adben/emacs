;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; toward balanced and colorful delimiters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (require 'rainbow-delimiters nil 'noerror) 
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode))

(provide 'adben-rainbow-delimiters)
