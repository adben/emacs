(require 'rainbow-delimiters)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
;;Make return automatically indent
(add-hook 'lisp-mode-hook (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'emacs-lisp-mode-hook (lambda () (local-set-key (kbd "RET") 'newline-and-indent)))
;;auto-compile elisp files
;;see http://xahlee.blogspot.com/2011/07/emacs-unique-buffer-names-auto-compile.html
(defun auto-recompile-el-buffer ()
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))
(add-hook 'after-save-hook 'auto-recompile-el-buffer)
