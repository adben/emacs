;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm is incremental completion and selection narrowing framework for Emacs. It will help steer you in the right direction when you're looking for stuff in Emacs (like buffers, files, etc).
;; Helm is a fork of anything.el and can be considered to be its successor. Helm sets out to clean up the legacy code in anything.el and provide a cleaner, leaner and more modular tool, that's not tied in the trap of backward compatibility 
;;(load-file "~/.emacs.d/vendor/helm/helm.el")
;;(load-file "~/.emacs.d/vendor/helm/helm-config.el")
;;(load-file "~/.emacs.d/vendor/helm/helm-match-plugin.el")
;; (setq default-frame-alist '((vertical-scroll-bars . nil)
;;                             (tool-bar-lines . 0)
;;                             (menu-bar-lines . 0)
;;                             (fullscreen . nil)))
;; (blink-cursor-mode -1)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/elpa/helm-20121020.1209/"))
(require 'helm-config)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key lisp-interaction-mode-map [remap indent-for-tab-command] 'helm-lisp-completion-at-point-or-indent)
(define-key emacs-lisp-mode-map       [remap indent-for-tab-command] 'helm-lisp-completion-at-point-or-indent)
;;(add-hook 'kill-emacs-hook #'(lambda () (delete-file "$TMP")))
(cd "~/")
