;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm is incremental completion and selection narrowing framework for Emacs. It will help steer you in the right direction when you're looking for stuff in Emacs (like buffers, files, etc).
;; Helm is a fork of anything.el and can be considered to be its successor. Helm sets out to clean up the legacy code in anything.el and provide a cleaner, leaner and more modular tool, that's not tied in the trap of backward compatibility. 
(load-file "~/.emacs.d/vendor/helm/helm.el")
(load-file "~/.emacs.d/vendor/helm/helm-config.el")
(load-file "~/.emacs.d/vendor/helm/helm-match-plugin.el")
(require 'helm-config)
