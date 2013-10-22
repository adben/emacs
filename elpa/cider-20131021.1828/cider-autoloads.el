;;; cider-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "cider" "cider.el" (21094 11875 0 0))
;;; Generated autoloads from cider.el

(autoload 'cider-jack-in "cider" "\
Start a nREPL server for the current project and connect to it.
If PROMPT-PROJECT is t, then prompt for the project for which to
start the server.

\(fn &optional PROMPT-PROJECT)" t nil)

(autoload 'cider "cider" "\
Connect to an nREPL server identified by HOST and PORT.

\(fn HOST PORT)" t nil)

(eval-after-load 'clojure-mode '(progn (define-key clojure-mode-map (kbd "C-c M-j") 'cider-jack-in) (define-key clojure-mode-map (kbd "C-c M-c") 'cider)))

;;;***

;;;### (autoloads nil "cider-mode" "cider-mode.el" (21094 11875 0
;;;;;;  0))
;;; Generated autoloads from cider-mode.el

(autoload 'cider-mode "cider-mode" "\
Minor mode for REPL interaction from a Clojure buffer.

\\{cider-mode-map}

\(fn &optional ARG)" t nil)

(defalias 'cider-interaction-mode 'cider-mode)

(defalias 'cider-interaction-mode-hook 'cider-mode-hook)

;;;***

;;;### (autoloads nil "nrepl-client" "nrepl-client.el" (21094 11875
;;;;;;  0 0))
;;; Generated autoloads from nrepl-client.el

(autoload 'nrepl-enable-on-existing-clojure-buffers "nrepl-client" "\
Enable interaction mode on existing Clojure buffers.
See command `nrepl-interaction-mode'.

\(fn)" t nil)

(autoload 'nrepl-disable-on-existing-clojure-buffers "nrepl-client" "\
Disable interaction mode on existing Clojure buffers.
See command `nrepl-interaction-mode'.

\(fn)" t nil)

(add-hook 'nrepl-connected-hook 'nrepl-enable-on-existing-clojure-buffers)

;;;***

;;;### (autoloads nil nil ("cider-eldoc.el" "cider-interaction.el"
;;;;;;  "cider-macroexpansion.el" "cider-pkg.el" "cider-repl-mode.el"
;;;;;;  "cider-repl.el" "cider-selector.el" "cider-version.el") (21094
;;;;;;  11875 463339 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; cider-autoloads.el ends here
