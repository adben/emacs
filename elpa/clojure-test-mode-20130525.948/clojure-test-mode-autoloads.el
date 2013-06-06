;;; clojure-test-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads (clojure-test-mode) "clojure-test-mode" "clojure-test-mode.el"
;;;;;;  (20899 7312 0 0))
;;; Generated autoloads from clojure-test-mode.el

(autoload 'clojure-test-mode "clojure-test-mode" "\
A minor mode for running Clojure tests.

\\{clojure-test-mode-map}

\(fn &optional ARG)" t nil)

(defun clojure-test-maybe-enable nil "\
Enable clojure-test-mode if the current buffer contains a \"clojure.test\" bit in it." (when (clojure-find-clojure-test) (save-window-excursion (clojure-test-mode t))))

(add-hook 'clojure-mode-hook 'clojure-test-maybe-enable)

;;;***

;;;### (autoloads nil nil ("clojure-test-mode-pkg.el") (20899 7312
;;;;;;  318332 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; clojure-test-mode-autoloads.el ends here
