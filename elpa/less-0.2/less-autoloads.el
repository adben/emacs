;;; less-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (less-minor-mode-off less-minor-mode-on auto-less-minor-mode
;;;;;;  less-minor-mode) "less" "less.el" (19575 44201))
;;; Generated autoloads from less.el

(autoload 'less-minor-mode "less" "\
Toggle less-minor-mode.

With less-minor-mode enabled, you could use `less' like keys to view files.
\\{less-minor-mode-map}.

\(fn &optional ARG)" t nil)

(autoload 'auto-less-minor-mode "less" "\
Auto enter `less-minor-mode' when visiting read-only files. You can
add this to `find-file-hooks'.

\(fn)" nil nil)

(autoload 'less-minor-mode-on "less" "\
Turn on `less-minor-mode'.

\(fn)" nil nil)

(autoload 'less-minor-mode-off "less" "\
Turn off `less-minor-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("less-pkg.el") (19575 44201 281200))

;;;***

(provide 'less-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; less-autoloads.el ends here
