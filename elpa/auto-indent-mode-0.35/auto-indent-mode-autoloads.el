;;; auto-indent-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (auto-indent-global-mode auto-indent-minor-mode-on
;;;;;;  auto-indent-eol-char-newline auto-indent-eol-newline) "auto-indent-mode"
;;;;;;  "auto-indent-mode.el" (19877 39348))
;;; Generated autoloads from auto-indent-mode.el

(autoload 'auto-indent-eol-newline "auto-indent-mode" "\
*Auto-indent function for end-of-line and then newline.

\(fn)" t nil)

(autoload 'auto-indent-eol-char-newline "auto-indent-mode" "\
* Auto-indent function for end-of-line, insert `auto-indent-eol-char', and then newline

\(fn)" t nil)

(defalias 'auto-indent-mode 'auto-indent-minor-mode)

(autoload 'auto-indent-minor-mode-on "auto-indent-mode" "\
* Turn on auto-indent minor mode.

\(fn)" nil nil)

(defvar auto-indent-global-mode nil "\
Non-nil if Auto-Indent-Global mode is enabled.
See the command `auto-indent-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `auto-indent-global-mode'.")

(custom-autoload 'auto-indent-global-mode "auto-indent-mode" nil)

(autoload 'auto-indent-global-mode "auto-indent-mode" "\
Toggle Auto-Indent minor mode in every possible buffer.
With prefix ARG, turn Auto-Indent-Global mode on if and only if
ARG is positive.
Auto-Indent minor mode is enabled in all buffers where
`auto-indent-minor-mode-on' would do it.
See `auto-indent-minor-mode' for more information on Auto-Indent minor mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("auto-indent-mode-pkg.el") (19877 39348
;;;;;;  448169))

;;;***

(provide 'auto-indent-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; auto-indent-mode-autoloads.el ends here
