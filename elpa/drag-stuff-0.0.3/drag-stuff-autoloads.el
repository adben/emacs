;;; drag-stuff-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (drag-stuff-global-mode turn-off-drag-stuff-mode
;;;;;;  turn-on-drag-stuff-mode drag-stuff-mode) "drag-stuff" "drag-stuff.el"
;;;;;;  (19546 37088))
;;; Generated autoloads from drag-stuff.el

(autoload 'drag-stuff-mode "drag-stuff" "\
Drag stuff around.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-drag-stuff-mode "drag-stuff" "\
Turn on `drag-stuff-mode'

\(fn)" t nil)

(autoload 'turn-off-drag-stuff-mode "drag-stuff" "\
Turn off `drag-stuff-mode'

\(fn)" t nil)

(defvar drag-stuff-global-mode nil "\
Non-nil if Drag-Stuff-Global mode is enabled.
See the command `drag-stuff-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `drag-stuff-global-mode'.")

(custom-autoload 'drag-stuff-global-mode "drag-stuff" nil)

(autoload 'drag-stuff-global-mode "drag-stuff" "\
Toggle Drag-Stuff mode in every possible buffer.
With prefix ARG, turn Drag-Stuff-Global mode on if and only if
ARG is positive.
Drag-Stuff mode is enabled in all buffers where
`turn-on-drag-stuff-mode' would do it.
See `drag-stuff-mode' for more information on Drag-Stuff mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("drag-stuff-pkg.el") (19546 37088 481573))

;;;***

(provide 'drag-stuff-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; drag-stuff-autoloads.el ends here
