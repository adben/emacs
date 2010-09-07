;;; guess-style-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (guess-style-guess-all guess-style-guess-variable
;;;;;;  guess-style-set-variable) "guess-style" "guess-style.el"
;;;;;;  (19483 29841))
;;; Generated autoloads from guess-style.el

(autoload (quote guess-style-set-variable) "guess-style" "\
Override VARIABLE's guessed value for future guesses.
If FILE is a directory, the variable will be overridden for the entire
directory, unless single files are later overridden.
If called interactively, the current buffer's file name will be used for FILE.
With a prefix argument a directory name may be entered.

\(fn VARIABLE VALUE FILE)" t nil)

(autoload (quote guess-style-guess-variable) "guess-style" "\
Guess a value for VARIABLE according to `guess-style-guesser-alist'.
If GUESSER is set, it's used instead of the default.

\(fn VARIABLE &optional GUESSER)" nil nil)

(autoload (quote guess-style-guess-all) "guess-style" "\
Guess all variables in `guess-style-guesser-alist'.
Special care is taken so no guesser is called twice.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("guess-style-pkg.el") (19483 29841 904628))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; guess-style-autoloads.el ends here
