;;; confluence-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (confluence-search confluence-get-page) "confluence"
;;;;;;  "confluence.el" (19486 23000))
;;; Generated autoloads from confluence.el

(autoload (quote confluence-get-page) "confluence" "\
Loads a confluence page for the given SPACE-NAME and PAGE-NAME
into a buffer (if not already loaded) and switches to it.
Analogous to `find-file'.  Every time you navitage to a page with
this function (or M-. `confluence-get-page-at-point'), it is
saved off into a stack (`confluence-tag-stack') that you can then
pop back out of to return back through your navigation path (with
M-* `confluence-pop-tag-stack').

\(fn &optional PAGE-NAME SPACE-NAME ANCHOR-NAME)" t nil)

(autoload (quote confluence-search) "confluence" "\
Runs a confluence search for QUERY, optionally restricting the results to
the given SPACE-NAME.

\(fn &optional QUERY SPACE-NAME)" t nil)

;;;***

;;;### (autoloads nil nil ("confluence-pkg.el") (19486 23000 366975))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; confluence-autoloads.el ends here
