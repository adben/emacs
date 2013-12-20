;;; multi-project-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "multi-project" "multi-project.el" (21148 48185
;;;;;;  0 0))
;;; Generated autoloads from multi-project.el

(autoload 'multi-project-compile "multi-project" "\
Compiles a project based upon the current directory of the buffer.

\(fn)" t nil)

(autoload 'multi-project-root "multi-project" "\
Jumps to the root of a project based upon current directory.

\(fn)" t nil)

(autoload 'multi-project-change-tags "multi-project" "\
Visits tags file based upon current directory

\(fn &optional PROJECT)" t nil)

(autoload 'multi-project-last "multi-project" "\
Jumps to the last chosen project

\(fn)" t nil)

(autoload 'multi-project-anchor "multi-project" "\
Chooses a project that will be constant no matter the default directory

\(fn)" t nil)

(autoload 'multi-project-reset-anchor "multi-project" "\
Resets the multi-project-anchored variable.

\(fn)" t nil)

(autoload 'multi-project-display-change-tags "multi-project" "\


\(fn)" t nil)

(autoload 'multi-project-display-projects "multi-project" "\
Displays a buffer with the various projects

\(fn)" t nil)

(autoload 'multi-project-find-file "multi-project" "\
Search a TAGS file for a particular file that match a user's input.

\(fn)" t nil)

(defadvice find-tag (before multi-project-find-tag (TAGNAME &optional NEXT-P REGEXP-P)) "\
Determine which TAGS file should be used based upon the current directory." (let ((project (multi-project-find-by-directory))) (when project (multi-project-change-tags (car project)))))

(defvar global-multi-project-mode nil "\
Non-nil if Global-Multi-Project mode is enabled.
See the command `global-multi-project-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-multi-project-mode'.")

(custom-autoload 'global-multi-project-mode "multi-project" nil)

(autoload 'global-multi-project-mode "multi-project" "\
Toggle multi-project mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; multi-project-autoloads.el ends here
