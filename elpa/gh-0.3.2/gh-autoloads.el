;;; gh-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (gh-api-v3 gh-api-v2 gh-api) "gh-api" "gh-api.el"
;;;;;;  (20172 10769))
;;; Generated autoloads from gh-api.el

(eieio-defclass-autoload 'gh-api 'nil "gh-api" "Github API")

(eieio-defclass-autoload 'gh-api-v2 '(gh-api) "gh-api" nil)

(eieio-defclass-autoload 'gh-api-v3 '(gh-api) "gh-api" "Github API v3")

;;;***

;;;### (autoloads (gh-oauth-authenticator gh-password-authenticator
;;;;;;  gh-authenticator) "gh-auth" "gh-auth.el" (20172 10769))
;;; Generated autoloads from gh-auth.el

(eieio-defclass-autoload 'gh-authenticator 'nil "gh-auth" "Abstract authenticator")

(eieio-defclass-autoload 'gh-password-authenticator '(gh-authenticator) "gh-auth" "Password-based authenticator")

(eieio-defclass-autoload 'gh-oauth-authenticator '(gh-authenticator) "gh-auth" "Oauth-based authenticator")

;;;***

;;;### (autoloads (gh-gist-gist gh-gist-gist-stub gh-gist-api) "gh-gist"
;;;;;;  "gh-gist.el" (20172 10769))
;;; Generated autoloads from gh-gist.el

(eieio-defclass-autoload 'gh-gist-api '(gh-api-v3) "gh-gist" "Gist API")

(eieio-defclass-autoload 'gh-gist-gist-stub 'nil "gh-gist" "Class for user-created gist objects")

(eieio-defclass-autoload 'gh-gist-gist '(gh-gist-gist-stub) "gh-gist" "Gist object")

;;;***

;;;### (autoloads (gh-pulls-request gh-pulls-ref gh-pulls-api) "gh-pulls"
;;;;;;  "gh-pulls.el" (20172 10769))
;;; Generated autoloads from gh-pulls.el

(eieio-defclass-autoload 'gh-pulls-api '(gh-api-v3) "gh-pulls" "Git pull requests API")

(eieio-defclass-autoload 'gh-pulls-ref 'nil "gh-pulls" nil)

(eieio-defclass-autoload 'gh-pulls-request '(gh-pulls-request-stub) "gh-pulls" "Git pull requests API")

;;;***

;;;### (autoloads (gh-repos-repo gh-repos-repo-stub gh-repos-api)
;;;;;;  "gh-repos" "gh-repos.el" (20172 10769))
;;; Generated autoloads from gh-repos.el

(eieio-defclass-autoload 'gh-repos-api '(gh-api-v3) "gh-repos" "Repos API")

(eieio-defclass-autoload 'gh-repos-repo-stub 'nil "gh-repos" "Class for user-created repository objects")

(eieio-defclass-autoload 'gh-repos-repo '(gh-repos-repo-stub) "gh-repos" "Class for GitHub repositories")

;;;***

;;;### (autoloads nil nil ("gh-common.el" "gh-pkg.el" "gh.el") (20172
;;;;;;  10769 812502))

;;;***

(provide 'gh-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; gh-autoloads.el ends here
