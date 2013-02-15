;; (when
;;     (load 
;;      (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize)
;;   ;;Another repositories for elpa and... (gnus)
;;   (setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
;;                            ("gnu" . "http://elpa.gnu.org/packages/")
;;                            ("technomancy" . "http://repo.technomancy.us/emacs/")
;;                            ("marmalade" . "http://marmalade-repo.org/packages/"))))
(package-initialize)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("technomancy" . "http://repo.technomancy.us/emacs/"))
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; issue with emacs24 => (setq url-http-attempt-keepalives nil)
;; There are a number of small bugs in Emacs24’s package.el.
;; First, when installing dependencies, the packages were
;; not getting initialized. Second, the order for dependencies
;; was coming out backwards for what I needed. Both problems
;; are easily patched by some advice.
;; via http://melpa.milkbox.net/
;; (progn
;;   (switch-to-buffer
;;    (url-retrieve-synchronously
;;     "https://raw.github.com/milkypostman/melpa/master/melpa.el"))
;;   (package-install-from-buffer  (package-buffer-info) 'single))

(defadvice package-compute-transaction
  (before
   package-compute-transaction-reverse (package-list requirements)
   activate compile)
  "reverse the requirements"
  (setq requirements (reverse requirements))
  (print requirements))

(defadvice package-download-tar
  (after package-download-tar-initialize activate compile)
  "initialize the package after compilation"
  (package-initialize))

(defadvice package-download-single
  (after package-download-single-initialize activate compile)
  "initialize the package after compilation"
  (package-initialize))
