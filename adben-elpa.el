(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize)
  ;;Another repositories for elpa and... (gnus)
  ;;adding swank-clojure slime-connect for version
  (setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ("technomancy" . "http://repo.technomancy.us/emacs/")
                           ("marmalade" . "http://marmalade-repo.org/packages/"))))
