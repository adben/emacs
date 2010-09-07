;; ;; Adben Clojure shit
;; ;; Clojure
;; (add-to-list 'load-path "~/git/clojure-mode")
;; (add-to-list 'load-path "~/git/swank-clojure")
;; (add-to-list 'load-path "~/git/slime")
;; ;;(add-to-list 'load-path "~/git/slime")

;; ;;(add-to-list 'load-path "~/git/swak")
;; (setq swank-clojure-jar-path "~/git/clojure/clojure-1.2.0-master-SNAPSHOT.jar")
;; ;; clojure-contrib, when compiled as above, is not working at the moment
;; ;; (setq swank-clojure-extra-classpaths
;; ;;      (list "~/git/clojure-contrib/clojure-contrib.jar"))
;; (require 'clojure-mode)
;; (require 'swank-clojure)
;; (require 'slime)
;; (eval-after-load "slime" (slime-setup '(slime-repl)))
;; (slime-setup)
;; clojure-mode
(add-to-list 'load-path "~/git/clojure-mode")
(require 'clojure-mode)

;; swank-clojure
(add-to-list 'load-path "~/git/swank-clojure/")

(setq swank-clojure-jar-path "~/git/clojure/clojure-1.2.0-master-SNAPSHOT.jar"
      swank-clojure-extra-classpaths (list
				      "~/git/swank-clojure/src/main/clojure"
				      "~/git/clojure/clojure-1.2.0-master-SNAPSHOT.jar"))

(require 'swank-clojure-autoload)

;; slime
(eval-after-load "slime" 
  '(progn (slime-setup '(slime-repl))))

(add-to-list 'load-path "~/git/slime")
(require 'slime)
(slime-setup) 