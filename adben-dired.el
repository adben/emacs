(require 'dired)
;Make sure each dired buffer doesn't spawn new dired buffers
(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
  loaded."
  ;; <add other stuff here>
  ;; (define-key dired-mode-map [return] 'joc-dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse))
;; (define-key dired-mode-map "^"
;;   (function
;;    (lambda nil (interactive)
;;      (joc-dired-single-buffer "")
;;      ;;(joc-dired-single-buffer "..")
;;      ))))
;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))
;;enable recursive deletion of dirs, but doubly ask if it's not empty.
(setq dired-recursive-deletes 'top)
;;By default Emacs will pass -exec to find and that makes it very slow.
;;It is better to collate the matches and then use xargs to run the command.
(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
