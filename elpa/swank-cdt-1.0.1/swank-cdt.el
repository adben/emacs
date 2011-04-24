;;; swank-cdt.el --- swank-cdt helper functions
 
;; Copyright 2011 George Jahad
 
;; Author: George Jahad
;; Version: 1.0.1


(defun sldb-line-bp ()
  "Set breakpoint on current buffer line."
  (interactive)
  (slime-eval-async `(swank:sldb-line-bp
                      ,(buffer-file-name) ,(line-number-at-pos))))

(defun slime-force-continue ()
  "force swank server to continue"
  (interactive)
  (slime-dispatch-event `(:emacs-interrupt :cdt)))

(defun slime-get-thing-at-point ()
  (interactive)
  (let ((thing (thing-at-point 'sexp)))
    (set-text-properties 0 (length thing) nil thing)
    thing))

(defun slime-eval-last-frame ()
  "Eval thing at point in the context of the last frame viewed"
  (interactive)
  (slime-eval-with-transcript `(swank:eval-last-frame
                                ,(slime-get-thing-at-point))))

(define-prefix-command 'cdt-map)
(define-key cdt-map (kbd "C-b") 'sldb-line-bp)
(define-key cdt-map (kbd "C-g") 'slime-force-continue)
(define-key cdt-map (kbd "C-p") 'slime-eval-last-frame)

(eval-after-load 'slime
  '(progn
     (define-key slime-mode-map (kbd "C-c C-x") 'cdt-map)
     (define-key sldb-mode-map (kbd "C-c C-x") 'cdt-map)))

(eval-after-load 'slime-repl
  '(define-key slime-repl-mode-map (kbd "C-c C-x") 'cdt-map))

(eval-after-load 'cc-mode
  '(define-key java-mode-map (kbd "C-c C-x") 'cdt-map))

;;; swank-cdt.el ends here
