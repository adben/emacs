;;; highline-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (defhlsep highline-fill highline-raw defhltext
;;;;;;  highline-concat highline-mouse highline-hud) "highline" "highline.el"
;;;;;;  (20461 27059 0 0))
;;; Generated autoloads from highline.el

(autoload 'highline-hud "highline" "\


\(fn FACE1 FACE2)" nil nil)

(autoload 'highline-mouse "highline" "\


\(fn CLICK-GROUP CLICK-TYPE STRING)" nil nil)

(autoload 'highline-concat "highline" "\
Concatonate STRINGS and pad sides by spaces.

\(fn &rest STRINGS)" nil nil)

(autoload 'defhltext "highline" "\


\(fn NAME BODY)" nil t)

(autoload 'highline-raw "highline" "\


\(fn STR &optional FACE PAD)" nil nil)

(autoload 'highline-fill "highline" "\


\(fn FACE RESERVE)" nil nil)

(defhltext highline-major-mode (propertize mode-name 'help-echo "Major mode\nmouse-1: Display major mode menu\nmouse-2: Show help for major mode\nmouse-3: Toggle minor modes" 'local-map (let ((map (make-sparse-keymap))) (define-key map [mode-line down-mouse-1] `(menu-item ,(purecopy "Menu Bar") ignore :filter (lambda (_) (mouse-menu-major-mode-map)))) (define-key map [mode-line mouse-2] 'describe-mode) (define-key map [mode-line down-mouse-3] mode-line-mode-menu) map)))

(defhltext highline-minor-modes (mapconcat (lambda (mm) (propertize mm 'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes" 'local-map (let ((map (make-sparse-keymap))) (define-key map [mode-line down-mouse-1] (highline-mouse 'minor 'menu mm)) (define-key map [mode-line mouse-2] (highline-mouse 'minor 'help mm)) (define-key map [mode-line down-mouse-3] (highline-mouse 'minor 'menu mm)) (define-key map [header-line down-mouse-3] (highline-mouse 'minor 'menu mm)) map))) (split-string (format-mode-line minor-mode-alist)) " "))

(defhltext highline-narrow (let (real-point-min real-point-max) (save-excursion (save-restriction (widen) (setq real-point-min (point-min) real-point-max (point-max)))) (when (or (/= real-point-min (point-min)) (/= real-point-max (point-max))) (propertize "Narrow" 'help-echo "mouse-1: Remove narrowing from the current buffer" 'local-map (make-mode-line-mouse-map 'mouse-1 'mode-line-widen)))))

(defhltext highline-vc (when (and (buffer-file-name (current-buffer)) vc-mode) (format-mode-line '(vc-mode vc-mode))))

(defhltext highline-buffer-size (propertize (if highline-buffer-size-suffix "%I" "%i") 'local-map (make-mode-line-mouse-map 'mouse-1 (lambda nil (interactive) (setq highline-buffer-size-suffix (not highline-buffer-size-suffix)) (redraw-modeline)))))

(defhltext highline-buffer-id (format-mode-line mode-line-buffer-identification))

(autoload 'defhlsep "highline" "\
Create a function NAME with optional DOCSTRING that takes arguments FACE1, FACE2 and call FUNC with the background colors for those faces or \"None\".

\(fn NAME DOCSTRING &optional FUNC)" nil t)

(defhlsep highline-arrow-left hl/arrow-xpm-left)

(defhlsep highline-arrow-right hl/arrow-xpm-right)

(setq-default mode-line-format '("%e" (:eval (let* ((active (eq (frame-selected-window) (selected-window))) (face1 (if active 'highline-active1 'highline-inactive1)) (face2 (if active 'highline-active2 'highline-inactive2)) (lhs (concat (highline-raw "%*" nil 'l) (highline-buffer-size nil 'l) (highline-buffer-id nil 'l) (highline-arrow-right nil face1) (highline-major-mode face1 'l) (highline-minor-modes face1 'l) (highline-raw mode-line-process face1 'l) (highline-narrow face1 'l) (highline-arrow-right face1 face2) (highline-vc face2))) (rhs (concat (highline-raw global-mode-string face2 'r) (highline-arrow-left face2 face1) (highline-raw "%4l" face1 'r) (highline-raw ":" face1) (highline-raw "%3c" face1 'r) (highline-arrow-left face1 nil) (highline-raw " ") (highline-raw "%6p" nil 'r) (highline-hud face2 face1)))) (concat lhs (highline-fill face2 (length (format-mode-line rhs))) rhs)))))

;;;***

;;;### (autoloads nil nil ("highline-pkg.el") (20461 27059 948882
;;;;;;  0))

;;;***

(provide 'highline-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; highline-autoloads.el ends here
