;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual Nicities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'sr-speedbar)
;;(require 'color-theme)
;;(require 'zenburn)
;;(zenburn)
;;(load-library "adben-color-theme")
;;(load-library "adben-color-theme-textmate")
;;(global-font-lock-mode 1)
;;mac
;;(require 'redo)
(setq-default fill-column 100)
(setq auto-fill-mode 1)
;;Show what's being selected
(transient-mark-mode 1)
;;Show matching parentheses
(show-paren-mode 1)
;;Line by line scrolling
(setq scroll-step 1)
(setq inhibit-startup-message t)
;;Disable the menubar (promotes good emacs memory :)
(menu-bar-mode 1)
(tool-bar-mode 0)
(scroll-bar-mode -1)
;;activate by default the speedbar into the workspace
;;(speedbar 0)
;;Make page up and page down a whole lot nicer
(global-set-key "\C-v"	   'pager-page-down)
(global-set-key [next] 	   'pager-page-down)
(global-set-key "\ev"	   'pager-page-up)
(global-set-key [prior]	   'pager-page-up)
(global-set-key '[M-up]    'pager-row-up)
(global-set-key '[M-kp-8]  'pager-row-up)
(global-set-key '[M-down]  'pager-row-down)
(global-set-key '[M-kp-2]  'pager-row-down)
;;Show newlines at end of file
(define-fringe-bitmap 'empty-line [0 0 #x3c #x3c #x3c #x3c 0 0])
(set-default 'indicate-empty-lines nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Increase/Decrease font size on the fly
;;; Taken from: http://is.gd/iaAo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun adben/increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))
(defun adben/decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                (face-attribute 'default :height)))))
(global-set-key (kbd "C-+") 'adben/increase-font-size)
(global-set-key (kbd "C--") 'adben/decrease-font-size)
;;column line-numbers 1 for on, 0 for off.
;;(global-visual-line-mode 1)
;;Text Highlighting, see http://xahlee.org/emacs/emacs_make_modern.html
(transient-mark-mode 1) ; highlight text selection
(delete-selection-mode 1) ; delete seleted text when typing
(global-font-lock-mode 1) ; turn on syntax coloring, default in emacs 22, 23, 24
(show-paren-mode 1) ; turn on paren match highlighting
(global-hl-line-mode 1) ;; turn on highlighting current line
(setq redisplay-dont-pause t)
;;(set-face-attribute hl-line-face nil :underline nil) ; avoid the underline in the highlighted line
