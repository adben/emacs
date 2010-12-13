;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual Nicities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'color-theme)
;;(load-library "adben-color-theme")
;;(load-library "adben-color-theme-textmate")
;;(global-font-lock-mode 1)
(color-theme-initialize)
;;(color-theme-adben)
;;(color-theme-tango)
;;(color-theme-standard)
;;(color-theme-bharadwaj)
;;(color-theme-textmate)
(color-theme-robin-hood)
(column-number-mode 1)
(setq-default fill-column 80)
(setq auto-fill-mode 1)
;;Show what's being selected
(transient-mark-mode 1)
;;Show matching parentheses
(show-paren-mode 1)
;;Line by line scrolling
(setq scroll-step 1)
(setq inhibit-startup-message t)
;;Disable the menubar (promotes good emacs memory :)
(menu-bar-mode 0)
(tool-bar-mode -1)
(scroll-bar-mode 1)
;;activate by default the speedbar into thw workspace
(speedbar 1)
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
