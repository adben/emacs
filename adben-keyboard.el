;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Global Keyboard Behaviour
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mark-ring is navigable by typing C-u C-SPC and then repeating C-SPC forever
(setq set-mark-command-repeat-pop t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Global Keyboard Shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Suspend-frame is stupid
(global-set-key "\C-z" 'ignore)
(global-set-key "\C-x\C-z" 'ignore)
(global-set-key [S-left] 'windmove-left)          ; move to left window
(global-set-key [S-right] 'windmove-right)        ; move to right window
(global-set-key [S-up] 'windmove-up)              ; move to upper window
(global-set-key [S-down] 'windmove-down)          ; move to lower window;The generic apropos (of any symbol) is MUCH more useful than apropos-command
(global-set-key "\C-ha" 'apropos)
;;enable Redo see http://xahlee.org/emacs/emacs_make_modern.html
(global-set-key (kbd "C-z") 'undo) ; Ctrl+z
(global-set-key (kbd "C-S-z") 'redo) ;  Ctrl+Shift+z
;;Voor Helm
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
;; amounts to the same thing (TMTOWTDI)
;; C-a: move cursor to start of line
;; C-k: cut ("kill") the line
;; C-k: cut the newline
;; C-y: paste ("yank") (we're back at square one)
;; C-y: paste again (now we've got two copies of the line)
;; These are both embarrassingly verbose compared to C-d in your editor,
;; but in Emacs there's always a customization. C-d is bound to delete-char by default, so how about C-c C-d?
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")
;;Switching Between Two Recently Used Buffers
(global-set-key [C-tab] 'switch-to-previous-buffer)




