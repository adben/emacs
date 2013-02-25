;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac OSX customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(normal-erase-is-backspace-mode t)
(require 'mac-key-mode)
;; The default keymaps are as follows:
;;     ⌘ + O — Open an existing file into an Emacs buffer
;;     ⌘ + W — Discard (kill) current buffer
;;     ⌘ + S — Save current buffer into its file
;;     ⌘ + ⇧ (Shift) + S — Write current buffer into another file
;;     ⌘ + I — Display current file/directory in a Finder window
;;     ⌘ + P — Print current buffer
;;     ⌘ + Q — Quit
;;     ⌘ + Z — Undo
;;     ⌘ +⇧ (Shift) + Z — Redo
;;     ⌘ + X — Cut
;;     ⌘ + C — Copy
;;     ⌘ + V — Paste
;;     ⌘ + A — Select All
;;     ⌘ + F — Search for a string
;;     ⌘ + ⌥ (Option) + F — Advanced Search (M-x occur)
;;     ⌘ + G — Search forward for a string
;;     ⌘ + ⇧ (Shift) + G — Search backward for a string
;;     ⌘ + L — Go to Line
;;     ⌘ + T — Show/Hide the font panel.
;;     ⌘ + M — Minimize the window
;;     ⌘ + ` — Move to a different visible window (frame)
;;     ⌘ + ⇧ (Shift) + N — Make a new window (frame)
;;     ⌘ + ⇧ (Shift) + W — Close window (frame)
;;     ⌘ + ? — Show help files (M-x info)
;;     ⌘ + / — Same as ⌘ + ?
;;     ⌘ + . — Interrupt operation
;;     ⌘ + ↑ — Move point to the beginning of the buffer
;;     ⌘ + ↓ — Move point to the end of the buffer
;;     ⌘ + ← — Move point to beginning of current line
;;     ⌘ + → — Move point to end of current line
;;     ⌘ + Click — Open URL with a browser
;;     ⌃ (Control) + Click — Show contextual menu
;;     ⇧ (Shift) + Click — Select region
;;; MacOS X specific stuff
; setting Hyper and Super keys for the Mac keyboard, for emacs running in OS X
(setq mac-option-modifier 'hyper) ; sets the Option key as Hyper
(setq mac-option-modifier 'super) ; sets the Option key as Super
;;(setq mac-command-modifier 'meta) ; sets the Command key as Meta
(setq mac-control-modifier 'meta) ; sets the Control key as Meta
;;(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'ctrl) ;sets command key as ctrl
;;(setq mac-command-modifier 'crtl mac-option-modifier 'meta)
(define-key mac-key-mode-map [(alt l)] 'goto-line)
;; (global-set-key [(hyper a)] 'mark-whole-buffer)
;; (global-set-key [(hyper v)] 'yank)
;; (global-set-key [(hyper c)] 'kill-ring-save)
;; (global-set-key [(hyper x)] 'kill-region)
;; (global-set-key [(hyper s)] 'save-buffer)
;; (global-set-key [(hyper l)] 'goto-line)
;; (global-set-key [(hyper o)] 'find-file)
;; (global-set-key [(hyper f)] 'isearch-forward)
;; (global-set-key [(hyper g)] 'isearch-repeat-forward)
;; (global-set-key [(hyper w)]
;; (global-set-key [(hyper .)] 'keyboard-quit)
;; I disabled this since I want to avoid hitting Cmd-q accidentally.
(global-set-key [(hyper q)] 'save-buffers-kill-emacs)
(require 'redo+)
(global-set-key [(hyper z)] 'undo)
(global-set-key [(hyper shift z)] 'redo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Global Keyboard Behaviour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings, took from http://www.emacswiki.org/emacs/EmacsForMacOS
;; mac-function-modifier
;; mac-control-modifier
;; mac-command-modifier
;; mac-option-modifier
;; mac-right-command-modifier
;; mac-right-control-modifier
;; mac-right-option-modifier
;; values can be 'control, 'alt, 'meta, 'super, 'hyper, nil (setting to nil allows the OS to assign values)
;;(setq mac-control-modifier 'control)
;; (setq mac-control-modifier 'control)
;;(setq mac-command-modifier 'meta)
;; http://whattheemacsd.com/mac.el-01.html
;;(setq mac-option-modifier 'super)
;;(setq ns-function-modifier 'hyper)
(global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
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
;; Cursor movement settings
(global-set-key "\C-v"	   'pager-page-down)
(global-set-key [next] 	   'pager-page-down)
(global-set-key "\ev"	   'pager-page-up)
(global-set-key [prior]	   'pager-page-up)
(global-set-key '[M-up]    'pager-row-up)
(global-set-key '[M-kp-8]  'pager-row-up)
(global-set-key '[M-down]  'pager-row-down)
(global-set-key '[M-kp-2]  'pager-row-down)
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
;; Keybonds
;; see https://gist.github.com/raw/3498096/392fa92ae4b5feb5626f5ea92b5da531235720d4/mac-switch-meta.el
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)
;; ;; mac switch meta key
;; (defun mac-switch-meta nil 
;;   "switch meta between Option and Command"
;;   (interactive)
;;   (if (eq mac-option-modifier nil)
;;       (progn
;; 	(setq mac-option-modifier 'meta)
;; 	(setq mac-command-modifier 'hyper)
;; 	)
;;     (progn 
;;       (setq mac-option-modifier nil)
;;       (setq mac-command-modifier 'meta)
;;       )
;;     )
;;   )
