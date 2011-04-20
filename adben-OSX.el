
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
;;(setq mac-option-modifier 'hyper) ; sets the Option key as Hyper
;;(setq mac-option-modifier 'super) ; sets the Option key as Super
;;(setq mac-command-modifier 'meta) ; sets the Command key as Meta
;;(setq mac-control-modifier 'meta) ; sets the Control key as Meta
;;(setq mac-option-modifier 'meta)
;;(setq mac-command-modifier 'ctrl) ;sets command key as ctrl
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
;;                 (lambda () (interactive) (kill-buffer (current-buffer))))
;; (global-set-key [(hyper .)] 'keyboard-quit)
;; I disabled this since I want to avoid hitting Cmd-q accidentally.
(global-set-key [(hyper q)] 'save-buffers-kill-emacs)
(require 'redo+)
(global-set-key [(hyper z)] 'undo)
(global-set-key [(hyper shift z)] 'redo)
