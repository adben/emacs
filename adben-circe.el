(require 'circe)

(setq circe-network-options
      `(("Freenode"
         :nick "adben"
         :channels ("#emacs" "#emacs-circe" "#clojure" "#liferay")
         :nickserv-password freenode-passwd
         )))
;;Quick IRC Commandx
;;If you have a number of networks you use, it can be useful to have a simple command to connect to them all.
(defun irc ()
  "Connect to IRC"
  (interactive)
  (circe "Freenode")
  (circe "Bitlbee")
  (circe "IRCnet"))
;; Spell Checker
;; Circe's user interface, Lui, comes with spellchecking support. You can configure per-channel dictionaries. The following example will enable flyspell in Lui globally, and use the american dictionary by default, except in the channel #nederland, where it will use holandes.
(setq lui-flyspell-p t
      lui-flyspell-alist '(("#nederland" "nl_NL_hunspell")
                           (".*" "en_US_hunspell"))
      )
;;Hiding the Join/Part Spam
;;Say you don't want Circe to show JOIN, PART and QUIT messages. You have two options.
;;
;;First, Circe comes with a special feature to reduce such spam. If enabled, Circe will hide JOIN, PART and QUIT messages. If a user speaks, Circe will say that this is the first activity of this user, and how long ago they joined. Once they have spoken up like this, Circe will show PART and QUIT normally.
;;
(setq circe-reduce-lurker-spam t)
;;Alternatively, you can pass :reduce-lurker-spam t to your network specification to enable this feature locally.

;; Logging
;; The logging module provides a history of chat conversations. Whenever a logging enabled chat buffer is closed its contents will be saved in a log file. When you enter the same chat again the log will be inserted.
;; To enable logging, use M-x enable-lui-logging
;;(enable-lui-logging)

;; Automatic Pasting
;; Circe (actually, lui) has the ability to intercept long pastes if it is done in a single input. Lui will then ask if the user would prefer to use a paste service.
(require 'lui-autopaste)
(add-hook 'circe-channel-mode-hook 'enable-lui-autopaste)

;; Topic Diffs
;; Some channels have very long topics. When they change, it's difficult to tell what of it exactly changed. Circe has a feature to actually display a diff for a topic change. 
(setq circe-format-server-topic "*** Topic change by {origin}: {topic-diff}")

;; Spelling Training
;; Sometimes, there are typos you just do over and over again. You can tell Circe (or rather, Lui) to simply not send lines containing those misspellings.
;; (add-hook 'lui-pre-input-hook 'fc-there-is-no-wether);; (defun fc-there-is-no-wether ()
;;  "Throw an error when the buffer contains \"wether\".
;; Or other words I used repeatedly."
;;   (goto-char (point-min))
;;   (when (re-search-forward (regexp-opt '("wether"
;;                                          "occurence" "occurrance"
;;                                          "occurance"
;;                                          )
;;                                        'words)
;;                            nil t)
;;     (error "It's \"whether\" and \"occurrence\"!")))
;; ;;Channel Name in the Prompt
;; (add-hook 'circe-chat-mode-hook 'my-circe-prompt)
;; (defun my-circe-prompt ()
;;   (lui-set-prompt
;;    (concat (propertize (concat (buffer-name) ">")
;;                        'face 'circe-prompt-face)
;;            " ")))

;; Timestamps in Margins
;; Emacs 23 added a feature called 'margins' which lets you have annotations in a margin area to the right or left of a buffer. Circe is able to put the timestamps in this area. Below is an example of how to do this. Note that in addition to telling Circe to use margins, you also need to tell emacs to turn on margins for your circe buffers.
(setq
 lui-time-stamp-position 'right-margin
 lui-time-stamp-format "%H:%M")

(add-hook 'lui-mode-hook 'my-circe-set-margin)
(defun my-circe-set-margin ()
  (setq right-margin-width 5))

;; Fluid-width windows
;; Thanks to several interesting emacs features, Lui buffers can be made to dynamically fit arbitrary window sizes without turning ugly, even with right-aligned time-stamps. For this, put right-aligned time-stamps into the margin, preferably enable fringes-outside-margins, disable filling, enable word wrapping, and set wrap-prefix to a preferred value, e.g. the string you had in lui-fill-type. (The non-string values accepted in lui-fill-type are sadly not accepted here.)
(setq
 lui-time-stamp-position 'right-margin
 lui-fill-type nil)
(add-hook 'lui-mode-hook 'my-lui-setup)
(defun my-lui-setup ()
  (setq
   fringes-outside-margins t
   right-margin-width 5
   word-wrap t
   wrap-prefix "    "))

;; Chanop Commands
;; The optional module circe-chanop provides the chanop commands /MODE, /BANS, /KICK, /GETOP, and /DROPOP. To enable these commands:
(eval-after-load 'circe '(require 'circe-chanop))

;; Selectively Disable Tracking
;; Tracking can be fine-tuned on a per-buffer basis by configuring the variable tracking-ignored-buffers. The value of this variable is a list of regular expressions or conses. Regular expressions cover the simple case: tracking will be disabled for any buffer whose name is matched by one of the regular expressions. Giving a cons allows you more control. That form is a regular expression consed to a list of faces for which tracking is selectively enabled. The cons form is what you usually want, because you still want tracking to work when somebody says your name in a channel that you are otherwise ignoring. The simple form might look like this in your config:
;; (setq tracking-ignored-buffers '("#emacs"))
;; The cons form that allows you to still track when your nickname is said would look like this:
;; (setq tracking-ignored-buffers '(("#emacs" circe-highlight-nick-face)))
;; Do remember, though, that the channel names are given as regular expressions, not simple strings, so "#c" would match not only "#c", but also "#chat". To match only "#c", give the regular expression "#c$".

;; Auto-track Ignored Channels on Talk
;; Let's say you've set up your tracking-ignored-buffers as described above to not track certain channels that you usually don't care to follow. But what if occasionally you do want to track those channels, like when you're actively conversing in them? The following code lets you do just that. It advises circe-command-SAY so that if you speak in one of your untracked channels, tracking will automatically be enabled in that channel for the rest of the session.
;; (eval-after-load 'circe
;;   '(progn
;;      (defadvice circe-command-SAY (after jjf-circe-unignore-target)
;;        (let ((ignored (tracking-ignored-p (current-buffer) nil)))
;;          (when ignored
;;            (setq tracking-ignored-buffers
;;                  (remove ignored tracking-ignored-buffers))
;;            (message "This buffer will now be tracked."))))
;;      (ad-activate 'circe-command-SAY)))

;;Strip mIRC Color Codes
(eval-after-load 'circe
  '(defun lui-irc-propertize (&rest args)))

;; Define a Command
;; Defining a new slash-command in Circe is trivially easy. Just define a function with a name like circe-command-FOO where FOO, in uppercase, is the name of the slash command. When the command is invoked from the Circe prompt, this function is called with a single argument: a string of all text given after the command name (and one space) on the command line. It is the responsibility of the command to parse its arguments further.
(defun circe-command-RECONNECT (&optional ignored)
  (circe-reconnect))

;; Some of the built-in commands have longer lists of arguments and 'interactive' forms. These are simply to allow those commands to be called in other contexts such from key bindings or the M-x prompt.
