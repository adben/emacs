(require 'erc)

;;Much of ERC is customized through Emacs customize-mode
;;But here's some extra stuff

(load-library "adben-libnotify")

;;Set the default path for erc logs
(setq erc-log-channels-directory "~/.erc/logs/")

(defun bip-freenode()
  (interactive)
  "Connect to EnigmaCurry's Bip server, soon another server configuration"
  (erc :server "localhost" :port "7778" :nick bip-username :password bip-password)
  )

;; Notify me when someone wants to talk to me.
;; Heavily based off of ErcPageMe on emacswiki.org, with some improvements.
;; I wanted to learn and I used my own notification system with pymacs
;; Delay is on a per user, per channel basis now.
(defvar erc-page-nick-alist nil
  "Alist of 'nickname|target' and last time they triggered a notification"
  )
(defun erc-notify-allowed (nick target &optional delay)
  "Return true if a certain nick has waited long enough to notify"
  (unless delay (setq delay 30))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc (format "%s|%s" nick target) erc-page-nick-alist))
        (last-time))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
       (push (cons (format "%s|%s" nick target) cur-time) erc-page-nick-alist)
       t)
     )
   )
 (defun erc-notify-PRIVMSG (proc parsed)
   (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
	 (target (car (erc-response.command-args parsed)))
	 (msg (erc-response.contents parsed)))
     ;;Handle true private/direct messages (non channel)
     (when (and (not (erc-is-message-ctcp-and-not-action-p msg))
		(erc-current-nick-p target)
		(erc-notify-allowed nick target)
		)
       ;Do actual notification
       (ding)
       (notify-desktop (format "%s - %s" nick
			       (format-time-string "%b %d %I:%M %p"))
		       msg 0 "gnome-emacs")
       )
     ;;Handle channel messages when my nick is mentioned
     (when (and (not (erc-is-message-ctcp-and-not-action-p msg))
		(string-match (erc-current-nick) msg)
		(erc-notify-allowed nick target)
		)
       ;Do actual notification
       (ding)
       (notify-desktop (format "%s - %s" target
			       (format-time-string "%b %d %I:%M %p"))
		       (format "%s: %s" nick msg) 0 "gnome-emacs")
       )
     )

   )

 ;(add-hook 'erc-server-PRIVMSG-functions 'erc-notify-PRIVMSG)


 ;; Ignore certain types of messages from being tracked in the modeline:
 (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				 "324" "329" "332" "333" "353" "477"))
 ;; znc + erc helpers
(defun kill-buffer-always (&optional buffer)
  "Murderface a buffer, don't listen to nobody, son!"
  (interactive "b")
  (let ((buffer (or buffer (current-buffer)))
        (kill-buffer-query-functions nil))
    (kill-buffer buffer)))

(defun erc-all-kill (&optional prefix)
  "Kill all buffers in erc-mode. With prefix, kill-buffer-query-functions is preserved"
  (interactive "P")
  (flet ((kill-buffer-p (b) (with-current-buffer b (eq major-mode seek-mode))))
    (let* ((seek-mode 'erc-mode)
           (kill-buffer-query-functions (if prefix kill-buffer-query-functions '()))
           (killed (loop for buffer in (buffer-list)
                         when (kill-buffer-p buffer)
                         collect (buffer-name buffer)
                         and do
                         (kill-buffer buffer))))
      (message "Killed: %s buffers: %s" (length killed) killed)
      killed)))

 (defadvice erc-server-reconnect (after erc-znc-rename last nil activate)
   "Maybe rename the buffer we create"
   (let* ((wants-name (and (local-variable-p 'znc-buffer-name (erc-server-buffer))
                           (buffer-local-value 'znc-buffer-name (erc-server-buffer))))
          (current (erc-server-buffer))
          (returning ad-return-value))
     (if wants-name
         (progn
           (ignore-errors (kill-buffer-always wants-name))
           (with-current-buffer returning
             (erc-znc-set-name wants-name)
             (rename-buffer wants-name))
           (get-buffer wants-name))
       returning)))

 (defun erc-znc-set-name (znc-name &optional buffer)
   "Set the znc-buffer-name buffer local to znc-name in buffer or (current-buffer)"
   (let ((buffer (get-buffer (or buffer (current-buffer)))))
     (with-current-buffer buffer
       (make-local-variable 'znc-buffer-name)
       (setf znc-buffer-name znc-name))))

 (defun erc-znc (network user pass)
   (let ((buffer (format "*irc-%s*" network))
         (erc-buffer (erc :server "localhost"
                          :port 12533
                          :nick "Muta"
                          :password (format "%s:%s" user pass))))
     (when (get-buffer buffer)
       (kill-buffer-always buffer))
     (erc-znc-set-name buffer erc-buffer)
     (with-current-buffer erc-buffer
       (rename-buffer buffer))))
