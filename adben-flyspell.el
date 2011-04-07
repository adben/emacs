;; When turning on flyspell-mode, automatically check the entire buffer.
;; Why this isn't the default baffles me.
(defadvice flyspell-mode (after advice-flyspell-check-buffer-on-start activate)
  (flyspell-buffer))
(setq flyspell-issue-welcome-flag nil)
(autoload 'ispell-word "ispell"
         "Check the spelling of word in buffer." t)
      (global-set-key "\e$" 'ispell-word)
      (autoload 'ispell-region "ispell"
         "Check the spelling of region." t)
      (autoload 'ispell-buffer "ispell"
         "Check the spelling of buffer." t)
      (autoload 'ispell-complete-word "ispell"
         "Look up current word in dictionary and try to complete it." t)
      (autoload 'ispell-change-dictionary "ispell"
         "Change ispell dictionary." t)
      (autoload 'ispell-message "ispell"
         "Check spelling of mail message or news post.")
      (autoload 'ispell-minor-mode "ispell"
         "Toggle mode to automatically spell check words as they are typed in.")
