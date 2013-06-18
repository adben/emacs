;;; gruber-darker-theme.el --- Gruber Darker color theme for Emacs 24.

;; Copyright (C) 2013 Alexey Kutepov a.k.a rexim

;; Author: Alexey Kutepov <reximkut@gmail.com>
;; URL: http://github.com/rexim/gruber-darker-theme
;; Version: 20130616.1008
;; X-Original-Version: 0.4

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; Gruber Darker color theme for Emacs by Jason Blevins. A darker
;; variant of the Gruber Dark theme for BBEdit by John Gruber. Adapted
;; for deftheme and extended by Alexey Kutepov a.k.a. rexim.


(deftheme gruber-darker
  "Gruber Darker color theme for Emacs 24")

;; Please, install rainbow-mode.
;; Colors with +x are lighter. Colors with -x are darker.
(let ((gruber-darker-fg       "#e4e4ef")
      (gruber-darker-fg+1     "#f4f4ff")
      (gruber-darker-fg+2     "#f5f5f5")
      (gruber-darker-white    "#ffffff")

      ;; FIXME(rexim): I think those can be replaced be one of bg(+|-)x ones.
      (gruber-darker-gray     "#444")
      (gruber-darker-gray+1   "#999")

      (gruber-darker-black    "#000000")
      (gruber-darker-bg-1     "#101010")
      (gruber-darker-bg       "#181818")
      (gruber-darker-bg+1     "#282828")
      (gruber-darker-bg+2     "#453d41")
      (gruber-darker-bg+3     "#484848")
      (gruber-darker-bg+4     "#52494e")
      (gruber-darker-red-1    "#c73c3f")
      (gruber-darker-red      "#f43841")
      (gruber-darker-green    "#73c936")
      (gruber-darker-yellow   "#ffdd33")
      (gruber-darker-brown    "#cc8c3c")
      (gruber-darker-sea-wave "#5f627f")
      (gruber-darker-quartz   "#95a99f")
      (gruber-darker-niagara  "#96a6c8")
      (gruber-darker-wisteria "#9e95c7")
      )
  (custom-theme-set-variables
   'gruber-darker
   '(frame-brackground-mode (quote dark)))

  (custom-theme-set-faces
   'gruber-darker

   ;; Standard font lock faces
   `(default ((t ,(list :foreground gruber-darker-fg
                        :background gruber-darker-bg))))
   `(cursor ((t (:background ,gruber-darker-fg))))
   `(font-lock-comment-face ((t (:foreground ,gruber-darker-brown))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,gruber-darker-brown))))
   `(font-lock-doc-face ((t (:foreground ,gruber-darker-green))))
   `(font-lock-doc-string-face ((t (:foreground ,gruber-darker-green))))
   `(font-lock-string-face ((t (:foreground ,gruber-darker-green))))
   `(font-lock-keyword-face ((t (:foreground ,gruber-darker-yellow :bold t))))
   `(font-lock-builtin-face ((t (:foreground ,gruber-darker-yellow))))
   `(font-lock-function-name-face ((t (:foreground ,gruber-darker-niagara))))
   `(font-lock-variable-name-face ((t (:foreground ,gruber-darker-fg+1))))
   `(font-lock-preprocessor-face ((t (:foreground ,gruber-darker-quartz))))
   `(font-lock-constant-face ((t (:foreground ,gruber-darker-quartz))))
   `(font-lock-type-face ((t (:foreground ,gruber-darker-quartz))))
   `(font-lock-warning-face ((t (:foreground ,gruber-darker-red))))
   `(font-lock-reference-face ((t (:foreground ,gruber-darker-quartz))))
   `(trailing-whitespace ((t ,(list :foreground gruber-darker-black
                                    :background gruber-darker-red))))
   `(link ((t (:foreground ,gruber-darker-niagara :underline t))))
   `(link-visited ((t (:foreground ,gruber-darker-wisteria :underline t))))
   `(match ((t (:background ,gruber-darker-bg+4))))

   ;; Search
   `(isearch ((t ,(list :foreground gruber-darker-black
                        :background gruber-darker-fg+2))))
   `(isearch-lazy-highlight-face ((t ,(list
                                       :foreground gruber-darker-fg+1
                                       :background gruber-darker-sea-wave))))
   `(isearch-fail ((t ,(list :foreground gruber-darker-black
                             :background gruber-darker-red))))

   ;; User interface
   `(fringe ((t ,(list :background gruber-darker-bg-1
                       :foreground gruber-darker-gray))))
   `(border ((t ,(list :background gruber-darker-bg-1
                       :foreground gruber-darker-gray))))
   `(mode-line ((t ,(list :background gruber-darker-bg+2
                          :foreground gruber-darker-white))))
   `(mode-line-buffer-id ((t ,(list :background gruber-darker-bg+2
                                    :foreground gruber-darker-white))))
   `(mode-line-inactive ((t ,(list :background gruber-darker-bg+2
                                   :foreground gruber-darker-gray+1))))
   `(minibuffer-prompt ((t (:foreground ,gruber-darker-niagara))))
   `(region ((t (:background ,gruber-darker-bg+3))))
   `(secondary-selection ((t ,(list :background gruber-darker-bg+3
                                    :foreground gruber-darker-fg+1))))
   `(tooltip ((t ,(list :background gruber-darker-bg+4
                        :foreground gruber-darker-white))))

   ;; Custom
   `(custom-state ((t (:foreground ,gruber-darker-green))))

   ;; show-paren
   `(show-paren-match-face ((t ,(list :background gruber-darker-bg
                                      :foreground gruber-darker-green
                                      :weight 'bold))))
   `(show-paren-mismatch-face ((t ,(list :background gruber-darker-bg
                                         :foreground gruber-darker-red
                                         :weight 'bold))))

   ;; Line highlighting
   `(highlight ((t (:background ,gruber-darker-bg+1 :foreground nil))))
   `(highlight-current-line-face ((t ,(list :background gruber-darker-bg+1
                                            :foreground nil))))

   ;; Calendar
   `(holiday-face ((t (:foreground ,gruber-darker-red))))

   ;; Info
   `(info-xref ((t (:foreground ,gruber-darker-niagara))))
   `(info-visited ((t (:foreground ,gruber-darker-wisteria))))

   ;; AUCTeX
   `(font-latex-sectioning-5-face ((t ,(list :foreground gruber-darker-niagara
                                             :bold t))))
   `(font-latex-bold-face ((t (:foreground ,gruber-darker-quartz :bold t))))
   `(font-latex-italic-face ((t (:foreground ,gruber-darker-quartz :italic t))))
   `(font-latex-math-face ((t (:foreground ,gruber-darker-green))))
   `(font-latex-string-face ((t (:foreground ,gruber-darker-green))))
   `(font-latex-warning-face ((t (:foreground ,gruber-darker-red))))
   `(font-latex-slide-title-face ((t (:foreground ,gruber-darker-niagara))))

   ;; linum-mode
   `(linum ((t `(list :foreground gruber-darker-quartz
                      :background gruber-darker-bg))))

   ;; EShell
   `(eshell-ls-directory ((t (:foreground ,gruber-darker-niagara))))
   `(eshell-ls-symlink ((t (:foreground ,gruber-darker-yellow))))
   `(eshell-ls-backup ((t (:foreground ,gruber-darker-quartz))))
   `(eshell-ls-executable ((t (:foreground ,gruber-darker-green))))

   ;; Dired
   `(dired-directory ((t (:foreground ,gruber-darker-niagara :weight bold))))
   `(dired-ignored ((t (:foreground ,gruber-darker-quartz :inherit nil))))

   ;; ido-mode
   `(ido-first-match ((t (:foreground ,gruber-darker-yellow :bold nil))))
   `(ido-only-match ((t (:foreground ,gruber-darker-brown :weight bold))))
   `(ido-subdir ((t (:foreground ,gruber-darker-niagara :weight bold))))

   ;; helm-mode
   `(helm-source-header ((t ,(list :foreground gruber-darker-yellow
                                   :background gruber-darker-bg
                                   :box (list :line-width -1
                                              :style 'released-button)))))
   `(helm-selection ((t (:background ,gruber-darker-bg+1 :underline nil))))
   `(helm-selection-line ((t (:background ,gruber-darker-bg+1))))
   `(helm-candidate-number ((t ,(list :background gruber-darker-bg+2
                                      :foreground gruber-darker-yellow
                                      :bold t))))
   `(helm-ff-file ((t (:foreground ,gruber-darker-fg :inherit nil))))
   `(helm-ff-directory ((t ,(list :foreground gruber-darker-niagara
                                  :background gruber-darker-bg
                                  :bold t))))
   `(helm-ff-symlink ((t (:foreground ,gruber-darker-yellow :bold t))))
   `(helm-ff-executable ((t (:foreground ,gruber-darker-green))))
   `(helm-ff-invalid-symlink ((t ,(list :foreground gruber-darker-bg
                                        :background gruber-darker-red))))

   ;; Compilation
   `(compilation-info ((t (:foreground ,gruber-darker-green :inherit nil))))
   `(compilation-warning ((t ,(list :foreground gruber-darker-brown
                                    :bold t
                                    :inherit nil))))

   ;; flymake
   `(flymake-errline ((t ,(list :background gruber-darker-bg-1
                                :foreground gruber-darker-red
                                :bold t
                                :underline t))))
   `(flymake-warnline ((t (:background ,gruber-darker-bg+1 :italic t))))

   ;; egg
   `(egg-text-base ((t (:foreground ,gruber-darker-fg))))
   `(egg-help-header-1 ((t (:foreground ,gruber-darker-yellow))))
   `(egg-help-header-2 ((t (:foreground ,gruber-darker-niagara))))
   `(egg-branch ((t (:foreground ,gruber-darker-yellow))))
   `(egg-branch-mono ((t (:foreground ,gruber-darker-yellow))))
   `(egg-term ((t (:foreground ,gruber-darker-yellow))))
   `(egg-diff-add ((t (:foreground ,gruber-darker-green))))
   `(egg-diff-del ((t (:foreground ,gruber-darker-red))))
   `(egg-diff-file-header ((t (:foreground ,gruber-darker-wisteria))))
   `(egg-section-title ((t (:foreground ,gruber-darker-yellow))))

   ;; message
   `(message-header-name ((t (:foreground ,gruber-darker-green))))

   ;; jabber
   `(jabber-chat-prompt-foreign ((t ,(list :foreground gruber-darker-quartz
                                           :bold nil))))
   `(jabber-chat-prompt-local ((t (:foreground ,gruber-darker-yellow))))
   `(jabber-chat-prompt-system ((t (:foreground ,gruber-darker-green))))
   `(jabber-roster-user-online ((t (:foreground ,gruber-darker-green))))
   `(jabber-rare-time-face ((t (:foreground ,gruber-darker-green))))
   ))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'gruber-darker)

;; Local Variables:
;; no-byte-compile: t
;; indent-tabs-mode: nil
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; gruber-darker-theme.el ends here.