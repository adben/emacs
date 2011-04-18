;; Org-mode settings
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org.txt$" . org-mode))
<<<<<<< HEAD
<<<<<<< HEAD
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
=======
=======
>>>>>>> emacs24
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/school.org" 
                             "~/org/home.org"))
;; Funtion to calculate Hours
(defun uphours (n)
  "update all timestamps n hours"
  (interactive "nAdd hours: ")
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[[<]" nil t)
      (when (org-at-timestamp-p t)
        (org-timestamp-change n 'hour)
        ))))
<<<<<<< HEAD
>>>>>>> emacs24
=======
>>>>>>> emacs24
