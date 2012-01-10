;;; multi-project.el --- Easily work with multiple projects.

;; Copyright (C) 2010

;; Author: Shawn Ellis <shawn.ellis17@gmail.com>
;; Version: 0.0.5
;; Keywords: project management
;;

;; multi-project.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; multi-project.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;
;; Multi-project makes it easier to work with different projects by
;; providing support for creating, deleting, and switching between
;; projects.  Multi-project also supports interactively finding a file
;; within a project or automatically switching the TAGS file for
;; symbol lookup.
;;
;; To use multi-project just add the following lines within your
;; .emacs file:
;; 
;; (require 'multi-project)
;; (global-multi-project-mode)
;;
;; The following bindings are created for multi-project
;; C-xpj - Project jump              Displays a list of projects
;; C-xpc - Project compile           Run the compilation command for a project
;; C-xpa - Anchor a project          Stores the project to be retrieved via
;;                                   multi-project-anchored
;; C-xpu - Resets the anchor         Clears out the multi-project-anchored var
;; C-xpl - Last project from Anchor  Jumps to the project stored via the anchor
;; C-xpp - Jump to current project   Allows switching between last and current
;;                                   projects
;; C-xpf - Find project files        Interactively find project files
;; C-xpn - Add a new project         Prompts for new project information
;;
;; When displaying the projects, the following bindings are present:
;; s     - Search projects:          Searches from the list of possible projects
;; C-n   - Next project              Move the cursor to the next project
;; C-p   - Previous project          Move the cursor to the previous project
;; a     - Anchor a project          Holds the last project to the anchored
;;                                   project
;; r     - Reset search              Resets the project search
;; N     - Add new project           Prompts for project information
;; d     - Delete a project          Marks the project for deletion
;; u     - Unmark a project          Removes the mark for a project
;; x     - Executes actions          Executes the deletions
;; q     - quit
;;
;; The multi-project-compilation-command variable can be set to a function
;; that provides a customized compilation command.  For example,
;;
;; (defun my-compilation-command (project-list)
;;   (let ((project-name (car project-list))
;;	   (project-dir (nth 1 project-list))
;;	   (project-subdir (nth 2 project-list)))
;;		       
;;     (cond ((string-match "proj1" project-name)
;;	      (concat "ant -f " project-dir "/" project-subdir "/build.xml"))
;;	     (t
;;	      (concat "make -C " project-dir "/" project-subdir)))))
;;
;; (setq multi-project-compilation-command 'my-compilation-command)

;;; Code:

(require 'compile)
(require 'etags)
(require 'easymenu)

(defgroup multi-project nil
  "Support for working with multiple projects."
  :prefix "multi-project"
  :group 'convenience)
  
(defcustom multi-project-roots nil
  "A list describing the project, filesystem root, subdirectory under the root, and the TAGS location."
  :group 'multi-project)

(defcustom multi-project-compilation-command 'multi-project-compile-command
  "The fuction to use when compiling a project."
  :group 'multi-project)

(defvar multi-project-dir "~/.emacs.d/multi-project"
  "Directory of the saved settings for multi-project.")

(defvar multi-project-file "mp"
  "File of the saved settings for multi-project.")

(defvar multi-project-last nil
  "Visits the last project that was switched to.")

(defvar multi-project-anchored nil
  "Visits the anchored project.")

(defvar multi-project-current nil
  "The current selected project name.")

(defvar multi-project-overlay nil
  "Overlay used to highlight the current selection.")

(defvar multi-project-previous-input nil
  "Prior input when performing a search." )

(defvar multi-project-previous-file-input nil
  "Prior input when performing a file search." )

(defconst multi-project-buffer "*mp*"
  "Buffer used for finding projects.")

(defface multi-project-selection-face
  ;; check if inherit attribute is supported
  (if (assq :inherit custom-face-attributes)
      '((t (:inherit highlight :underline nil)))

    '((((class color) (background light))
       (:background "darkseagreen2"))
      (((class color) (background dark))
       (:background "darkolivegreen"))
      (t (:inverse-video t))))
  "Face for highlighting the currently selected file name."
  :group 'multi-project)

(defvar global-multi-project-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x pa") 'multi-project-anchor)
    (define-key map (kbd "C-x pu") 'multi-project-reset-anchor)
    (define-key map (kbd "C-x pl") 'multi-project-last)
    (define-key map (kbd "C-x pr") 'multi-project-root)
    (define-key map (kbd "C-x pj") 'multi-project-display-projects)
    (define-key map (kbd "C-x pc") 'multi-project-compile)
    (define-key map (kbd "C-x pv")  'multi-project-change-tags)
    (define-key map (kbd "C-x pf") 'multi-project-find-file)
    (define-key map (kbd "C-x pn") 'multi-project-add-project)
    (define-key map (kbd "C-x pp") 'multi-project-current-project)
    map)
  "Global keymap for multi-project.")

(defvar multi-project-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "<down>") 'multi-project-next-line)
    (define-key map (kbd "C-n") 'multi-project-next-line)
    (define-key map (kbd "M-n") 'multi-project-next-line)
    (define-key map (kbd "n") 'multi-project-next-line)
    (define-key map (kbd "<up>") 'multi-project-previous-line)
    (define-key map (kbd "C-p") 'multi-project-previous-line)
    (define-key map (kbd "M-p") 'multi-project-previous-line)
    (define-key map (kbd "p") 'multi-project-previous-line)
    (define-key map (kbd "<prior>") 'multi-project-previous-page)
    (define-key map (kbd "<RET>") 'multi-project-display-select)
    (define-key map (kbd "f") 'multi-project-display-select)
    (define-key map (kbd "a") 'multi-project-display-anchor)
    (define-key map (kbd "o") 'multi-project-display-select-other-window)
    (define-key map (kbd "<C-return>") 'multi-project-display-select-other-window)
    (define-key map (kbd "q") 'multi-project-quit)
    (define-key map (kbd "s") 'multi-project-display-search)
    (define-key map (kbd "r") 'multi-project-display-reset)
    (define-key map (kbd "t") 'multi-project-display-change-tags)
    (define-key map (kbd "d") 'multi-project-mark-deletions)
    (define-key map (kbd "u") 'multi-project-unmark-project)
    (define-key map (kbd "x") 'multi-project-execute-actions)
    (define-key map (kbd "N") 'multi-project-new-project)
    (define-key map [mouse-2] 'multi-project-mouse-select)
    map)
  "Keymap for multi-project.")

(defvar multi-project-minibuffer-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "<RET>") 'multi-project-exit-minibuffer)
    (define-key map (kbd "<down>") 'multi-project-next-line)
    (define-key map (kbd "<up>") 'multi-project-previous-line)
    (define-key map (kbd "C-p") 'multi-project-previous-line)
    (define-key map (kbd "C-n") 'multi-project-next-line)
    map)
  "Keymap for multi-project-minibuffer.")

(defun multi-project-dired (projectdir directory &optional searchdirectory otherwindow)
  "Run `dired` on a particular project.
The PROJECTDIR argument specifies the directory and the DIRECTORY
argument is used to place the cursor on a directory within
PROJECTDIR.  The SEARCHDIRECTORY argument specifies a different
directory for the cursor instead of DIRECTORY.  Optional argument
OTHERWINDOW if non-nil, then open up a buffer in a different
windows."
  (if projectdir
      (let ((directorypath projectdir)
            (dir directory))

        (if searchdirectory
            (setq dir searchdirectory))

        (when directorypath
          (if otherwindow
              (dired-other-window directorypath)
            (dired directorypath))
          (goto-char (point-min))
          (when dir
            (if (re-search-forward (concat "[0-9]+:[0-9]+ " dir) nil t)
                (backward-word 1)))))))

(defun multi-project-dired-solution (solutionlist &optional searchdirectory otherwindow)
  "Open up a dired window based upon the project.
Argument SOLUTIONLIST
Optional argument SEARCHDIRECTORY
Optional argument OTHERWINDOW open another window."
  (multi-project-dired (nth 1 solutionlist) (nth 2 solutionlist) searchdirectory otherwindow))

(defun multi-project-filter-name (project lst)
  "Filter based upon the the PROJECT name of the LST."
  (car
   (delq nil
	 (mapcar (lambda (x) (and (string= project (car x)) x)) lst))))

(defun multi-project-filter-dir (projectdir lst)
  "Filter based upon the PROJECTDIR of the LST."
  (car
   (delq nil
	 (mapcar
	  (lambda (x) (and (string-match (expand-file-name (nth 1 x))
					 (expand-file-name projectdir))
			   x)) lst))))

(defun multi-project-filter-empty-string (lst)
  "Filter out empty strings from LST."
  (delq nil
	(mapcar (lambda (x) (when (> (length x) 0) x)) lst)))

(defun multi-project-find-by-directory ()
  "Return the project list from the set of defined projects in multi-projects-roots."
  (interactive)
  (let ((result)
        (directory (replace-regexp-in-string "/plink:" "/" default-directory)))

    (setq directory (expand-file-name (replace-regexp-in-string "/$" "" directory)))
    (setq result (multi-project-filter-dir directory multi-project-roots))
    result))

(defun multi-project-find-by-name(projectname)
  "Returns the project list that corresponds to the project name"
  (multi-project-filter-name projectname multi-project-roots))

(defun multi-project-prompt ()
  "Prompts for the project to work with."
  (let ((result)
        (solution)
        (prompt nil))
    (dolist (item (reverse multi-project-roots) prompt)
      (setq prompt (append (car item) " " prompt)))
    (setq solution (read-from-minibuffer (concat "Project: " prompt "? ") nil))
    (setq result (multi-project-find-by-name solution))
    result))
    
(defun multi-project-compile-command (project-list)
  "Provide a compilation command based upon the PROJECT-LIST."
  (when project-list
    (let ((tree (nth 1 project-list))
          (solution (nth 2 project-list)))
      (when (file-remote-p tree)
	  (setq tree (replace-regexp-in-string "/.*:" ""
					       (expand-file-name tree)))
	  (setq tree (replace-regexp-in-string "/$" "" tree)))
      
      (concat "make -C " tree))))

(defun multi-project-compile-prompt (command)
  "Read the compilation COMMAND from the minibuffer."
  (read-from-minibuffer "Compile command: "
                        command nil nil
                        (if (equal (car compile-history) command)
                            '(compile-history . 1)
                          'compile-history)))

(defun multi-project-compile-buffer-name (mode-name)
  "Return the compilation buffer name based upon the project and MODE-NAME."

  (let ((projectlist (multi-project-find-by-directory)))
    (cond (projectlist
	   (concat "*" (car projectlist) "-" (downcase mode-name) "*"))
	  (t
	   (concat "*" (downcase mode-name) "*")))))

;;;###autoload
(defun multi-project-compile ()
  "Compiles a project based upon the current directory of the buffer."
  (interactive)
  (let ((solutionlist (multi-project-find-by-directory)))
    (cond ((and solutionlist (boundp 'compile-history) compile-history
                (string-match (funcall multi-project-compilation-command solutionlist)
                              (car compile-history)))
           (setq compile-command (car compile-history)))

          (solutionlist
           (setq compile-command
                 (funcall multi-project-compilation-command solutionlist)))
          
          (t
           (setq solutionlist (multi-project-find-by-name multi-project-last))
           (when solutionlist
             (setq compile-command
                   (funcall multi-project-compilation-command solutionlist)))))

    ; Set the function for naming the compilation buffer
    (unless compilation-buffer-name-function
      (setq compilation-buffer-name-function 'multi-project-compile-buffer-name))
    (compile (multi-project-compile-prompt compile-command))))

(defun multi-project-find-root (parentDir childDir)
  "Takes two directories as arguments and return the first directory path that is different Argument PARENTDIR The parent directory of the child.  Argument CHILDDIR A directory found under the parent."
  (interactive)
  
  (let ((tlst (split-string childDir "[/\\]"))
        (lst (split-string parentDir "[/\\]"))
        (fpath)
        (tfpath)
        (index 0)
        (root))
    (while lst
      (setq fpath (car lst))
      (setq lst (cdr lst))
      (setq tfpath (nth index tlst))
      (setq index (1+ index))
      
      (if (string-equal fpath tfpath)
          (if root
              (setq root (append root (list fpath)))
            (setq root (list fpath)))))

    (if (nth index tlst)
        (setq root (append root (list (nth index tlst)))))
    (mapconcat 'identity root "/")))

(defun multi-project-basename (directory)
  "Return the basename of a DIRECTORY."
  (let ((lst (split-string directory "[/\\]")))
    (car (last lst))))

(defun multi-project-dir-as-file (directory)
  "Convert a DIRECTORY name that trails with a slash to a filename."
  (replace-regexp-in-string "/$" "" directory))

(defun multi-project-remote-file (filename)
  "Return t if the FILENAME is remote."
  (if (fboundp 'file-remote-p)
      (file-remote-p filename)
    ;; No 'file-remote-p so try to determine by filename
    (if (string-match "@?\\w+:" filename)
	t)))
    

;;;###autoload
(defun multi-project-root ()
  "Jumps to the root of a project based upon current directory."
  (interactive)
  (let ((solutionlist (multi-project-find-by-directory)))
    (if solutionlist
        (let ((searchdir (multi-project-find-root (nth 1 solutionlist)
                                                  default-directory)))
          (multi-project-dired (nth 1 solutionlist) (nth 2 solutionlist)
                               (multi-project-basename searchdir)))
      (multi-project-display-projects))))

(defun multi-project-dirname (filename)
  "Return the directory name of FILENAME."
  (let ((filelist)
        (result))
    (setq filelist (reverse (split-string filename "/")))
    (mapc (lambda (x) (setq result (concat x "/" result)))
          (cdr filelist))
    (directory-file-name result)))

;;;###autoload
(defun multi-project-change-tags(&optional project)
  "Visits tags file based upon current directory"
  (interactive)
  (let ((solutionlist))
    
    (if project
        (setq solutionlist (multi-project-find-by-name project))
      (setq solutionlist (multi-project-find-by-directory)))

    (if solutionlist
        (let ((filename (nth 3 solutionlist)))

	  ;; if TAGS wasn't specified look for one in the top level
	  ;; directory
	  (unless filename
	    (setq filename (concat (nth 1 solutionlist) "/" "TAGS")))

          ;; if TAGS still doesn't exist visit the parent to see if the
          ;; the file exists
          (unless (file-exists-p filename)
            (setq filename (concat (multi-project-dirname (nth 1 solutionlist))
                                   "/" "TAGS")))

	  (setq filename (expand-file-name filename))
          (when (and (file-exists-p filename) (not (multi-project-remote-file filename)))
            (let ((large-file-warning-threshold nil)
                  (tags-add-tables nil)
		  (tags-buffer (get-buffer "TAGS")))

	      (let ((load-tags))
		(cond (tags-buffer
		       (let ((tags-filename (buffer-file-name tags-buffer)))
			 (when (not (string= tags-filename filename))
			   (kill-buffer tags-buffer)
			   (setq load-tags t))))
		      (t
		       (setq load-tags t)))

		(when load-tags
		  (visit-tags-table filename)
		  (message "TAGS changed to %s" tags-file-name))
		load-tags)))))))



;;;###autoload
(defun multi-project-last()
  "Jumps to the last chosen project"
  (interactive)
  (let ((project) (result))
    (if multi-project-anchored
        (setq project multi-project-anchored)
      (setq project multi-project-last))
  
    (setq result (multi-project-find-by-name project))
    (when result
      (multi-project-dired-solution result)
      (message "Last project %s" project))))

;;;###autoload
(defun multi-project-anchor()
  "Chooses a project that will be constant no matter the default directory"
  (interactive)
  (setq multi-project-anchored (car (multi-project-find-by-directory)))
  (if multi-project-anchored
      (message "%s anchored" multi-project-anchored)))

;;;###autoload
(defun multi-project-reset-anchor()
  "Resets the multi-project-anchored variable."
  (interactive)
  (when multi-project-anchored
    (message "%s no longer anchored." multi-project-anchored)
    (setq multi-project-anchored nil)))

(defun multi-project-display-anchor()
  (interactive)
  (let ((project-list (multi-project-select)))
    (when project-list
      (setq multi-project-anchored (car project-list))
      (message "%s anchored" multi-project-anchored))))

;;;###autoload
(defun multi-project-display-change-tags()
  (interactive)
  (let ((project-list (multi-project-select)))
    (when project-list
      (multi-project-change-tags (car project-list))
      (message "Loaded tags for %s " (car project-list)))))

(defun multi-project-max-length(projects)
  "Retrun the max length of a project."
  (apply 'max(mapcar (lambda (x) (length (car x))) projects)))

(defun multi-project-insert-line(key fs max-length)
  (let ((numspaces (- max-length (length key))))
    
    (insert (concat "  " key))
    (while (> numspaces 0)
      (insert " ")
      (setq numspaces (- numspaces 1)))
    (insert "\t")
    (insert fs)
    
    (insert " ")
    (add-text-properties (point-at-bol) (point-at-eol)
                         '(mouse-face highlight))
    (insert "\n")))

;;;###autoload
(defun multi-project-display-projects()
  "Displays a buffer with the various projects"
  (interactive)
  (multi-project-create-display multi-project-previous-input)
  (switch-to-buffer multi-project-buffer))

(defun multi-project-display-reset()
  "Resets the filter used for the projects"
  (interactive)
  (setq multi-project-previous-input nil)
  (multi-project-display-projects))


(defun multi-project-create-display(&optional projectkey)
  "Inserts the configured projects into the multi-project buffer"
  (get-buffer-create multi-project-buffer)

  (with-current-buffer multi-project-buffer
    (multi-project-minor-mode 1)
    (setq buffer-read-only nil)

    ;; Borrowed from package.el.  Thanks!
    (setq header-line-format
	  (mapconcat
	   (lambda (pair)
	     (let ((column (car pair))
		   (name (cdr pair)))
	       (concat
		;; Insert a space that aligns the button properly.
		(propertize " " 'display (list 'space :align-to column)
			    'face 'fixed-pitch)
		;; Set up the column button.
		(if (string= name "Directory")
		    name
		  (propertize name
			      'column-name name
			      'help-echo "mouse-1: sort by column"
			      'mouse-face 'highlight
			      )))))
	   ;; We take a trick from buff-menu and have a dummy leading
	   ;; space to align the header line with the beginning of the
	   ;; text.  This doesn't really work properly on Emacs 21,
	   ;; but it is close enough.
	   '((0 . "")
	     (2 . "Project")
	     (30 . "Directory"))
	   ""))

    (setq multi-project-roots (sort multi-project-roots (lambda (a b) (string< (car a) (car b)))))
    (erase-buffer)
    (let ((max-length (multi-project-max-length multi-project-roots)))
      (dolist (item multi-project-roots)
	(if (and projectkey
		 (string-match projectkey (car item)))
	    (multi-project-insert-line (car item) (nth 1 item) max-length))

	(if (equal projectkey nil)
	    (multi-project-insert-line (car item) (nth 1 item) max-length))))
      (setq buffer-read-only t)

    (goto-char (point-min))

    (setq multi-project-overlay (make-overlay (point-min) (point-min)))
    (overlay-put multi-project-overlay 'face 'multi-project-selection-face)
    (multi-project-mark-line)))


(defun multi-project-mark-line ()
  "Mark the current line."
  (move-overlay multi-project-overlay (point-at-bol) (point-at-eol)))

(defun multi-project-move-selection (buf movefunc movearg)
  "Move the selection marker to a new position in BUF determined by MOVEFUNC and MOVEARG."
  (unless (= (buffer-size (get-buffer buf)) 0)
    (save-selected-window
      (select-window (get-buffer-window buf))

      (condition-case nil
          (funcall movefunc movearg)
        (beginning-of-buffer (goto-char (point-min)))
        (end-of-buffer (goto-char (point-max))))

      ;; if line end is point-max then it's either an incomplete line or
      ;; the end of the output, so move up a line
      (if (= (point-at-eol) (point-max))
          (forward-line -1))

      (multi-project-mark-line))))

(defun multi-project-previous-line ()
  "Move selection to the previous line."
  (interactive)
  (multi-project-move-selection multi-project-buffer 'next-line -1))
 
(defun multi-project-next-line ()
  "Move selection to the next line."
  (interactive)
  (multi-project-move-selection multi-project-buffer 'next-line 1))

(define-minor-mode multi-project-minor-mode
  "Minor mode for working with multiple projects"
  nil
  " MP-Proj"
  multi-project-map)
  
(defun multi-project-quit ()
  "Kill the MP buffer."
  (interactive)
  (quit-window))

(defun multi-project-select ()
  "Select the project from the displayed list."
  (interactive)
  (let ((selectedline (buffer-substring-no-properties (point-at-bol)
						      (point-at-eol)))
        (solution)
        (project-list))
    (setq solution (multi-project-filter-empty-string
		    (split-string selectedline "[\t ]+")))
    (setq project-list (multi-project-find-by-name (car solution)))
    project-list))

(defun multi-project-display-select (&optional otherwindow)
  "Select the project and visit the project's tree.
Optional argument OTHERWINDOW if true, the display is created in a secondary window.e."
  (interactive)
  (let ((project-list (multi-project-select)))
    (when project-list
      (if (not (string= multi-project-current multi-project-last))
          (setq multi-project-last multi-project-current))

      (setq multi-project-current (car project-list))
      (multi-project-change-tags (car project-list))
      (multi-project-dired-solution project-list nil otherwindow))))
                      
(defun multi-project-display-select-other-window ()
  "Select the project, but places it in another window."
  (interactive)
  (multi-project-display-select t))

(defun multi-project-check-input()
  "Check for input"
  (let ((input (minibuffer-contents)))
    (if (not (string-equal input multi-project-previous-input))
        (progn
          (multi-project-create-display input)
          (setq multi-project-previous-input input)))))

(defun multi-project-exit-minibuffer()
  "Exit from the minibuffer"
  (interactive)
  (exit-minibuffer))

(defun multi-project-display-search ()
  "Search the list of projects for keywords."
  (interactive)
  (add-hook 'post-command-hook 'multi-project-check-input)

  (unwind-protect
      (let ((minibuffer-local-map multi-project-minibuffer-map))
        (read-string "substring: "))
    (remove-hook 'post-command-hook 'multi-project-check-input))

  (with-current-buffer multi-project-buffer
    (multi-project-display-select)))
  

(defconst multi-project-file-buffer "*mp-find-file*"
  "Buffer used for finding files.")

(defun multi-project-tag-find-files (pattern)
  "Find a list of files based upon a regular expression PATTERN."
  (let ((result nil))
    (save-excursion
      (let ((large-file-warning-threshold nil)
            (tags-add-tables nil))
        (when  (visit-tags-table-buffer)
          (unless tags-table-files (tags-table-files))

          (dolist (file tags-table-files)
            (when (and (string-match pattern (file-name-nondirectory file)) file)
              (setq result (cons file result)))))))
    (sort result (lambda (a b) (string< a b)))))

(defun multi-project-gtag-find-files (pattern)
  "Find a list of files based upon a regular expression PATTERN using global."
  (let ((mp-gtags-buffer (get-buffer-create "*mp-gtags*")))
    (with-current-buffer mp-gtags-buffer
      (erase-buffer)
      (call-process "global" nil t nil "-Poe" pattern)
      (list (buffer-string)))))

(defun multi-project-find-files (pattern)
  "Find a list of files based upon a PATTERN."
  (let ((tags-type (multi-project-tags-type
		    (multi-project-find-by-name multi-project-current))))
    (cond ((string= tags-type 'TAGS)
	   (multi-project-tag-find-files pattern))
	  ((string= tags-type 'GTAGS)
	   (multi-project-gtag-find-files pattern)))))

(defun multi-project-tags-type (project)
  "Return TAGS or GTAGS based upon the PROJECT."
  (let ((project-dir (nth 1 project)))
    (cond ((and (>= (length project) 4) (file-exists-p (nth 3 project)))
	   'TAGS)
	  ((file-exists-p (concat project-dir "/" "GTAGS"))
	   'GTAGS)
	  ((file-exists-p (concat project-dir "/" "TAGS"))
	   'TAGS))))
			 
(defvar multi-project-file-minibuffer-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "<down>") 'multi-project-file-next-line)
    (define-key map (kbd "C-n") 'multi-project-file-next-line)
    (define-key map (kbd "<up>") 'multi-project-file-previous-line)
    (define-key map (kbd "C-p") 'multi-project-file-previous-line)
    (define-key map (kbd "<RET>") 'multi-project-exit-minibuffer)
    map)
  "Keymap for `multi-project-file' mode.")

(defun multi-project-file-previous-line ()
  "Move selection to the previous line."
  (interactive)
  (multi-project-move-selection multi-project-file-buffer 'next-logical-line -1))
 
(defun multi-project-file-next-line ()
  "Move selection to the next line."
  (interactive)
  (save-excursion multi-project-file-buffer
                  (multi-project-move-selection multi-project-file-buffer
                                                'next-logical-line 1)))

(defun multi-project-find-file-display (input)
  "Display the list of files that match INPUT from the minibuffer."
  (interactive)

  (with-current-buffer multi-project-file-buffer
    (let ((result nil))
      (setq result (multi-project-find-files input))
      (setq buffer-read-only nil)
      (erase-buffer)
      (dolist (item result)
        (insert item "\n"))

      (if (= (point) (point-max))
	  (goto-char (point-min)))

      (setq buffer-read-only t)

      (multi-project-mark-line))))

(defun multi-project-check-file-input()
  "Check for input"
  (if (sit-for 0.2)
      (let ((input (minibuffer-contents)))
        (if (and (not (string-equal input multi-project-previous-file-input))
                 (>= (length input) 1))
            (progn
              (multi-project-find-file-display input)
              (setq multi-project-previous-file-input input))))))

(defun multi-project-file-select ()
  "Select from the list of files presented."
  (with-current-buffer multi-project-file-buffer
    (let ((filename (buffer-substring-no-properties (point-at-bol)
						    (point-at-eol))))
      (save-excursion
	(visit-tags-table-buffer)
	(find-file filename)))))
  
;;;###autoload
(defun multi-project-find-file ()
  "Search a TAGS file for a particular file that match a user's input."
  (interactive)

  ;; Try determining which TAGS file
  (unless (get-buffer "TAGS")
    (unless (multi-project-change-tags)
      (error "Unable to get TAGS for project")))

  (add-hook 'post-command-hook 'multi-project-check-file-input)

  (switch-to-buffer multi-project-file-buffer)
  (setq multi-project-overlay (make-overlay (point-min) (point-min)))
  (overlay-put multi-project-overlay 'face 'multi-project-selection-face)

  (unwind-protect
      (let ((minibuffer-local-map multi-project-file-minibuffer-map))
	(read-string "Filename substring: "))
    (remove-hook 'post-command-hook 'multi-project-check-file-input))

  (with-current-buffer multi-project-file-buffer
    (multi-project-file-select))
  (kill-buffer multi-project-file-buffer))

;;;###autoload
(defadvice find-tag (before multi-project-find-tag
                            (TAGNAME &optional NEXT-P REGEXP-P))
  "Determine which TAGS file should be used based upon the current directory."
  (let ((project (multi-project-find-by-directory)))
    (when project
      (multi-project-change-tags (car project)))))

(defun multi-project-file-base (directory filename)
  "The DIRECTORY is removed from FILENAME."
  (replace-regexp-in-string (concat directory "/?")  "" filename))

(defun multi-project-add-file (filename)
  "Add the FILENAME to the TAGS file."
  (interactive)
  (let ((project (multi-project-filter-dir filename multi-project-roots))
	(tags-buf (get-buffer "TAGS"))
	(file))

    (setq file (multi-project-file-base (nth 1 project) filename))
    
    (save-excursion
      (with-current-buffer tags-buf
        (goto-char (point-max))
        (insert "\n")
        (insert file ",0\n")))))

(defun multi-project-current-project ()
  "Jump to the current project."
  (interactive)
  (when multi-project-current
    (let ((project (multi-project-find-by-name multi-project-current)))
      (when project
        (multi-project-dired-solution project)
        (message "Present project %s" (car project))))))

(defun multi-project-add-project ()
  "Add a project to the list of projects."
  (interactive)
  (let ((project-name)
        (project-directory)
        (project-tags)
        (project-subdir)
        (project-list))

    (setq project-name (read-from-minibuffer "Project name: "))
    (setq project-directory
          (multi-project-dir-as-file
           (read-file-name "Project directory: " nil default-directory)))

    (setq project-subdir
	  (multi-project-basename
           (multi-project-dir-as-file
            (read-file-name "Place cursor on: " project-directory
                            project-directory))))

    (setq project-list (list project-name project-directory project-subdir))

    (let ((tags-file (concat project-directory "/TAGS")))
      (unless (file-exists-p tags-file)
        (setq tags-file nil))

      (setq project-tags (read-file-name "Project tags: " nil tags-file))
      (if (and (> (length project-tags) 0)
	       (file-exists-p project-tags)
	       (string-match "TAGS$" project-tags))
          (add-to-list 'project-list project-tags t))
      (when (y-or-n-p "Create a TAGS file? ")
	(message "Creating TAGS file...")
	(call-process-shell-command (concat "find " project-directory
					    " -type f | etags -"))))

    (add-to-list 'multi-project-roots project-list t)
    (multi-project-save-projects)

    (message "Added %s" project-name)
    project-name))
                         
(defun multi-project-delete-project (project)
  "Delete a project named PROJECT from the list of managed projects."
  (let ((lst (multi-project-filter-name project multi-project-roots)))
    (setq multi-project-roots (delq lst multi-project-roots))
    (multi-project-save-projects)))

(defun multi-project-filename ()
  "Construct the filename used for saving or loading projects."
  (concat multi-project-dir "/" multi-project-file))

(defun multi-project-save-projects ()
  "Save the project configuration to a file."
  (interactive)
  (let ((mp-file (multi-project-filename)))
    (if (not (file-exists-p multi-project-dir))
        (make-directory multi-project-dir t))

    (multi-project-write-file mp-file)
    (message "Projects saved to %s" mp-file)))

(defun multi-project-write-file (filename)
  "Write `multi-project-roots' to FILENAME."
  (message "Saving project configuration to file %s..." filename)
  (with-current-buffer (get-buffer-create "*MP-Projects*")
    (erase-buffer)
    (goto-char (point-min))
    (pp multi-project-roots (current-buffer))
    (condition-case nil
        (write-region (point-min) (point-max) filename)
      (file-error (message "Can't write %s" filename)))
    (kill-buffer (current-buffer))))

(defun multi-project-list-from-buffer ()
  "Create a list from the `current-buffer'."
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "(" nil t)
        (progn
          (forward-char -1)
          (read (current-buffer)))
      (error "Not multi-project format"))))

(defun multi-project-read-file (filename)
  "Read the FILENAME and set `multi-project-roots'."
  (when (file-exists-p filename)
    (with-current-buffer (find-file-noselect filename)
      (multi-project-list-from-buffer))))
    
(defun multi-project-read-projects ()
  "Read the project configuration."
  (interactive)
  (setq multi-project-roots (multi-project-read-file (multi-project-filename)))
  (message "Projects read from %s." (multi-project-filename)))

(defun multi-project-mark-project (mark-symbol)
  "Mark the selected projects with MARK-SYMBOL."
  (setq buffer-read-only nil)
  (goto-char (point-at-bol))
  (insert mark-symbol)
  (delete-char 1)
  (multi-project-next-line)
  (goto-char (point-at-bol))
  (setq buffer-read-only t))

(defun multi-project-unmark-project ()
  "Unmark the selected projects."
  (interactive)
  (setq buffer-read-only nil)
  (goto-char (point-at-bol))
  (delete-char 1)
  (insert " ")
  (goto-char (point-at-bol))
  (multi-project-next-line)
  (setq buffer-read-only t))

(defun multi-project-new-project ()
  "Add a new project from the multi-project display."
  (interactive)
  (let ((project (multi-project-add-project)))
    (when project
      (multi-project-display-projects)
      (goto-char (point-min))
      (when (re-search-forward project nil t)
          (goto-char (point-at-bol))
          (multi-project-mark-line)))))

(defun multi-project-mark-deletions ()
  "Mark the project for deletion."
  (interactive)
  (multi-project-mark-project "D"))
  
(defun multi-project-marked-projects (marker)
  "Return a list of marked projects based upon MARKER."
  (let ((lst))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat "^" marker " +\\([^ \t]+\\)") nil t)
        (setq lst (cons (match-string 1) lst))))
    lst))

(defun multi-project-execute-actions ()
  "Execute the action on the marked projects."
  (interactive)
  (let ((current-point (point))
        (projects (multi-project-marked-projects "D")))
    (if (> (length projects) 0)
	(when (y-or-n-p (concat "Remove "
				(mapconcat 'identity projects ", ")
				"? "))
	  (dolist (project (multi-project-marked-projects "D"))
	    (multi-project-delete-project project))
	  (multi-project-display-projects)
	  (if (> current-point (point-max))
	      (setq current-point (point-max)))
	  (goto-char current-point)
	  (goto-char (point-at-bol))
	  (multi-project-mark-line)))))

(defun multi-project-mouse-select (event)
  "Visit the project that was clicked on based upon EVENT."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))

    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (multi-project-display-select))))

(easy-menu-define multi-project-mode-menu global-multi-project-map
  "'multi-project-mode' menu"
  '("MP"
    ["Jump to a project" multi-project-display-projects t]
    ["Compile..." multi-project-compile t]
    ["Find file..." multi-project-find-file t]
    ["Add project..." multi-project-add-project t]
    ["Anchor project" multi-project-anchor t]
    ["Reset anchor" multi-project-reset-anchor t]
    ["Last project" multi-project-last t]
    ["Jump to current project" multi-project-current-project t]
    ))

;;;###autoload
(define-minor-mode global-multi-project-mode
  "Toggle multi-project mode."
  nil
  " MP"
  global-multi-project-map
  :global t
  :group 'project
  (if global-multi-project-mode
      (progn
        (unless multi-project-roots
          (multi-project-read-projects))

        (ad-enable-advice 'find-tag 'before 'multi-project-find-tag)
        (ad-activate 'find-tag))
    (ad-disable-advice 'find-tag 'before 'multi-project-find-tag)))

(provide 'multi-project)

;;; multi-project.el ends here
