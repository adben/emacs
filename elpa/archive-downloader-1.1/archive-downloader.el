;;; archive-downloader.el --- Download files from archive.org

;; Copyright (C) 2007 Michael Olson

;; Author: Michael Olson <mwolson@gnu.org>
;; Date: Sat 31-May-2007
;; Version: 1.1
;; URL: http://mwolson.org/static/dist/elisp/archive-downloader.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is useful for downloading music at http://archive.org.  Just
;; pass it a link to the listing of all files for a particular album,
;; and it will get the music for you.

;; * Prerequisites:
;;
;; - wget (binary): This is used to download the music.
;; - url.el: This is used to download the file listing.

;; * Use:

;; Copy a link from the http://archive.org/ website that contains a
;; listing of all of the files available for an album.  Then, do:
;;
;;   M-x archive-downloader-get RET
;;
;; Enter the location of the directory to store the files in.  If it
;; does not exist, it will be created.

;;; History:

;; 1.1: Fix an error that occurred when the kill ring was empty.

;; 1.0: Initial release.

;;; Code:

(require 'url)

;;; Options

(defgroup archive-downloader nil
  "Options for the archive.org music downloader."
  :group 'hypermedia)

(defcustom archive-downloader-default-location "/stuff/sounds/concerts"
  "Default location to use when storing downloaded files."
  :type 'regexp
  :group 'archive-downloader)

(defcustom archive-downloader-file-regexp "\\.ogg"
  "The regexp specifying the files to download from the directory listing.
This should match the end of the filename."
  :type 'regexp
  :group 'archive-downloader)

(defcustom archive-downloader-wget-program "wget"
  "The path to the wget program."
  :type 'string
  :group 'archive-downloader)

(defcustom archive-downloader-wget-options '("--progress=dot:mega")
  "Options to pass to wget."
  :type 'list
  :group 'archive-downloader)

(defcustom archive-downloader-after-download-hook '()
  "Functions to be called after downloading files.
Each function is passed FILE-LIST and LOCATION as arguments."
  :type 'hook
  :group 'archive-downloader)

;;; Built-in variables

(defvar archive-downloader-url-regexp "<a href=\"\\([^\"]+%s\\)\">"
  "Regexp used to scan for URLs.")

(defvar archive-downloader-file-list nil
  "Used internally.")
(make-variable-buffer-local 'archive-downloader-file-list)

(defvar archive-downloader-location nil
  "Used internally.")
(make-variable-buffer-local 'archive-downloader-location)

;;; Examples

(defun mwolson/archive-downloader-m3u-filename (location)
  "Get a name for an M3U file based on LOCATION.
This is used by `mwolson/archive-downloader-write-m3u'."
  (when (string-match "/\\([^/]+\\)/\\([^/]+\\)/$" location)
    (format "/stuff/sounds/albums/%s-concert_%s.m3u"
            (match-string 1 location) (match-string 2 location))))

(defun mwolson/archive-downloader-write-m3u (file-list location)
  "Write an M3U file based on LOCATION and FILE-LIST.
This is an example of a function that may be placed in
`archive-downloader-after-download-hook'."
  (let ((m3u (mwolson/archive-downloader-m3u-filename location)))
    (when m3u
      (with-temp-buffer
        (dolist (file file-list)
          (insert location file "\n"))
        (let ((backup-inhibited t))
          (write-file m3u))))))

;;; Main

(defun archive-downloader-get-file-list (url)
  (let ((buffer (url-retrieve-synchronously url))
        (regexp (format archive-downloader-url-regexp
                        archive-downloader-file-regexp))
        (file-list nil))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (setq file-list (cons (match-string 1) file-list)))
        (kill-buffer (current-buffer))))
    (nreverse file-list)))

(defun archive-downloader-write-index (url file-list location)
  "Write FILE-LIST to the file \"index\" at the LOCATION directory.
Each entry in FILE-LIST is relative to URL."
  (cond ((string-match "\\.html?$" url)
         (setq url (file-name-directory url)))
        ((string= (substring url -1) "/") t)
        (t (setq url (concat url "/"))))
  (with-temp-buffer
    (dolist (file file-list)
      (insert url file "\n"))
    (let ((backup-inhibited t))
      (write-file (concat location "index")))))

(defun archive-downloader-sentinel (proc event)
  "Finish the wget session."
  (let ((status (process-status proc))
        (buf (process-buffer proc)))
    (cond ((not (buffer-live-p buf))
           (message "Retrieving files...aborted"))
          ((string-match "^\\(deleted\\|terminated\\)" event)
           (with-current-buffer buf
             (message "Retrieving %s files...aborted"
                      (length archive-downloader-file-list))))
          ((memq status '(exit signal closed))
           (with-current-buffer buf
             (let ((len (length archive-downloader-file-list)))
               (when archive-downloader-after-download-hook
                 (message "Retrieving %s files...post-download hooks" len)
                 (run-hook-with-args 'archive-downloader-after-download-hook
                                     archive-downloader-file-list
                                     archive-downloader-location))
               (message "Retrieving %s files...done"
                        (length archive-downloader-file-list)))))
          (t (message "Other wget status change: %s, %s" status event)))))

(defun archive-downloader-run-wget (file-list location)
  "Use wget to download files in the file \"index\" at the directory
LOCATION.  Run `archive-downloader-after-download-hook' afterward."
  (message "Retrieving %s files..." (length file-list))
  (let* ((buffer (generate-new-buffer "*Archive Download Progress*"))
         (proc (let ((default-directory location))
                 (apply #'start-process "Archive Downloader"
                        buffer archive-downloader-wget-program
                        "-i" "index" archive-downloader-wget-options))))
    (with-current-buffer buffer
      (setq archive-downloader-file-list file-list
            archive-downloader-location location))
    (set-process-sentinel proc #'archive-downloader-sentinel)
    (pop-to-buffer buffer)))

;;;###autoload
(defun archive-downloader-get (url location)
  "Download files from URL to LOCATION."
  (interactive
   (let ((kill (and kill-ring (current-kill 0 t))))
     (list (read-string "URL: " (and (stringp kill)
                                     (string-match "^http://" kill)
                                     kill))
           (read-string "Save to directory: "
                        archive-downloader-default-location))))
  (unless (string= (substring location -1) "/")
    (setq location (concat location "/")))
  (let ((file-list (archive-downloader-get-file-list url)))
    (unless (file-exists-p location)
      (make-directory location t))
    (if (not file-list)
        (message "No files found")
      (archive-downloader-write-index url file-list location)
      (archive-downloader-run-wget file-list location))))

(provide 'archive-downloader)

;;; archive-downloader.el ends here
