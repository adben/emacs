;;; iresize.el --- Interactively resize a window

;; Copyright (C) 2005,2007  Shawn Betts

;; Author: Shawn Betts <sabetts gmail.com>
;; Version: 0.2
;; Keywords: convenience

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A minor mode to interactively resize a window. To start resizing,
;; run M-x iresize-mode. When you've resized the window the way you
;; like hit C-c C-c.

;;; Code:

(defvar iresize-mode-map 
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-p") 'enlarge-window)
    (define-key m (kbd "p") 'enlarge-window)
    (define-key m (kbd "<up>") 'enlarge-window)
    (define-key m (kbd "C-n") 'shrink-window)
    (define-key m (kbd "n") 'shrink-window)
    (define-key m (kbd "<down>") 'shrink-window)
    (define-key m (kbd "C-c C-c") 'iresize-mode)
    m))

;;;###autoload
(define-minor-mode iresize-mode
  :initial-value nil
  :lighter " IResize"
  :keymap iresize-mode-map
  :group 'iresize)

(provide 'iresize)
;;; iresize.el ends here
