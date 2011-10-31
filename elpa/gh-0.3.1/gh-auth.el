;;; gh-auth.el --- authentication for gh.el

;; Copyright (C) 2011  Yann Hodique

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords:

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

;;

;;; Code:
(require 'eieio)
(require 'gh-common)

(defgroup gh-auth nil
  "Github authentication."
  :group 'gh)

(defvar gh-auth-username nil)
(defvar gh-auth-password nil)
(defvar gh-auth-oauth-token nil)

(defun gh-auth-get-username ()
  (let ((user (or gh-auth-username
                  (setq gh-auth-username (gh-config "user")))))
    (when (not user)
      (setq user (read-string "GitHub username: "))
      (setq gh-auth-username user)
      (gh-set-config "user" user))
    user))

(defun gh-auth-get-password ()
  (let ((pass (or gh-auth-password
                  (setq gh-auth-password (gh-config "password")))))
    (when (not pass)
      (setq pass (read-passwd "GitHub password: "))
      (setq gh-auth-password pass)
      (gh-set-config "password" pass))
    pass))

(defun gh-auth-get-oauth-token ()
  (let ((token (or gh-auth-oauth-token
                   (setq gh-auth-oauth-token (gh-config "token")))))
    (when (not token)
      (setq token (read-string "GitHub OAuth token: "))
      (setq gh-auth-oauth-token token)
      (gh-set-config "token" token))
    token))

;;;###autoload
(defclass gh-authenticator ()
  ((username :initarg :username :initform nil))
  "Abstract authenticator")

(defmethod constructor :static ((auth gh-authenticator) newname &rest args)
  (let ((obj (call-next-method)))
    (or (oref obj :username)
        (oset obj :username (gh-auth-get-username)))
    obj))

;;;###autoload
(defclass gh-password-authenticator (gh-authenticator)
  ((password :initarg :password :protection :private :initform nil))
  "Password-based authenticator")

(defmethod constructor :static ((auth gh-password-authenticator) newname &rest args)
  (let ((obj (call-next-method)))
    (or (oref obj :password)
        (oset obj :password (gh-auth-get-password)))
    obj))

(defmethod gh-auth-modify-request ((auth gh-authenticator) req))

(defmethod gh-auth-modify-request ((auth gh-password-authenticator) req)
  (object-add-to-list req :headers
                      (cons "Authorization"
                            (concat "Basic "
                                    (base64-encode-string
                                     (format "%s:%s" (oref auth :username)
                                             (encode-coding-string
                                              (oref auth :password) 'utf-8))))))
  req)

;;;###autoload
(defclass gh-oauth-authenticator (gh-authenticator)
  ((token :initarg :token :protection :private :initform nil))
  "Oauth-based authenticator")

(defmethod constructor :static ((auth gh-oauth-authenticator) newname &rest args)
  (let ((obj (call-next-method)))
    (or (oref obj :token)
        (oset obj :token (gh-auth-get-oauth-token)))
    obj))

(defmethod gh-auth-modify-request ((auth gh-oauth-authenticator) req)
  (object-add-to-list req :headers
                      (cons "Authorization"
                            (format "token %s" (oref auth :token))))
  req)

(provide 'gh-auth)
;;; gh-auth.el ends here
