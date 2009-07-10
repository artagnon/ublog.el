;;;; twhelper.el -- Helper functions for Twitel
;;;; This file is part of µblog.el (http://github.com/artagnon/ublog.el)

;; Copyright (C) 2009 Ramkumar R <artagnon@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

(provide 'twhelper)

(defmacro case-string (str &rest clauses)
  `(cond
    ,@(mapcar
       (lambda (clause)
	 (let ((keylist (car clause))
	       (body (cdr clause)))
	   `(,(if (listp keylist)
		  `(or ,@(mapcar (lambda (key) `(string-equal ,str ,key))
				 keylist))
		't)
	     ,@body)))
       clauses)))

(defmacro get-or-generate-buffer (buffer)
  "Get an exisiting Emacs buffer or generate a new one"
    (if (bufferp buffer)
      (if (buffer-live-p buffer)
	  buffer
	(generate-new-buffer (buffer-name buffer)))
    (if (stringp buffer)
	(or (get-buffer buffer)
	    (generate-new-buffer buffer)))))

(defun twitel-url-reserved-p (ch)
  (or (and (<= ?A ch) (<= ch ?z))
      (and (<= ?0 ch) (<= ch ?9))
      (eq ?. ch)
      (eq ?- ch)
      (eq ?_ ch)
      (eq ?~ ch)))

(defun url-percent-encode (str &optional coding-system)
  (if (or (null coding-system)
          (not (coding-system-p coding-system)))
      (setq coding-system 'utf-8))
  (mapconcat
   (lambda (c)
     (cond
       ((twitel-url-reserved-p c)
        (char-to-string c))
       ((eq c ? ) "+")
       (t (format "%%%x" c))))
   (encode-coding-string str coding-system)
   ""))

(defun build-url-parameters (parameters)
  "Build a string of url parameters given the parameters in a list"
  (concat "?"
	  (mapconcat
	   (lambda (param-pair)
	     (format "%s=%s"
		     (url-percent-encode (car param-pair))
		     (url-percent-encode (or (cdr param-pair) ""))))
	   parameters
	   "&")))

(defun extract-http-info (response-dump)
  "Extract HTTP information from a response dump"
  (let* ((double-ret-marker
	  (string-match "\r?\n\r?\n" response-dump))
	 (response-header
	  (substring response-dump 0 double-ret-marker))
	 (response-status
	  (progn
	    (string-match "HTTP/1\.1 \\([a-z0-9 ]+\\)\r?\n" response-header)
	    (match-string-no-properties 1 response-header)))
	 (response-body
	  (substring response-dump double-ret-marker)))
    (cons response-status response-body)))

(defun error-status-p (error-status)
  "Was an error encountered?"
  (eq :error (car error-status)))

(defun error-status-to-string (error-status)
  "Build error string"
  (second (second error-status)))

(defmacro union (list-1 list-2)
  "Build a union of two lists"
  (delete-dups (append list-1 list-2)))

(defmacro intersect (list-1 list-2)
  "Build an intersection of two lists"
  (delq (append list-1 list-2) (union list-1 list-2)))
