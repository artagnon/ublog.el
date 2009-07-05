;;;; rewrite.el -- Twitel binder
;;;; This file is part of Twitel (http://github.com/artagnon/twitel)

;; Copyright (C) 2009 Ramkumar R (artagnon@gmail.com)

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

(require 'json)
(require 'url)
(require 'twparse)
(require 'twhelper)
(require 'oauth)

;; Configuration constants
(defvar *twitter-host* "twitter.com")
(defvar *twitter-search-host* "search.twitter.com")
(defvar *twitter-response-format* "json")
(defvar *twitter-port* 80)

;; Proxy configuration
;; TODO: Unimplemented!
(defvar *twitter-proxy-use* nil)
(defvar *twitter-proxy-server* nil)
(defvar *twitter-proxy-port* nil)

;; Twitter OAuth URLs
(defvar *twitel-request-url* "http://twitter.com/oauth/request_token")
(defvar *twitel-access-url* "http://twitter.com/oauth/access_token")
(defvar *twitel-user-authorize* "http://twitter.com/oauth/authorize")

;; Twitter OAuth keys
(defvar *twitel-consumer-key* "K9vrCLHpp5vuln72ROufzQ")
(defvar *twitel-consumer-secret* "sUTgzZRgm0GCHQNYixFx3TS2D94TwaQv90gIXhTQNcE")

(defvar *twitel-access-token* nil)
(defvar *twitel-token-file* "~/.twitel/token")

(defun twitel-master-callback (status)
  "Function gets called with current-buffer as the response dump of a HTTP request"
  (if (error-status-p status)
      ;; What to do if an error has occured
      (error (message (error-status-to-string status)))
      
      (let* ((response-dump (buffer-string))
	     (http-info (extract-http-info response-dump))
	     (json-object-type 'hash-table))
	(case-string (car http-info)
		     (("200 OK")
		      (message "Success"))
		     (t
		      (message status)))
	;; Now extract the response-body and parse the json
	;; This is either a hashtable or a vector of hashtables; handle appropriately
	(json-read-from-string (cdr http-info)))))

(defun create-dir-noexist (directory)
  (if (file-directory-p directory)
      nil
      (make-directory directory)))

(defun twitel-init ()
  "Check if the configuration directory exists and authenticate"
  (interactive)
  (mapc #'(lambda (directory) (create-dir-noexist directory))
	'("~/.twitel" "~/.twitel/dp-cache" "~/.twitel/tweet-cache"))
  (twitel-authenticate))

(defun twitel-authenticate ()
  "Get authentication token"
  (if (file-exists-p *twitel-token-file*)
      ;; Enter this only after authenticating the first time
      (save-excursion
	;; Read file to get `acess token'
	(find-file *twitel-token-file*)
	(let ((str (buffer-substring (point-min) (point-max))))
	  (if (string-match "\\([^:]*\\):\\(.*\\)"
			    (buffer-substring (point-min) (point-max)))
	      (setq *twitel-access-token*
		    ;; *twitel-access-token* is set
		    (make-oauth-access-token
		     :consumer-key *twitel-consumer-key*
		     :consumer-secret *twitel-consumer-secret*
		     :auth-t (make-oauth-t
			      :token (match-string 1 str)
			      :token-secret (match-string 2 str))))))
	(save-buffer)
	(kill-this-buffer))
      (unless *twitel-access-token*
	;; Unless *twitel-access-token* was set without entering the if branch
	;; ie. twitel-authenticate called unnecessarily. Just return *twitel-access-token*
        (let ((callback
               (lambda ()
		 ;; This function will be invoked later, somewhere within oauth-twitter-app
		 ;; Adds parameter callback_token to access_url
                 (let ((callback-token (read-string
                                        "Please enter the provided code: ")))
                   (setq access-url
                         (concat access-url "?oauth_verifier=" callback-token))))))
          (setq *twitel-access-token*
                (oauth-authorize-app *twitel-consumer-key*
                                     *twitel-consumer-secret*
                                     *twitel-request-url*
                                     *twitel-access-url*
                                     *twitel-user-authorize*
				     callback)))

	;; We just got *twitel-access-token*. Save it to a file for all future authentication.
        (save-excursion
          (find-file *twitel-token-file*)
          (end-of-buffer)
          (let ((token (oauth-access-token-auth-t *twitel-access-token*)))
            (insert (format "%s:%s\n"
                            (oauth-t-token token)
                            (oauth-t-token-secret token))))
          (save-buffer)
          (kill-this-buffer)))
      *twitel-access-token*))

(defun twitter-url (&optional relative search-flag)
  "Generate a Twitter URL with an optional relative"
  (format "http://%s:%d/%s"
	  (if search-flag *twitter-search-host* *twitter-host*)
	  *twitter-port* (or relative "")))

(defun twitter-request (url http-method &optional parameters)
  "Use HTTP METHOD to request URL with some optional parameters"
  (oauth-url-retrieve *twitel-access-token*
		      (concat url
			      "."
			      *twitter-response-format*
			      (if parameters (build-url-parameters parameters) ""))
		      http-method
		      'twitel-master-callback))
