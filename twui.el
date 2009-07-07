;;;; twui.el -- User interface library
;;;; This file is part of Twitel (http://github.com/artagnon/twitel)

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

(require 'json)
(require 'url)
(require 'twparse)
(require 'twapi)
(require 'twhelper)
(require 'oauth)

;; ================
;; Global variables
;; ================

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

(defvar *buffer-names-assoc*
  (list (cons 'own "*timeline*")
	(cons 'user "*user-timeline*")
	(cons 'search "*search*")
	(cons 'mentions "*mentions*")))
(defvar *max-status-len* 140)
(defface *exceed-warn-face*
  '((t (:foreground "red"))) "Face used to highlight extra characters")


;; TODO: `artagnon' cannot be hardcoded here!
(defvar *dp-cache-dir* "/home/artagnon/.twitel/dp-cache")
(defvar *dp-fetch-queue* '())
(defvar *frame-config-view* nil)

;; Keymaps
(defvar status-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'update-status-buffer-string)
    (define-key map "\C-c\C-k" 'kill-status-buffer)
    map)
  "Keymap for `status-edit-mode'")

(defvar timeline-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-n" 'forward-tweet)
    (define-key map "\C-p" 'backward-tweet)
    (define-key map "\C-f" 'forward-button)
    (define-key map "\C-b" 'backward-button)
    (define-key map "\C-cr" 'reply-to-this-tweet)
    (define-key map "\C-c\C-r" 'retweet-this-tweet)
    map)
  "Keymap for `timeline-view-mode'")

;; =====================
;; Interactive functions
;; =====================

(defun twitter-init ()
  "Check if the configuration directory exists and authenticate"
  (interactive)
  (mapc #'(lambda (directory) (create-dir-noexist directory))
	'("~/.twitel" "~/.twitel/dp-cache" "~/.twitel/tweet-cache"))
  (twitter-authenticate)
  (twitter-friends-timeline))

(defun update-status-minibuffer ()
  "Update status from the minibuffer"
  (interactive)
  (let status (read-string "Status: ")
       (twitter-update-status (url-hexify-string status))))

(defun zbuffer-popout ()
  "Pops out a special zbuffer for editing tweets"
  (setq twitter-frame-configuration (current-frame-configuration))
  (interactive)
  (pop-to-buffer "*zbuffer*")
  (zbuffer-mode))

(defun update-status-buffer-string ()
  "Update status with contents of current buffer"
  (interactive)
  (twitter-update-status (url-hexify-string (buffer-string))))

(defun reply-to-this-tweet (pos)
  "Reply to tweet at point"
  (interactive "d")
  (let ((status-id (get-text-property pos 'tweet-id))
	(status-screen-name (get-text-property pos 'tweet-author-screen-name)))
    (when (null status-screen-name)
      (error "Missing screen name in status"))
    (when (null status-id)
      (error "Missing status id"))
    (zbuffer-popout)
    (setq twitter-reply-status-id status-id)
    (insert "@" status-screen-name " ")))

(defun retweet-this-tweet (pos)
  "Retweet tweet at point"
  (interactive "d")
  (let ((status-id (get-text-property pos 'tweet-id))
	(status-screen-name (get-text-property pos 'tweet-author-screen-name)))
    (when (null status-screen-name)
      (error "Missing screen name in status"))
    (when (null status-id)
      (error "Missing status id"))
    (zbuffer-popout)
    (insert "RT @" status-screen-name ": ")))

(defun kill-status-buffer ()
  "Kill the *Twitter Status* buffer and restore the previous frame configuration."
  (interactive)
  (kill-buffer "*Twitter Status*")
  (set-frame-configuration twitter-frame-configuration))

(defun forward-tweet (pos)
  "Jump to next tweet"
  (interactive "d")
  (goto-char (or (next-single-property-change pos 'tweet-id) (point-max))))

(defun backward-tweet (pos)
  "Jump to next tweet"
  (interactive "d")
  (goto-char (or (previous-single-property-change pos 'tweet-id) (point-min))))

;; =====
;; Modes
;; =====

(define-derived-mode timeline-view-mode view-mode "Timeline view"
  "Major mode for viewing Twitter timelines"
  (setf left-margin-width 6
	fringes-outside-margins t))

(define-derived-mode zbuffer-mode text-mode "Status edit"
  "Major mode for updating your Twitter status."
  ;; Schedule to update the character count after altering the buffer
  (make-local-variable 'after-change-functions)
  (add-hook 'after-change-functions 'twitter-status-edit-after-change)
  ;; Add the remaining character count to the mode line
  (make-local-variable 'twitter-status-edit-remaining-length)
  ;; Copy the mode line format list so we can safely edit it without
  ;; affecting other buffers
  (setq mode-line-format (copy-sequence mode-line-format))
  ;; Add the remaining characters variable after the mode display
  (let ((n mode-line-format))
    (catch 'found
      (while n
        (when (eq 'mode-line-modes (car n))
          (setcdr n (cons 'twitter-status-edit-remaining-length
                          (cdr n)))
          (throw 'found nil))
        (setq n (cdr n)))))
  ;; Make a buffer-local reference to the overlay for overlong
  ;; messages
  (make-local-variable 'twitter-status-edit-overlay)
  ;; A buffer local variable for the reply id. This is filled in when
  ;; the reply button is pressed
  (make-local-variable 'twitter-reply-status-id)
  (setq twitter-reply-status-id nil)
  ;; Update the mode line immediatly
  (twitter-status-edit-update-length))

;; =========
;; Functions
;; =========

(defun twitter-http-callback (status)
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
	(save-excursion
	  (let ((parsed-object (json-read-from-string (cdr http-info))))
	    (render-timeline
	     (master-response-parser
	      parsed-object)
	     'own)))
	(kill-buffer))))

(defun create-dir-noexist (directory)
  (if (file-directory-p directory)
      nil
      (make-directory directory)))

(defun twitter-authenticate ()
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
		      'twitter-http-callback))

(defun make-screen-name-button (screen-name)
  "Inserts a link to screen-name into the current buffer"
  (let* ((caption (concat "@" (copy-seq screen-name)))
	 (target-uri (concat "http://twitter.com/" caption)))
    (make-text-button caption nil
;;		      'action (let ((buf-name (cdr  (assoc 'user-timeline *buffer-names-assoc*)))) 
;;				(twitter-user-timeline nil nil screen-name)
;;				(get-buffer-create buf-name)
;;				(switch-to-buffer buf-name))
		      'uri target-uri
		      'follow-link target-uri)
    caption))

(defun make-uri-button (link)
  "Inserts an external link into the current buffer"
  (let ((link (copy-seq link)))
    (make-text-button link nil
		   'uri link
		   'follow-link link)
    link))

(defun twitter-status-edit-update-length ()
  "Updates the character count in Twitter status buffers.
This should be run after the text in the buffer is changed. Any
characters after the maximum status update length are
hightlighted in the face twitter-status-overlong-face and the
character count on the mode line is updated."
  ;; Update the remaining characters in the mode line
  (let ((remaining (- *max-status-len*
                      (buffer-size))))
    (setq twitter-status-edit-remaining-length
          (concat " "
                  (if (>= remaining 0)
                      (number-to-string remaining)
                    (propertize (number-to-string remaining)
                                'face 'exceed-warn-face))
                  " ")))
  (force-mode-line-update)
  ;; Highlight the characters in the buffer that are over the limit
  (if (> (buffer-size) *max-status-len*)
      (let ((start (+ (point-min) *max-status-length*)))
        (if (null twitter-status-edit-overlay)
            (overlay-put (setq twitter-status-edit-overlay
                               (make-overlay start (point-max)))
                         'face 'exceed-warn-face)
          (move-overlay twitter-status-edit-overlay
                        start (point-max))))
    ;; Buffer is not too long so just hide the overlay
    (when twitter-status-edit-overlay
      (delete-overlay twitter-status-edit-overlay))))

(defun twitter-status-edit ()
  "Edit your twitter status in a new buffer.
A new buffer is popped up in a special edit mode. Press
\\[twitter-status-post] when you are finished editing to send the
message."
  (setq twitter-frame-configuration (current-frame-configuration))
  (interactive)
  (pop-to-buffer "*Twitter Status*")
  (twitter-status-edit-mode))

(defun fill-line (&rest text)
  "Carefully fills region with text tracking point"
  (fill-region (prog1
		   (point)
		 (insert (apply 'concat text)))
	       (progn (backward-char) (point))))

(defun insert-tweet (tweet)
  "Inserts a tweet into the current buffer"
  (goto-char (point-min))  
  (let ((tweet-begin (point))
	(tweet-id (gethash 'tweet-id tweet))
	(text (gethash 'text tweet))
	(screen-name (gethash 'screen-name tweet))
	(source (car (gethash 'source tweet)))
	(timestamp (gethash 'timestamp tweet))
	(dp-url (gethash 'dp-url tweet))
	(uri-list (gethash 'uri-list tweet))
	(screen-name-list (gethash 'screen-name-list tweet))
	(fav-p (gethash 'fav-p tweet)))
    (insert-image (build-image-descriptor dp-url) nil 'left-margin)
    (fill-line screen-name " | " source " | " (format-twitter-time timestamp))
    (insert "\n")
    (fill-line text)
    (insert "\n")
    (when (and screen-name uri-list)
      (fill-line
       (mapconcat #'(lambda (uri)
		      (make-uri-button uri)) uri-list " | ")
       (if screen-name-list " | ")
       (mapconcat #'(lambda (screen-name)
		      (make-screen-name-button screen-name)) screen-name-list " | "))
      (insert "\n"))
    (insert "\n")
  
    ;; Tweet inserted. Now add text properties to the tweet
    (add-text-properties tweet-begin (point)
			 `(tweet-id
			   ,tweet-id
			   tweet-author-screen-name
			   ,screen-name)))
  (fetch-beautify-dp))

(defun render-timeline (tweet-list buf-name)
  "Renders a list of tweets"
  (let ((timeline-buffer (get-buffer-create (cdr (assoc buf-name *buffer-names-assoc*)))))
    (with-current-buffer timeline-buffer
      (kill-all-local-variables)
      (timeline-view-mode)
      (let ((inhibit-read-only t))
	(goto-char (point-min))
	(mapcar
	 #'(lambda (tweet-hashtable) (insert-tweet tweet-hashtable))
	 tweet-list)))))

(defun guess-image-type-extn (file-name)
  (cond
   ((string-match "\\.jpe?g" file-name) 'jpeg)
   ((string-match "\\.png" file-name) 'png)
   ((string-match "\\.gif" file-name) 'gif)
   (t nil)))

(defun build-image-descriptor (filename-uri-pair)
  (let* ((filename (car filename-uri-pair))
	 (uri (cdr filename-uri-pair))
	 (filepath (concat *dp-cache-dir* "/" filename))
	 (file-type (guess-image-type-extn filename)))
    (unless (file-exists-p filepath) (push (cons filename uri) *dp-fetch-queue*))
    (if file-type (create-image filepath file-type nil))))

(defun fetch-beautify-dp ()
  "Fetch the display pics in *dp-fetch-queue*: An alist of (filename . uri)"
  (mapc
   #'(lambda (cons-pair)
       (start-process-shell-command
	"dp-fetcher"
	nil
	(format
	 (concat "wget -O - %s -q "
		 "| convert - -resize 48 - "
		 "| composite -compose Dst_In "
		 "-gravity center -matte round_mask_48.png - %s")
	 (cdr cons-pair)
	 (concat *dp-cache-dir* "/" (car cons-pair)))))
   *dp-fetch-queue*)
  (setq *dp-fetch-queue* '()))

(defun format-twitter-time (time)
  "Convert TIME to a friendly human readable string.
TIME should be a high/low pair as returned by encode-time."
  ;; This is based on a similar function from Tweet
  (let* ((now (current-time))
         (age (subtract-time now time))
         (age-days (- (time-to-days now) (time-to-days time))))
    (if (or (< (car age) 0)
            (>= (car age) 16) ; more than about 12 days
            (>= age-days 7))
        (format-time-string "%x at %H:%M" time)
      (let* ((age-seconds (logior (lsh (car age) 16) (cadr age)))
             (age-minutes (/ age-seconds 60))
             (age-hours (/ age-minutes 60)))
        (cond ((< age-seconds 60)
               "Less than a minute ago")
              ((<= age-minutes 1)
               "About a minute ago")
              ((< age-minutes 60)
               (format "About %d minutes ago" age-minutes))
              ((<= age-hours 1)
               "About an hour ago")
              ((< age-minutes 360)
               (format "About %d hours ago" age-hours))
              ((<= age-days 0)
               (format-time-string "Today at %H:%M" time))
              ((<= age-days 1)
               (format-time-string "Yesterday at %H:%M" time))
              (t
               (format-time-string "Last %A at %H:%M" time)))))))
