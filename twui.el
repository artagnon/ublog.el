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

(provide 'twui)
(defvar *frame-config-view*)

;; ================
;; Global variables
;; ================
(defvar *buffer-names-assoc*
  (list (cons 'own-timeline "*timeline*")
	(cons 'user-timeline "*user-timeline*")
	(cons 'search "*search*")
	(cons 'mentions "*mentions*")))

;; TODO: `artagnon' cannot be hardcoded here!
(defvar *dp-cache-dir* "/home/artagnon/.twitel/dp-cache")
(defvar *dp-cache-stack* '())

;; =====================
;; Interactive functions
;; =====================
(defun update-status-minibuffer (status)
  (interactive)
  (let status (read-string "Status: ")
       (twitter-update-status (url-hexify-string status))))

(defun twitter-kill-status-buffer ()
  "Kill the *Twitter Status* buffer and restore the previous
frame configuration."
  (interactive)
  (kill-buffer "*Twitter Status*")
  (set-frame-configuration twitter-frame-configuration))

;; ===========
;; Keybindings
;; ===========

;; =====
;; Modes
;; =====

(define-derived-mode twitter-status-edit-mode text-mode "Twitter Status Edit"
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

(define-derived-mode timeline-view-mode view-mode
  "Twitter TimelineMajor mode for viewing timelines from Twitter")

;; =========
;; Functions
;; =========

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
  (let ((remaining (- twitter-maximum-status-length
                      (buffer-size))))
    (setq twitter-status-edit-remaining-length
          (concat " "
                  (if (>= remaining 0)
                      (number-to-string remaining)
                    (propertize (number-to-string remaining)
                                'face 'twitter-status-overlong-face))
                  " ")))
  (force-mode-line-update)
  ;; Highlight the characters in the buffer that are over the limit
  (if (> (buffer-size) twitter-maximum-status-length)
      (let ((start (+ (point-min) twitter-maximum-status-length)))
        (if (null twitter-status-edit-overlay)
            (overlay-put (setq twitter-status-edit-overlay
                               (make-overlay start (point-max)))
                         'face 'twitter-status-overlong-face)
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
	       (progn (backward-char) (point)))
  (insert "\n"))

(defun insert-tweet (tweet)
  "Inserts a tweet into the current buffer"
  (goto-char (point-min))
  (let ((text (gethash 'text tweet))
	(screen-name (gethash 'screen-name tweet))
	(source (car (gethash 'source tweet)))
	(timestamp (gethash 'timestamp tweet))
	(dp-url (gethash 'dp-url tweet))
	(uri-list (gethash 'uri-list tweet))
	(screen-name-list (gethash 'screen-name-list tweet)))
    (insert-image (build-image-descriptor dp-url) nil 'left-margin)
    (fill-line screen-name " | " source " | " timestamp)
    (fill-line text)
    (fill-line
     (mapconcat #'(lambda (uri)
		    (make-uri-button uri)) uri-list " | ")
     (if screen-name-list " | " "")
     (mapconcat #'(lambda (screen-name)
		    (make-screen-name-button screen-name)) screen-name-list " | ")))
  (insert "\n\n"))

(defun render-timeline (tweet-list buf-name)
  "Renders a list of tweets"
  (let ((timeline-buffer (get-buffer-create (cdr (assoc buf-name *buffer-names-assoc*)))))
    (with-current-buffer timeline-buffer
      (setq left-margin-width 6)
      (setq fringes-outside-margins t)
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
	 (filepath (concat *dp-cache-dir* "/" filename)))
    (unless (file-exists-p filepath) (push (cons filename uri) *dp-fetch-queue*))
    (create-image filepath (guess-image-type-extn filename) nil)))

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
