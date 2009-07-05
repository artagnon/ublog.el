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

(define-derived-mode twitter-timeline-view-mode fundamental-mode
  "Twitter Timeline"
  "Major mode for viewing timelines from Twitter.")

;; =========
;; Functions
;; =========

(defun make-link-clickable (link)
  "Inserts a link into the current buffer"
  (let ((caption (car link))
	(target-uri (cdr link)))
    (add-text-properties
     0 (length caption)
     `(mouse-face highlight
		  uri ,target-uri) caption)
    caption))

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

(defun insert-tweet (tweet)
  "Inserts a tweet into the current buffer"
  (let ((text (gethash 'text tweet))
	(screen-name (gethash 'screen-name tweet))
	(source (car (gethash 'source tweet)))
	(timestamp (gethash 'timestamp tweet)))
    (fill-region (prog1
		     (point)
		   (insert screen-name " | " source " | " timestamp))
		 (point) 'left)
    (insert "\n")
    (fill-region (prog1 (point) (insert text)) (point) 'left)
    (insert "\n"))
  (let ((uri-list (gethash 'uri-list tweet))
	(screen-name-list (gethash 'screen-name-list tweet)))
    (fill-region
     (prog1 (point) 
       (insert
	(mapconcat #'(lambda (link) (make-link-clickable link))
		   (concatenate 'list uri-list screen-name-list) " | ")))
     (point)))
  (insert "\n\n"))

(defun render-timeline (tweet-list)
  "Renders a list of tweets"
  (let ((timeline-buffer (get-buffer-create "*timeline-buffer*")))
    (with-current-buffer timeline-buffer
      (let ((inhibit-read-only t))
	(goto-char (point-min))
	(mapcar
	 #'(lambda (tweet-hashtable) (insert-tweet tweet-hashtable))
	 tweet-list)))))
