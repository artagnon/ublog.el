;;;; twparse.el -- Tweet parsing library
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

(provide 'twparse)

(defvar *tweet-hashtable-select-keys*
  ;; Design based on Gravity
  `(("id" . tweet-id)
    ("favorited" . fav-p)
    ("created_at" . created-at)
    ("source" . source)
    ("text" . text)
    ("in_reply_to_status_id" . in-reply-to-status-id)))

(defvar *user-hashtable-select-keys*
  ;; Design based on Gravity
  `(("id" . user-id)
    ("screen_name" . screen-name)
    ("profile_image_url" . profile-image-url)
    ("following" . following-p)))

(defun alist-to-car-list (list)
  (mapcar #'(lambda (cons-pair) (car cons-pair)) list))

(defun master-parser (response-object)
  "Master response-object parser"
  (if (vector-or-char-table-p response-object)
      (loop for hashtable in vector-of-hashtables
         do (hashtable-parser hashtable))
      (hashtable-parser hashtable)))

(defun hashtable-parser (response-hashtable)
  "Crops, sanitizes, and enriches hashtable"
  (let* ((user-entry (gethash "user" response-hashtable))
	 ;; Crop response-hashtable to produce tweet-hashtable
	 (tweet-hashtable (hashtable-parser-expander
			   response-hashtable
			   (alist-to-car-list *tweet-hashtable-select-keys*)))
	 ;; Crop response-hashtable again to produce user-hashtable
	 (user-hashtable (hashtable-parser-expander
			  user-entry
			  (alist-to-car-list *user-hashtable-select-keys*)))
	 ;; Merge user-hashtable and tweet-hashtable into an eql hashtable (after key mapping)
	 (merged-hashtable (merge-hashtable tweet-hashtable user-hashtable)))
    (enrich-hashtable merged-hashtable)))

(defun find-assoc (k)
  (let ((assoc-1 (assoc k *tweet-hashtable-select-keys*)))
    (if assoc-1
	(cdr assoc-1)
	(cdr (assoc k *user-hashtable-select-keys*)))))

(defun sanitize-hashtable-keys (hashtable)
  (let ((final-hashtable (make-hash-table :size 10)))
    (maphash #'(lambda (k v)
		 (let ((key-assoc (find-assoc k)))
		   (setf (gethash key-assoc final-hashtable) v)))
	     hashtable)
    final-hashtable))

(defun merge-hashtable (hashtable-1 hashtable-2)
  (if (and hashtable-1 hashtable-2)
      (let ((final-hashtable (make-hash-table :size 10))
	    (hashtable-1 (sanitize-hashtable-keys hashtable-1))
	    (hashtable-2 (sanitize-hashtable-keys hashtable-2)))
	(maphash #'(lambda (k v) (setf (gethash k final-hashtable) v)) hashtable-1)
	(maphash #'(lambda (k v) (setf (gethash k final-hashtable) v)) hashtable-2)
	final-hashtable)
      (if hashtable-1
	  hashtable-1
	  hashtable-2)))

(defun hashtable-parser-expander (hashtable select-keys-list)
  (if hashtable
      (let ((hashtable (crop-hashtable hashtable select-keys-list)))
	(maphash (lambda (k v) (setf (gethash k hashtable) (decode-html-entities v))) hashtable)
	hashtable)
      nil))

(defun crop-hashtable (hashtable select-keys)
  (let ((cropped-hashtable (make-hash-table :test 'equal :size 10)))
    (loop
       for crop-key in select-keys
       do (cond
            ((or (gethash crop-key hashtable) (nth-value 1 (gethash crop-key hashtable)))
             (setf (gethash crop-key cropped-hashtable)
                   (gethash crop-key hashtable)))))
    cropped-hashtable))

(defun enrich-hashtable (hashtable)
  "Parses hashtable for information that can be added to it as keys"
  (let* ((final-hashtable (copy-hash-table hashtable))
	 (text-extract (extract-text-uri (gethash 'text final-hashtable)))
	 (source-extract (extract-source-uri (gethash 'source final-hashtable))))
    (setf (gethash 'uri-list final-hashtable) (cdr text-extract))
    (setf (gethash 'screen-name-list final-hashtable) (car text-extract))
    (setf (gethash 'source final-hashtable) source-extract)
    final-hashtable))

(defun extract-source-uri (source-text)
  "Extracts the anchor href and text between the anchor in a given text string"
  (if (equal source-text "web")
      (cons "web" "http://twitter.com")
      (if (string-match "<a href=\"\\(.*\\)\">\\(.*\\)</a>" source-text)
	  (let ((uri (match-string-no-properties 1 source-text))
		(caption (match-string-no-properties 2 source-text)))
	    (cons caption uri)))))

(defun extract-text-uri (text)
  "Extracts mentions of Twitter handles and plain URIs from a given text string"
  (let ((regex-index 0)
	(screen-name-list '())
	(uri-list '()))
    (while regex-index
      (setq regex-index
	    (string-match "@\\([_a-zA-Z0-9]+\\)\\|\\(https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)"
			  text
			  regex-index))
      (when regex-index
	(let* ((matched-string (match-string-no-properties 0 text))
	       (screen-name (match-string-no-properties 1 text))
	       (uri (match-string-no-properties 2 text)))
	  (if screen-name
	      (push (cons screen-name (concat "http://twitter.com/" screen-name))
		    screen-name-list)
	      (push uri uri-list)))
	(setq regex-index (match-end 0))))
    (cons screen-name-list uri-list)))

(defmacro list-push (value listvar)
  `(setq ,listvar (cons ,value ,listvar)))

(defmacro twitel-ucs-to-char (num)
  (if (functionp 'ucs-to-char)
      `(ucs-to-char ,num)
    `(decode-char 'ucs ,num)))

(defun decode-html-entities (encoded-str)
  "Please refer http://www.w3.org/TR/REC-html40/sgml/entities.html"
  (if encoded-str
      (let ((cursor 0)
            (found-at nil)
            (result '()))
        (while (setq found-at
                     (string-match "&\\(#\\([0-9]+\\)\\|\\([A-Za-z]+\\)\\);"
                                   encoded-str cursor))
          (when (> found-at cursor)
            (list-push (substring encoded-str cursor found-at) result))
          (let ((number-entity (match-string-no-properties 2 encoded-str))
                (letter-entity (match-string-no-properties 3 encoded-str)))
            (cond (number-entity
                   (list-push
                    (char-to-string
                     (twitel-ucs-to-char
                      (string-to-number number-entity))) result))
                  (letter-entity
                   (cond ((string= "gt" letter-entity) (list-push ">" result))
                         ((string= "lt" letter-entity) (list-push "<" result))
                         (t (list-push "?" result))))
                  (t (list-push "?" result)))
            (setq cursor (match-end 0))))
        (list-push (substring encoded-str cursor) result)
        (apply 'concat (nreverse result)))
      ""))

(defun twitter-time-to-time (time)
  "Convert TIME to a number of seconds since some epoch."
  (let ((case-fold-search t))
    (if (null (string-match (concat "\\`[a-z]\\{3\\} "
                                    "\\([a-z]\\{3\\}\\) "
                                    "\\([0-9]\\{1,2\\}\\) "
                                    "\\([0-9]\\{2\\}\\):"
                                    "\\([0-9]\\{2\\}\\):"
                                    "\\([0-9]\\{2\\}\\) "
                                    "\\([+-][0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\) "
                                    "\\([0-9]\\{4\\}\\)\\'") time))
        (error "Invalid time string: %s" time))
    (encode-time (string-to-number (match-string 5 time))
                 (string-to-number (match-string 4 time))
                 (string-to-number (match-string 3 time))
                 (string-to-number (match-string 2 time))
                 (cdr (assoc (match-string 1 time) twitter-month-map))
                 (string-to-number (match-string 8 time))
                 (concat (match-string 6 time) ":" (match-string 7 time)))))
