;;;; twapi.el -- Twitter API implementation library
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

(provide 'twapi)

(defvar *buffer-names-assoc*
  ;; Buffer name symbol <-> string mapper
  (list (cons 'own "*timeline*")
	(cons 'user "*user-timeline*")
	(cons 'public "*public-timeline*")
	(cons 'trends "*trends*")
	(cons 'mentions "*mentions*")
	(cons 'friends "*friends*")
	(cons 'followers "*followers*")
	(cons 'profile "*profile*")))

(defun build-buf-name-string (buf-name)
  (cdr (assoc buf-name *buffer-names-assoc*)))

;; Search Methods
(defun twitter-search (search-term &optional since)
  (twitter-request (concat "*search-" search-term "*")
		   (twitter-url (format "%s" "search") t)
		   "GET"
		   `(("q" . ,search-term))))

(defun twitter-trends ()
  (twitter-request (build-buf-name-string 'trends)
		   (twitter-url (format "%s" "trends") t)
		   "GET"))

(defun twitter-trends-current (since)
  (twitter-request (build-buf-name-string 'trends-current)
		   (twitter-url (format "%s/%s" "statuses" "user_timeline"))
		   "GET"
		   '(("since" since))))

(defun twitter-trends-daily (since)
  (twitter-request (build-buf-name-string 'trends-daily)
		   (twitter-url (format "%s/%s" "statuses" "user_timeline"))
		   "GET"
		   '(("since" since))))

(defun twitter-trends-weekly (since)
  (twitter-request (build-buf-name-string 'trends-weekly)
		   (twitter-url (format "%s/%s" "statuses" "user_timeline"))
		   "GET"
		   '(("since" since))))

;; Timeline Methods
(defun twitter-public-timeline (&optional since)
  (twitter-request (build-buf-name-string 'public)
		   (twitter-url (format "%s/%s" "statuses" "public_timeline"))
		   "GET"
		   `(("since" . ,since))))

(defun twitter-friends-timeline (&optional since)
  (twitter-request (build-buf-name-string 'own)
		   (twitter-url (format "%s/%s" "statuses" "friends_timeline"))
		   "GET"
		   `(("since" . ,since))))

(defun twitter-user-timeline (&optional id user-id screen-name since-id max-id count page)
  (twitter-request (build-buf-name-string 'user)
		   (twitter-url (format "%s/%s" "statuses" "user_timeline"))
		   "GET"
		   `(("id" . ,id)
		     ("user_id" . ,user-id)
		     ("screen_name" . ,screen-name)
		     ("since_id" . ,since-id)
		     ("max_id" . ,max-id)
		     ("count" . ,count)
		     ("page" . ,page))))

(defun twitter-mentions (&optional since)
  (twitter-request (build-buf-name-string 'mentions)
		   (twitter-url (format "%s/%s" "statuses" "mentions"))
		   "GET"
		   `(("since" . ,since))))

;; Status Methods
(defun twitter-show-status (id)
  ;; Will this API call ever be used independently?
  (twitter-request (twitter-url (format "%s/%s/%d" "statuses" "show" id))
		   "GET"))

(defun twitter-update-status (status)
  (twitter-request (twitter-url (format "%s/%s" "statuses" "update"))
		   "POST"
		   `(("status" . ,status))))

(defun twitter-show-status (id)
  (twitter-request (twitter-url (format "%s/%s/%d" "statuses" "destroy" id))
		   "DELETE"))

;; User Methods
(defun twitter-show-user (screen-name)
  (twitter-request (build-buf-name-string 'profile)
		   (twitter-url (format "%s/%s" "users" "show"))
		   "GET"
		   `(("screen_name" . ,screen-name))))

(defun twitter-show-friends (screen-name)
  (twitter-request (build-buf-name-string 'friends)
		   (twitter-url (format "%s/%s" "statuses" "friends"))
		   "GET"
		   `(("screen_name" . ,screen-name))))

(defun twitter-show-followers (screen-name)
  (twitter-request (build-buf-name-string 'followers)
		   (twitter-url (format "%s/%s" "statuses" "followers"))
		   "GET"
		   `(("screen_name" . ,screen-name))))


;; Account Methods

;; Favorite Methods

;; Notification Methods

;; Block Methods

;; Saved Searches Methods

;; OAuth Methods [removed to oauth.el]
;; OAuth authentication happens in rewrite.el twitel-authenticate()

;; Help Methods
