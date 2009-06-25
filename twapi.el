(provide 'twapi)

;; Search Methods
(defun twitter-search (search-term &optional since)
  (twitter-request "GET" (twitter-url (format "%s" "search") t)
		   `(("q" . ,search-term))))

(defun twitter-trends ()
  (twitter-request (twitter-url (format "%s" "trends") t)
		   "GET"))

(defun twitter-trends-current (since)
  (twitter-request (twitter-url (format "%s/%s" "statuses" "user_timeline"))
		   "GET"
		   '(("since" since))))

(defun twitter-trends-daily (since)
  (twitter-request "GET" (twitter-url (format "%s/%s" "statuses" "user_timeline"))
		   '(("since" since))))

(defun twitter-trends-weekly (since)
  (twitter-request "GET" (twitter-url (format "%s/%s" "statuses" "user_timeline"))
		   '(("since" since))))

;; Timeline Methods
(defun twitter-public-timeline (since)
  (twitter-request "GET" (twitter-url (format "%s/%s" "statuses" "public_timeline"))
		   '(("since" since))))

(defun twitter-friends-timeline (since)
  (twitter-request "GET" (twitter-url (format "%s/%s" "statuses" "friends_timeline"))
		   '(("since" since))))

(defun twitter-user-timeline (&optional since)
  (twitter-request "GET" (twitter-url (format "%s/%s" "statuses" "user_timeline"))
		   '(("since" since))))

(defun twitter-mentions (&optional since)
  (twitter-request "GET" (twitter-url (format "%s/%s" "statuses" "mentions"))
		   '(("since" since))))

;; Status Methods
(defun twitter-show-status (id)
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
  (twitter-request (twitter-url (format "%s/%s" "users" "show"))
		   "GET"
		   `(("screen_name" . ,screen-name))))

(defun twitter-show-friends (screen-name)
  (twitter-request (twitter-url (format "%s/%s" "statuses" "friends"))
		   "GET"
		   `(("screen_name" . ,screen-name))))

(defun twitter-show-followers (screen-name)
  (twitter-request (twitter-url (format "%s/%s" "statuses" "followers"))
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
