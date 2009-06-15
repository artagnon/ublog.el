;; Extra interfaces
(defun twitter-follow-user (username)
    (twitter-request "GET" (twitter-url (format "%s/%s" "statuses" "update"))
		     :parameters '("status" (concat "follow" username))))

(defun twitter-unfollow-user (username)
    (twitter-request "GET" (twitter-url (format "%s/%s" "statuses" "update"))
		     :parameters '("status" (concat "unfollow" username))))

(defun twitter-show-fans ()
  "People who follow you, but you don't follow them back"
  (delq (twitter-show-followers) (twitter-show-following)))

(defun twitter-show-friends ()
  "People who follow you and you follow them back"
  (intersect (twitter-show-following) (twitter-show-followers)))

(defun twitter-show-stars ()
  "People you follow, but don't follow you back"
  (delq (twitter-show-following) (twitter-show-followers)))

(defun twitter-mass-unfollow (username-list)
  "Mass unfollow a list of usernames"
  (mapcar '#twitter-unfollow-user username-list))

(defun twitter-unfollow-all ()
  "Unfollow everyone"
  (mapcar '#twitter-unfollow-user (twitter-show-following)))

(defun twitter-show-conversation (latest-tweet-id)
  "Build a list of tweets corresponding to a certain conversation")
