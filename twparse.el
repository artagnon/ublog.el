(provide 'twparse)

(defstruct tweet-entity
  source
  truncated
  id
  in-reply-to-screen-name
  favorited
  in-reply-to-user-id
  created-at
  in-reply-to-status-id
  text
)

(defstruct user-entity
  verified
  id
  location
  following
  screen-name
  profile-image-url
  protected
  name
  url
  friends-count
  statuses-count
  followers-count
  created-at
  time-zone
  utc-offset
  description
  notifications
)

(defun twittering-status-to-status-datum (status)
  (flet ((assq-get (item seq)
		   (car (cddr (assq item seq)))))
    (let* ((status-data (cddr status))
	   id text source created-at truncated
	   (user-data (cddr (assq 'user status-data)))
	   user-id user-name
	   user-screen-name
	   user-location
	   user-description
	   user-profile-image-url
	   user-url
	   user-protected
	   regex-index)

      (setq id (string-to-number (assq-get 'id status-data)))
      (setq text (twittering-decode-html-entities
		  (assq-get 'text status-data)))
      (setq source (twittering-decode-html-entities
		    (assq-get 'source status-data)))
      (setq created-at (assq-get 'created_at status-data))
      (setq truncated (assq-get 'truncated status-data))
      (setq user-id (string-to-number (assq-get 'id user-data)))
      (setq user-name (twittering-decode-html-entities
		       (assq-get 'name user-data)))
      (setq user-screen-name (twittering-decode-html-entities
			      (assq-get 'screen_name user-data)))
      (setq user-location (twittering-decode-html-entities
			   (assq-get 'location user-data)))
      (setq user-description (twittering-decode-html-entities
			      (assq-get 'description user-data)))
      (setq user-profile-image-url (assq-get 'profile_image_url user-data))
      (setq user-url (assq-get 'url user-data))
      (setq user-protected (assq-get 'protected user-data))

      ;; make username clickable
      (add-text-properties
       0 (length user-name)
       `(mouse-face highlight
		    uri ,(concat "http://twitter.com/" user-screen-name)
		    face twittering-username-face)
       user-name)

      ;; make screen-name clickable
      (add-text-properties
       0 (length user-screen-name)
       `(mouse-face highlight
		    face twittering-username-face
		    uri ,(concat "http://twitter.com/" user-screen-name)
		    face twittering-username-face)
       user-screen-name)

      ;; make URI clickable
      (setq regex-index 0)
      (while regex-index
	(setq regex-index
	      (string-match "@\\([_a-zA-Z0-9]+\\)\\|\\(https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)"
			    text
			    regex-index))
	(when regex-index
	  (let* ((matched-string (match-string-no-properties 0 text))
		 (screen-name (match-string-no-properties 1 text))
		 (uri (match-string-no-properties 2 text)))
	    (add-text-properties
	     (if screen-name
		 (+ 1 (match-beginning 0))
	       (match-beginning 0))
	     (match-end 0)
	     (if screen-name
		 `(mouse-face
		   highlight
		   face twittering-uri-face
		   uri ,(concat "http://twitter.com/" screen-name))
	       `(mouse-face highlight
			    face twittering-uri-face
			    uri ,uri))
	     text))
	  (setq regex-index (match-end 0)) ))


      ;; make source pretty and clickable
      (if (string-match "<a href=\"\\(.*\\)\">\\(.*\\)</a>" source)
	  (let ((uri (match-string-no-properties 1 source))
		(caption (match-string-no-properties 2 source)))
	    (setq source caption)
	    (add-text-properties
	     0 (length source)
	     `(mouse-face highlight
			  uri ,uri
			  face twittering-uri-face
			  source ,source)
	     source)
	    ))

      ;; save last update time
      (setq twittering-timeline-last-update created-at)

      (mapcar
       (lambda (sym)
	 `(,sym . ,(symbol-value sym)))
       '(id text source created-at truncated
	    user-id user-name user-screen-name user-location
	    user-description
	    user-profile-image-url
	    user-url
	    user-protected)))))
