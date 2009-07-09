(defun random-tweet-creator (tweet-id in-reply-to)
  (setf response-hashtable (make-hash-table :test 'equal))
  (setf user-hashtable (make-hash-table :test 'equal))
  (setf handle-list (list "artagnon" "ghoseb" "aatifh" "Brajeshwar" "aditya" "ankurb" "realin"))
  (setf (gethash "text" response-hashtable) (concat "RT @" (nth (abs (random 5)) handle-list) ": \"Do not try and bend the list. It's impossible. Instead, only try to realize the truth\" \"What truth?\" \"There is no list\" http:\/\/is.gd\/1ihyb"))
  (setf (gethash "junk_key" response-hashtable) "morejunk")
  (setf (gethash "source" response-hashtable) "<a href=\"http:\/\/mobileways.de\/gravity\">Gravity<\/a>")
  (setf (gethash "created_at" response-hashtable) (concat
						   "Wed Jul "
						   (number-to-string (+ 10 (abs (random 20))))
						   " "
						   (number-to-string (+ 10 (abs (random 14))))
						   ":"
						   (number-to-string (+ 10 (abs (random 50))))
						   ":"
						   (number-to-string (+ 10 (abs (random 50))))
						   " +0000 2009"))
  (setf (gethash "id" response-hashtable) (or tweet-id (float (abs (random)))))
  (setf (gethash "in_reply_to_status_id" response-hashtable) in-reply-to)
  (setf (gethash "screen_name" user-hashtable) (nth (abs (random 5)) handle-list))
  (setf (gethash "profile_image_url" user-hashtable) "http:\/\/s3.amazonaws.com\/twitter_production\/profile_images\/79367886\/artagnon_normal.jpg")
  (setf (gethash "id" user-hashtable) (float (abs (random))))
  (setf (gethash "user" response-hashtable) user-hashtable)
  response-hashtable)

(defun test-conversation ()
  "Testcase: Conversation"
  (let* ((id1 (float (abs (random))))
	 (id2 (float (abs (random))))
	 (id3 (float (abs (random))))
	 (t1 (random-tweet-creator id1 nil))
	 (t2 (random-tweet-creator id2 id1))
	 (t3 (random-tweet-creator id3 id2)))
    (vector t1 t2 t3)))

(defun test-twitter-show-status (id hashvector)
  "Emulates twitter-show-status"
  (loop
     for table in hashvector
     for to-return = nil
     until (let ((return-table-p (equal id (gethash "id" table))))
	     (progn (when return-table-p (setq to-return table)) return-table-p))
     finally return to-return))

(defun test-conversation-parser (id hashvector)
  "Recursively parses in_reply_to to generate a conversation from a tweet-id"
  (loop
     for tweet-id = id then (gethash 'in-reply-to tweet)
     for tweet = (master-response-parser (test-twitter-show-status tweet-id hashvector))
     while tweet-id
     collecting tweet))

