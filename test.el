(defun testcase ()
  "Testcases"
  (setf response-hashtable (make-hash-table :test 'equal))
  (setf user-hashtable (make-hash-table :test 'equal))
  (setf (gethash "text" response-hashtable) "\"Do not try and bend the list. It's impossible. Instead, only try to realize the truth\" \"What truth?\" \"There is no list\" http:\/\/is.gd\/1ihyb")
  (setf (gethash "screen_name" user-hashtable) "artagnon")
  (setf (gethash "junk_key" response-hashtable) "morejunk")
  (setf (gethash "user" response-hashtable) user-hashtable)
  (hashtable-parser response-hashtable))
