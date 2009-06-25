(require 'json)
(require 'url)
(require 'parse-twresponse)
(require 'twhelper)
(require 'twhttp)
(require 'oauth)

;; Configuration constants
(defvar twitter-host "twitter.com")
(defvar twitter-search-host "search.twitter.com")
(defvar twitter-response-format "json")
(defvar twitter-port 80)
(defvar twitter-user-agent "twitel")
(defvar twitter-proxy-use nil)
(defvar twitter-proxy-server nil)
(defvar twitter-proxy-port nil)
(defvar twitter-request-headers
  "HTTP headers to be used with corresponding HTTP requests"
  '(("GET" ("Accept" . "application/json, image/png"))
    ("POST" ("Content-type" . "application/json"))
    ("PUT" ("Content-type" . "application/json"))
    ("DELETE")))

;; Process objects
(defvar twitel-proc nil)
(defvar twitel-proc-sentinel nil)

;; Twitter OAuth URLs
(defvar twitel-request-url "http://twitter.com/oauth/request_token")
(defvar twitel-access-url "http://twitter.com/oauth/access_token")
(defvar twitel-user-authorize "http://twitter.com/oauth/authorize")

;; Twitter OAuth keys
(defvar twitel-consumer-key "K9vrCLHpp5vuln72ROufzQ")
(defvar twitel-consumer-secret "sUTgzZRgm0GCHQNYixFx3TS2D94TwaQv90gIXhTQNcE")

(defvar twitel-access-token nil)
;; TODO: `artagnon' cannot be hardcoded here!
(defvar twitel-token-file "/home/artagnon/.twitel/token")

(defun twitel-network-buffer ()
  "The network buffer in which twitel-proc runs"
  (get-or-generate-buffer "*twitel-network-buffer*"))

(defun twitel-proc-sentinel (proc stat &optional success-message)
  (condition-case err-signal
      (let ((header (twittering-get-response-header))
            (status nil))
        (string-match "HTTP/1\.1 \\([a-z0-9 ]+\\)\r?\n" header)
        (setq status (match-string-no-properties 1 header))
        (case-string status
                     (("200 OK")
		      (message (if success-message success-message "Success: Post")))
                     (t
		      (message status))))
    (error (message (prin1-to-string err-signal)))))

(defun twitel-init ()
  "Open a network stream, set process sentinel, and create cache directory"
  (interactive)
  (if twitter-proxy-use
      (setq server twitter-proxy-server
            port (if (integerp twitter-proxy-port)
                     (int-to-string twitter-proxy-port)
                     twittering-proxy-port))
      (setq server "twitter.com"
            port 80))

  (setq twitel-proc
        (open-network-stream
         "twitel-connection" (twitel-network-buffer) server port))
  (set-process-sentinel twitel-proc 'twitel-proc-sentinel)
  (if (file-directory-p "~/.twitel")
      nil
      (make-directory "~/.twitel")))

(defun twitel-authenticate ()
  "Get authentication token"
  (if (file-exists-p twitel-token-file)
      ;; Enter this only after authenticating the first time
      (progn
        (save-excursion
          (find-file (twitel-token-file)
		     ;; Read file to get `acess token'
                     (let ((str (buffer-substring (point-min) (point-max))))
                       (if (string-match "\\([^:]*\\):\\(.*\\)"
                                         (buffer-substring (point-min) (point-max)))
                           (setq twitel-access-token
				 ;; twitel-access-token is set
                                 (make-oauth-access-token
                                  :consumer-key twitel-consumer-key
                                  :consumer-secret twitel-consumer-secret
                                  :auth-t (make-oauth-t
                                           :token (match-string 1 str)
                                           :token-secret (match-string 2 str))))))
                     (save-buffer)
                     (kill-this-buffer))))
      (unless twitel-access-token
	;; Unless twitel-access-token was set without entering the if branch
	;; ie. twitel-authenticate called unnecessarily. Just return twitel-access-token
        (let ((callback
               (lambda ()
		 ;; This function will be invoked later, somewhere within oauth-twitter-app
		 ;; Adds parameter callback_token to access_url
                 (let ((callback-token (read-string
                                        "Please enter the provided code: ")))
                   (setq access-url
                         (concat access-url "?oauth_verifier=" callback-token))))))
          (setq twitel-access-token
                (oauth-authorize-app twitel-consumer-key
                                     twitel-consumer-secret
                                     twitel-request-url
                                     twitel-access-url
                                     twitel-user-authorize
				     callback)))

	;; We just got twitel-access-token. Save it to a file for all future authentication.
        (save-excursion
          (find-file twitel-token-file)
          (end-of-buffer)
          (let ((token (oauth-access-token-auth-t twitel-access-token)))
            (insert (format "%s:%s\n"
                            (oauth-t-token token)
                            (oauth-t-token-secret token))))
          (save-buffer)
          (kill-this-buffer)))
      twitel-access-token))

(defun twitter-url (&optional relative search-flag)
  "Generate a Twitter URL with an optional relative"
  (format "http://%s:%d/%s" (if search-flag twitter-search-host twitter-host) twitter-port (or relative "")))

(defun twitter-response (response)
  "Parsing the Twitter response returned in JSON"
  (let ((response-struct (json-read-from-string response)))
    (progn)))

(defun twitel-master-callback (json-response)
  "Function needs to process the JSON response"
  nil)

(defun twitter-request (url http-method &optional parameters)
  "Use HTTP METHOD to request URL with some optional parameters"
  (oauth-url-retrieve twitel-access-token
		      (concat url "." twitter-response-format (if parameters (build-url-parameters parameters) nil))
		      http-method
		      'twitel-master-callback))

;; User interfaces

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
