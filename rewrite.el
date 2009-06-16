(require 'json)
(require 'url)
(require 'parse-twresponse)
(require 'twhelper)
(require 'twinterface)
(require 'twhttp)

(defvar twitter-host "twitter.com")
(defvar twitter-search-host "search.twitter.com")
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

;; Twitter OAuth URLs
(defvar twoauth-request-url "https://www.yammer.com/oauth/request_token")
(defvar twoauth-access-url "https://www.yammer.com/oauth/access_token")
(defvar twoauth-user-authorize "https://www.yammer.com/oauth/authorize")

;; Twitter OAuth keys
(defvar twoauth-consumer-key "K9vrCLHpp5vuln72ROufzQ")
(defvar twoauth-consumer-secret "sUTgzZRgm0GCHQNYixFx3TS2D94TwaQv90gIXhTQNcE")
(defvar twitel-token-path "/home/%s/.twitel-token")

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
		     (t (message status))))
    (error (message (prin1-to-string err-signal)))))

(defun twitel-init ()
  "Open a network stream, set process sentinel, and create cache directory"
  (if (twitter-proxy-use)
      (setq server twitter-proxy-server
	    port (if (integerp twitter-proxy-port)
		     (int-to-string twitter-proxy-port)
		     twittering-proxy-port))
      (setq server "twitter.com"
	    port 80))

  (setq twitel-proc
	(open-network-stream
	 "twitel-connection" (twitel-network-buffer) server port))
  (set-process-sentinel twitel-proc twitel-proc-sentinel)
  (if (file-directory-p "~/.twitel")
      nil
      (make-directory "~/.twitel")))

(defun twitel-authenticate ()
  "Get authentication token"
  (if (file-exists-p (twitel-token-path))
      (progn
        (save-excursion
          (find-file (twitel-token-path)
          (let ((str (buffer-substring (point-min) (point-max))))
            (if (string-match "\\([^:]*\\):\\(.*\\)"
                              (buffer-substring (point-min) (point-max)))
                (setq twitel-access-token
                      (make-oauth-access-token
                       :consumer-key twitel-consumer-key
                       :consumer-secret twitel-consumer-secret
                       :auth-t (make-oauth-t
                                :token (match-string 1 str)
                                :token-secret (match-string 2 str))))))
          (save-buffer)
          (kill-this-buffer))))
  (unless twitel-access-token
    (let ((callback
           (lambda ()
             (let ((callback-token (read-string
                                    "Please enter the provided code: ")))
               (setq access-url
                     (concat access-url "?callback_token=" callback-token))))))
      (setq twitel-access-token
            (oauth-authorize-app :consuer-key twitel-consumer-key
				 :consumer-secret twitel-consumer-secret
                                 :request-url twitel-request-url
				 :access-url twitel-access-url
                                 :authorize-url twitel-user-authorize
                                 :access-callback callback)))
    (save-excursion
      (find-file (twitel-token-path))
      (end-of-buffer)
      (let ((token (oauth-access-token-auth-t twitel-access-token)))
        (insert (format "%s:%s\n"
                        (oauth-t-token token)
                        (oauth-t-token-secret token))))
      (save-buffer)
      (kill-this-buffer)))
  twitel-access-token)

(defun twitter-url (&optional (search nil) relative)
  "Generate a Twitter URL with an optional relative"
  (format "http://%s:%d/%s" (if search 'twitter-search-host 'twitter-host) twitter-port (or relative "")))

(defun twitter-response ()
  "Parsing the Twitter response returned in JSON")

(defun url-percent-encode (str &optional coding-system)
  (if (or (null coding-system)
	  (not (coding-system-p coding-system)))
      (setq coding-system 'utf-8))
  (mapconcat
   (lambda (c)
     (cond
      ((twittering-url-reserved-p c)
       (char-to-string c))
      ((eq c ? ) "+")
      (t (format "%%%x" c))))
   (encode-coding-string str coding-system)
   ""))

(defun twitter-request (http-method url &optional parameters)
  "Use HTTP METHOD to request URL with some optional parameters"
  (process-send-string twitel-proc
		       (concat http-method url (build-url-parameters parameters))))
