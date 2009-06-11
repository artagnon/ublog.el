(require 'json)
(require 'url)
(require 'parse-twresponse)
(require 'twhelper)
(require 'twinterface)

(defvar twitter-host "twitter.com")
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

(defun twitter-url (&optional relative)
  "Generate a Twitter URL with an optional relative"
  (format "http://%s:%d/%s" twitter-host twitter-port (or relative "")))

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

(defun twittering-http-post
    (http-method url &optional parameters contents sentinel)
  "Send HTTP POST request to twitter.com
METHOD-CLASS must be one of Twitter API method classes
 (statuses, users or direct_messages).
METHOD must be one of Twitter API method which belongs to METHOD-CLASS.
PARAMETERS is alist of URI parameters.
 ex) ((\"mode\" . \"view\") (\"page\" . \"6\")) => <URI>?mode=view&page=6"

  (if (null sentinel) (setq sentinel 'twittering-http-post-default-sentinel))

  ;; clear the buffer
  (save-excursion
    (set-buffer (twittering-http-buffer))
    (erase-buffer))

  (let (proc server port
             (proxy-user twittering-proxy-user)
             (proxy-password twittering-proxy-password))
    (progn
      (if (twittering-proxy-use)
          (setq server twittering-proxy-server
                port (if (integerp twittering-proxy-port)
                         (int-to-string twittering-proxy-port)
                         twittering-proxy-port))
          (setq server "twitter.com"
                port "80"))
      (setq proc
            (open-network-stream
             "network-connection-process" (twittering-http-buffer)
             server (string-to-number port)))
      (set-process-sentinel proc sentinel)
      (process-send-string
       proc
       (let ((nl "\r\n")
             request)
         (setq  request
                (concat "POST http://twitter.com/" method-class "/" method ".json"
                        (when parameters
                          (concat "?"
                                  (mapconcat
                                   (lambda (param-pair)
                                     (format "%s=%s"
                                             (twittering-percent-encode (car param-pair))
                                             (twittering-percent-encode (cdr param-pair))))
                                   parameters
                                   "&")))
                        " HTTP/1.1" nl
                        "Host: twitter.com" nl
                        "User-Agent: " (twittering-user-agent) nl
                        "Authorization: Basic "
                        (base64-encode-string
                         (concat twittering-username ":" (twittering-get-password)))
                        nl
                        "Content-Type: text/plain" nl
                        "Content-Length: 0" nl
                        (when twittering-proxy-use
                          "Proxy-Connection: Keep-Alive" nl
                          (when (and proxy-user proxy-password)
                            (concat
                             "Proxy-Authorization: Basic "
                             (base64-encode-string
                              (concat proxy-user ":"
                                      proxy-password))
                             nl)))
                        nl))
         (debug-print (concat "POST Request\n" request))
         request)))))
