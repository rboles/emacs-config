;;; r25ws.el --- Emacs R25WS Client

;; Copyright (c) 2010 Shawn Boles, CollegeNet Inc
;; Author: Shawn Boles
;; Keywords: R25WS, Series25, REST, HTTP

;; This file is not part of GNU Emacs.

;;; Commentary:

;; An R25WS client in Emacs 
;;
;; M-x r25ws
;;
;; To change R25WS instances:
;;
;; M-x r25ws-login

;;; Setup:

;; M-x customize-group r25ws
;; Set a default R25WS base URL.

;; Requires the UrlPackage: url.el. If you don't have it, see:
;;
;; http://www.emacswiki.org/emacs/UrlPackage
;;
;; Note that r25ws.el requires fixes I made to url-cookie.el. My
;; updated url-cookie.el ships with GNU Emacs versions 23.3 and
;; higher.
;;
;; GNU Emacs + Windows + HTTPS
;;
;; If you are running GNU Emacs on Windows and plan to connect to an
;; R25WS instance over HTTPS you need GnuTLS. I recommend using the
;; GnuTLS package available from Cygwin. You can find it under Libs or
;; Net, labeled as gnutls. Add Cygwin/bin to your Windows path.  path.
;; Make sure you do not have any GnuTLS or OpenSSL directories in the
;; path before Cygwin/bin

;;; TODO:

;; Store default base URLs as list
;; Initialize base URL history from list
;; R25WS request log

;;; Code:

(require 'r25ws-xml)
(require 'r25ws-url)
(require 'r25ws-log)
(require 'r25ws-http)
(require 'r25ws-alist)
(require 'r25ws-session)

(require 'url-cookie)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

(defconst r25ws-session-cookie-name "JSESSIONID"
	"Cookie name identifiying the R25WS session ID cookie")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

(defvar r25ws-client-version-number "0.1")

(defvar r25ws-user-agent
  (concat "r25ws.el/" r25ws-client-version-number
          " GNU Emacs/" emacs-version)
	"Value of HTTP User-Agent request header")

(defvar r25ws-base-url nil
	"Base URL for R25WS service requests. Set by r25ws-login")

(defvar r25ws-username nil
	"Cached username, used to auto-renew an expired session")

(defvar r25ws-password nil
	"Cached password, used to auto-renew an expired session")

(defvar r25ws-session-cookie nil
	"Cookie identifying the current R25WS session. See url-cookie-storage")

(defvar r25ws-using-default-user nil
	"True if making requests using the server-side default user")

(defvar r25ws-history '()
	"List of R25WS base URLs")

(defvar r25ws-service-history '()
	"List of R25WS service requests")

(defvar r25ws-request-key nil
  "A timestamp that serves as a unique identifier for the last request.")

(defvar r25ws-log-dir "R:/voodoo/log/wasv"
  "This is a temporary variable that points to the log directory that
  matches the current base URL.")

(defvar r25ws-log-max-age 15
  "Maximum minutes since log file last modified")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable Group

(defgroup r25ws nil
	"r25ws.el customizations."
	:version "0.1"
	:group 'r25ws)

(defcustom r25ws-default-base-url
	""
	"Default R25WS base URL"
	:group 'r25ws
	:type 'string)

(defcustom r25ws-default-base-url-list
  '()
  "AList of default R25WS base URLs.
Each element has the form (DESCRIPTION BASE-URL)"
  :options '("Test" "Development" "Production")
  :group 'r25ws
  :type '(alist :value-type (group string)))

(defcustom r25ws-xml-hook
	'xml-mode
	"Hook to run when viewing an XML response"
	:options '(xml-mode nxml-mode)
	:group 'r25ws
	:type 'hook)

(defcustom r25ws-session-length
	3600
	"Default session length in seconds"
	:options '(1800 3600 7200)
	:group 'r25ws
	:type 'integer)

(defcustom r25ws-log-enable
  t
  "Enable R25WS request logging"
  :options '(t nil)
  :group 'r25ws
  :type 'boolean)

(defcustom r25ws-log-buffer
  "*R25WS-Log*"
  "Name of request log buffer"
  :group 'r25ws
  :type 'string)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primary Interface

(defun r25ws ()
  "Call this function to start R25WS-EL up for the first time.

To change instance base URLs, use R25WS-LOGIN."
	(interactive)
  (r25ws-init-history)
	(r25ws-login)
  )

(defun r25ws-logout ()
	"Requests logout.xml and expires the R25WS session."
	(interactive)
	(r25ws-get "logout.xml")
	(r25ws-expire-session)
	)

(defun r25ws-silent-logout ()
	"Requests logout.xml?async=T and expires the R25WS session.
Does not show the resulting logout.xml buffer."
	(save-excursion
		(let* ((logout-url (r25ws-make-url "logout.xml"))
					 (logout-buffer (r25ws-http-get logout-url)))
			(kill-buffer logout-buffer)))
	(r25ws-expire-session)
	)

(defun r25ws-login ()
	"Interactive login to an R25WS instance. Kills any existing session by
silently requesting logout.xml. If a username is not provided, assumes the
user wants to use the R25WS default user and stores the base URL and quits.
Otherwise, negotiates login and displays the result."
	(interactive)
	(if (and (= 0 (length r25ws-history)) r25ws-default-base-url)
			(r25ws-add-history r25ws-default-base-url))
	(let* ((baseurl (r25ws-get-base-url))
				 (user (read-string "Username: "))
				 (pass (if (> (length user) 0) (read-string "Password: ") nil))
				 (login-url nil))
		(setq	r25ws-base-url baseurl
					r25ws-username (if (= (length user) 0) nil user)
					r25ws-password (if (= (length user) 0) nil pass)
					r25ws-using-default-user (if (= (length user) 0) t nil))
		(if r25ws-session-cookie (r25ws-silent-logout))
		(r25ws-add-history baseurl)
		(if (not r25ws-using-default-user)
				(progn
					(setq login-url (r25ws-make-url "login.xml"))
					(setq login-buffer (r25ws-http-get login-url))
					(r25ws-http-strip-header login-buffer)
					(r25ws-http-clean-body login-buffer)
					(r25ws-session-set-login user pass login-buffer)
					(setq login-buffer (r25ws-http-post login-url login-buffer))
					(r25ws-show-response "login.xml" login-buffer)
					(if (r25ws-fetch-session) (r25ws-update-session))))
		)
	)

(defun r25ws-auto-renew ()
	"Uses the ``r25ws-base-url'', ``r25ws-username'' and ``r25ws-password''
variables to automatically and silently renew a session"
	(if (and r25ws-base-url r25ws-username r25ws-password)
			(let* ((login-url (r25ws-make-url "login.xml"))
						 (login-buffer (r25ws-http-get login-url))
						 (user r25ws-username)
						 (pass r25ws-password))
				(r25ws-http-strip-header login-buffer)
				(r25ws-http-clean-body login-buffer)
				(if (r25ws-already-logged-in login-buffer)
						(progn
							(message "r25ws-auto-renew: Already logged in")
							(if (r25ws-fetch-session) (r25ws-update-session)))
					(r25ws-session-set-login user pass login-buffer)
					(setq login-buffer (r25ws-http-post login-url login-buffer))
					(kill-buffer login-buffer)
					(if (r25ws-fetch-session) (r25ws-update-session))
					(message "r25ws-auto-renew: Renewed session"))))
	)

(defun r25ws-get (&optional service silent)
	"Makes an HTTP GET request for an R25WS SERVICE.

SERVICE should identify an R25WS service name and any additional
request parameters.

r25ws-get checks for an active (or default) session. If the session
has expired, it will attempt to auto-renew the session. If auto-renew
fails, user is requested to re-login."
  (interactive)
	(if (r25ws-session-expired-p)
			(progn
				(message "r25ws-get: R25WS session expired, auto-renewing.")
				(r25ws-auto-renew)))
	(if (not (r25ws-session-p))
			(message "r25ws-get: Sorry no active sesson. Please M-x r25ws-login")
		(let* ((req (if service service
                  (read-string "Service: "
                               (car r25ws-service-history)
                               (cons 'r25ws-service-history 1))))
           (url (r25ws-make-url (r25ws-service-name req)))
					 (response-buffer (r25ws-http-get url)))
      (if silent (kill-buffer response-buffer)
        (r25ws-show-response req response-buffer))
      (r25ws-add-service-history req))
    (r25ws-update-session))
	)

(defun r25ws-get-href ()
	"Searches the buffer forward and then backward for an xl:href attribute.

If a match is found an ``r25ws-get'' request is made for the
service identified in the xl:href attribute"
	(interactive)
	(save-excursion
		(search-backward " " nil t)
		(if (re-search-forward "xl:href=\"\\(.*?\\)\"" nil t)
				(r25ws-get (match-string-no-properties 1))
			(if (re-search-backward "xl:href=\"\\(.*?\\)\"")
					(r25ws-get (match-string-no-properties 1))
				(message "Sorry, can't find @xl:href in buffer")
				nil))
		)
	)

(defun r25ws-refresh ()
	"Reload the R25WS request in the active buffer"
	(interactive)
	(r25ws-get (buffer-name))
	)

(defun r25ws-reload ()
	"Synonym for r25ws-refresh- reloads the R25WS request in the active buffer"
	(interactive)
	(r25ws-refresh)
	)

(defun r25ws-post ()
	"Sends the content of the current buffer to R25WS with an HTTP
POST request. Shows the R25WS response."
	(interactive)
	(if (r25ws-session-expired-p)
			(progn
				(message "r25ws-post: R25WS session expired, auto-renewing.")
				(r25ws-auto-renew)))
	(if (not (r25ws-session-p))
			(message "r25ws-post: Sorry no active sesson. Please M-x r25ws-login")
		(let* ((service (read-string "Service: " (buffer-name)))
					 (url (r25ws-make-url (r25ws-service-name service)))
					 (response-buffer (r25ws-http-post url)))
			(r25ws-show-response service response-buffer))
		(r25ws-update-session))
	)

(defun r25ws-post-new ()
	"This is a special form of ``r25ws-post'' that sends an empty
HTTP post request to R25WS with the intent of getting an object
template."
	(interactive)
	(if (r25ws-session-expired-p)
			(progn
				(message "r25ws-template: R25WS session expired, auto-renewing.")
				(r25ws-auto-renew)))
	(if (not (r25ws-session-p))
			(message (concat "r25ws-post-new: "
											 "Sorry no active sesson. Please M-x r25ws-login"))
		(with-temp-buffer
			(insert "<?xml version=\"1.0\"?>")
			(let* ((service (read-string "Service: "))
						 (url (r25ws-make-url (r25ws-service-name service)))
						 (response-buffer (r25ws-http-post url)))
				(r25ws-show-response service response-buffer)))
		(r25ws-update-session))
	)
			
(defun r25ws-put ()
	"Sends the content of the current buffer to R25WS with an HTTP
PUT request. Shows the R25WS response."
	(interactive)
	(if (r25ws-session-expired-p)
			(progn
				(message "r25ws-put: R25WS session expired, auto-renewing.")
				(r25ws-auto-renew)))
	(if (not (r25ws-session-p))
			(message "r25ws-put: Sorry no active session. Please M-x r25ws-login.")
		(let* ((service (read-string "Service: " (buffer-name)))
					 (url (r25ws-make-url service))
					 (response-buffer (r25ws-http-put url)))
			(r25ws-show-response service response-buffer))
		(r25ws-update-session))
	)

(defun r25ws-delete (&optional service silent)
	"Makes an HTTP delete request for an R25WS SERVICE and shows the R25WS
response.

SERVICE should identify an R25WS service name and any additional
request parameters."
  (interactive)
	(if (r25ws-session-expired-p)
			(progn
				(message "r25ws-delete: R25WS session expired, auto-renewing.")
				(r25ws-auto-renew)))
	(if (not (r25ws-session-p))
			(message "r25ws-delete: Sorry no R25WS session. Please M-x r25ws-login")
		(let* ((req (if service service
                  (read-string "Service: "
                               (car r25ws-service-history)
                               (cons 'r25ws-service-history 1))))
					 (url (r25ws-make-url (r25ws-service-name req)))
					 (response-buffer (r25ws-http-delete url)))
      (if silent (kill-buffer response-buffer)
        (r25ws-show-response service response-buffer)))
		(r25ws-update-session))
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defun r25ws-get-log ()
  "Searches R25WS logs for a log of the last request, identified by
R25WS-REQUEST-KEY. Note that sometimes it takes a minute or two for
the request to show up in the request logs."
  (interactive)
  (let ((logdir (if (> (length r25ws-log-dir) 0) r25ws-log-dir
                  (read-string "R25WS log directory: "))))
    (if (> (length logdir) 0)
        (progn
          (message "Searching for R25WS request key %s" r25ws-request-key)
          (r25ws-log-grep logdir r25ws-request-key))))
  )

(defun r25ws-grep-log (&optional needle)
  "Searches R25WS logs for a recent log that contains NEEDLE."
  (interactive)
  (let ((logdir (if (> (length r25ws-log-dir) 0) r25ws-log-dir
                  (read-string "R25WS log directory: ")))
        (match (if needle needle
                 (read-string "Search for: "))))
    (if (> (length logdir) 0)
        (progn
          (message "Searching logs for string: %s" match)
          (r25ws-log-grep logdir match))))
  )

(defun r25ws-mod ()
  "Searches the buffer forward and then backward for the next
status=``est'' attribute.

If a match is found the status attribute value is set to ``mod''"
	(interactive)
	(save-excursion
		(search-backward " " nil t)
    (if (re-search-forward "status=\"\\(est\\)\"" nil t)
        (progn
          (replace-match "status=\"mod\"" 1)
          (message "Status updated to mod"))
      (if (re-search-backward "status=\"\\(est\\)\"" nil t)
          (progn
            (replace-match "status=\"mod\"" 1)
            (message "Status updated to mod"))
        (message "Failed to match a status")))
		)
	)

(defun r25ws-get-base-url ()
	"Requests an R25WS base URL from the user.

If the base URL does not have a protocol, ``http://'' is prefixed to the URL.
The base URL is guaranteed to end with `/'.

Returns an R25WS base url"
	(interactive)
	(let ((base-url (read-string "R25WS Base URL: "
															 (car r25ws-history)
															 (cons 'r25ws-history 1))))
		(if (not (string= (downcase (substring base-url 0 4)) "http"))
				(setq base-url (concat "http://" base-url)))
		(if (not (string= (substring base-url -1 nil) "/"))
				(setq base-url (concat base-url "/")))
		base-url)
	)

(defun r25ws-get-inst-name ()
  "Requests an R25WS instance name from the user.

The instance name is used to get the base URL, log directory,
etc. from the R25WS-ALIST. The R25WS-ALIST is used for default values;
the user has the option of specifying a new instance"
  (interactive)
  (let* ((names (append (r25ws-alist-names) (list "(New instance...)")))
         (name (read-string "Conect to R25WS instance: "
                            (car names) (cons 'names 1))))
    (if (string= name "(New instance...)")
        (r25ws-alist-add)
      name))
  )

(defun r25ws-make-url (service)
	"Creates an R25WS request URL for SERVICE.

Returns the concatenation of ``r25ws-base-url'' and SERVICE."
	(concat r25ws-base-url service)
	)

(defun r25ws-add-history (base-url)
	"Adds a BASE-URL to history.

Returns true if the BASE-URL is added to history; false if already exists."
	(let ((match (downcase base-url))
				(history r25ws-history)
				(found nil)
				(cur nil))
		(while (and history (not found))
			(setq cur (downcase (car history))
						history (cdr history)
						found (string= cur match)))
		(if (not found) (push base-url r25ws-history))
		(not found))
	)

(defun r25ws-init-history ()
  "Initializes the R25WS base URL history list from the
R25WS-DEFAULT-BASE-URL-LIST custom variable. Note that this function
first empties the R25WS-HISTORY list."
  (setq r25ws-history nil)
  (let ((urls r25ws-default-base-url-list)
        (burl nil)
        (cur nil))
    (while urls
      (setq cur (car urls))
      (setq urls (cdr urls))
      (setq burl (car (cdr cur)))
      (push burl r25ws-history)))
  )

(defun r25ws-add-service-history (service)
  "Adds SERVICE to the service history.

Returns true if SERVICE is added to history; false if already in history."
  (let ((match (downcase service))
        (history r25ws-service-history)
        (found nil)
        (cur nil))
    (while (and history (not found))
      (setq cur (downcase (car history))
            history (cdr history)
            found (string= cur match)))
    (if (not found) (push service r25ws-service-history))
    (not found))
  )

(defun r25ws-service-name (name)
  "Cleans up and massages a service name (including
parameters). Appends ``.xml'' if service NAME looks like it should
include it. Encodes space characters.

Eventually, this function should url-encode all special characters."
  (let ((service (if (or (string-match "\\.\\|\\?" name)
                         (string= name "ping"))
                     name
                   (concat name ".xml"))))
    (replace-in-string service " " "%20"))
  )

(defun r25ws-trunc-service-name (service)
	"Truncates a service request string to just the service name."
	(if (string-match "\\(.*?\\)\\?" service)
			(r25ws-service-name (match-string 1 service))
		(r25ws-service-name service)))

(defun r25ws-show-response (service response)
	"Copies content of RESPONSE to a new buffer named SERVICE.

The content of the new buffer is cleaned up and displayed as XML"
	(r25ws-http-strip-header response)
	(r25ws-xml-clean-body response)
	(switch-to-buffer service)
	(erase-buffer)
	(insert-buffer-substring response)
	(run-hooks 'r25ws-xml-hook)
	(kill-buffer response)
	(indent-region (point-min) (point-max))
	(goto-char (point-min))
	)

(defun r25ws-fetch-session ()
	"Searches the url-cookie-store for an R25WS session cookie matching the
current r25ws-base-url.

If a match is found, sets r25ws-session-cookie and returns true.

If not found, sets r25ws-session-cookie to nil and returns nil."
	(let ((cookie (r25ws-url-find-cookie
								 r25ws-base-url r25ws-session-cookie-name)))
		(if cookie (message "Set R25WS session cookie: %s"
												(url-cookie-value cookie))
			(message "Failed to find R25WS session cookie"))
		(setq r25ws-session-cookie cookie)
		(if cookie t nil))
	)

(defun r25ws-update-session ()
	"If an R25WS session cookie exists and the client is not using the default
user, this function updates the expiration on the session cookie,
``r25ws-session-length'' seconds into the future.

Returns t if the session is updates; nil if not"
	(if (and (not r25ws-using-default-user) r25ws-session-cookie)
			(progn
				(setf (url-cookie-expires r25ws-session-cookie)
							(r25ws-session-renew-time r25ws-session-length))
				(message "Updated R25WS cookie expires: %s"
								 (url-cookie-expires r25ws-session-cookie))
				t)
		nil)
	)

(defun r25ws-expire-session ()
	"Updates the session cookie identified by r25ws-session-cookie.
Sets the cookie expiration, 1 hour in the past."
	(if r25ws-session-cookie
			(progn
				(setf (url-cookie-expires r25ws-session-cookie)
							(r25ws-session-expire-time))
				(message "Expired R25WS cookie: %s"
								 (url-cookie-expires r25ws-session-cookie))
				t)
		nil)
	)

(defun r25ws-already-logged-in (&optional buffer)
	"Checks content of BUFFER (default: current) for a login.xml response
denoting that the user is already logged in."
	(save-excursion
		(if buffer (set-buffer buffer))
		(goto-char (point-min))
		(setq case-fold-search t)
		(if (re-search-forward "message>already logged in<" nil t)
				t
			nil))
	)

(provide 'r25ws)

;;; r25ws.el ends here

