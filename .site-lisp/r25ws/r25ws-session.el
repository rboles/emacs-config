;;; r25ws-session.el --- Session management functions

;; Copyright (c) 2010 Shawn Boles, CollegeNet Inc
;; Author: Shawn Boles

;;; Code:

(defun r25ws-session-p ()
	"Checks to see if the client has an active session.

At minimum the client needs a base url. If the client has a base
url then the client is either: 1. making requests as the default
R25WS user; 2. using an active session; 3. has an expired session.

Returns true if the client has a base url and some sort of active
session."
	(and r25ws-base-url
			 (or r25ws-using-default-user (not(r25ws-session-expired-p))))
	)

(defun r25ws-session-expired-p ()
	"Returns true if the R25WS client has a session cookie and that
cookie has expired.

Be sure to verify that the client has a cookie before checking to
see if has expired."
	(and r25ws-session-cookie (url-cookie-expired-p r25ws-session-cookie))
	)

(defun r25ws-session-set-login (username password &optional buffer)
	"Wraps the function calls that update the R25WS login.xml
service response body.

Sets the USERNAME and challenge; unsets the PASSWORD."
	(r25ws-session-set-login-username username buffer)
	(r25ws-session-set-login-response password buffer)
	(r25ws-session-unset-login-challenge buffer)
	)

(defun r25ws-session-set-login-username (username &optional buffer)
	"Sets USERNAME in the login.xml service XML body."
	(save-excursion
		(if buffer (set-buffer buffer))
		(goto-char (point-min))
		(if (re-search-forward "<r25:username/>" nil t)
        (replace-match (concat "<r25:username>" username "</r25:username>") 1))
		)
	)

(defun r25ws-session-set-login-response (password &optional buffer)
	"Uses the PASSWORD and R25WS login challenge string to set the response"
	(let ((resp (r25ws-session-md5-challenge
							 password
							 (r25ws-session-find-login-challenge buffer))))
		(save-excursion
			(if buffer (set-buffer buffer))
			(goto-char (point-min))
			(if (re-search-forward "<r25:response/>" nil t)
					(replace-match (concat "<r25:response>" resp "</r25:response>")))
			)
		)
	)

(defun r25ws-session-find-login-challenge (&optional buffer)
	"Searches BUFFER (default: current buffer) for the R25WS login
challenge string. Returns challenge string or nil"
	(save-excursion
		(if buffer (set-buffer buffer))
		(goto-char (point-min))
		(if (re-search-forward "r25:challenge>\\(.*?\\)<" nil t)
				(match-string-no-properties 1)
			nil)
		)
	)

(defun r25ws-session-unset-login-challenge (&optional buffer)
	"Searches BUFFER (default: current buffer) for the R25WS login
challenge string and removes the text content"
	(save-excursion
		(if buffer (set-buffer buffer))
		(goto-char (point-min))
		(if (re-search-forward "<r25:challenge>.*?</r25:challenge>" nil t)
				(replace-match "<r25:challenge/>"))
		)
	)

(defun r25ws-session-md5-challenge (password challenge)
	"Creates the login challenge response hash:

md5(md5(PASSWORD):CHALLENGE)

Returns the hash value"
	(md5 (concat (md5 password) ":" challenge))
	)

(defun r25ws-session-renew-time (session-length)
	"Returns a (GMT) timestamp that renews the session cookie
 SESSION-LENGTH seconds into the future."
	(let ((offset (- 0 (car (current-time-zone))))
				(adjusted-time (+ session-length (float-time))))
		(format-time-string "%d-%b-%Y %T.00 GMT"
												(seconds-to-time (+ offset adjusted-time))))
	)

(defun r25ws-session-expire-time ()
	"Returns a (GMT) timestamp that should expire a cookie.
 The timestamp is current time (GMT) minus 1 hour."
	(let ((offset (- 0 (car (current-time-zone))))
				(adjusted-time (- (float-time) 3600)))
		(format-time-string "%d-%b-%Y %T.00 GMT"
												(seconds-to-time (+ offset adjusted-time))))
	)

(provide 'r25ws-session)

;;; r25ws-session.el ends here.
