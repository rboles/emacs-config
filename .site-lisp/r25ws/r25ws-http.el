;;; r25ws-http.el --- HTTP functions

;; Copyright (c) 2010 Shawn Boles, CollegeNet Inc
;; Author: Shawn Boles

;;; Code:

(defun r25ws-http-key ()
  "Creates a key that should uniquely identify the request. Sets
r25ws-request-key and returns the value."
  (let ((key (format-time-string "%Y%m%d%H%M%S")))
    (setq r25ws-request-key key)
    key)
  )

(defun r25ws-http-get (url)
	"Makes an HTTP get request for URL.
Returns a new buffer containing the raw HTTP response."
	(interactive "sGET URL: ")
	(message "r25ws-http-get %s" url)
	(let ((url-request-method "GET")
        (url-request-extra-headers (list (cons "X-R25WS-KEY" (r25ws-http-key))))
				(url-request-data ""))
		(url-retrieve-synchronously url)))

(defun r25ws-http-post (url &optional buffer)
	"Sends the content of BUFFER (default: current buffer) to URL
with an HTTP post request. The Content-Type header defaults to text/xml.
Returns the new buffer containing the raw HTTP response."
	(interactive "sPOST URL: ")
	(message "r25ws-http-post %s" url)
	(save-excursion
		(if buffer (set-buffer buffer))
		(let ((url-request-method "POST")
					(url-request-extra-headers (list (cons "Content-Type" "text/xml")
                                           (cons "X-R25WS-KEY" (r25ws-http-key))))
					(url-request-data (buffer-substring-no-properties
														 (point-min) (point-max))))
			(url-retrieve-synchronously url))
		)
	)

(defun r25ws-http-put (url &optional buffer)
	"Sends the content of BUFFER (default: current buffer) to URL
with an HTTP put request. The Content-Type header defaults to text/xml.
Returns the new buffer containing the raw HTTP response."
	(interactive "sPUT URL: ")
	(message "r25ws-http-put %s" url)
	(save-excursion
		(if buffer (set-buffer buffer))
		(let ((url-request-method "PUT")
					(url-request-extra-headers (list (cons "Content-Type" "text/xml")
                                           (cons "X-R25WS-KEY" (r25ws-http-key))))
					(url-request-data (buffer-substring-no-properties
														 (point-min) (point-max))))
			(url-retrieve-synchronously url))
		)
	)

(defun r25ws-http-delete (url)
	"Makes an HTTP delete request to URL.
Returns a new buffer containing the raw HTTP response."
	(interactive "sDELETE URL: ")
	(message "r25ws-http-delete %s" url)
	(let ((url-request-method "DELETE")
        (url-request-extra-headers (list (cons "X-R25WS-KEY" (r25ws-http-key))))
				(url-request-data ""))
		(url-retrieve-synchronously url))
	)

(defun r25ws-http-status (&optional buffer)
	"Parses an HTTP response status code out of BUFFER (default: current).

Expect strange results if BUFFER does not contain a raw HTTP response.

Returns the status code."
	(save-excursion
		(if buffer (switch-to-buffer buffer))
		(goto-char (point-min))
		(skip-chars-forward " \t\n")
		(skip-chars-forward "HTTP/")
		(buffer-substring  (point)
											 (progn
												 (skip-chars-forward "[0-9].")
												 (point)))
		(read (current-buffer)))
	)

(defun r25ws-http-content-type-xml-p (&optional buffer)
	"Return t if BUFFER (default: current) contains an HTTP response
header that specifies a content type of XML."
	(save-excursion
		(setq case-fold-search t)
		(goto-char (point-min))
		(if (re-search-forward "^content-type:\s*\\(.*?\\)$"
													 (r25ws-http-header-delimiter) t)
				(if (string-match "xml" (match-string-no-properties 1)) t nil)
			nil))
	)

(defun r25ws-http-header-delimiter (&optional buffer)
	"Searches BUFFER (default: current) for an HTTP header delimiter: ``\n\n''
Expect strange results if BUFFER does not contain a raw HTTP response.
Returns the location of the delimiter or nil"
	(save-excursion
		(if buffer (set-buffer buffer))
		(goto-char (point-min))
		(re-search-forward "\n\n" (point-max) t))
	)

(defun r25ws-http-request-body (&optional buffer)
	"Searches BUFFER (default: current) for HTTP request body.
BUFFER should contain the raw HTTP response; expect strange results
if BUFFER does not contain an HTTP header.
Returns a string containing the HTTP body"
	(save-excursion
		(if buffer (set-buffer buffer))
		(goto-char (point-min))
		(buffer-substring-no-properties
		 (r25ws-http-header-delimiter) (point-max)))
	)

(defun r25ws-http-clean-body (&optional buffer)
	"Removes trailing ``\r'' from body content."
	(interactive)
	(save-excursion
		(if buffer (set-buffer buffer))
		(goto-char (point-min))
		(while (re-search-forward "\r" (point-max) t)
			(replace-match "")))
	)

(defun r25ws-http-strip-header (&optional buffer)
	"Strips the HTTP header from BUFFER (default: current).
Expect strange results if BUFFER does not contain an HTTP header"
	(save-excursion
		(if buffer (set-buffer buffer))
		(goto-char (point-min))
		(let ((end (re-search-forward "\n\n" nil t)))
			(if end (delete-region (point-min) end)))
		)
	)

(provide 'r25ws-http)

;;; r25ws-http.el ends here.
