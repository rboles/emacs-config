;;; trumba-http.el --- Trumba HTTP Functions

;; Copyright (c) 2011 Shawn Boles, CollegeNet Inc
;; Author: Shawn Boles

;;; Code:

;;; trumba-http.el ends here

(defun trumba-http-get (url)
	"Makes an HTTP get request for URL.

Returns a new buffer containing the raw HTTP response."
	(interactive "sGET URL: ")
	(message "trumba-http-get %s" url)
	(let ((url-request-method "GET")
				(url-request-data ""))
		(url-retrieve-synchronously url))
  )

(defun trumba-http-put (url &optional buffer)
	"Sends the content of BUFFER (default: current buffer) to URL
with an HTTP put request. The content-type is text/calendar.

Returns the new buffer containing the raw HTTP response."
	(interactive "sPUT URL: ")
	(message "trumba-http-put %s" url)
	(save-excursion
		(if buffer (set-buffer buffer))
		(let ((url-request-method "PUT")
					(url-request-extra-headers (list (cons "Content-Type" "text/calendar; charset=utf-8")
                                           (cons "Connection" "close")))
					(url-request-data (buffer-substring-no-properties
														 (point-min) (point-max))))
			(url-retrieve-synchronously url))
		)
	)

(defun trumba-http-clean-body (&optional buffer)
	"Removes trailing ``\r'' from body content."
	(interactive)
	(save-excursion
		(if buffer (set-buffer buffer))
		(goto-char (point-min))
		(while (re-search-forward "\r" (point-max) t)
			(replace-match "")))
	)

(defun trumba-http-strip-header (&optional buffer)
	"Strips the HTTP header from BUFFER (default: current).
Expect strange results if BUFFER does not contain an HTTP header"
	(save-excursion
		(if buffer (set-buffer buffer))
		(goto-char (point-min))
		(let ((end (re-search-forward "\n\n" nil t)))
			(if end (delete-region (point-min) end)))
		)
	)

(provide 'trumba-http)

;;; trumba-http.el ends here
