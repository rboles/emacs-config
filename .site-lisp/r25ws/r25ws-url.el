;;; r25ws-url.el --- URLPackage utilities

;; Copyright (c) 2010 Shawn Boles, CollegeNet Inc
;; Author: Shawn Boles

;;; Code:

(require 'url-parse)
(require 'url-cookie)

(defun r25ws-url-find-cookie (baseurl cookie-name)
	"Find a cookie that matches BASEURL and COOKIE-NAME.
BASEURL provides the host name and local-part matches.
Note that this function does not use url-cookie-retrieve;
it scans url-cookie-storage and may return an expired cookie.
Returns matching cookie object from url-cookie-storage or nil"
	(let* ((urlobj (url-generic-parse-url baseurl))
				 (match-name (downcase cookie-name))
				 (match-host (downcase (url-host urlobj)))
				 (match-path (downcase (url-filename urlobj)))
				 (storage url-cookie-storage)
				 (cookies nil)
				 (rehost nil)
				 (repath nil)
				 (found nil)
				 (cur nil))
		(while (and storage (not found))
			(setq cur (car storage)
						storage (cdr storage)
						cookies (cdr cur)
						rehost (regexp-quote
										(replace-regexp-in-string
										 "\\.\\." "."
										 (replace-regexp-in-string
											"\\*" "" (car cur)))))
			(if (string-match (downcase rehost) match-host)
					; domain match, check cookies
					(while (and cookies (not found))
						(setq cur (car cookies)
									cookies (cdr cookies)
									repath (regexp-quote (url-cookie-localpart cur)))
						(if (and (string= (downcase (url-cookie-name cur)) match-name)
										 (string-match (downcase repath) match-path))
								(setq found cur)))))
		found)
	)

(provide 'r25ws-url)

;;; r25ws-url.el ends here.
