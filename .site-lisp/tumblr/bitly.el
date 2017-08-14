;;; bitly.el --- Emacs bit.ly client

;; This is free software.

;; Copyright (C) 2010 Shawn Boles

;; Author: Shawn Boles <wb4sed@gmail.com>
;; Version: 1.0
;; Keywords: bit.ly, url shortener, rest, http
;; Bikini Body: Bad and mean

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:

;; A bit.ly client for Emacs. 
;;
;; bit.ly is a popular service that shortens URLs.
;;
;; M-x bitly Shorten thing at point
;;
;; M-x bitly-shorten Shorten URL; returns shortened URL.
;;
;; M-x bitly-link Insert an HTML link with shortened URL
;;
;; M-x bitly-image Insert an HTML IMG element with shortend src attribute
;;
;; M-x bitly-shorten-href Shorten the next HTML link href attribute
;;
;; M-x bitly-shorten-src Shorten the next HTML image src attribute

;;; Setup:

;; Put bitly.el somewhere in your load path and the following to your
;; init file:
;;
;; (require 'bitly)
;;
;; You can use the default bitly.el login user: ``bitlydotel'' and
;; associated API key or substitute your own with:
;;
;; M-x customize-group bitly
;;
;; Requires the UrlPackage: url.el. If you don't have it, see:
;;
;; http://www.emacswiki.org/emacs/UrlPackage
;;
;; bit.ly API docs: http://code.google.com/p/bitly-api/wiki/ApiDocumentation
;;
;; I don't know if bitly.el works with XEmacs. I don't know anything
;; about XEmacs.

;;; History:
;; 2010-11-01 Originally by Shawn Boles <wb4sed@gmail.com>

;;; TODO
;;
;; ``bitly'' to support (thing-at-point 'url)

;;; Code:

(defgroup bitly nil
	"bitly.el Customizations"
	:version "1.0"
	:group 'bitly)

(defcustom bitly-username
	"bitlydotel"
	"bit.ly user name associated with the API key"
	:group 'bitly
	:type 'string)

(defcustom bitly-api-key
	"R_579591c890d392ffe5f5faf0e85bbd40"
	"Key required to make requests to the bit.ly API"
	:group 'bitly
	:type 'string)

(defconst bitly-api-base-url "http://api.bit.ly/v3/"
	"bit.ly base URL")

(defun bitly (&optional longurl)
  "Shortens LONGURL.

The shortened URL is echoed to the minibuffer and added to the
kill-ring. Returns shortened URL."
  (interactive)
  (let* ((url (if longurl longurl
                (read-string "Shorten URL: " (thing-at-point 'url))))
         (shorturl (bitly-shorten url)))
    (save-excursion
      (with-temp-buffer
        (insert shorturl)
        (kill-ring-save (point-min) (point-max))))
    (message shorturl)
    shorturl)
  )

(defun bitly-shorten (url)
	"Shorten URL and return shortened value."
	(interactive "sShorten URL: ")
	(let* ((url-request-method "GET")
				 (url-request-extra-headers '(("User-Agent" . "Emacs/bitly.el")))
				 (url-request-data "")
				 (req (concat bitly-api-base-url "shorten?"
											"&login=" bitly-username
											"&apiKey=" bitly-api-key
											"&longUrl=" (url-hexify-string url)
											"&format=xml"))
				 (buf (url-retrieve-synchronously req))
				 (status (bitly-http-status buf))
				 (shorturl (if (= status 200)
											 (bitly-find-value "url" buf)
										 nil)))
		(kill-buffer buf)
		shorturl)
	)

(defun bitly-shorten-href ()
	"Looks for the next HTML ``href'' attribute. If not already shortened,
replaces the href attribute value with a shortened URL."
	(interactive)
	(save-excursion
		(setq case-fold-search t)
		(if (re-search-forward "href=\"\\(.*?\\)\"" nil t)
				(let* ((url (match-string-no-properties 1))
							 (msg (concat "Shorten link URL: " url " (y/n) "))
							 (shorten (and (not (bitly-short-p url))
														 (string= (read-string msg) "y")))
							 (shorturl (and shorten (bitly-shorten url))))
					(if (and shorturl (re-search-backward "href=\"\\(.*?\\)\"" nil t))
							(replace-match (concat "href=\"" shorturl "\"") 1)))))
	)

(defun bitly-shorten-src ()
	"Looks for the next HTML ``src'' attribute. If not already shortened,
replaces the src attribute value with a shortened URL."
	(interactive)
	(save-excursion
		(setq case-fold-search t)
		(if (re-search-forward "src=\"\\(.*?\\)\"" nil t)
				(let* ((url (match-string-no-properties 1))
							 (msg (concat "Shorten image URL: " url " (y/n) "))
							 (shorten (and (not (bitly-short-p url))
														 (string= (read-string msg) "y")))
							 (shorturl (and shorten (bitly-shorten url))))
					(if (and shorturl (re-search-backward "src=\"\\(.*?\\)\"" nil t))
							(replace-match (concat "src=\"" shorturl "\"") 1)))))
	)

(defun bitly-link (url)
	"Inserts an HTML A element with a shortened href attribute."
	(interactive "sLink URL: ")
	(let* ((href (bitly-shorten url))
				 (txt (read-string "Link Text: ")))
		(insert "<a href=\"" href "\">" txt "</a>"))
	)

(defun bitly-image (url)
	"Inserts an HTML IMG element with a shortened src attribute."
	(interactive "sImage URL: ")
	(let* ((src (bitly-shorten url)))
		(insert "<img src=\"" src "\" />"))
	)

(defun bitly-a (url)
	"Synonym for ``bitly-link

Inserts an HTML A element with a shortened href attribute"
	(interactive "sLink URL:")
	(bitly-link url)
	)

(defun bitly-img (url)
	"Synonym for ``bitly-image''

Inserts an HTML IMG element with a shortened src attribute."
	(interactive "sImage URL: ")
	(bitly-image url)
	)

(defun bitly-short-p (url)
	"Checks URL to determine if it has already been shortened.
Returns t if URL contains ``http://bit.ly/'' or ``http://j.mp''"
	(if (or (string-match "^http://bit\\.ly/" url)
					(string-match "^http://j.mp/" url))
			t
		nil))

(defun bitly-http-status (&optional buffer)
	"Parse an HTTP status code out of BUFFER (default: current).
Expect strange results if BUFFER does not contain an HTTP response header.
Returns status code."
	(save-excursion
		(if buffer (set-buffer buffer))
		(goto-char (point-min))
		(skip-chars-forward " \t\n")
		(skip-chars-forward "HTTP/")
		(buffer-substring  (point)
											 (progn
												 (skip-chars-forward "[0-9].")
												 (point)))
		(read (current-buffer)))
	)

(defun bitly-find-value (key &optional buffer)
	"Search bit.ly request response BUFFER (default: current)
for a value that matches KEY. Assumes the bit.ly response format is XML.
Return value or nil if nothing matched."
	(save-excursion
		(if buffer (set-buffer buffer))
		(goto-char (point-min))
		(let ((regex (concat "<" key ">\\(.*?\\)<")))
			(if (re-search-forward regex nil t)
					(match-string-no-properties 1)
				nil)
			)
		)
	)

(provide 'bitly)

;;; bitly.el ends here
