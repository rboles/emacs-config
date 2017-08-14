;;; freshbooks.el --- GNU Emacs Freshbooks Client

;; Copyright (c) 2010, 2011 Shawn Boles, CollegeNet Inc
;; Author: Shawn Boles
;; Keywords: Freshbooks, XML, Series25, REST, HTTP
;; Ride Around With: No Tint

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; A Freshbooks web services client in Emacs 
;; See http://developers.freshbooks.com/
;;
;; I know this runs in GNU Emacs version >= 23.2. I don't know
;; anything about XEmacs.

;;; Setup:
;;
;; Put freshbooks.el somewhere in your load path.
;;
;; In your Emacs init file add: (require 'freshbooks)
;;
;; M-x customize-group freshbook
;;
;; Set your Freshbooks API URL and token.
;;
;; Requires the UrlPackage: url.el. If you don't have it, see:
;; http://www.emacswiki.org/emacs/UrlPackage

;;; Notes:
;;
;; When adding service support, make sure the request body is always
;; terminated by a newline.

;;; History:
;; 10/2010 -sboles- Started implementing services.

;;; Code:

(defgroup freshbooks nil
  "freshbooks.el Customizations."
  :version "0.1"
  :group 'freshbooks)

(defcustom freshbooks-api-url
  ""
  "Your Freshbooks service URL"
  :group 'freshbooks
  :type 'string)

(defcustom freshbooks-api-token
  ""
  "Your Freshbooks API token"
  :group 'freshbooks
  :type 'string)

(defconst freshbooks-el-version "0.1")

(defun freshbooks-http-post (request-data)
  "POST request to Freshbooks API identified by URL.

Returns a buffer containing the response."
  (let* ((auth (concat "Basic " (base64-encode-string
                                (concat freshbooks-api-token ":" "X"))))
         (agent (concat "freshbooks.el/" freshbooks-el-version))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/xml")
                                      ("User-Agent" . nil)
                                      ("Authorization" . nil)))
         (url-request-data request-data))
    (setcdr (assoc "User-Agent" url-request-extra-headers) agent)
    (setcdr (assoc "Authorization" url-request-extra-headers) auth)
    (url-retrieve-synchronously freshbooks-api-url))
  )

(defun freshbooks-strip-header (&optional buffer)
  "Strips the HTTP header from BUFFER (default: current).

Expect strange results if BUFFER does not contain an HTTP header"
  (save-excursion
    (if buffer (set-buffer buffer))
    (goto-char (point-min))
    (let ((end (re-search-forward "\n\n" nil t)))
      (if end (delete-region (point-min) end)))
    )
  )

(defun freshbooks-api-call (service request-data)
  "Wraps a Freshbooks API request.

SERVICE identifies the API method and REQUEST-DATA specifies the
request parameters."
  (let ((response (freshbooks-http-post request-data)))
    (freshbooks-strip-header response)
    (switch-to-buffer service)
    (erase-buffer)
    (insert-buffer-substring response)
    (nxml-mode)
    (indent-region (point-min) (point-max))
    (goto-char (point-min))
    (kill-buffer response)))

(defun freshbooks-xml-dec ()
  "Returns an xml declaration followed by a newline."
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
  )
                                   
(defun freshbooks-client-list ()
  "Requests active clients.

By default, Freshbooks returns the first 100 active clients"
  (interactive)
  (let* ((service "client.list")
         (email (read-string "Client Email Address: "))
         (data (concat (freshbooks-xml-dec)
                       "<request method=\"client.list\">"
                       (if (and email (> (length email) 0))
                           (concat "<email>" email "</email>"))
                       "<folder>active</folder>"
                       "</request>\n")))
    (freshbooks-api-call service data))
  )

(defun freshbooks-invoice ()
  "Requests a specific Freshbooks invoice, by invoice ID"
  (interactive)
  (let* ((service "invoice.get")
         (invoice (read-string "Invoice ID: "))
         (data (concat (freshbooks-xml-dec)
                       "<request method=\"invoice.get\">"
                       (concat "<invoice_id>" invoice "</invoice_id>")
                       "</request>\n")))
    (freshbooks-api-call service data))
  )

(provide 'freshbooks)

;;; freshbooks.el ends here
