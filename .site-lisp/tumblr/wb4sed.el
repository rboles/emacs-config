;;; wb4sed.el --- WB4SED blog utilities

;; Author: Shawn Boles
;; Keywords: blog, tumblr

;;; Commentary

;; Some helper functions for my WB4SED tumblr blog.
;;
;; Typical usage:
;;
;; M-x wb4sed Creates a new blog template
;;
;; M-x wb4sed-image Inserts a an HTML image
;;
;; M-x wb4sed-link Inserts an HTML link, with option to shorten.

;;; Setup

;; Requires tumblr.el and bitly.el

;;; Code:

(require 'tumblr)
(require 'bitly)

(defun wb4sed (title)
	"Inserts an entry template.
When calling this function provide a title string for the new post"
	(interactive "sEntry title: ")
	(insert "\n<p>")
	(wb4sed-bookkeeping)
	(wb4sed-title title)
	(insert "\n\t<p></p>\n\n</p>"))

(defun wb4sed-bookkeeping ()
	"Outputs an HTML comment string of bookkeeping information used by
other WB4SED utilities"
	(interactive)
	(insert "\n\t<!--"
					" WB4SED-BOOKKEEPING {"
					" WB4SED-CLIENT:wb4sed.el;"
					" WB4SED-TIMESTAMP:" (number-to-string (float-time)) ";"
					" } -->\n")
	)

(defun wb4sed-timestamp ()
	"Parses the timestamp out of the bookkeeping data
Returns the timestamp as a string or nil if unable to find bookkeeping data"
	(interactive)
	(save-excursion
		(goto-char (point-min))
		(setq case-fold-search t)
		(if (re-search-forward "WB4SED-TIMESTAMP:\\(.*?\\);" (point-max) t)
				(match-string-no-properties 1)
			nil)
		)
	)

(defun wb4sed-image (url)
	"Inserts an HTML IMG element with an optional caption.
The IMG URL may be shortened with bit.ly"
	(interactive "sImage URL: ")
	(let* ((shorten (read-string "Shorten with bit.ly? (y/n): "))
				 (src (if (string= (downcase shorten) "y") (bitly-shorten url) url))
				 (txt (read-string "Caption: ")))
		(insert "\n\t<center>\n\t\t<p><img src=\"" src "\" />"
						"</p>\n\t\t<p><i>" txt "</i></p>\n\t</center>\n"))
	)

(defun wb4sed-img (url)
	"Synonym for wb4sed-image"
	(wb4sed-image url))

(defun wb4sed-link (url)
	"Inserts an HTML A element and link text.
The URL may be shortened with bit.ly"
	(interactive "sLink URL: ")
	(let* ((shorten (read-string "Shorten with bit.ly? (y/n): "))
				 (href (if (string= (downcase shorten) "y")
									 (bitly-shorten url)
								 url))
				 (txt (read-string "Text: ")))
		(insert "<a href=\"" href "\">" txt "</a>"))
	)

(defun wb4sed-a (url)
	"Synonym for wb4sed-link"
	(wb4sed-link url))

(defun wb4sed-title (title)
	"Inserts TITLE inside h1 elements.
The title is intentionally commented out. Tumblr will title the entry.
The title is parsed as part of wb4sed-post-entry"
	(interactive "sTitle: ")
	(insert "\n\t<!-- <h1 rel=\"title\">" title "</h1> -->\n"))

(defun wb4sed-tag (tag)
	"Inserts a single tumblr TAG.
When calling the function provide the name of the tag to insert"
	(interactive "sInsert tag: ")
	(insert "\n\t\t<span><a style=\"color:#999;\""
					" href=\"http://www.tumblr.com/tagged/" tag "\""
					" rel=\"tag\">#" tag "</a></span>\n"))

(defun wb4sed-tags (tag)
	"Inserts a tumblr tag container.
When calling the function provide the name of the first tag to insert"
	(interactive "sFirst tag to insert: ")
	(insert "\n\t<div style=\"font-size:x-small;text-align:right;\">")
	(wb4sed-tag tag)
	(insert "\t</div>\n"))

(defun wb4sed-separator ()
	"Inserts a hard-return-like seperator element."
	(interactive)
	(insert "\n\t<div style=\"width:90%; margin:8px 0 8px 0; line-height:1px; border-top:1px solid #999;\"></div>\n"))

(defun wb4sed-superscript (chr)
	"Inserts a superscript container.
Call this function with the superscript character"
	(interactive "sSuperscript character: ")
	(let* ((ts (wb4sed-timestamp))
				 (pre (or ts (buffer-name)))
				 (link (concat pre "-" chr)))
		(insert "<a name=\"_" link "\""
						" href=\"#" link "\""
						"><sup>" chr "</sup></a>"))
	)

(defun wb4sed-footnote (chr)
	"Inserts a footnote container.
Call this function with the character that identifies the footnote"
	(interactive "sFootnote identifier: ")
	(let* ((ts (wb4sed-timestamp))
				 (pre (or ts (buffer-name)))
				 (link (concat pre "-" chr)))
		(insert "\n\t<p>\n\t\t<a name=\"" link "\""
						" href=\"#_" link "\">" chr ".</a>\n\t</p>\n"))
	)

(provide 'wb4sed)

;;; wb4sed.el ends here
