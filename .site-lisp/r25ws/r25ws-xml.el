;;; r25ws-xml.el --- XML utilities

;; Copyright (c) 2010 Shawn Boles, CollegeNet Inc
;; Author: Shawn Boles

;;; Code:

(defun r25ws-xml-declaration-p (&optional buffer)
	"Return t if BUFFER (default: current) contains an XML declaration"
	(save-excursion
		(setq case-fold-search t)
		(goto-char (point-min))
		(if (re-search-forward "<?xml" (point-max) t) t nil))
	)

(defun r25ws-xml-clean-body (&optional buffer)
	"Removes junk between XML elements in BUFFER (default: current)"
	(interactive)
	(save-excursion
		(if buffer (set-buffer buffer))
		(goto-char (point-min))
		(while (re-search-forward "\r" (point-max) t) (replace-match ""))
		(goto-char (point-min))
		(while (re-search-forward "><" (point-max) t) (replace-match ">\n<")))
	)

(provide 'r25ws-xml)

;;; r25ws-xml.el ends here.
