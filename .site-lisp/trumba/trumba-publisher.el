;;; trumba-publisher.el --- Publisher service functions

;; Copyright (c) 2011 Shawn Boles, CollegeNet Inc
;; Author: Shawn Boles

;;; Code:

(defun trumba-publisher-run (&optional feed silent)
  "Request publisher.run for FEED"
  (interactive)
  )

(defun trumba-publisher-data-delete (&optional feed force silent)
  "Request delete on the publisher.data service for the specified FEED.

If FEED is not provided, a delete request is made for every feed if
the user responds with an affirmative at the prompt or FORCE is true

SILENT is passed on to the R25WS service request, if true then the
buffer showing R25WS response(s) is not shown.

Returns the number of delete requests."
  (interactive)
  (let* ((feedfile (if feed feed
                     (if force ""
                       (read-string "Delete tracking data for feed: "))))
         (doall (if (= (length feedfile) 0)
                    (if force t
                      (string= "y" (read-string
                                    "Delete tracking for all feeds? (y/n): ")))
                  nil))
         (feedfiles (if doall (trumba-feed-files) (list feedfile)))
         (count 0)
         (cur nil))
    (while feedfiles
      (setq cur (car feedfiles))
      (setq feedfiles (cdr feedfiles))
      (message "Deleting tracking data for feed: %s" cur)
      (r25ws-delete (concat "publisher.data?feed=" cur) silent)
      (setq count (+ count 1)))
    count)
  )

(provide 'trumba-publisher)

;;; trumba-publisher.el ends here
