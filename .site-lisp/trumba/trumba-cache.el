;;; trumba-cache.el --- Cache Functions

;; Copyright (c) 2011 Shawn Boles, CollegeNet Inc
;; Author: Shawn Boles

;;; Code:

(defun trumba-cache-verify (&optional cachedir)
  "Returns true if CACHEDIR looks like a Trumba cache. False if not.

A directory looks like a Trumba cache if it exists and has at least
data and feeds directories."
  (interactive)
  (let ((cache (if cachedir cachedir
                 (read-string "Cache Path: "))))
    (if (file-directory-p cache)
        (progn
          (let ((hasdata nil)
                (hasfeeds nil)
                (children (directory-files cache))
                (child nil))
            (while children
              (setq child (car children))
              (setq children (cdr children))
              (if (string= child "data") (setq hasdata t))
              (if (string= child "feeds") (setq hasfeeds t))
              )
            (and hasdata hasfeeds)))
      nil))
  )

(provide 'trumba-cache)

;;; trumba-cache.el ends here
