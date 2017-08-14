;;; r25ws-log.el --- Log utilities

;; Copyright (c) 2010, 2011 Shawn Boles, CollegeNet Inc
;; Author: Shawn Boles

;;; Code:

(defun r25ws-log-grep (logdir needle)
  "Executes lgrep on the logs in LOGDIR for the text in NEEDLE"
  (let* ((logs (r25ws-log-search logdir))
         (files (r25ws-logs-to-s logs)))
    (if (> (length logs) 0)
        (progn
          (message "Grepping %s, %s for %s" logdir files needle)
          (lgrep needle files logdir))
      (message "No files modified in the past %d minutes" r25ws-log-max-age)
      nil)
    )
  )

(defun r25ws-log-search (searchdir)
  "Searches the directory identified by SEARCHDIR and returns a list
of log files with recent last mod timestamps."
  (let* ((logdir searchdir)
         (logs (directory-files logdir t))
         (log nil)
         (attr nil)
         (curtime (float-time (current-time)))
         (lastmod nil)
         (timediff nil)
         (found nil))
    (while logs
      (setq log (car logs))
      (setq logs (cdr logs))
      (setq attr (file-attributes log))
      (if (not (nth 0 attr))
          (progn
            (setq lastmod (float-time (nth 5 attr)))
            (setq timedif (- curtime lastmod))
            (if (< (/ timedif 60) r25ws-log-max-age) (push log found)))))
    found)
  )

(defun r25ws-logs-to-s (logs)
  "Returns a space delimited list of log names from the content of LOGS"
  (let ((files nil) (log nil) (i 0))
    (while logs
      (if (> i 0) (setq files (concat files " ")))
      (setq i (+ i 1))
      (setq log (car logs))
      (setq logs (cdr logs))
      (setq files (concat files (file-name-nondirectory log))))
    files)
  )

(provide 'r25ws-log)

;;; r25ws-log.el ends here.
