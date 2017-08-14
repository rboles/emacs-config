;;; trumba.el --- Emacs Trumba Client

;; Copyright (c) 2011 Shawn Boles, CollegeNET Inc
;; Author: Shawn Boles
;; Keywords: Trumba, Series25, REST, HTTP

;; This file is not part of GNU Emacs.

;; A Trumba client in Emacs 
;;
;; M-x trumba
;;

;;; Code:

(require 'trumba-cache)
(require 'trumba-publisher)
(require 'trumba-http)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

(defvar trumba-cache nil
  "Path to Trumba cache")

(defvar trumba-baseurl "https://25livepub.collegenet.com/service/"
  "Path to Trumba services")

(defvar trumba-cache-history '()
  "List of Trumba cache paths")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable Group

(defgroup trumba nil
  "trumba.el customizations."
  :version "0.1"
  :group 'trumba)

(defcustom trumba-default-cache
	""
	"Default Trumba cache"
	:group 'trumba
	:type 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primary Interface

(defun trumba (&optional cachedir)
  ""
  (interactive)
  (let ((cache (if cachedir cachedir
                 (read-string "Cache Path: "
                              (car trumba-cache-history)
                              (cons 'trumba-cache-history 1)))))
    (if (not (string= (substring cache -1 nil) "/"))
        (setq cache (concat cache "/")))
    (setq trumba-cache cache)
    (if (not (trumba-cache-verify trumba-cache))
        (progn
          (message "Warning %s does not look like a cache" trumba-cache)
          nil)
      (message "Set cache path: %s" trumba-cache)
      t))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils

(defun trumba-calendars ()
  "Request calendar list

The call to ``url-basic-auth'' is used to get auth credentials used
internall by the URL library."
  (interactive)
  (let* ((url (concat trumba-baseurl "calendars.asmx/GetCalendarList"))
         (auth (url-basic-auth (url-generic-parse-url url) t t))
         (buffer (trumba-http-get url)))
    (trumba-http-strip-header buffer)
    (trumba-http-clean-body buffer)
    (switch-to-buffer "GetCalendarList")
    (erase-buffer)
    (insert-buffer-substring buffer)
    (kill-buffer buffer)
    (nxml-mode)
    (indent-region (point-min) (point-max))
    (goto-char (point-min)))
  )

(defun trumba-feeds ()
  "Opens a dired buffer containing the feeds directory"
  (interactive)
  )

(defun trumba-data ()
  "Opens a dired buffer containing the data directory"
  (interactive)
  )


(defun trumba-data-grep (&optional needle)
  "Grep across Trumba iCal data files for NEEDLE"
  (interactive)
  (let ((match (if needle needle
                 (read-string "Search iCal data for: "))))
    (trumba-cache-grep (trumba-files-to-s (trumba-data-files)) match))
  )

(defun trumba-feed-grep (&optional needle)
  "Grep across Trumba iCal feed files for NEEDLE"
  (interactive)
  (let ((match (if needle needle
                 (read-string "Search feed files for: "))))
    (trumba-cache-grep (trumba-files-to-s (trumba-feed-files)) match))
  )

(defun trumba-files (subdir)
  "Returns a list of files from SUBDIR ignoring ``.'' and ``..''"
  (let ((files (directory-files (concat trumba-cache subdir)))
        (feeds nil)
        (file nil))
    (while files
      (setq file (car files))
      (setq files (cdr files))
      (if (not (file-directory-p file)) (push file feeds)))
    feeds)
  )

(defun trumba-files-to-s (files)
  "Returns a space delimited list of file names from the content of FILES"
  (let ((file nil)
        (s "")
        (i 0))
    (while files
      (if (> i 0) (setq s (concat s " ")))
      (setq i (+ i 1))
      (setq file (car files))
      (setq files (cdr files))
      (setq s (concat s (file-name-nondirectory file))))
    s)
  )


(defun trumba-feed-files ()
  "Returns a list of feed files"
  (trumba-files "feeds")
  )

(defun trumba-data-files ()
  "Returns a list of data files"
  (trumba-files "data")
  )

(defun trumba-grep (files needle)
  "Grep across the children of SUBDIR for NEEDLE"
  (let* ((dir (concat trumba-cache subdir))
         (files (trumba-cache-files-to-s (directory-files dir))))
    (if (> (length files) 0)
        (progn
          (message "Grepping %s, %s for %s" dir files needle)
          (lgrep needle files dir))
      (message "No children in cache directory: %s" dir)
      nil)
    )
  )

(provide 'trumba)

;;; trumba.el ends here
