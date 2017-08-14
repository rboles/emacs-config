;;; r25ws-alist.el --- R25WS instance identifiers and utilities

;; Copyright (c) 2011 Shawn Boles, CollegeNet Inc
;; Author: Shawn Boles

;;; Code:

(defvar r25ws-alist nil
  "List of R25WS instances")

; Temporary instance list used for testing
(setq r25ws-alist
      '(("25LiveDev" .
         (:baseurl "http://tiamat.unival.com/25live/voodoo/run/"
                   :logdir "R:/voodoo/log/wasv"
                   :username "r25admin"
                   :password ""))
        ("25LiveConfig" .
         (:baseurl "http://tiamat.unival.com/25live-config/voodoo/run/"
                   :logdir "R:/voodoo/log/config"
                   :username "r25admin"
                   :password ""))
        ("25LiveAdmin" .
         (:baseurl "http://tiamat.unival.com/25live-admin/voodoo/run/"
                   :logdir "R:/voodoo/log/admin"
                   :username "r25admin"
                   :password ""))
        ("R25WSDev" .
         (:baseurl "http://balrog.unival.com/r25d/servlet/wrd/run/"
                   :logdir "Q:/log"
                   :username "r25demo"
                   :password ""))
        ))

(defun r25ws-alist-names ()
  "Returns a list of all R25WS instance names"
  (let ((alist r25ws-alist)
        (cur nil)
        (names nil))
    (while alist
      (push (car (car alist)) names)
      (setq alist (cdr alist))
      )
    names)
  )

(defun r25ws-alist-inst (name)
  "Returns instance property list for the R25WS instance associated
with NAME or nil if no match"
  (cdr (assoc name r25ws-alist))
  )

(defun r25ws-alist-property (name prop &optional val)
  "Returns PROP value associated with R25WS instance identified
by NAME.

If VAL is provided, the R25WS-ALIST property is set to VAL and VAL
returned."
  (let ((plist (cdr (assoc name r25ws-alist)))
        (tlist r25ws-alist)
        (alist nil)
        (cur nil))
    (if (and plist val)
        (progn
          (setq plist (plist-put plist prop val))
          (while tlist
            (setq cur (car tlist))
            (setq tlist (cdr tlist))
            (if (string= name (car cur))
                (push (cons name plist) alist)
              (push cur alist)))
          (setq r25ws-alist alist)
          val)
      (plist-get plist prop)))
  )

(defun r25ws-alist-baseurl (name &optional baseurl)
  "Get the base URL for the R25WS instance identified by NAME

If BASEURL is provided, set the base url property and return new
value."
  (if baseurl
      (r25ws-alist-property name :baseurl baseurl)
    (r25ws-alist-property name :baseurl))
  )

(defun r25ws-alist-username (name &optional username)
  "Get the username for the R25WS instance identified by NAME

If USERNAME is provided, set the username property and return new
value."
  (if username
      (r25ws-alist-property name :username username)
    (r25ws-alist-property name :username))
  )

(defun r25ws-alist-password (name &optional password)
  "Get the password for the R25WS instance identified by NAME

If PASSWORD is provided, set the password property and return new
value."
  (if password
      (r25ws-alist-property name :password password)
    (r25ws-alist-property name :password))
  )

(defun r25ws-alist-logdir (name &optional logdir)
  "Get the logdir for the R25WS instance identified by NAME

If LOGDIR is provided, set the logdir property and return new
value."
  (if logdir
      (r25ws-alist-property name :logdir logdir)
    (r25ws-alist-property name :logdir))
  )

(defun r25ws-alist-add ()
  "Prompts a user to add an instance to the R25WS instance
list. Returns the name of the new instance."
  (interactive)
  (let ((inst (read-string "New R25WS instance name: "))
        (burl (read-string "Base URL: "))
        (logd (read-string "Log directory: "))
        (user (read-string "Username: "))
        (pass (read-string "Password: ")))
    (setq r25ws-alist
          (cons 
           (cons inst (list :baseurl burl :logdir logd 
                            :username user :password pass))
           r25ws-alist))
    inst
    )
  )

(defun r25ws-alist-del ()
  "Prompts user to remove an instance from the R25WS instance list."
  (interactive)
  (let* ((alist r25ws-alist)
         (names (r25ws-alist-names))
         (name (read-string "Remove R25WS instance: "
                           (car names) (cons 'names 1)))
         (plist nil)
         (nlist nil)
         (cur nil))
    (while alist
      (setq cur (car alist))
      (setq alist (cdr alist))
      (if (string= name (car cur))
          (message "Removing %s from instance list" name)
        (push cur nlist)))
    (setq r25ws-alist nlist))
  )

(defun r25ws-alist-print ()
  "Creates a buffer, prints the R25WS-ALIST and shows the buffer."
  (interactive)
  (let ((buffer (get-buffer-create "r25ws-alist"))
        (alist r25ws-alist)
        (plist nil)
        (inst nil)
        (prop nil)
        (cur nil))
    (switch-to-buffer buffer)
    (erase-buffer)
    (lisp-mode)
    (insert ";; Current state of R25WS-ALIST\n"
            ";;\n"
            ";; Copy this to .emacs to remember it for your next session."
            "\n\n(setq r25ws-alist\n")
    (insert "'(")
    (while alist
      (setq cur (car alist))
      (setq alist (cdr alist))
      (setq inst (car cur))
      (setq plist (cdr (assoc inst r25ws-alist)))
      (insert "\n(\"" inst "\" .\n("
              ":baseurl "
              "\"" (plist-get plist :baseurl) "\"\n"
              ":logdir "
              "\"" (plist-get plist :logdir) "\"\n"
              ":username "
              "\"" (plist-get plist :username) "\"\n"
              ":password "
              "\"" (plist-get plist :password) "\""
              "))")
      )
    (insert "\n))\n")
    (indent-region (point-min) (point-max))
    (goto-char (point-min)))
  )

(provide 'r25ws-alist)

;;; r25ws-alist.el ends here.
