;;; my-project.el --- Sugar for Emacs projects

;; Author: Shawn Boles
;; Version 0.1
;; URL: http://github.com/rboles/my-projects-el
;; This file is not part of GNU Emacs

;;; Commentary:

;; Opens project buffers. See the example ``my-project-alist'' below.
;; The project alist defines project spaces. Each space is opened in a
;; buffer.
;;
;; Load the package with:
;;
;; (require 'my-project)
;;
;; To open a project:
;;
;; M-x my-project

;;; Code:

(defvar my-project-alist nil
  "List of project spaces")

; Example project list for testing
; Override my-project-alist with your own project list
(setq my-project-alist
      '(("BrewTools" .
         (:prefix "~/code/Scala/BrewTools"
                  :spaces (("webapp" "/src/main/webapp")
                           ("scala" "/src/main/scala/com/sboles/brew"))
                  ))
        ("Zola" .
         (:prefix "~/code/Scala/Zola"
                  :spaces (("webapp" "/src/main/webapp")
                           ("scala" "/src/main/scala/com/collegenet/series25"))
                  ))
        ))

(defun my-project-alist-names ()
  "Returns a list of all project names"
  (let ((alist my-project-alist)
        (cur nil)
        (names nil))
    (while alist
      (push (car (car alist)) names)
      (setq alist (cdr alist))
      )
    names)
  )

(defun my-project-alist-project (name)
  "Returns property list for project identified by NAME or nil if no
match"
  (cdr (assoc name my-project-alist))
  )

(defun my-project-alist-property (name prop &optional val
                                       )
  "Returns PROP value associated with project identified by NAME.

If VAL is provided, the project alist property is set to VAL and VAL
returned."
  (let ((plist (cdr (assoc name my-project-alist)))
        (alist nil)
        (cur nil))
    (if (and plist val)
        (progn
          (setq plist (plist-put plist prop val))
          val)
      (plist-get plist prop)))
  )

(defun my-project (&optional my-project)
  "Opens a workspace"
  (interactive)
  (let* ((projects (my-project-alist-names))
         (project (if my-project my-project
                    (read-string "Project: "
                                 (car projects)
                                 (cons 'projects 1))))
         (prefix (my-project-alist-property project :prefix))
         (spaces (my-project-alist-property project :spaces))
         (space nil))
    (while spaces
      (setq space (car spaces))
      (setq spaces (cdr spaces))
      (message "Opening project %s" project)
      (let ((buf-name (car space))
            (buf-path (car (cdr space))))
        (find-file (concat prefix buf-path))
        (rename-buffer (concat project "-" buf-name))
        )
      )
    )
  )

(provide 'my-project)

;;; my-project.el ends here
