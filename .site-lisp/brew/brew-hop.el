;;; brew-hop.el --- Emacs homebrew hop tools

;; This file is not part of GNU Emacs

;;; Commentary

;;; Code:

(defstruct brew-hop index name type grown profile usage aa substitute)

(defvar brew-hop-db-source
  "c:/cygwin/home/sboles/code/ELisp/brew/brew-hop-db.el"
  "File containing the hop database")

(defvar brew-hop-db nil
  "Hop database. This value is set by the content of brew-hop-db.el")

(require 'brew-hop-db)

(defun brew-hop-db-name-list ()
  (let ((hops brew-hop-db)
        (cur nil)
        (names nil))
    (while hops
      (setq cur (car hops))
      (setq hops (cdr brew-hop-db))
      (push cur-name names))
    names)
  )

(defun brew-hop-db-save ()
  "Writes the hop database to the file identified by brew-hop-db-source"
  (save-excursion
    (let ((hops brew-hop-db)
          (cur nil))
      )
    )
  )

(defun brew-hop-db-add ()
  "Prompts user for hop characteristics and adds hop to the database."
  (interactive)
  (let ((hop (make-brew-hop
              :name (read-string "Hop name: ")
              :type (brew-hop-db-read-type)
              :grown (brew-hop-db-read-grown)
              :profile (read-string "Profile: ")
              :usage (read-string "Usage: ")
              :aa (brew-hop-db-read-aa)
              :substitute (brew-hop-db-read-subst)))
        (add nil)))
  )

(defun brew-hop-db-read-grown ()
  "Prompts user for countries where a particular hop is grown."
  (let ((l nil)
        (more t))
    (while more
      (push (read-string "Country grown: ") l)
      (setq more (downcase (read-string "Add another country? (y/n): ")))
      (setq more (if (string= more "y") t nil)))
    l)
  )

(defun brew-hop-db-read-type ()
  "Prompts user for hop type; provides sane defaults"
  (let* ((l (list "Aroma" "Bittering" "Aroma/Bittering"))
         (v (read-string (concat "Hop type (" (car l) "): ")
                         nil nil l)))
    v)
  )

(defun brew-hop-db-read-aa ()
  "Prompts user for hop AA min and max."
  (let ((min (read-number "Hop AA min: "))
        (max (read-number "Max: ")))
    (list min max)))

(defun brew-hop-db-read-subst ()
  "Prompts user for hop substitutes.

TODO Ideally the defaults would come from the hop db"
  (let ((l nil)
        (more t))
    (while more
      (push (read-string "Substitute: ") l)
      (setq more (downcase (read-string "Add another substitute? (y/n): ")))
      (setq more (if (string= more "y") t nil)))
    l)
  )

(provide 'brew-hop)

;;; brew-hop.el
