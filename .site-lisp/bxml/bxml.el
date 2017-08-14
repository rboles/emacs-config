;;; bxml.el --- Backbase editting utilities

;; Author: Shawn Boles
;; Keywords: Backbase, BXML
;; Doctor: Octagon

;;; Code:

(provide 'bxml)

(defun bxml-variable ()
  "Inserts a BXML variable declaration at point. Places point inside
b:select attribute."
  (interactive)
  (let ((name (read-string "Variable name: "))
        (scope (bxml-read-scope)))
    (insert "<s:variable b:name=\"" name "\""
            " b:scope=\"" scope "\""
            " b:select=\"\" />"))
  (goto-char (- (point) 4))
  )

(defun bxml-assign ()
  "Inserts a BXML assign task at point. Places point inside b:select
attribute."
  (interactive)
  (let ((target (read-string "Target variable: " "$"))
        (scope (read-string "Scope: " "local")))
    (insert "<s:task b:action=\"assign\""
            " b:target=\"" target "\""
            " b:scope=\"" scope "\""
            "\nb:select=\"\" />"))
  (goto-char (- (point) 4))
  )

(defun bxml-handler ()
  "Inserts a BXML event handler at point."
  (interactive)
  (let ((start (point))
        (on (read-string "Handle event: ")))
    (insert "<!--**\n-->\n"
            "<s:event b:on=\"" on "\"></s:event>")
    (indent-region start (point))
    (goto-char (- (point) 10)))
  )

(defun bxml-render ()
  "Inserts a BXML render element at point."
  (interactive)
  (let ((dest (read-string "Destination: " "."))
        (mode (bxml-read-mode)))
    (insert "<s:render"
            " b:destination=\"" dest "\""
            " b:mode=\"" mode "\"></s:render>"))
  (goto-char (- (point) 11))
  )

(defun bxml-task (action)
  "Generic task handler. Inserts a BXML task element at point."
  (interactive "sAction: ")
  (cond
   ((string= action "msg") (bxml-msg))
   ((string= action "trigger") (bxml-trigger))
   ((string= action "assign") (bxml-assign))
   ((string= action "load") (bxml-load))
   ((string= action "show") (bxml-show))
   ((string= action "hide") (bxml-hide))
   ((string= action "string2xml") (bxml-string2xml))
   ((string= action "xml2string") (bxml-xml2string))
   ((string= action "transform") (bxml-transform))
   ((insert "<s:task b:action=\"" action "\" />")))
  )

(defun bxml-msg ()
  "Inserts a BXML message task at point."
  (interactive)
  (let ((prefix (concat ":" (bxml-handler-name) ":"))
        (value (read-string "Message: ")))
    (insert "<s:task b:action=\"msg\""
            " b:value=\"{concat(@c:id,':" prefix " " value "')}\" />"))
  )

(defun bxml-message ()
  "Synonym for ``bxml-msg''

Inserts a BXML message task at point."
  (bxml-msg)
  )

(defun bxml-trigger ()
  "Inserts a BXML trigger task at point."
  (interactive)
  (let ((event (read-string "Trigger event: "))
        (target (read-string "Target: " ".")))
    (insert "<s:task b:action=\"trigger\""
            " b:event=\"" event "\""
            " b:target=\"" target "\" />"))
  )

(defun bxml-load ()
  "Inserts a BXML load task at point."
  (interactive)
  (let* ((method (read-string "Load method: "))
         (url (read-string "URL: "))
         (data (if (string= (downcase method) "get") ""
                 (read-string "Data: ")))
         (dest (read-string "Destination: " "." ))
         (mode (bxml-read-mode)))
    (insert "<s:task b:action=\"load\""
            " b:method=\"" (upcase method) "\""
            " b:url=\"" url "\""
            " b:data=\"" data "\""
            " b:destination=\"" dest "\""
            " b:mode=\"" mode "\" />"))
  )

(defun bxml-hide ()
  "Inserts a BXML hide task at point."
  (interactive)
  (let ((target (read-string "Hide target: " ".")))
    (insert "<s:task b:action=\"hide\""
            " b:target=\"" target "\" />"))
  )

(defun bxml-show ()
  "Inserts a BXML show task at point."
  (interactive)
  (let ((target (read-string "Show target: " ".")))
    (insert "<s:task b:action=\"show\""
            " b:target=\"" target "\" />"))
  )

(defun bxml-string2xml ()
  "Inserts a BXML string2xml task at point."
  (interactive)
  (let ((variable (read-string "String to XML variable: $"))
        (source (read-string "Source: ")))
    (insert "<s:task b:action=\"string2xml\"")
    (if (and source (> 0 (length source)))
        (insert " b:source=\"" source "\""))
    (insert " b:variable=\"" variable "\" />"))
  )

(defun bxml-xml2string ()
  "Inserts a BXML xml2string task at point."
  (interactive)
  (let ((variable (read-string "XML to string variable: $"))
        (source (read-string "Source: ")))
    (insert "<s:task b:action=\"xml2string\"")
    (if (and source (> 0 (length source)))
        (insert " b:source=\"" source "\""))
    (insert " b:variable=\"" variable "\" />"))
  )

(defun bxml-transform ()
  "Inserts a BXML transform task at point."
  (interactive)
  (let ((xsl (read-string "Stylesheet: "))
        (data (read-string "Datasource: "))
        (dest (read-string "Destination: "))
        (mode (bxml-read-mode)))
    (insert "<s:task b:action=\"transform\""
            " b:stylesheet=\"" xsl "\""
            " b:datasource=\"" data "\""
            " b:destination=\"" dest "\""
            " b:mode=\"" mode "\" />"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bxml-find-handler (&optional handler)
  "Search buffer for event HANDLER. If found places the point on the
handler name. Search is case-insensitive."
  (interactive)
  (let* ((name (if handler handler
                 (read-string "Handler: ")))
         (start (point))
         (found nil)
         (regex (concat "<s:event b:on=\"" name)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward regex nil t)
          (setq found (point))))
    (if found 
        (progn
          (goto-char (- found 1))
          (search-backward "\"")
          (goto-char (+ (point) 1)))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities start here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bxml-handler-name ()
  "Searches backwards for the name of the event handler containing the
point.

Returns the event name or nil if not match."
  (save-excursion
    (if (re-search-backward "<s:event b:on=\"\\(.*?\\)\"" nil t)
        (match-string-no-properties 1)
      nil))
  )

(defun bxml-behavior-name ()
  "Searches backwards for the name of the behavior containing the point.

Returns the behavior name or nil if none found."
  (save-excursion
    (if (re-search-backward "<s:behavior b:name=\"\\(.*?\\)\"" nil t)
        (match-string-no-properties 1)
      nil))
  )

(defun bxml-read-scope ()
  "Reads a variable scope string, returns value"
  (let* ((scopes '("local" "tag" "global"))
         (scope (read-string "Scope: "
                             (car scopes)
                             (cons 'scopes 1))))
    scope)
  )

(defun bxml-read-mode ()
  "Reads a render mode string, returns value"
  (let* ((modes '("replacechildren" "aslastchild" "asfirstchild" "replace"))
         (mode (read-string "Mode: " (car modes)
                            (cons 'modes 1))))
    mode)
  )
;;; bxml.el ends here
