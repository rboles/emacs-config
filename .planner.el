;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .planner.el
;;
;; Emacs planner configuration
;;
;; sboles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; location of planner files
(defvar my-planner-home (concat home-dir "/Documents/plans")
  "Location of planner files")

;; destination for planner HTML files
(defvar my-planner-publish-path
  (concat home-dir "/Sites/plans")
  "Location of planner generated HTML files")

(defvar my-planner-project-name	"WikiPlanner"
	"Identifies a Planner project.
``WikiPlanner'' is a common default")

(defvar my-planner-trunk-rule-list
	(list "TaskPool"
        "serv")
	"Visual grouping of Planner projects on plan pages.")

(defvar my-planner-project-conf
	(list my-planner-home
				:default "index"
				:force-publish '("index")
				:major-mode 'planner-mode
				:visit-link 'planner-visit-link)
	"Planner project configurables.")

(defvar my-planner-publish-conf
	(list :base "planner-html" :path my-planner-publish-path)
	"Planner publishing configurables.")

(defvar my-muse-project-alist
	(cons (list my-planner-project-name
							my-planner-project-conf
							my-planner-publish-conf) ())
	"A list containing Muse project configurables.
You should not have to worry about changing this variable.")

(defvar my-planner-stylesheet
	(concat "<link rel=\"stylesheet\" type=\"text/css\""
					" href=\"gfx/planner.css\" />")
	"HTML element identifying the CSS stylesheet
used for planner pages published as HTML")

(defvar my-planner-private-projects
	(list "TaskPool" "Test")
	"List of project names or name prefixes
identifying projects not to be published.")

(add-to-list 'load-path (concat site-lisp-path "/muse/lisp"))
(add-to-list 'load-path (concat site-lisp-path "/planner"))
(add-to-list 'load-path (concat site-lisp-path "/remember"))

(require 'muse-wiki)
(require 'planner)
(require 'remember-planner)
(require 'planner-publish)
(require 'planner-trunk)

;; set project
(setq planner-project "WikiPlanner")

;; defines the muse project
(setq muse-project-alist my-muse-project-alist)

;; do not try to show images in muse mode
(setq muse-colors-inline-images nil)

;; don't complain about wiki-like words that dont have pages
(setq muse-wiki-allow-nonexistent-wikiword t)

;; planner-html publishing stylesheet
(setq planner-html-style-sheet my-planner-stylesheet)

;; add link back to index.html in html footer
(setq planner-html-inner-footer "<a href=\"index.html\">~sboles</a>")

;; add remember entries as notes to planner
(setq remember-handler-functions '(remember-planner-append))
(setq remember-annotation-functions planner-annotation-functions)

;; planner task grouping
(setq planner-trunk-rule-list
			(cons (list "." nil my-planner-trunk-rule-list) ())
			)

;; only show planner tasks and notes; I don't use the schedule
(setq planner-day-page-template	"* Tasks\n\n\n* Notes")

;; auto publish pages after save
(eval-after-load "muse-mode"
	'(add-hook 'after-save-hook
						 #'(lambda ()
								 (when (planner-derived-mode-p 'muse-mode)
									 (muse-project-publish nil)))
						 nil t))

(defun my-planner-publish-gfx (src-file)
  "Copies src-file to my-planner-publish-path.
Looks for global variable my-planner-publish-path
Called by the my-muse-after-publish-hook"
  (setq fname (file-name-nondirectory src-file))      ; get src file
  (setq dest (concat my-planner-publish-path "/gfx")) ; get dist dir
  (if (not (equal "." (substring fname 0 1)))         ; ignore .*
      (if (file-newer-than-file-p src-file (concat dest "/" fname))
          (copy-file src-file dest t))))

(defun my-muse-after-publish-hook ()
  "Called by muse-after-publish-hook.
Looks for global variables: my-planner-home, my-planner-publish-path
Checks the planner-home gfx directory for updated images and stylesheets.
Copies updated files to the publish directory. See my-planner-publish-gfx"
  (if (file-writable-p my-planner-publish-path)
      (mapcar 'my-planner-publish-gfx
              (directory-files (concat my-planner-home "/gfx") 1))
    (message "Error cannot write to %s" my-planner-publish-path)))

(defun my-is-private-project (project-file private-list)
	"Tests a project name against a private project list.
The test is a substring match, starting at zero.

For example, the private list item matches the project
name ``Foo-BarBaz''.

Returns t if project name has a private list match; nil if not"
	(setq found nil)
	(while (and private-list (not found))
		(setq match (car private-list))
		(setq max
					(if (< (length project-file) (length match))
							(length project-file)
						(length match)))
		(if (string= (substring project-file 0 max) match)
				 (setq found t))
		(if found
				(message (concat "Project file: " project-file
												 ", matches private member "
												 match "; not publishing"))
			)
		(setq private-list (cdr private-list)))
	found
	)

(defun muse-project-private-p (file)
	"Overrides Muse function that determines if a project is private.
Checks file against my private project list instead. If the project
file matches the private list, it is not published"
	(my-is-private-project (file-name-nondirectory file)
												 my-planner-private-projects)
	)

;; copy images and stylesheets to HTML after planner publish
(add-hook 'muse-after-publish-hook 'my-muse-after-publish-hook)

;; automatically trunk tasks after task creation
(add-hook 'planner-create-task-hook 'planner-trunk-tasks)

