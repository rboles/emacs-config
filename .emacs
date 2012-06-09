;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs
;; 
;; Viperized GNU Emacs
;;
;; sboles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; configuration variables
(defvar home-dir
	(if (eq window-system 'w32)
			"c:/Cygwin/home/sboles"
		"/home/sboles")
	"Path to home directory")

(defvar site-lisp-path
	(if (eq window-system 'w32)
			"c:/emacs/site-lisp"
		"/usr/share/emacs/site-lisp")
	"Path to site-lisp directory. No trailing slash.")

(defvar my-planner-home 
  (if (eq window-system 'w32)
      "f:/Dropbox/docs/plans"
    (concat home-dir "/docs/plans"))
	"Path to Planner files. No trailing slash.")

(defvar my-planner-publish-path
	(if (eq window-system 'w32)
			"y:/plans"
		(concat home-dir "/public-html/plans"))
	"Path to publish Planner files")

(defvar my-planner-project-name	"WikiPlanner"
	"Identifies a Planner project.
``WikiPlanner'' is a common default")

(defvar my-planner-trunk-rule-list
	(list "TaskPool"
        "R25WS"
        "25LiveInvoicing"
				"25LivePublisher"
				"25LiveRegistration"
				"R25Import"
				"RoomWizard"
				"Emacs")
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
	(list "TaskPool" "Test" "JobSearch")
	"List of project names or name prefixes
identifying projects not to be published.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; general emacs
(add-to-list 'load-path site-lisp-path)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(toggle-scroll-bar -1)
(tool-bar-mode 0)
(transient-mark-mode t)
(column-number-mode t)

;; confirm exit
(setq confirm-kill-emacs 'yes-or-no-p)

;; move point between windows with meta and arrow keys
(windmove-default-keybindings 'meta)

;; tabs displayed as 2 characters and saved as spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; show paren matches
(setq show-paren-mode 1)

;; start in home
(cd home-dir)

;; beautification
(setq fill-column 75)
(add-to-list 'load-path (concat site-lisp-path "/color-theme"))
(require 'color-theme)
(color-theme-initialize)
(color-theme-billw)
(set-default-font
 (if (eq window-system 'w32)
		 "Bitstream Vera Sans mono-10"
   "Bitstream Vera Sans Mono 10"))

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; smart-tab
(require 'smart-tab)
(global-smart-tab-mode 1)

;; Windows copy/paste
(setq cua-mode 1)

;; viper & vimpulse
(add-to-list 'load-path (concat site-lisp-path "/vimpulse"))
(setq viper-mode t)
(setq viper-ex-style-editing nil)
(require 'viper)
(require 'redo)
(require 'rect-mark)
(require 'vimpulse)
(setq woman-use-own-frame nil)
(setq woman-use-topic-at-point t)
(define-key viper-vi-global-user-map "\C-wj" 'windmove-down)
(define-key viper-vi-global-user-map "\C-wk" 'windmove-up)

(require 'viper-in-more-modes)

;; spellcheck
;; see http://www.delorie.com/gnu/docs/emacs/emacs_109.html
(setq-default ispell-program-name 
              (if (eq window-system 'w32)
                  "C:/Program Files/Aspell/bin/aspell.exe"
                "aspell"))

;; start emacs server
(server-start)

;; try to improve slow performance on Windows
(if (eq window-system 'w32)
    (setq w32-get-true-file-attributes nil))

;; gnus
;(require 'gnus)

;; r25ws client
;(add-to-list 'load-path (concat site-lisp-path "/r25ws"))
;(require 'r25ws)

;; backbase
;(add-to-list 'load-path (concat site-lisp-path "/bxml"))
;(require 'bxml)

;; freshbooks
;(add-to-list 'load-path (concat site-lisp-path "/freshbooks"))
;(require 'freshbooks)

;; trumba
;(add-to-list 'load-path (concat site-lisp-path "/trumba"))
;(require 'trumba)

;; tumblr, wb4sed
(add-to-list 'load-path (concat site-lisp-path "/tumblr"))
(require 'wb4sed)

;; brew tools
(add-to-list 'load-path (concat site-lisp-path "/brew"))
(require 'brew)

;; project tools
(add-to-list 'load-path (concat site-lisp-path "/my-project"))
(require 'my-project)

(setq my-projects (concat home-dir "/.my-projects.el"))
(load my-projects)

;; twittering-mode
(add-to-list 'load-path (concat site-lisp-path "/twittering-mode-1.0.0"))
(require 'twittering-mode)
(setq twittering-timer-interval 300)
(setq twittering-curl-program
      (concat site-lisp-path "/twittering-mode-1.0.0/win-curl/curl.exe"))
(setq twittering-cert-file
      (concat site-lisp-path "/twittering-mode-1.0.0/win-curl/equifax.cer"))

;; Magit
;; See: http://daemianmack.com/magit-cheatsheet.html
(add-to-list 'load-path (concat site-lisp-path "/magit"))
(require 'magit)

;; Maven
(add-to-list 'load-path (concat site-lisp-path "/maven"))
(load "mvn.el")

;; Scala mode
;; See also, http://scala.sygneca.com/tools/emacs
(add-to-list 'load-path (concat site-lisp-path "/scala"))
(load "scala-mode-auto.el")

(defun scala-turn-off-indent-tabs-mode ()
  (setq indent-tabs-mode nil))

(add-hook 'scala-mode-hook 'scala-turn-off-indent-tabs-mode)

(add-hook 'scala-mode-hook
          '(lambda ()
             (define-key scala-mode-map '[f6] 'mvn-scala)))

;; Haskell mode
(add-to-list 'load-path (concat site-lisp-path "/haskell-mode"))
(load "haskell-site-file")

;(setq my-haskell (concat home-dir "/.haskell-mode.el"))
;(load my-haskell)

;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; Various mode hooks
(add-hook 'text-mode-hook
					'(lambda () (auto-fill-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <planner>
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

For example, the project name ``Foo-BarBaz'' matches the
private list item ``Foo-''.

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

;; </planner>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; handy utility to get an x-style font string for a windows font
(if (eq window-system 'w32)
    (defun insert-x-style-font()
      "Insert a string in the X format which describes a font
the user can select from the Windows font selector."
      (interactive)
      (insert (prin1-to-string (w32-select-font)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations

(setq custom-file (concat home-dir "/.emacs-custom.el"))
(load custom-file)

