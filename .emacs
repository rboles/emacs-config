;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs
;; 
;; Viperized GNU Emacs configuration
;;
;; sboles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; configuration variables
(defvar home-dir "/home/likewise-open/JIVELAND/shawn.boles")

(defvar site-lisp-path "/usr/local/share/emacs/site-lisp")

;; general emacs
(add-to-list 'load-path site-lisp-path)
(add-to-list 'exec-path "/usr/bin")
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(toggle-scroll-bar -1)
(scroll-bar-mode -1)
(tool-bar-mode 0)
(transient-mark-mode t)
(column-number-mode t)
(setq ring-bell-function 'ignore)

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

;; default font
(set-default-font
 (if (eq window-system 'w32)
     "Bitstream Vera Sans mono-10"
   (if (eq window-system 'ns)
       "Monaco 14"
     "DejaVu Sans Mono 12")))

(set-face-attribute 'default t :font
                    (if (eq window-system 'w32)
                        "Bitstream Vera Sans mono-10"
                      (if (eq window-system 'ns)
                          "Monaco 14"
                        "DejaVu Sans Mono 12")))

(set-face-attribute 'default nil :font
                    (if (eq window-system 'w32)
                        "Bitstream Vera Sans mono-10"
                      (if (eq window-system 'ns)
                          "Monaco 14"
                        "DejaVu Sans Mono 12")))

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; smart-tab
(require 'smart-tab)
(global-smart-tab-mode 1)

;; revbufs
(require 'revbufs)

;; viper & vimpulse
(add-to-list 'load-path (concat site-lisp-path "/vimpulse"))
(setq viper-mode t)
(setq viper-ex-style-editing nil)
(require 'viper)
(require 'redo)
(require 'rect-mark)
(require 'vimpulse)
(require 'viper-in-more-modes)
(setq woman-use-own-frame nil)
(setq woman-use-topic-at-point t)
(define-key viper-vi-global-user-map "\C-wj" 'windmove-down)
(define-key viper-vi-global-user-map "\C-wk" 'windmove-up)

;; Don't allow viper to load in some modes
(when (boundp 'viper-emacs-state-mode-list)
      (mapc (lambda (mode)
      (add-to-list 'viper-emacs-state-mode-list mode))
      '(haskell-interactive-mode ensime-inferior-scala magit-key-mode)))

;; Package
;(require 'package)
;(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;                         ("marmalade" . "http://marmalade-repo.org/packages/")
;                         ("melpa" . "http://melpa.milkbox.net/packages/")))
;(setq package-load-list '(all))
;(setq package-enable-at-startup nil)
;(package-initialize)

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

;; tumblr, wb4sed
(add-to-list 'load-path (concat site-lisp-path "/tumblr"))
(require 'wb4sed)

;; brew tools
(add-to-list 'load-path (concat site-lisp-path "/brew"))
(require 'brew)

;; project tools
(add-to-list 'load-path (concat site-lisp-path "/my-project"))
(require 'my-project)
(load (concat home-dir "/.my-projects.el"))

;; twittering-mode
(add-to-list 'load-path (concat site-lisp-path "/twittering-mode-3.0.0"))
(require 'twittering-mode)
(setq twittering-timer-interval 300)
(setq twittering-use-master-password t)

;; Magit
;; See: http://daemianmack.com/magit-cheatsheet.html
(add-to-list 'load-path (concat site-lisp-path "/magit"))
(require 'magit)

;; Maven
(add-to-list 'load-path (concat site-lisp-path "/maven"))
(load "mvn.el")

;; scala-mode2
;(require 'scala-mode2)

;; Plain old Scala mode - comment out to use scala-mode2
;; See also, http://scala.sygneca.com/tools/emacs
;; and https://github.com/aemoncannon/ensime
(add-to-list 'load-path (concat site-lisp-path "/scala-mode"))
(load "scala-mode-auto.el")
(require 'scala-mode)
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . scala-mode))

(add-to-list'load-path (concat site-lisp-path "/ensime/elisp"))
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(defun scala-turn-off-indent-tabs-mode ()
  (setq indent-tabs-mode nil))

(add-hook 'scala-mode-hook 'scala-turn-off-indent-tabs-mode)

(add-hook 'scala-mode-hook
          '(lambda ()
             (define-key scala-mode-map '[f6] 'mvn-scala)))

;; Haskell mode
(add-to-list 'load-path (concat site-lisp-path "/haskell-mode"))
(load "haskell-site-file")

(load (concat home-dir "/.haskell-mode.el"))

;; Mustache mode
(add-to-list 'load-path (concat site-lisp-path "/mustache-mode"))
(require 'mustache-mode)
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . mustache-mode))

;; planner
(load (concat home-dir "/.planner.el"))

;; Various mode hooks
(add-hook 'text-mode-hook
					'(lambda () (auto-fill-mode 1)))

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

