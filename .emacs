;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .emacs
;; 
;; GNU Emacs configuration
;;
;; sboles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; configuration
(defvar home-dir "/Users/shawnboles")

(defvar site-lisp-path "/Users/shawnboles/.site-lisp")

;; paths
(add-to-list 'load-path site-lisp-path)
(add-to-list 'exec-path "/usr/local/bin")

;; look and feel
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(toggle-scroll-bar -1)
(tool-bar-mode 0)
(transient-mark-mode t)
(column-number-mode t)
(setq ring-bell-function #'ignore)

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
(setq fill-column 80)
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
     "Bitstream Vera Sana Mono 10")))

;; "y or n" instead of "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; ensure correct PATH in OSX
(when (memq window-system '(mac ns))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

;; smart-tab
(require 'smart-tab)
(global-smart-tab-mode 1)

;; goto-change
(require 'goto-chg)

;; vi modes with evil
;; https://www.emacswiki.org/emacs/Evil
(add-to-list 'load-path (concat site-lisp-path "/evil"))
(require 'evil)
(evil-mode 1)

;; package
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

;; spellcheck
;; http://www.delorie.com/gnu/docs/emacs/emacs_109.html
(setq-default ispell-program-name 
              (if (eq window-system 'w32)
                  "C:/Program Files/Aspell/bin/aspell.exe"
                "aspell"))

;; start emacs server
(server-start)

;; try to improve slow performance on Windows
(if (eq window-system 'w32)
    (setq w32-get-true-file-attributes nil))

;; brew tools
(add-to-list 'load-path (concat site-lisp-path "/brew"))
(require 'brew)

;; project tools
(add-to-list 'load-path (concat site-lisp-path "/my-project"))
(require 'my-project)
(load (concat home-dir "/.my-projects.el"))

;; maven
(add-to-list 'load-path (concat site-lisp-path "/maven"))
(load "mvn.el")

;; planner
(load (concat home-dir "/.planner.el"))

;; mode hooks
(add-hook 'text-mode-hook
					'(lambda () (auto-fill-mode 1)))

;; utility to get an x-style font string for a windows font
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

