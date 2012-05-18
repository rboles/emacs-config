;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Viper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq viper-always t)

(setq viper-expert-level '5)

(setq viper-inhibit-startup-message t)

(setq-default viper-auto-indent t) 

(setq viper-ex-style-motion nil)

(setq viper-ex-style-editing nil)

; make :n cycle through buffers on the current window
(setq ex-cycle-other-window nil)

; Keymaps to make viper more vim like (from vimpulse.el)
; Command mode keys
(define-key viper-vi-global-user-map "gg"
	'viper-goto-first-line) 
(define-key viper-vi-global-user-map "*" 
	'viper-search-forward-for-word-at-point) 
(define-key viper-vi-global-user-map "#"
	'viper-search-backward-for-word-at-point)

; Window manipulation
(define-key global-map "\C-w"     (make-sparse-keymap)) 
(define-key global-map "\C-w\C-w" 'viper-cycle-windows)
(define-key global-map "\C-w\C-j" 'viper-next-window) 
(define-key global-map "\C-wj"    'viper-next-window) 
(define-key global-map "\C-w\C-k" 'viper-previous-window) 
(define-key global-map "\C-wk"    'viper-previous-window)

; Vim-like completion key
(define-key viper-insert-global-user-map "\C-n" 'dabbrev-expand)

; Functions used by key mappings
(defun viper-goto-first-line ()
	"send point to the start of the first line."
	(interactive)
	(viper-goto-line 1))

(defun viper-search-for-word-at-point (forward)
	"Search forwards or backward for word under point."
	(let ((word (thing-at-point 'word)))
		(setq viper-s-string word)
		(setq viper-s-forward forward)
		(viper-search word forward 1)))

(defun viper-search-forward-for-word-at-point ()
	(interactive)
	(viper-search-for-word-at-point t))

(defun viper-search-backward-for-word-at-point ()
	(interactive)
	(viper-search-for-word-at-point nil)) 

(defun viper-cycle-windows ()
	"Cycle point to another window."
	(interactive)
	(select-window (next-window)))

(defun viper-next-window ()
	"Cycle point to next window."
	(interactive)
	(select-window (next-window)))

(defun viper-previous-window ()
	"Cycle point to previous window."
	(interactive)
	(select-window (previous-window)))

(defun viper-previous-window ()
	"Cycle point to previous window."
	(interactive)
	(select-window (previous-window)))

