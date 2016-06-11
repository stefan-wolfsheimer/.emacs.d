;;;;;;;;;;;;;;;;;;;;;;;;
;; load paths
;;;;;;;;;;;;;;;;;;;;;;;;
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("emacs-neotree" "column-marker")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; neotree
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; column-marker
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'column-marker)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; whitespace
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'whitespace)
(setq tab-width 2)
(setq indent-tabs-mode nil)
(setq column-number-mode t)
(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))
(global-whitespace-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alt+f -> other frame
(global-set-key "\M-f" 'other-frame)

;; 
(global-set-key [M-left] 'windmove-left) 
(global-set-key [M-right] 'windmove-right) 
(global-set-key [M-up] 'windmove-up) 
(global-set-key [M-down] 'windmove-down)

(global-set-key (kbd "C-c o") 'ff-find-other-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq js-indent-level 2)
(add-hook 'js-mode-hook 
	  (lambda () 
	    (interactive) (column-marker-1 80)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq c-basic-indent 2)
(add-hook 'c-mode-common-hook 
	  (lambda () 
	    (c-set-offset 'inline-open 0)
	    (c-set-offset 'substatement-open 0)))
(add-hook 'c-mode-hook 
	  (lambda () 
	    (interactive) (column-marker-1 80)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; c++ mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c++-mode-hook 
	  (lambda () 
	    (c-set-offset 'innamespace 0)))
(add-hook 'c++-mode-hook 
	  (lambda () 
	    (interactive) (column-marker-1 80)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook 
	  (lambda () 
	    (interactive) (column-marker-1 80)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs lisp mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'emacs-lisp-mode-hook 
	  (lambda () 
	    (interactive) (column-marker-1 80)))







