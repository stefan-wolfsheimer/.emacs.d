;;;;;;;;;;;;;;;;;;;;;;;;
;; load paths
;;;;;;;;;;;;;;;;;;;;;;;;
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("emacs-neotree" 
                                       "column-marker"
                                       "emacs-bash-completion"
                                       "js2-mode"
                                       "yaml-mode"
                                       "markdown-mode"
                                       "s.el" ; required by docker-file
                                       "dockerfile-mode")))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; neotree
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-window-width 60)


;;;;;;;;;;;;;;;;;;;;;;;;;
;; column-marker
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'column-marker)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; whitespace
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'whitespace)
(setq tab-width 2)
(setq-default indent-tabs-mode nil)
(setq column-number-mode t)
(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))
;(global-whitespace-toggle-options 1)
;(global-whitespace-mode 1)


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

(global-set-key (kbd "C-x 4") (function (lambda ()
                                          (interactive)
                                          (delete-other-windows)
                                          (split-window-vertically)
                                          (split-window-horizontally)
                                          (windmove-down)
                                          (split-window-horizontally))))

(global-set-key (kbd "C-x SPC") (function (lambda () 
                                            (interactive) 
                                            (whitespace-mode 1))))
(global-set-key (kbd "C-c SPC") (function (lambda () 
                                            (interactive) 
                                            (whitespace-mode 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'js2-mode)
(require 'js2-imenu-extras)
(add-to-list
 'auto-mode-alist
 '("\\.js\\'" . js2-mode))
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
            (c-set-offset 'innamespace 2)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bash
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'bash-completion)
(bash-completion-setup)
(global-set-key [f7] 'shell)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YAML
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dockerfile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))





