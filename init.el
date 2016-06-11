;;;;;;;;;;;;;;;;;;;;;;;;
;; load paths
;;;;;;;;;;;;;;;;;;;;;;;;
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("neotree")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; neotree
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

