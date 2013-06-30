(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 2)            ;; but maintain correct appearance

(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
)

(setq js-indent-level 2)

(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))

;; some more mac OS customization
(setq ns-function-modifier 'hyper)

(setq ring-bell-function 'ignore)
