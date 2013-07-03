;;;; Tab settings ;;;;
; Tab width is 2
(setq tab-width 2)

; Tab width is 2 by default..
(setq-default tab-width 2)

; Use spaces always.
(setq indent-tabs-mode nil)

; Jump by 2.
(setq c-basic-offset 2)

; this defaulted to 4 and had to be reset to 2. 
(setq perl-indent-level 2)

;Tab stop list out to col 52
;Manually set by x2
(setq tab-stop-list '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52))

(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
)

(setq js-indent-level 2)
(setq-default js2-basic-offset 2)
(setq js2-basic-offset 2)

(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))

;; some more mac OS customization
(setq ns-function-modifier 'hyper)

(setq ring-bell-function 'ignore)
