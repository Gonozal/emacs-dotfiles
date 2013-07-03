;;; vim scrolloff
(setq scroll-margin 4)


(scroll-bar-mode -1)


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(setq prelude-whitespace nil)
(disable-theme 'molokai)
(disable-theme 'zenburn)
(load-theme 'base16-default)

(set-face-attribute 'default nil
                    :family "Source Code Pro for Powerline" :height 100 :weight 'light)
;; (mapc
;;  (lambda (face)
;;    (set-face-attribute face nil :weight 'light :underline nil))
;;  (face-list))

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))


(add-hook 'org-mode-hook (lambda () (linenum-mode 0)))


; (mapc
;  (lambda (face)
;    (set-face-attribute face nil :weight 'light :underline nil))
;  (face-list))
