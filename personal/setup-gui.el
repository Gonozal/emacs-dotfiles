;;; package --- Summary

;;; Commentary:

;;; Code:

;; Set Font Face
(set-face-attribute 'mode-line nil :box nil
                    :family "Source Code Pro for Powerline"
                    :height 100 :weight 'light)
(set-face-attribute 'default nil
                    :family "Source Code Pro for Powerline"
                    :height 100 :weight 'light)
(set-face-attribute 'mode-line-inactive nil :box nil)

(load-theme 'base16-default) ;; Load the best theme ever
(powerline-default-theme)    ;; load powerline
(setq scroll-margin 4)       ;; Sane cursor and window movements
(scroll-bar-mode -1)         ;; No scrollbars, thank you
(global-diff-hl-mode)        ;; show visual VCS diffs

(provide 'setup-gui)
;;; setup-gui.el ends here
