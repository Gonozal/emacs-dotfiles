(require 'powerline)

(add-hook 'evil-normal-state-entry-hook
          '(lambda ()
             (set-face-background 'mode-line "#2a5000")
             (set-face-foreground 'mode-line "#B4C342")


             (set-face-background 'powerline-active1 "#586e75")
             (set-face-foreground 'powerline-active1 "#eee8d5")

             (set-face-background 'powerline-active2 "#657b83")
             (set-face-foreground 'powerline-active2 "#fdf6e3")

             (powerline-reset)))

(add-hook 'evil-emacs-state-entry-hook
          '(lambda ()
             (set-face-background 'mode-line "#93115C")
             (set-face-foreground 'mode-line "#FA91EC")

             (set-face-background 'powerline-active1 "#586d75")
             (set-face-foreground 'powerline-active1 "#eee8d5")

             (set-face-background 'powerline-active2 "#657b83")
             (set-face-foreground 'powerline-active2 "#fdf6e3")

             (powerline-reset)))

(add-hook 'evil-replace-state-entry-hook
          '(lambda ()
             (set-face-background 'mode-line "#990A1B")
             (set-face-foreground 'mode-line "#FF6E64")

             (set-face-background 'powerline-active1 "#586d75")
             (set-face-foreground 'powerline-active1 "#eee8d5")

             (set-face-background 'powerline-active2 "#657b83")
             (set-face-foreground 'powerline-active2 "#fdf6e3")

             (powerline-reset)))

(add-hook 'evil-visual-state-entry-hook
          '(lambda ()
             (set-face-background 'mode-line "#8B2C02")
             (set-face-foreground 'mode-line "#F2804F")

             (set-face-background 'powerline-active1 "#586d75")
             (set-face-foreground 'powerline-active1 "#eee8d5")

             (set-face-background 'powerline-active2 "#657b83")
             (set-face-foreground 'powerline-active2 "#fdf6e3")

             (powerline-reset)))

(add-hook 'evil-insert-state-entry-hook
          '(lambda ()
             (set-face-background 'mode-line "#073642")
             (set-face-foreground 'mode-line "#69b7f0")

             (set-face-background 'powerline-active1 "#024A73")
             (set-face-foreground 'powerline-active1 "#839496")

             (set-face-background 'powerline-active2 "#00629D")
             (set-face-foreground 'powerline-active2 "#93a1a1")

             (powerline-reset)))


(set-face-background 'mode-line-inactive "#002b36")
(set-face-foreground 'mode-line-inactive "#586e75")
(set-face-background 'powerline-inactive1 "#073642")
(set-face-foreground 'powerline-inactive1 "#657b83")
(set-face-background 'powerline-inactive2 "#002b36")
(set-face-foreground 'powerline-inactive2 "#586e75")
(set-face-foreground 'mode-line-buffer-id "#a3afaf")


(provide 'evil-mode-line-color-dark)
;;; evil-mode-line-color ends here
