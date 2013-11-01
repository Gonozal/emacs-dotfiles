(require 'powerline)

(add-hook 'evil-normal-state-entry-hook
          '(lambda ()
             (set-face-background 'mode-line "#b4c342")
             (set-face-foreground 'mode-line "#546e00")

             (set-face-background 'powerline-active1 "#eee8d5")
             (set-face-foreground 'powerline-active1 "#657b83")

             (set-face-background 'powerline-active2 "#fdf6e3")
             (set-face-foreground 'powerline-active2 "#839496")

             (powerline-reset)))

(add-hook 'evil-emacs-state-entry-hook
          '(lambda ()
             (set-face-background 'mode-line "#FA91EC")
             (set-face-foreground 'mode-line "#93115C")

             (set-face-background 'powerline-active1 "#eee8d5")
             (set-face-foreground 'powerline-active1 "#657b83")

             (set-face-background 'powerline-active2 "#fdf6e3")
             (set-face-foreground 'powerline-active2 "#839496")

             (powerline-reset)))

(add-hook 'evil-replace-state-entry-hook
          '(lambda ()
             (set-face-background 'mode-line "#FF6E64")
             (set-face-foreground 'mode-line "#990A1B")

             (set-face-background 'powerline-active1 "#eee8d5")
             (set-face-foreground 'powerline-active1 "#657b83")

             (set-face-background 'powerline-active2 "#fdf6e3")
             (set-face-foreground 'powerline-active2 "#839496")

             (powerline-reset)))

(add-hook 'evil-visual-state-entry-hook
          '(lambda ()
             (set-face-background 'mode-line "#F2804F")
             (set-face-foreground 'mode-line "#8B2C02")

             (set-face-background 'powerline-active1 "#eee8d5")
             (set-face-foreground 'powerline-active1 "#657b83")

             (set-face-background 'powerline-active2 "#fdf6e3")
             (set-face-foreground 'powerline-active2 "#839496")

             (powerline-reset)))

(add-hook 'evil-insert-state-entry-hook
          '(lambda ()
             (set-face-background 'mode-line "#073642")
             (set-face-foreground 'mode-line "#69b7f0")

             (set-face-background 'powerline-active1 "#00629d")
             (set-face-foreground 'powerline-active1 "#93a1a1")

             (set-face-background 'powerline-active2 "#69b7f0")
             (set-face-foreground 'powerline-active2 "#586e75")

             (powerline-reset)))

(set-face-background 'mode-line-inactive "#fdf6e3")
(set-face-foreground 'mode-line-inactive "#deb542")
(set-face-background 'powerline-inactive1 "#eee8d5")
(set-face-foreground 'powerline-inactive1 "#b58900")
(set-face-background 'powerline-inactive2 "#fdf6e3")
(set-face-foreground 'powerline-inactive2 "#deb542")

(provide 'evil-mode-line-color)
;;; evil-mode-line-color ends here
