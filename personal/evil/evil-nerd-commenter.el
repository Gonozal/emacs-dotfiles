;; Setup nerd commenter
(setq evil-leader/leader "," evil-leader/in-all-states t)
(evil-leader/set-key
  "c SPC" 'evilnc-comment-or-uncomment-lines
  "cc" 'evilnc-comment-or-uncomment-to-the-line
  )
