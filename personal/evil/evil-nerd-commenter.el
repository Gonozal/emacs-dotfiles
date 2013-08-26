;; Setup nerd commenter

(require 'evil)
(require 'evil-leader)
(evil-leader/set-key
  "c SPC" 'evilnc-comment-or-uncomment-lines
  )

(evil-define-key 'visual global-map (kbd ",c SPC") 'comment-or-uncomment-region)
