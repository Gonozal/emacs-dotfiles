(require 'evil)
(require 'evil-leader)

(evil-mode 1)

(setq evil-leader/leader "," evil-leader/in-all-states t)
(evil-leader/set-key
  "c SPC" 'evilnc-comment-or-uncomment-lines
  "cc" 'evilnc-comment-or-uncomment-to-the-line
  )

(evilnc-default-hotkeys)

(evil-leader/set-key
  "e" 'file-file
  "b" 'switch-to-buffer
  "d" 'kill-this-buffer
  "sv" 'split-window-horizontally
  "sh" 'split-window-vertically
  "k" 'kill-this-buffer)

(setq evil-shift-width 2)

;;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(provide 'personal-evil)
;;; personal-evil.el ends here
