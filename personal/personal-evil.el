(require 'evil)
(require 'evil-leader)

(load-file "~/.emacs.d/personal/evil/evil-user-settings.el")
(load-file "~/.emacs.d/personal/evil/evil-nerd-commenter.el")
(load-file "~/.emacs.d/personal/evil/evil-user-keymaps.el")
(load-file "~/.emacs.d/personal/evil/evil-surround.el")
(load-file "~/.emacs.d/personal/evil/evil-user-misc.el")

(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

(evil-mode nil)
(global-evil-leader-mode 1)
(evil-mode 1)

(provide 'personal-evil)
;;; personal-evil.el ends here
