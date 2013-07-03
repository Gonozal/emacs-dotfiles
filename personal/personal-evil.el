(require 'evil)
(require 'evil-leader)

(load-file "~/.emacs.d/personal/evil/evil-ace-jump.el")
(load-file "~/.emacs.d/personal/evil/evil-user-settings.el")
(load-file "~/.emacs.d/personal/evil/evil-nerd-commenter.el")
(load-file "~/.emacs.d/personal/evil/evil-user-keymaps.el")
(load-file "~/.emacs.d/personal/evil/evil-surround.el")
(load-file "~/.emacs.d/personal/evil/evil-user-misc.el")
(load-file "~/.emacs.d/personal/evil/evil-mode-line-color.el")
(load-file "~/.emacs.d/personal/evil/evil-mode-line.el")

(define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

(evil-mode nil)
(global-evil-leader-mode 1)
(evil-mode 1)

;;; personal-evil.el ends here
