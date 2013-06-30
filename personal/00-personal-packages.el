(add-to-list 'load-path "~/.emacs.d/personal-packages/ensime")

(prelude-ensure-module-deps
  '(
    evil
    surround
    evil-numbers
    evil-nerd-commenter
    evil-leader
    ruby-end
    simp
    rvm
    flycheck-color-mode-line
    indent-guide
    js2-mode
    js2-refactor
    diff-hl
    projectile
    linum-relative
    rainbow-delimiters
    helm-ag
    exec-path-from-shell
    move-text
    tagedit
    browse-kill-ring
    auto-complete
    scala-mode2
    buffer-move
    ac-js2
    smartparens
    emmet-mode
    )
  )


(require 'rvm)
(require 'evil)

(require 'evil-numbers)
(require 'evil-leader)
(require 'ruby-end)
(require 'simp)
(require 'powerline)
(require 'indent-guide)
(require 'js2-mode)
(require 'js2-refactor)
(require 'diff-hl)
(require 'projectile)
(require 'linum-relative)
(require 'rainbow-delimiters)
(require 'helm-ag)
(require 'exec-path-from-shell)
(require 'move-text)
(require 'tagedit)
(require 'browse-kill-ring)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'scala-mode2)
(require 'buffer-move)
(require 'ac-js2)
(require 'ensime)
(require 'smartparens)
(require 'smartparens-config)
(require 'emmet-mode)

(rvm-use-default)
(powerline-default-theme)
;; (indent-guide-global-mode)
(global-diff-hl-mode)

(move-text-default-bindings)
(global-pretty-mode 1)
(browse-kill-ring-default-keybindings)
(ac-config-default)
(global-linum-mode)


(provide 'personal-packages)
