(add-to-list 'load-path "~/.emacs.d/personal-packages/ensime")
(add-to-list 'load-path "~/.emacs.d/personal-packages/fiplr")

(prelude-ensure-module-deps
 '(
   evil
   surround
   evil-numbers
   evil-nerd-commenter
   evil-leader
   ruby-end
   rvm
   flycheck-color-mode-line
   indent-guide
   js2-mode
   js2-refactor
   diff-hl
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
   yasnippet
   restclient
   grizzl
   slim-mode
   coffee-mode
   nginx-mode
   popwin
   midnight
   pos-tip
   )
 )


(require 'rvm)
(require 'evil)

(require 'evil-numbers)
(require 'evil-leader)
(require 'ruby-end)
(require 'powerline)
(require 'indent-guide)
(require 'js2-mode)
(require 'js2-refactor)
(require 'diff-hl)
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
(require 'yasnippet)
(require 'restclient)
(require 'popwin)
(require 'midnight)


;; require prelude packages

;; (require 'prelude-c)
;; (require 'prelude-clojure)
;; (require 'prelude-coffee)
;; (require 'prelude-common-lisp)
(require 'prelude-css)
(require 'prelude-emacs-lisp)
;; (require 'prelude-erc)
;; (require 'prelude-erlang)
;; (require 'prelude-haskell)
(require 'prelude-js)
(require 'prelude-latex)
(require 'prelude-lisp)
(require 'prelude-markdown)
;; (require 'prelude-mediawiki)
(require 'prelude-org)
;; (require 'prelude-perl)
(require 'prelude-python)
(require 'prelude-ruby)
(require 'prelude-scala)
(require 'prelude-scheme)
(require 'prelude-scss)
(require 'prelude-xml)


(rvm-use-default)
(powerline-default-theme)
(set-face-attribute 'mode-line nil :box nil :family "Source Code Pro for Powerline" :height 100 :weight 'light)
(set-face-attribute 'mode-line-inactive nil :box nil)

;; (indent-guide-global-mode)
(global-diff-hl-mode)

(move-text-default-bindings)
(browse-kill-ring-default-keybindings)
(ac-config-default)
(global-linum-mode)
(yas-global-mode 1)
(popwin-mode 1)
; Clean up buffers
(add-to-list 'clean-buffer-list-kill-buffer-names
             '("*Packages*"
               "*Completions*"
               "*magit"
               "*Backtrace*"))
(setq clean-buffer-list-delay-special 0)
(run-with-idle-timer 300 t 'clean-buffer-list)


;; Develop and keep personal snippets under ~/emacs.d/mysnippets
(setq yas/root-directory "~/.emacs.d/mysnippets")
(yas/load-directory yas/root-directory)

(provide 'personal-packages)
