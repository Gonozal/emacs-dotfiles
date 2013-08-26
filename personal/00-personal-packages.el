(add-to-list 'load-path "~/.emacs.d/personal-packages/fiplr")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(prelude-ensure-module-deps
 '(
   ;; evil and plugins
   evil surround evil-numbers evil-nerd-commenter evil-leader
   ;; javascript
   js2-mode js2-refactor ac-js2 emmet-mode
   ;; grep etc
   ag wgrep wgrep-ag
   ;; visual
   diff-hl linum-relative rainbow-delimiters browse-kill-ring popwin
   pos-tip
   ;; misc language support
   slim-mode coffee-mode nginx-mode scala-mode2 rvm
   ;; editing
   move-text tagedit yasnippet smartparens auto-complete
   flycheck-color-mode-line
   exec-path-from-shell
   buffer-move
   restclient
   grizzl
   midnight
   )
 )


(require 'rvm)
(require 'evil)
(require 'evil-numbers)
(require 'evil-leader)
(require 'powerline)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'js2-mode)
(require 'js2-refactor)
(require 'ac-js2)
(require 'emmet-mode)
(require 'diff-hl)
(require 'ag)
(require 'wgrep-ag)
(require 'linum-relative)
(require 'smartparens)
(require 'smartparens-config)
(require 'rainbow-delimiters)
(require 'exec-path-from-shell)
(require 'move-text)
(require 'browse-kill-ring)
(require 'scala-mode2)
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


;; Require custom defuns
(require 'setup-gui)
(require 'setup-defuns)

;;;;;;;;;;;;;;;
;; Setup GUI ;;
;;;;;;;;;;;;;;;

(browse-kill-ring-default-keybindings) ;; load defult keybindings for killring browser
(ac-config-default)                    ;; load default autocomplete config
(global-linum-mode)                    ;; line numbering everywhere
(yas-global-mode 1)                    ;; use snippets everywhere
(popwin-mode 1)                        ;; use popup windows instead of idle windows

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic plugins setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Setup evil
(evil-mode nil)
(global-evil-leader-mode 1)
(evil-mode 1)
(setq evil-shift-width 2)

;; Midnight-like buffer killing
(add-to-list 'clean-buffer-list-kill-buffer-names
             '("*Packages*"
               "*Completions*"
               "*magit"
               "*Backtrace*"))
(setq clean-buffer-list-delay-special 0)
(run-with-idle-timer 300 t 'clean-buffer-list)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocomplete setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(global-auto-complete-mode t)
(setq ac-expand-on-auto-complete nil)
(setq ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed

;; extra modes auto-complete must support
(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode
                js2-mode css-mode less-css-mode coffee-mode scss-mode
                slim-mode))
  (add-to-list 'ac-modes mode))

(set-default 'ac-sources
             '(ac-source-abbrev
               ac-source-dictionary
               ac-source-yasnippet
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic))

(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)


(setq ac-delay 0.05)
(setq ac-auto-show-menu 0.1)
(setq ac-use-fuzzy 1)
(ac-config-default)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language customizations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ruby
(rvm-use-default)

;;;;;;;;;;;;;;;;;
;; Setup hooks ;;
;;;;;;;;;;;;;;;;;

;; disabled linum mode in org-mode
(add-hook 'org-mode-hook (lambda () (linenum-mode 0)))

;; stop ace-jump mode from going into insert mode
(add-hook 'ace-jump-mode-end-hook 'exit-recursive-edit)

;; load emmet when in sgml (xml, html etc) mode
(add-hook 'sgml-mode-hook 'emmet-mode)
;; Set emmet indentation to 2 spaces
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

;; Fix auto-complete when flyspell is active
(add-hook 'flyspell-mode-hook
          (lambda ()
            (ac-flyspell-workaround)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom keybindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)

;; Move-text keybindings (move text or region up/down)
(global-set-key [M-k] 'move-text-up)
(global-set-key [M-j] 'move-text-down)

(global-set-key (kbd "C-c f") 'simp-project-find-file)
(global-set-key (kbd "M-t") 'fiplr-find-file)

;; default mac shortcuts to save file and close window
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-w") 'quit-window)

(global-set-key (kbd "M-RET") 'toggle-fullscreen)

;; Autocomplete
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(define-key ac-completing-map (kbd "C-g") 'ac-stop)
(define-key ac-completing-map (kbd "ESC") 'evil-normal-state)
(evil-make-intercept-map ac-completing-map)

(ac-set-trigger-key "TAB") ; AFTER input prefix, press TAB key ASAP

;; Autoindent on newline
(global-set-key (kbd "RET") 'newline-and-indent)

(provide '00-personal-packages)
;;; 00-personal-packages ends here
