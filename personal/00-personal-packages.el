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
(require 'surround)
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

;; Midnight-like buffer killing
(add-to-list 'clean-buffer-list-kill-buffer-names
             '("*Packages*"
               "*Completions*"
               "*magit"
               "*Backtrace*"))
(setq clean-buffer-list-delay-special 0)
(run-with-idle-timer 300 t 'clean-buffer-list)

;; Setup undo-tree
(global-undo-tree-mode 1)
(setq undo-tree-auto-save-history t)


;;;;;;;;;;;;;;;;
;; Setup evil ;;
;;;;;;;;;;;;;;;;

;; General setup
(evil-mode nil)
(global-evil-leader-mode 1)
(evil-mode 1)
(setq evil-shift-width 2)

;; surround plugin
(global-surround-mode 1)
(surround-mode 1)

;; evil leader
(evil-leader/set-leader ",")

;; Make escape really escape everything
;; Clear insert state bindings.
(setcdr evil-insert-state-map nil)
;; Don't wait for any other keys after escape is pressed.
(setq evil-esc-delay 0)

;; Make sure escape gets back to normal state and quits things.
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(define-key evil-visual-state-map [escape] 'evil-normal-state)
(define-key evil-emacs-state-map [escape] 'evil-normal-state)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backups and auto save ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq backup-by-copying t    ; Don't delink hardlinks
    backup-directory-alist '(("." . "~/.emacs.d/temps/backups"))
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5    ; and how many of the old

    auto-save-file-name-transforms `((".*" ,"~/.emacs.d/temps/autosaves" t))
    undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/temps/undotrees")))
    )

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

;; Typo rebindings and general shortcuts
(evil-ex-define-cmd "W" 'evil-write)
(evil-ex-define-cmd "b" 'switch-to-previous-buffer)

;; keybindings that are in vim but not evil?
(define-key evil-visual-state-map "O" 'exchange-point-and-mark)
(define-key evil-visual-state-map "o" 'evil-visual-exchange-corners)
;; make ctrl-u scroll up as is in vim
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
;; J joins lines
(define-key evil-normal-state-map "J" 'evil-join-unfill)
(define-key evil-visual-state-map "J" 'evil-join-unfill)

;; custom evil motions / operators
(define-key evil-operator-state-map (kbd "lw") 'evil-little-word)

;; destroy (do not yank) chars with x
(define-key evil-normal-state-map "x" 'evil-destroy-char)
(define-key evil-visual-state-map "x" 'evil-destroy-char)

;;replace without yanking with ü
(define-key evil-normal-state-map "ü" 'evil-destroy-replace)
(define-key evil-visual-state-map "ü" 'evil-destroy-replace)

;; Bring back narrowing. And while we are a it, guess the syntax of new region
(define-key evil-normal-state-map "m" 'evil-narrow-indirect)
(define-key evil-visual-state-map "m" 'evil-narrow-indirect)

;; number incrementing and decrementing
(define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)
(define-key evil-visual-state-map "+" 'inc-num-region) ;; Increment rows of numbers ascendin

;; nerd commenter
(evil-leader/set-key "c SPC" 'evilnc-comment-or-uncomment-lines )
(evil-define-key 'visual global-map (kbd ",c SPC") 'comment-or-uncomment-region)

;; ace-jump-mode
(define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode)
(define-key evil-motion-state-map (kbd "C-SPC") #'evil-ace-jump-word-mode)

(define-key evil-operator-state-map (kbd "SPC") #'evil-ace-jump-char-mode)      ; similar to f
(define-key evil-operator-state-map (kbd "C-SPC") #'evil-ace-jump-char-to-mode) ; similar to t
(define-key evil-operator-state-map (kbd "M-SPC") #'evil-ace-jump-word-mode)

(evil-leader/set-key
  "f"  'evil-ace-jump-char-mode
  "w"  'evil-ace-jump-word-mode)

(defadvice evil-visual-char (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

(defadvice evil-visual-block (before spc-for-char-jump activate)
  (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

;; YAS bindings
(evil-leader/set-key "SPC" (lambda ()
                             (interactive)
                             (evil-insert-state)
                             (yas-insert-snippet))

;; Window management
(define-key evil-normal-state-map (kbd "H-k") 'buf-move-up)
(define-key evil-normal-state-map (kbd "H-j") 'buf-move-down)
(define-key evil-normal-state-map (kbd "H-h") 'buf-move-left)
(define-key evil-normal-state-map (kbd "H-l") 'buf-move-right)

;; Magit
(evil-leader/set-key
  "gs"  'magit-status
  "gh"  'magit-file-log
  "gb"  'magit-blame-mode
  "gg"  'vc-git-grep)

;; move to next/prev git item with j/k in Magit mode
(evil-add-hjkl-bindings magit-status-mode-map 'emacs
  "K" 'magit-discard-item
  "l" 'magit-key-mode-popup-logging
  "h" 'magit-toggle-diff-refine-hunk)

;; Move-text keybindings (move text or region up/down)
(define-key evil-normal-state-map (kbd "M-k") 'move-text-up)
(define-key evil-normal-state-map (kbd "M-j") 'move-text-down)
(define-key evil-visual-state-map (kbd "M-k") 'move-text-up)
(define-key evil-visual-state-map (kbd "M-j") 'move-text-down)

; Case conversion
(evil-leader/set-key
  "crm" 'mixedcase-word-at-point
  "crc" 'camelcase-word-at-point
  "crs" 'underscore-word-at-point
  "cru" 'uppercase-word-at-point
  "crd" 'dasherize-word-at-point
  "cr:" 'colonize-word-at-point)

(global-set-key (kbd "C-c f") 'simp-project-find-file)
(global-set-key (kbd "M-t") 'fiplr-find-file)

;; default mac shortcuts to save file and close window
(define-key evil-normal-state-map (kbd "M-s") 'save-buffer)
(define-key evil-insert-state-map (kbd "M-s") 'save-buffer)
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

;; window management
(evil-leader/set-key
  "bl"  'switch-to-buffer
  "bd"  'evil-delete-buffer
  "d"   'kill-this-buffer
  "k"   'kill-this-buffer)

;; Misc Keybindings
(evil-leader/set-key
  "sv" 'split-window-horizontally
  "sh" 'split-window-vertically
  "v"  'find-user-init-file
  "t"  'align-regexp
  "u"  'undo-tree-visualize)

;; Autoindent on newline
(global-set-key (kbd "RET") 'newline-and-indent)

(provide '00-personal-packages)
;;; 00-personal-packages ends here
