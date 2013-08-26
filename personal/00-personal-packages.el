;;; package --- Summary

;;; Commentary:

;;; Code:
(add-to-list 'load-path "~/.emacs.d/personal-packages/fiplr")
(add-to-list 'load-path "/Users/arne/Development/Scala/ensime/src/main/elisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(prelude-ensure-module-deps
 '(
   ;; evil and plugins
   evil surround evil-numbers evil-nerd-commenter evil-leader
   ;; javascript
   js2-mode js2-refactor ac-js2 tern
   ;; grep etc
   ag wgrep wgrep-ag
   ;; visual
   diff-hl linum-relative rainbow-delimiters browse-kill-ring popwin
   pos-tip
   ;; misc language support
   slim-mode coffee-mode nginx-mode scala-mode2 rvm
   ;; editing
   move-text tagedit yasnippet smartparens auto-complete
   emmet-mode 
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
(require 'tern)
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
(require 'ensime)
(require 'yasnippet)
(require 'restclient)
(require 'popwin)
(require 'flycheck)
(require 'flycheck-color-mode-line)
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
(require 'saveplace)


;; Require custom defuns
(require 'setup-defuns)

;;;;;;;;;;;;;;;
;; Setup GUI ;;
;;;;;;;;;;;;;;;

;; Set Font Face
(set-face-attribute 'mode-line nil :box nil
                    :family "Source Code Pro for Powerline"
                    :height 100 :weight 'light)
(set-face-attribute 'default nil
                    :family "Source Code Pro for Powerline"
                    :height 100 :weight 'light)
(set-face-attribute 'mode-line-inactive nil :box nil)

(load-theme 'base16-default) ;; Load the best theme ever
(powerline-default-theme)    ;; load powerline
(setq scroll-margin 4)       ;; Sane cursor and window movements
(scroll-bar-mode -1)         ;; No scrollbars, thank you
(global-diff-hl-mode)        ;; show visual VCS diffs

(browse-kill-ring-default-keybindings) ;; load defult keybindings for killring browser
(ac-config-default)                    ;; load default autocomplete config
(global-linum-mode)                    ;; line numbering everywhere
(yas-global-mode 1)                    ;; use snippets everywhere
(popwin-mode 1)                        ;; use popup windows instead of idle windows

;; silence emacs, damnit
(setq ring-bell-function 'ignore)

;; restore windows
(unless noninteractive
  (add-hook 'kill-emacs-hook 'stante-save-frame-parameters)
  (add-hook 'after-init-hook 'stante-restore-frame-parameters))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac customizations ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; map function key to hyper
(setq ns-function-modifier 'hyper)

;; set correct shell path on mac os
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace and tab behavior ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face empty trailing lines-tail))

;;;; Tab settings ;;;;
; Tab width is 2
(setq tab-width 2)

; Tab width is 2 by default..
(setq-default tab-width 2)

; Use spaces always.
(setq indent-tabs-mode nil)

; Jump by 2.
(setq c-basic-offset 2)

; this defaulted to 4 and had to be reset to 2.
(setq perl-indent-level 2)

;Tab stop list out to col 52
;Manually set by x2
(setq tab-stop-list
      '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52))

(custom-set-variables
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
)

(setq js-indent-level 2)
(setq-default js2-basic-offset 2)
(setq js2-basic-offset 2)


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

;; ido vertical customization
(setq ido-decorations
      (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]"
              " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

;; load flycheck
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;; projectile
(projectile-global-mode)
(global-set-key (kbd "C-c h") 'helm-projectile)

;; Fiplr
(setq fiplr-root-markers '(".git"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diminish modeline names ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(rename-modeline "js2-mode" js2-mode "JS2")

(diminish 'emmet-mode "")
(diminish 'flyspell-mode " ✓ ")
(diminish 'whitespace-mode " ☐ ")
(diminish 'undo-tree-mode " ⤺ ")
(diminish 'projectile-mode "")
(diminish 'tagedit-mode "<>")

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

;; html, xml etc
(tagedit-add-paredit-like-keybindings)
(tagedit-add-experimental-features)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript IDE configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ac-js2-evaluate-calls t)

(setq httpd-port 9090)

(setq js2-mirror-mode nil)

(setq-default js2-global-externs
              '("module" "require" "buster" "sinon" "assert" "refute"
                "setTimeout" "clearTimeout" "setInterval" "clearInterval"
                "location" "__dirname" "console" "JSON", "confirm"))

(setq-default js2-auto-indent-p t)

(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

;; JS refactoring
(js2r-add-keybindings-with-prefix "C-c C-m")

;; Use lambda for anonymous functions
(font-lock-add-keywords
 'js2-mode `(("\\(function\\) *("
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u0192")
                        nil)))))

;; Use right arrow for return in one-line functions
(font-lock-add-keywords
 'js2-mode `(("function *([^)]*) *{ *\\(return\\) "
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u2190")
                        nil)))))

(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))

(eval-after-load 'auto-complete
  '(eval-after-load 'tern
     '(progn
        (require 'tern-auto-complete)
        (tern-ac-setup))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File to language mappings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist  (cons '("Gemfile$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("Gemfile.lock$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("Rakefile$" . ruby-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))


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

(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;;;;;;;;;;;;;;;;;
;; Setup hooks ;;
;;;;;;;;;;;;;;;;;

;; ido vertical
(add-hook 'ido-setup-hook 'ido-define-keys)
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

;; activate ensime in scala files
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; disabled linum mode in org-mode
(add-hook 'org-mode-hook (lambda () (linenum-mode 0)))

;; stop ace-jump mode from going into insert mode
(add-hook 'ace-jump-mode-end-hook 'exit-recursive-edit)

;; load emmet when in sgml (xml, html etc) mode
(add-hook 'sgml-mode-hook 'emmet-mode)
;; Set emmet indentation to 2 spaces
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

;; laod tagedit in html modes
(add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))

;; Fix auto-complete when flyspell is active
(add-hook 'flyspell-mode-hook
          (lambda ()
            (ac-flyspell-workaround)))

;; activate tern in JS mode
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; activate flycheck ind js2-mode
(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))


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

;; More intuitive ido key bindings now that ido is vertical
(define-key ido-completion-map (kbd "C-n") 'ido-next-match)
(define-key ido-completion-map (kbd "C-p") 'ido-prev-match)

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

;;;;;;;;;;;;;;;;;;;;;;
;; Evil ex-commands ;;
;;;;;;;;;;;;;;;;;;;;;;
(evil-ex-define-cmd "Rename" 'rename-current-buffer-file)
(evil-ex-define-cmd "Remove" 'delete-current-buffer-file)

(provide '00-personal-packages)
;;; 00-personal-packages ends here
