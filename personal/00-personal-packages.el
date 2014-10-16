;;; package --- Summary

;;; Commentary:

;;; Code:
(server-start)

(add-to-list 'load-path "~/.emacs.d/personal-packages/powerline/")
(add-to-list 'load-path "~/.emacs.d/personal-packages/flycheck-hdevtools/")
(add-to-list 'load-path "~/.emacs.d/personal-packages/fuzzy-el/")
(add-to-list 'load-path "~/.emacs.d/personal-packages/popup-el/")
(add-to-list 'load-path "~/Development/Scala/ensime/dist/elisp/")
(add-to-list 'load-path "~/.emacs.d/personal-packages/snort-mode/")
(add-to-list 'load-path "~/.emacs.d/personal/evil/")
(add-to-list 'load-path "~/.emacs.d/personal/languages/")
(add-to-list 'load-path "~/.emacs.d/personal/misc/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/personal/")

(add-to-list 'exec-path "~/.cabal/bin")

;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(prelude-require-packages
 '(
   ;; evil and plugins
   evil surround evil-numbers evil-nerd-commenter evil-leader evil-paredit
   evil-exchange
   ;; grep etc
   ag wgrep wgrep-ag
   ;; visual
   diff-hl linum-relative rainbow-delimiters browse-kill-ring popwin
   highlight-indentation
   pos-tip
   ;; misc language support
   slim-mode coffee-mode nginx-mode scala-mode2
   ;; qt quick qml
   qml-mode
   ;; html
   evil-matchit evil-visualstar
   ;; editing
   move-text tagedit yasnippet smartparens company
   emmet-mode dash-at-point
   ;; flycheck-color-mode-line
   exec-path-from-shell
   buffer-move
   restclient
   grizzl
   flx-ido
   midnight))

(require 'evil)
(require 'evil-numbers)
(require 'evil-visualstar)
(require 'evil-leader)
(require 'evil-matchit)
(require 'evil-mode-line-color)
(require 'powerline)
(require 'popup)
(require 'fuzzy)

(require 'ag)
(require 'wgrep-ag)
(require 'move-text)
; (require 'solarized)
(require 'browse-kill-ring)
(require 'restclient)
(require 'multiple-cursors)
(require 'thingatpt+)
(require 'yasnippet)
(require 'popwin)

(require 'diff-hl)
(require 'linum-relative)
(require 'linum-off)
(require 'smartparens)
(require 'smartparens-config)
(require 'rainbow-delimiters)
(require 'flycheck)
(require 'midnight)
(require 'snort-mode)

(require 'company)

;; Require custom defuns
(require 'setup-defuns)

;; load language customizations
(require 'custom-ruby)
(require 'custom-haskell)
(require 'custom-elm)
(require 'custom-javascript)
(require 'custom-latex)

;; save buffer positions
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; GC tuninG
(setq gc-cons-threshold 50000000)

;;;;;;;;;;;;;;;
;; Setup GUI ;;
;;;;;;;;;;;;;;;

;; Set Font Face
(set-face-attribute 'mode-line nil :box nil
                    :family "Source Code Pro for Powerline"
                    :height 100
                    ;; :weight 'normal
                    )
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'default nil
                    :family "Source Code Pro for Powerline"
                    :height 100
                    ;; :weight 'normal
                    )

(disable-theme 'molokai)
(disable-theme 'zenburn)
;; powerline solarized customization
(powerline-default-theme)        ; load powerline
;; (setq solarized-distinct-fringe-background t)
;; (setq solarized-use-less-bold t)
(load-theme 'solarized-light t)                     ;; Load the best theme ever
(setq scroll-margin 4)                              ;; Sane cursor and window movements
(scroll-bar-mode -1)                                ;; No scrollbars, thank you
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil)            ;; don't accelerate scrolling
(global-diff-hl-mode)                               ;; show visual VCS diffs
(global-hl-line-mode -1)                            ;; do not highlight current line
;; (global-rainbow-delimiters-mode)

(browse-kill-ring-default-keybindings) ; load defult keybindings for killring browser
(global-linum-mode)                    ; line numbering everywhere
(yas-global-mode 1)                    ; use snippets everywhere
;; (popwin-mode 1)                        ; use popup windows instead of idle windows

;; save buffer configurations
(desktop-save-mode 1)


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
(setq ns-function-modifier 'hyper
      ns-use-native-fullscreen nil
      mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace and tab behavior ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq whitespace-line-column 80) ;; limit line length
(setq-default fill-column 80) ;; limit line length
(setq whitespace-style '(face empty trailing lines-tail))

;;;; Tab settings ;;;;
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq tab-width 2)
(setq indent-tabs-mode nil)

(setq tab-Stop-list
      '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic plugins setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; always use smartparens
(smartparens-global-mode t)

;; use ag coloring
(setq ag-highlight-search t)

;; Disable flyspell in every buffer
(setq prelude-flyspell nil)

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
;; enable ido vertical everywhere
(add-hook 'ido-setup-hook 'ido-define-keys)
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
;; More intuitive ido key bindings now that ido is vertical
;; (ac-set-trigger-key "TAB") ; AFTER input prefix, press TAB key ASAP

;; projectile
(projectile-global-mode)

;; Dash at point
(evil-leader/set-key
  "d" 'dash-at-point
  )

;;;;;;;;;;;;;;;;
;; Setup evil ;;
;;;;;;;;;;;;;;;;

;; were using evil, no need for keychoards!
(key-chord-mode 0)

;; General setup
(setq evil-default-state 'normal
      evil-esc-delay 0
      evil-shift-width 2)
(evil-mode nil)
(global-evil-leader-mode 1)
(global-evil-matchit-mode)
(evil-mode 1)

;; surround plugin
(global-surround-mode 1)
(surround-mode 1)

;; evil leader
(evil-leader/set-leader "<SPC>")

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

;; enter emacs state in certain modes
(loop for (mode . state) in
      '(
        (Info-mode . emacs)
        (term-mode . emacs)
        ;; (log-edit-mode . emacs)
        (inf-ruby-mode . emacs)
        ;; (yari-mode . emacs)
        (erc-mode . emacs)
        (gud-mode . emacs)
        (help-mode . emacs)
        (eshell-mode . emacs)
        (shell-mode . emacs)
        ;;(message-mode . emacs)
        ;; (magit-log-edit-mode . emacs)
        (fundamental-mode . emacs)
        (gtags-select-mode . emacs)
        (weibo-timeline-mode . emacs)
        (weibo-post-mode . emacs)
        (diff-mode . emacs)
        (sr-mode . emacs)
        (dired-mode . emacs)
        (compilation-mode . emacs)
        (speedbar-mode . emacs)
        ;; (magit-commit-mode . normal)
        )
      do (evil-set-initial-state mode state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diminish modeline names ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(diminish 'flyspell-mode "")
(diminish 'whitespace-mode "")
(diminish 'undo-tree-mode "")
(diminish 'projectile-mode "")
(diminish 'flycheck-mode " ✓ ")
;; (diminish 'guru-mode "")
(diminish 'company-mode " ⋯ ")

;;;;;;;;;;;;;;;;
;; Helm setup ;;
;;;;;;;;;;;;;;;;
;; (require 'helm-config)
;; (require 'helm-eshell)
;; (require 'helm-files)
;; (require 'helm-grep)

;; (setq
;;  helm-google-suggest-use-curl-p t
;;  helm-scroll-amount 4                  ; scroll 4 lines other window using M-<next>/M-<prior>
;;  helm-quick-update t                   ; do not display invisible candidates
;;  helm-idle-delay 0.01                  ; be idle for this many seconds, before updating in delayed sources.
;;  helm-input-idle-delay 0.01            ; be idle for this many seconds, before updating candidate buffer
;;  helm-ff-search-library-in-sexp t      ; search for library in `require' and `declare-function' sexp.

;;  helm-split-window-default-side 'other ;; open helm buffer in another window
;;  helm-split-window-in-side-p t         ;; open helm buffer inside current window, not occupy whole other window
;;  helm-buffers-favorite-modes (append helm-buffers-favorite-modes
;;                                      '(picture-mode artist-mode))
;;  helm-candidate-number-limit 200       ; limit the number of displayed canidates
;;  helm-M-x-requires-pattern 0           ; show all candidates when set to 0
;;  helm-ff-file-name-history-use-recentf t
;;  helm-move-to-line-cycle-in-source t   ; move to end or beginning of source
;;                                        ; when reaching top or bottom of source.
;;  ido-use-virtual-buffers t             ; Needed in helm-buffers-list
;;  helm-buffers-fuzzy-matching t         ; fuzzy matching buffer names when non--nil
;;                                        ; useful in helm-mini that lists buffers
;;  )

;; (helm-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-auto-complete-mode t)
;; (setq ac-use-menu-map t)
;; (setq ac-expand-on-auto-complete nil)
;; (setq ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed
;; (setq ac-delay 0.05)
;; (setq ac-auto-show-menu 0.1)
;; (setq ac-use-fuzzy 1)
;; (setq ac-quick-help-delay 1)
;; (setq ac-quick-help-height 60)

;; (ac-set-trigger-key "TAB")
;; (ac-set-trigger-key "<tab>")
;; (define-key ac-completing-map "\t" 'ac-complete)

;; ;; extra modes auto-complete must support
;; (dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
;;                                     sass-mode yaml-mode csv-mode espresso-mode haskell-mode
;;                                     html-mode nxml-mode sh-mode smarty-mode clojure-mode
;;                                     lisp-mode textile-mode markdown-mode tuareg-mode
;;                                     js2-mode css-mode less-css-mode coffee-mode scss-mode
;;                                     slim-mode))
;;   (add-to-list 'ac-modes mode))

;; (set-default 'ac-sources
;;              '(ac-source-abbrev
;;                ac-source-yasnippet
;;                ac-source-dictionary
;;                ac-source-words-in-buffer
;;                ac-source-words-in-same-mode-buffers
;;                ac-source-semantic))

;; (ac-config-default)

;; (setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;
;; Company mode setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(global-company-mode 1)
(setq company-idle-delay 0.15)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
;; invert the navigation direction if the the completion popup-isearch-match
;; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t)

(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Cursors Evil hack ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see https://github.com/magnars/multiple-cursors.el/issues/19
(defvar my-mc-evil-previous-state nil)

(defun my-mc-evil-switch-to-insert-state ()
  (when (and (bound-and-true-p evil-mode)
             (not (memq evil-state '(insert emacs))))
    (setq my-mc-evil-previous-state evil-state)
    (evil-insert 1)))

(defun my-mc-evil-back-to-previous-state ()
  (when my-mc-evil-previous-state
    (unwind-protect
        (case my-mc-evil-previous-state
          ((normal visual) (evil-force-normal-state))
          (t (message "Don't know how to handle previous state: %S"
                      my-mc-evil-previous-state)))
      (setq my-mc-evil-previous-state nil))))

(add-hook 'multiple-cursors-mode-enabled-hook
          'my-mc-evil-switch-to-insert-state)
(add-hook 'multiple-cursors-mode-disabled-hook
          'my-mc-evil-back-to-previous-state)

(defun my-rrm-evil-switch-state ()
  (if rectangular-region-mode
      (my-mc-evil-switch-to-insert-state)
    ;; (my-mc-evil-back-to-previous-state)  ; does not work...
    (setq my-mc-evil-previous-state nil)))

(add-hook 'rectangular-region-mode-hook 'my-rrm-evil-switch-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language customizations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Golang
(when (not (getenv "GOPATH"))
  (setenv "GOPATH" "/Users/arne/go"))

;; html, xml etc

(eval-after-load "sgml-mode"
  '(progn
     (require 'emmet-mode)
     (require 'tagedit)
     (diminish 'tagedit-mode "<>")
     (diminish 'emmet-mode "")
     (tagedit-add-paredit-like-keybindings)
     (tagedit-add-experimental-features)
     )
  )

;;;;;;;;;;;;;;;;;;;
;; markdown mode ;;
;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.text\\'" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . gfm-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scala IDE configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "scala-mode"
  '(progn
     (require 'scala-mode2)
     (require 'ensime)
     (push "*Inspector*" popwin:special-display-config)
     (push "*ENSIME-Compilation-Result*" popwin:special-display-config)
     (evil-leader/set-key-for-mode 'scala-mode
       "hi" 'ensime-inspect-type-at-point
       "ht" 'ensime-typecheck-all
       "hr" 'ensime-refactor-rename
       )
     )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General editing options ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(electric-indent-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backups and auto save ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq backup-by-copying t    ; Don't delink hardlinks
      backup-directory-alist '(("." . "~/.emacs.d/temps/backups/"))
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old

      auto-save-file-name-transforms `((".*" ,"~/.emacs.d/temps/autosaves/" t))
      undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/temps/undotrees/")))
      )

(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;;;;;;;;;;;;;;;;;
;; Setup hooks ;;
;;;;;;;;;;;;;;;;;

;; python jedi completion
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; return to normal mode after save
(add-hook 'after-save-hook 'evil-normal-state)

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; multiple cursors
(add-hook 'multiple-cursors-mode-enabled-hook
          'my-mc-evil-switch-to-insert-state)

(add-hook 'multiple-cursors-mode-disabled-hook
          'my-mc-evil-back-to-previous-state)

(add-hook 'rectangular-region-mode-hook 'my-rrm-evil-switch-state)

;; Show indentation guides in languages with semantic indentation
(add-hook 'slim-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'coffee-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'python-mode-hoocoffee-mode-hook 'highlight-indentation-current-column-mode)

;;; smartparens messes with multiple cursors, so disable it
(add-hook 'multiple-cursors-mode-enabled-hook (lambda ()
                                                (smartparens-mode -1)))
(add-hook 'multiple-cursors-mode-disabled-hook (lambda ()
                                                (smartparens-mode t)))

;; activate ensime in scala files
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook (lambda ()
                             (setq tab-width 4)
                             (defvaralias 'c-basic-offset 'tab-width)
                             (defvaralias 'cperl-indent-level 'tab-width)))

;; disabled linum mode in org-mode
;; (add-hook 'org-mode-hook (lambda () (linenum-mode 0)))

;; add a hook so we can display images on load
(add-hook 'org-mode-hook '(lambda () (org-turn-on-iimage-in-org)))

;; stop ace-jump mode from going into insert mode
(add-hook 'ace-jump-mode-end-hook 'exit-recursive-edit)

;; load emmet when in sgml (xml, html etc) mode
(add-hook 'sgml-mode-hook 'emmet-mode)
;; Set emmet indentation to 2 spaces
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

;; laod tagedit in html modes
(add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))


;; outline mode
(add-hook 'outline-minor-mode-hook
          (lambda ()
            (require 'outline-magic)
            (evil-define-key 'normal outline-minor-mode-map (kbd "TAB") 'outline-cycle)))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom keybindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; mac like backspace key
(define-key evil-insert-state-map (kbd "M-DEL") 'backward-kill-line)
(define-key evil-insert-state-map (kbd "C-DEL") 'backward-kill-word)
(define-key evil-insert-state-map (kbd "<C-backspace>") 'backward-kill-word)

;; Typo rebindings and general shortcuts
(evil-ex-define-cmd "W" 'evil-write)
(evil-ex-define-cmd "b" 'switch-to-previous-buffer)
(evil-ex-define-cmd "B" 'switch-to-previous-buffer)

;; make C-g also cancel insert state
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-visual-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-replace-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-operator-state-map (kbd "C-g") 'evil-normal-state)

;; keybindings that are in vim but not evil?
(define-key evil-visual-state-map "L" 'exchange-point-and-mark)
(define-key evil-visual-state-map "l" 'evil-visual-exchange-corners)
;; make ctrl-u scroll up as is in vim
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
;; J joins lines
;; (define-key evil-normal-state-map "J" 'evil-join-unfill)
;; (define-key evil-visual-state-map "J" 'evil-join-unfill)

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
(define-key evil-normal-state-map (kbd "C-A")
  'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-S-A")
  'evil-numbers/dec-at-pt)
(define-key evil-visual-state-map (kbd "C-A") 'inc-num-region) ;; Increment rows of numbers ascending

;; nerd commenter
(evil-leader/set-key "c SPC" 'evilnc-comment-or-uncomment-lines )
(evil-define-key 'visual global-map (kbd "SPC c SPC") 'comment-or-uncomment-region)

;; Flycheck
(evil-leader/set-key "e" 'nil )
(evil-leader/set-key "e r" 'flycheck-first-error )


;; ace-jump-mode
(evil-leader/set-key
  "f"  'evil-ace-jump-char-mode
  "t"  'evil-ace-jump-char-to-mode
  "w"  'evil-ace-jump-word-mode)

;; (defadvice evil-visual-char (before spc-for-char-jump activate)
;;   (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

;; (defadvice evil-visual-block (before spc-for-char-jump activate)
;;   (define-key evil-motion-state-map (kbd "SPC") #'evil-ace-jump-char-mode))

;; multiple cursors
(evil-leader/set-key
  "mf" 'mc/mark-all-like-this-in-defun
  "mw" 'mc/mark-all-words-like-this-in-defun
  "ms" 'mc/mark-all-symbols-like-this-in-defun
  "mn" 'mc/mark-next-like-this
  "ma" 'mc/mark-all-like-this-dwim)

(global-set-key (kbd "M-+") 'mc/mark-next-like-this)
(global-set-key (kbd "M--") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c M-+") 'mc/mark-all-like-this)

;; expand region
(define-key evil-visual-state-map "ar" 'er/expand-region)
(define-key evil-visual-state-map "ir" 'er/contract-region)

;; YAS bindings
(evil-leader/set-key "SPC" (lambda ()
                             (interactive)
                             (evil-insert-state)
                             (yas-insert-snippet)))

;; Window management
(define-key evil-normal-state-map (kbd "H-y") 'buf-move-left)
(define-key evil-normal-state-map (kbd "H-n") 'buf-move-down)
(define-key evil-normal-state-map (kbd "H-i") 'buf-move-up)
(define-key evil-normal-state-map (kbd "H-o") 'buf-move-right)

;; Magit
(evil-leader/set-key
  "gs"  'magit-status
  "gh"  'magit-file-log
  "gb"  'magit-blame-mode
  "gg"  'vc-git-grep)

;; move to next/prev git item with j/k in Magit mode
;; (evil-add-hjkl-bindings magit-status-mode-map 'emacs
;;   "K" 'magit-discard-item
;;   "l" 'magit-key-mode-popup-logging
;;   "h" 'magit-toggle-diff-refine-hunk)

;; Move-text keybindings (move text or region up/down)
(define-key evil-visual-state-map (kbd "M-i") 'move-text-up)
(define-key evil-normal-state-map (kbd "M-n") 'move-text-down)
(define-key evil-normal-state-map (kbd "M-i") 'move-text-up)
(define-key evil-visual-state-map (kbd "M-n") 'move-text-down)

;; jk escapes and switches to normal mode
;; (key-chord-define evil-insert-state-map  "" 'evil-normal-state)
;; (key-chord-define evil-visual-state-map  "jk" 'evil-normal-state)
;; (key-chord-define evil-operator-state-map  "jk" 'evil-normal-state)
;; (key-chord-define evil-emacs-state-map  "jk" 'evil-normal-state)

                                        ; Case conversion
(evil-leader/set-key
  "crm" 'mixedcase-word-at-point
  "crc" 'camelcase-word-at-point
  "crs" 'underscore-word-at-point
  "cru" 'uppercase-word-at-point
  "crd" 'dasherize-word-at-point
  "cr:" 'colonize-word-at-point)

(global-set-key (kbd "C-c f") 'simp-project-find-file)
(global-set-key (kbd "M-t") 'projectile-find-file)
(global-set-key (kbd "M-T") 'projectile-invalidate-cache)


(evil-leader/set-key
  "oss" 'show-entry
  "ohh" 'hide-entry
  "ost" 'show-subtree
  "osa" 'show-all
  "oht" 'hide-subtree
  "oha" 'hide-body)

;; default mac shortcuts to save file and close window
(define-key evil-normal-state-map (kbd "M-s") 'save-buffer)
(define-key evil-insert-state-map (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-w") 'quit-window)

(global-set-key (kbd "M-RET") 'toggle-fullscreen)

;; window management
(evil-leader/set-key
  "bl"  'switch-to-buffer
  "bd"  'evil-delete-buffer
  "bb"  'switch-to-previous-buffer
  "bk"  'kill-this-buffer)

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

;;;;;;;;;;;;;;;;;;
;; Evil keymaps ;;
;;;;;;;;;;;;;;;;;;
(require 'evil-custom-maps)


;;;;;;;;;;;;;;;;;;
;; Evil exchange ;;
;;;;;;;;;;;;;;;;;;
(defgroup evil-exchange nil
  "Easy text exchange operator for Evil."
  :prefix "evil-exchange"
  :group 'evil)

(defcustom evil-exchange-key (kbd "gs")
  "Default binding for evil-exchange."
  :type `,(if (get 'key-sequence 'widget-type)
              'key-sequence
            'sexp)
  :group 'evil-exchange)

(defcustom evil-exchange-cancel-key (kbd "gS")
  "Default binding for evil-exchange-cancel."
  :type `,(if (get 'key-sequence 'widget-type)
              'key-sequence
            'sexp)
  :group 'evil-exchange)

(defvar evil-exchange-position nil "Text position which will be exchanged.")

(evil-define-operator evil-exchange (beg end type)
  "Exchange two regions with evil motion."
  :move-point nil
  (interactive "<R>")
  (let ((beg-marker (copy-marker beg t))
        (end-marker (copy-marker end t)))
    (if (null evil-exchange-position)
        ;; call without evil-exchange-position set: store region
        (setq evil-exchange-position (list beg-marker end-marker type))
      ;; secondary call: do exchange
      (cl-destructuring-bind
          (orig-beg orig-end orig-type) evil-exchange-position
        (cond
         ;; exchange block region
         ((and (eq orig-type 'block) (eq type 'block))
          (let ((orig-rect (delete-extract-rectangle orig-beg orig-end))
                (curr-rect (delete-extract-rectangle beg-marker end-marker)))
            (save-excursion
              (goto-char orig-beg)
              (insert-rectangle curr-rect)
              (goto-char beg-marker)
              (insert-rectangle orig-rect)))
          (setq evil-exchange-position nil))
         ;; signal error if regions incompatible
         ((or (eq orig-type 'block) (eq type 'block))
          (error "Can't exchange block region with non-block region."))
         ;; exchange normal region
         (t
          (transpose-regions orig-beg orig-end beg end)
          (setq evil-exchange-position nil))))))
  ;; place cursor on beginning of line
  (when (and (evil-called-interactively-p) (eq type 'line))
    (evil-first-non-blank)))

(defun evil-exchange-cancel ()
  "Cancel current pending exchange."
  (interactive)
  (setq evil-exchange-position nil)
  (message "Exchange cancelled"))

(defun evil-exchange-install ()
  "Setting evil-exchange key bindings."
  (define-key evil-normal-state-map evil-exchange-key 'evil-exchange)
  (define-key evil-visual-state-map evil-exchange-key 'evil-exchange)
  (define-key evil-normal-state-map evil-exchange-cancel-key 'evil-exchange-cancel)
  (define-key evil-visual-state-map evil-exchange-cancel-key 'evil-exchange-cancel))

(evil-exchange-install)

(provide '00-personal-packages)
;;; 00-personal-packages ends here
