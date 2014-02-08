;;; package --- Summary

;;; Commentary:

;;; Code:
(server-start)

(add-to-list 'load-path "~/.emacs.d/personal-packages/fiplr/")
(add-to-list 'load-path "~/.emacs.d/personal-packages/fuzzy-el/")
(add-to-list 'load-path "~/.emacs.d/personal-packages/popup-el/")
(add-to-list 'load-path "~/Development/Scala/ensime/dist/elisp/")
(add-to-list 'load-path "~/.emacs.d/personal-packages/evil-org-mode/")
(add-to-list 'load-path "~/.emacs.d/personal-packages/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/personal-packages/org-mode/lisp/contrib/lisp")
(add-to-list 'load-path "~/.emacs.d/personal/")
(add-to-list 'load-path "~/.emacs.d/personal/evil/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(add-to-list 'exec-path "~/.cabal/bin")

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(prelude-ensure-module-deps
 '(
   ;; evil and plugins
   evil surround evil-numbers evil-nerd-commenter evil-leader evil-paredit
        ;; javascript
        js2-mode js2-refactor ac-js2 tern tern-auto-complete
        ;; grep etc
        ag wgrep wgrep-ag
        ;; visual
        diff-hl linum-relative rainbow-delimiters browse-kill-ring popwin
        highlight-indentation
        pos-tip
        ;; ruby
        rvm robe rinari ruby-electric ruby-block
        ;; Latex AcuTex
        auctex ac-math outline-magic
        ;; clojure
        nrepl ac-nrepl
        ;; misc language support
        slim-mode coffee-mode nginx-mode scala-mode2
        ;; haskell
        ghc hi2
        ;; html
        evil-matchit
        ;; editing
        move-text tagedit yasnippet smartparens auto-complete
        emmet-mode dash-at-point
        ;; flycheck-color-mode-line
        flycheck-haskell
        exec-path-from-shell
        buffer-move
        restclient
        grizzl
        flx-ido
        midnight))


(require 'evil)
(require 'evil-numbers)
(require 'evil-leader)
(require 'evil-matchit)
(require 'surround)
(require 'powerline)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'popup)
(require 'fuzzy)

(require 'rvm)
(require 'rinari)
(require 'robe)
(require 'ruby-block)
(require 'js2-mode)
(require 'js2-refactor)
(require 'tern)
(require 'ac-js2)
(require 'emmet-mode)
(require 'tagedit)
(require 'ag)
(require 'wgrep-ag)
(require 'exec-path-from-shell)
(require 'move-text)
(require 'browse-kill-ring)
(require 'scala-mode2)
(require 'ensime)
(require 'restclient)
(require 'multiple-cursors)
(require 'ruby-electric)
(require 'org)
(require 'ox-latex)
(require 'evil-org)
(require 'thingatpt+)
(require 'yasnippet)
(require 'popwin)
(require 'ac-ghc-mod)

(require 'diff-hl)
(require 'fiplr)
(require 'linum-relative)
(require 'smartparens)
(require 'smartparens-config)
(require 'rainbow-delimiters)
(require 'flycheck)
(require 'saveplace)
(require 'midnight)
(require 'evil-mode-line-color)

(autoload 'ghc-init "ghc" nil t)

;; Require custom defuns
(require 'setup-defuns)
                                        ; (require 'evil-custom-maps)

;;;;;;;;;;;;;;;
;; Setup GUI ;;
;;;;;;;;;;;;;;;

;; Set Font Face
(set-face-attribute 'mode-line nil :box nil
                    :family "Source Code Pro for Powerline"
                    :height 100 :weight 'normal)
(set-face-attribute 'mode-line-inactive nil :box nil)
(set-face-attribute 'default nil
                    :family "Source Code Pro for Powerline"
                    :height 100 :weight 'normal)

(disable-theme 'molokai)
(disable-theme 'zenburn)
(setq solarized-distinct-fringe-background t)
;; powerline solarized customization
;; (load-theme 'base16-default t)  ; Load the best theme ever
(load-theme 'solarized-light t) ; Load the best theme ever
(powerline-default-theme)        ; load powerline
(setq scroll-margin 4)          ; Sane cursor and window movements
(scroll-bar-mode -1)            ; No scrollbars, thank you
(global-diff-hl-mode)           ; show visual VCS diffs
(global-rainbow-delimiters-mode)

(browse-kill-ring-default-keybindings) ; load defult keybindings for killring browser
(global-linum-mode)                    ; line numbering everywhere
(yas-global-mode 1)                    ; use snippets everywhere
(popwin-mode 1)                        ; use popup windows instead of idle windows

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
;; (eval-after-load "flycheck"
;;   '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
(eval-after-load 'flycheck
  '(require 'flycheck-haskell))

;; projectile
(projectile-global-mode)
(global-set-key (kbd "C-c h") 'helm-projectile)

;; Fiplr
(setq fiplr-root-markers '(".git"))

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
(evil-mode nil)
(global-evil-leader-mode 1)
(global-evil-matchit-mode)
(evil-mode 1)
(setq evil-shift-width 2)

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
(rename-modeline "js2-mode" js2-mode "JS2")

(diminish 'emmet-mode "")
(diminish 'flyspell-mode "")
(diminish 'whitespace-mode "")
(diminish 'undo-tree-mode "")
(diminish 'projectile-mode "")
(diminish 'tagedit-mode "<>")
(diminish 'flycheck-mode " ✓ ")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(global-auto-complete-mode t)
(setq ac-use-menu-map t)
(setq ac-expand-on-auto-complete nil)
(setq ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed
(setq ac-delay 0.05)
(setq ac-auto-show-menu 0.1)
(setq ac-use-fuzzy 1)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)

(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(define-key ac-completing-map "\t" 'ac-complete)

;; extra modes auto-complete must support
(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                                    sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                                    html-mode nxml-mode sh-mode smarty-mode clojure-mode
                                    lisp-mode textile-mode markdown-mode tuareg-mode
                                    js2-mode js3-mode css-mode less-css-mode coffee-mode scss-mode
                                    slim-mode))
  (add-to-list 'ac-modes mode))

(set-default 'ac-sources
             '(ac-source-abbrev
               ac-source-yasnippet
               ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic))

(ac-config-default)

(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language customizations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ruby
(eval-after-load 'ruby-mode
  '(progn
     (rvm-use-default)
     (setq ruby-deep-indent-paren nil)
     (ruby-block-mode t)
     (setq ruby-block-highlight-toggle t)
     (custom-set-variables '(ruby-electric-expand-delimiters-list '(?\|)))
     )
  )

;; html, xml etc
(tagedit-add-paredit-like-keybindings)
(tagedit-add-experimental-features)

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
(push "*Inspector*" popwin:special-display-config)
(push "*ENSIME-Compilation-Result*" popwin:special-display-config)
(evil-leader/set-key-for-mode 'scala-mode
  "hi" 'ensime-inspect-type-at-point
  "ht" 'ensime-typecheck-all
  "hr" 'ensime-refactor-rename
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell IDE configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq haskell-stylish-on-save t)
(define-key haskell-mode-map (kbd "M-s") 'haskell-mode-save-buffer)
(push "*GHC Info*" popwin:special-display-config)
;; (push "*Warnings*" popwin:special-display-config)

;; evil haskell keybindings
(evil-leader/set-key-for-mode 'haskell-mode
  ;; "hat" (lambda ()
  ;;         (interactive)
  ;;         (let* ((haskell-type (haskell-get-type-string))
  ;;                (sym (word-at-point)))
  ;;           (save-excursion
  ;;             (beginning-of-line)
  ;;             (open-line 1)
  ;;             (insert (format "%s :: %s" sym haskell-type))
  ;;             (when newline-and-indent
  ;;               (indent-according-to-mode)))))
  "hat" (lambda ()
          (interactive)
          (haskell-process-insert-type))
  "ht" 'haskell-get-type
  "hl" 'haskell-process-load-file
  "hi" 'haskell-process-do-info
  "hd" 'ghc-browse-document
  )

;; other haskell keybindings
(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key haskell-mode-map (kbd "C-,") " <- ")
(define-key haskell-mode-map (kbd "C-$") " <$> ")
(define-key haskell-mode-map (kbd "C-$") " <*> ")

;; dash integration
(add-to-list 'dash-at-point-mode-alist '(haskell-mode . "hs"))


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

(defun javascript-unicode ()
  (interactive)
  (substitute-patterns-with-unicode
   (list (cons "\\(function\\) *(" 'function)
         (cons "function *([^)]*) *{ *\\(return\\) " 'left-arrow)
         (cons "\\(<-\\)" 'left-arrow)
         (cons "\\(->\\)" 'right-arrow)
         (cons "\\(Math.sqrt\\)" 'square-root)
         (cons "\\(&&\\)" 'logical-and)
         (cons "\\(||\\)" 'logical-or)
         (cons "\\<\\(not\\)\\>" 'logical-neg)
         (cons "\\(>\\)\\[^=\\]" 'greater-than)
         (cons "\\(<\\)\\[^=\\]" 'less-than)
         (cons "\\(>=\\)" 'greater-than-or-equal-to)
         (cons "\\(<=\\)" 'less-than-or-equal-to) )))

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


;;;;;;;;;;;;;;;;;;;;
;; LATEX / AUCTEX ;;
;;;;;;;;;;;;;;;;;;;;
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; compile to pdf
(setq TeX-PDF-mode t)

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-lisT
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
;; (setq TeX-view-program-list
;;       '(("PDF Viewer" "open %o")))


;; make auto-complete aware of `latex-mode`
(add-to-list 'ac-modes 'latex-mode)

;; add ac-sources to default ac-sources
(defun ac-latex-mode-setup ()
  (setq ac-sources
        (append '(ac-source-math-unicode
                  ac-source-math-latex
                  c-source-latex-commands)
                ac-sources)))

(font-lock-add-keywords
 'latex-mode
 `((,(concat "^\\s-*\\\\\\("
             "\\(documentclass\\|\\(sub\\)?section[*]?\\)"
             "\\(\\[[^]% \t\n]*\\]\\)?{[-[:alnum:]_ ]+"
             "\\|"
             "\\(begin\\|end\\){document"
             "\\)}.*\n?")
    (0 'your-face append))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File to language mappings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist  (cons '("Gemfile$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("Gemfile.lock$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("Rakefile$" . ruby-mode) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))

;; org mode
(setq org-export-in-background t)

(add-to-list 'auto-mode-alist '("\\. org \\'" . org-mode))

;; KOMA Script
(add-to-list 'org-latex-classes
             '("koma-article"
               "\\documentclass{scrartcl}
\\usepackage[T1]{fontenc}
\\usepackage{libertine}
\\renewcommand*\\oldstylenums[1]{{\\fontfamily{fxlj}\\selectfont #1}}
\\usepackage{lmodern}
\\usepackage{xunicode}
\\usepackage{xltxtra}
\\usepackage[xetex]{hyperref}
\\usepackage{geometry}
\\geometry{a4paper, textwidth=6.5in, textheight=10in, marginparsep=7pt, marginparwidth=.6in}
 [NO-DEFAULT-PACKAGES]
 [NO-PACKAGES]"

               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;; Tell org mode about xelatex
;;; XeLaTeX customisations
;; remove "inputenc" from default packages as it clashes with xelatex
(setf org-latex-default-packages-alist
      (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))

(add-to-list 'org-latex-packages-alist '("" "xltxtra" t))

;; org to latex customisations, -shell-escape needed for minted
(setq org-latex-to-pdf-process          ; for regular export
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o
%f")
      org-export-dispatch-use-expert-ui t ; non-intrusive export dispatch
      org-latex-pdf-process           ; for experimental org-export
      '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "xelatex -shell-escape -interaction nonstopmode -output-directory %o
%f"))

;; Restore window on quit agenda
(setq org-agenda-restore-windows-after-quit t)

;; add the org file link format to the iimage mode regex
;; (add-to-list 'iimage-mode-image-regex-alist
;;             (cons (concat "\\[\\[file:\\(~?" iimage-mode-image-filename-regex "\\)\\]") 1))

;; function to setup images for display on load
(defun org-turn-on-iimage-in-org ()
  "Display images in your org file."
  (interactive)
  (clear-image-cache nil)
  (turn-on-iimage-mode)
  (set-face-underline-p 'org-link nil))

;; function to toggle images in a org bugger
(defun org-toggle-iimage-in-org ()
  "Display images in your org file."
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline-p 'org-link nil)
    (set-face-underline-p 'org-link t))
  (call-interactively 'iimage-mode))

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

;; return to normal mode after save
(add-hook 'after-save-hook 'evil-normal-state)

;; fontlocking
(add-hook 'js2-mode-hook 'javascript-unicode)

;; multiple cursors
(add-hook 'multiple-cursors-mode-enabled-hook
          'my-mc-evil-switch-to-insert-state)

(add-hook 'multiple-cursors-mode-disabled-hook
          'my-mc-evil-back-to-previous-state)

(add-hook 'rectangular-region-mode-hook 'my-rrm-evil-switch-state)

;; haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'hi2-mode)
(add-hook 'haskell-mode-hook (lambda ()
                               (define-key haskell-mode-map (kbd "M-t") 'fiplr-find-file)
                               (ghc-init)
                               (flycheck-mode 1)
                               (setq tab-width 4)
                               (setq ac-sources
                                     (append '(ac-source-ghc-module
                                               ac-source-ghc-symbol
                                               ac-source-ghc-pragmas
                                               ac-source-ghc-langexts)
                                             ac-sources))))

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "M-t") 'fiplr-find-file)
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c M-.") nil)
     (define-key haskell-mode-map (kbd "C-c C-d") nil)))

;; Show indentation guides in languages with semantic indentation
(add-hook 'slim-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'coffee-mode-hook 'highlight-indentation-current-column-mode)
(add-hook 'python-mode-hoocoffee-mode-hook 'highlight-indentation-current-column-mode)

;;; autopair messes with multiple cursors, so disable it
;; (add-hook 'multiple-cursors-mode-enabled-hook (lambda ()
;;                                                 (autopair-mode -1)))
;; (add-hook 'multiple-cursors-mode-disabled-hook (lambda ()
;;                                                 (autopair-mode t)))

;; ruby
(add-hook 'ruby-mode-hook #'(lambda () (smart-parens-mode -1)))
;; (add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode nil)))

;; activate robe
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-robe)
            (setq completion-at-point-functions '(auto-complete))))

;; ido vertical
(add-hook 'ido-setup-hook 'ido-define-keys)
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)

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

;; Fix auto-complete when flyspell is active
(add-hook 'flyspell-mode-hook
          (lambda ()
            (ac-flyspell-workaround)))

;; activate tern in JS mode
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; activate flycheck ind js2-mode
(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))

;; use latex ac sources
(add-hook 'latex-mode-hook 'ac-latex-mode-setup)
(add-hook 'latex-mode-hook 'outline-minor-mode)

;; forward and backwards search
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

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

;; Why the hell is Y yank whole line anyways?!
(define-key evil-normal-state-map "H" (kbd "hg_"))

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
(define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)
(define-key evil-visual-state-map "+" 'inc-num-region) ;; Increment rows of numbers ascendin

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
(define-key evil-normal-state-map (kbd "M-i") 'move-text-up)
(define-key evil-normal-state-map (kbd "M-n") 'move-text-down)
(define-key evil-visual-state-map (kbd "M-i") 'move-text-up)
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
(global-set-key (kbd "M-t") 'fiplr-find-file)


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

;; Auto-complete
(define-key ac-completing-map (kbd "C-j") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(define-key ac-completing-map (kbd "C-g") 'evil-normal-state)
(define-key ac-completing-map (kbd "ESC") 'evil-normal-state)
(evil-make-intercept-map ac-completing-map)

;; More intuitive ido key bindings now that ido is vertical

(ac-set-trigger-key "TAB") ; AFTER input prefix, press TAB key ASAP

;; window management
(evil-leader/set-key
  "bl"  'switch-to-buffer
  "bd"  'evil-delete-buffer
  "bk"   'kill-this-buffer)

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
