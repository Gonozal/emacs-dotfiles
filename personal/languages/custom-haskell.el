;;; package --- Haskell mode customizations

;;; Commentary:
;;; Haskell customizations for my Emacs environment


;;; Code:

;; require additional Haskell packages
(prelude-require-packages '(ghc hi2 hamlet-mode mmm-mode company-ghc))

;; load structured haskell mode
(add-to-list 'load-path "~/.emacs.d/personal-packages/structured-haskell-mode/elisp")
(require 'shm)

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)


;; use hamlet, css, js modes in yesod templates
(add-to-list 'auto-mode-alist '("\\.hamlet$" . hamlet-mode))
(add-to-list 'auto-mode-alist '("\\.lucius$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.julius$" . js2-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell IDE configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dash integration
(add-to-list 'dash-at-point-mode-alist '(haskell-mode . "hs"))

(push "*Warnings*" popwin:special-display-config)

;; keybindings etc
(eval-after-load "haskell-mode"
  '(progn
     (require 'haskell-process)
     (require 'shm-case-split)
     (require 'mmm-vars)
     (require 'mmm-auto)
     (require 'company)
     (add-to-list 'company-backends 'company-ghc)

     ;; hamlet inline syntax highlighting
     (setq mmm-global-mode 'maybe)
     (mmm-add-classes
      '((hamlet-quasiquote
         :submode hamlet-mode
         :delimiter-mode nil
         :front "\\[x?hamlet|"
         :back "|\\]")))
     (mmm-add-mode-ext-class 'haskell-mode nil 'hamlet-quasiquote)

     (push "*GHC Info*" popwin:special-display-config)
     (setq haskell-stylish-on-save t)
     (define-key haskell-mode-map (kbd "M-s") 'haskell-mode-save-buffer)
     (define-key haskell-mode-map (kbd "M-t") 'projectile-find-file)
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c M-.") nil)
     (define-key shm-map (kbd "C-n") 'shm/newline-indent)
     (define-key shm-map (kbd "C-c C-n") 'shm/swing-down)
     (define-key shm-map (kbd "C-c C-i") 'shm/swing-up)
     (define-key haskell-mode-map (kbd "C-c C-d") nil)
     (evil-define-key 'normal haskell-mode-map ")" (lambda ()
                                                     (interactive)
                                                     (progn (shm/reparse)
                                                            (shm/goto-parent-end))))
     (define-key haskell-mode-map (kbd "C-,") " <- ")
     (define-key flyspell-mode-map (kbd "C-,") nil)
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
       "ht" 'ghc-show-type
       "hl" 'haskell-process-load-file
       "hi" 'haskell-get-type
       "hc" 'shm/case-split
       )
     ))

;; (setq haskell-font-lock-symbols t)

;; haskell
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
(eval-after-load 'flycheck
 '(require 'flycheck-hdevtools))
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
;; (add-hook 'haskell-mode-hook 'hi2-mode)
(add-hook 'haskell-mode-hook (lambda ()
                               (ghc-init)
                               (turn-on-haskell-font-lock)
                               ;; (setq ghc-insert-key      "\e]")
                               ;; (setq ghc-debug t)
                               (define-key haskell-mode-map (kbd "M-t") 'fiplr-find-file)
                               (flycheck-mode +1)
                               (structured-haskell-mode +1)
                               (haskell-indentation-mode -1)
                               (subword-mode +1)
                               (interactive-haskell-mode +1)
                               (setq tab-width 4)
                               (haskell-auto-insert-module-template)
                               (setq ac-sources
                                     (append '(ac-source-ghc-module
                                               ac-source-ghc-symbol
                                               ac-source-ghc-pragmas
                                               ac-source-ghc-langexts)
                                             ac-sources))))


(provide 'custom-haskell)
;;; custom-haskell.el ends here
