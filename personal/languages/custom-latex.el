;;; package --- LaTeX mode customizations

;;; Commentary:
;;; LaTeX customizations for my Emacs environment


;;; Code:

;; require additional LaTeX packages
(prelude-require-packages '(auctex outline-magic company-auctex))

;;;;;;;;;;;;;;;;;;;;
;; LATEX / AUCTEX ;;
;;;;;;;;;;;;;;;;;;;;
(require 'company-auctex)
(require 'reftex)
(require 'texmathp)
(company-auctex-init)

(setq
 TeX-auto-save t
 TeX-parse-self t
 reftex-toc-split-windows-horizontally t
 reftex-toc-split-windows-fraction 0.35
 font-latex-fontify-script nil
 font-latex-fontify-sectioning 'color
 )

(setq-default TeX-master nil)

;; (add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook '(lambda ()
                              (diff-hl-mode nil)
                              (latex-math-mode)
                              (turn-on-reftex)
                              (setq TeX-command-default "latexmk")
                              (ac-latex-mode-setup)
                              (outline-minor-mode)
                              (TeX-source-correlate-mode)
                              (auto-fill-mode)
                              ))

(setq reftex-plug-into-AUCTeX t)

;; compile to pdf
(setq-default TeX-PDF-mode t)

;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(push
 '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
   :help "Run latexmk on file")
 TeX-command-list)

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
;; (setq TeX-view-program-list
;;       '(("PDF Viewer" "open %o")))


;; make auto-complete aware of `latex-mode`
;; (add-to-list 'ac-modes 'latex-mode)

;; add ac-sources to default ac-sources
;; (defun ac-latex-mode-setup ()
;;   (setq ac-sources
;;         (append '(ac-source-math-unicode
;;                   ac-source-math-latex
;;                   c-source-latex-commands)
;;                 ac-sources)))

(font-lock-add-keywords
 'latex-mode
 `((,(concat "^\\s-*\\\\\\("
             "\\(documentclass\\|\\(sub\\)?section[*]?\\)"
             "\\(\\[[^]% \t\n]*\\]\\)?{[-[:alnum:]_ ]+"
             "\\|"
             "\\(begin\\|end\\){document"
             "\\)}.*\n?")
    (0 'your-face append))))

;; start in evil normal state

(evil-set-initial-state 'reftex-toc-mode 'normal)
(add-hook 'reftex-toc-mode-hook
          (lambda ()
            (rainbow-delimiters-mode nil)
            (evil-leader-mode nil)
            (smartparens-mode nil)
            (linum-mode 0)
            (diff-hl-mode 0)
            (pdf-tools-install)
            )
          )

;; add keyboard shortcuts for toc mode in evil normal state
(evil-define-key 'normal reftex-toc-mode-map (kbd "RET") 'reftex-toc-goto-line-and-hide)
(evil-define-key 'normal reftex-toc-mode-map (kbd "TAB") 'reftex-toc-goto-line)
(evil-define-key 'normal reftex-toc-mode-map (kbd "SPC") 'reftex-toc-view-line)
(evil-define-key 'normal reftex-toc-mode-map (kbd "<") 'reftex-toc-demote)
(evil-define-key 'normal reftex-toc-mode-map (kbd ">") 'reftex-toc-promote)
(evil-define-key 'normal reftex-toc-mode-map (kbd "q") 'reftex-toc-quit-and-kill)
(evil-define-key 'normal reftex-toc-mode-map (kbd "t") 'reftex-toc-max-level)
(evil-define-key 'normal reftex-toc-mode-map (kbd "z") 'reftex-toc-jump)


(provide 'custom-latex)
;;; custom-latex.el ends here
