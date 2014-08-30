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
(company-auctex-init)

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


;; use latex ac sources
(add-hook 'latex-mode-hook 'ac-latex-mode-setup)
(add-hook 'latex-mode-hook 'outline-minor-mode)
;; forward and backwards search
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)


(provide 'custom-latex)
;;; custom-latex.el ends here
