;;; package --- Org Mode mode customizations

;;; Commentary:
;;; Org Mode customizations for my Emacs environment


;;; Code:

;; require additional packages
(add-to-list 'load-path "~/.emacs.d/personal-packages/evil-org-mode/")
(add-to-list 'load-path "~/.emacs.d/personal-packages/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/personal-packages/org-mode/contrib/lisp")

(require 'org)

;; org mode
(add-to-list 'auto-mode-alist '("\\. org \\'" . org-mode))
(eval-after-load 'ox
  '(progn
     (require 'evil-org)
     (require 'ox-latex)
     (require 'ox-koma-letter)
     ;; (require 'ox-latex)
     (setq org-export-in-background t)
     (setq org-export-latex-listings t)
     (add-to-list 'org-latex-classes
                  '("koma-article"
                    "\\documentclass{scrartcl}
\\usepackage[T1]{fontenc}
\\usepackage{libertine}
\\renewcommand*\\oldstylenums[1]{{\\fontfamily{fxlj}\\selectfont #1}}
\\usepackage{lmodern}
\\usepackage{xunicode}
\\usepackage{xltxtra}
\\usepackage{minted}
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
     ;; XeLaTeX customisations
     ;; remove "inputenc" from default packages as it clashes with xelatex
     (setf org-latex-default-packages-alist
           (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))

     (add-to-list 'org-latex-packages-alist '("" "xltxtra" t))
     ;; Add minted to the defaults packages to include when exporting.
     (add-to-list 'org-latex-packages-alist '("" "minted"))
     ;; Tell the latex export to use the minted package for source
     ;; code coloration.
     (setq org-latex-listings 'minted)

     ;; org to latex customisations, -shell-escape needed for minted
     (setq org-latex-to-pdf-process          ; for regular export
           '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
             "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
             "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")
           org-export-dispatch-use-expert-ui t ; non-intrusive export dispatch
           org-latex-pdf-process           ; for experimental org-export
           '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
             "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
             "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
     )
  )


;; KOMA Script

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
  (iimage-mode)
  (set-face-underline 'org-link nil))

;; function to toggle images in a org bugger
(defun org-toggle-iimage-in-org ()
  "Display images in your org file."
  (interactive)
  (if (face-underline-p 'org-link)
      (set-face-underline 'org-link nil)
    (set-face-underline 'org-link t))
  (call-interactively 'iimage-mode))


(provide 'custom-org)
;;; custom-org.el ends here
