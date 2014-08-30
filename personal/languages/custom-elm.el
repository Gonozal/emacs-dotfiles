;;; package --- Elm mode customizations

;;; Commentary:
;;; Elm customizations for my Emacs environment


;;; Code:

;; require additional Elm packages
(prelude-require-packages '(elm-mode))


(require 'elm-mode)
(require 'flycheck)
;; elm language flycheck integration
(flycheck-define-checker elm-elm
  "An Elm syntax and style checker using the elm compiler."
  :command ("elm" "-p" "-m" source)
  :error-patterns
  ((error line-start "Error on line " line
          ", column " column (zero-or-more not-newline) ":\n"
          (message) line-end)
   (error line-start "Parse Error at (line " line
          ", column " column (zero-or-more not-newline) "):\n"
          (message) line-end)
   (error line-start "Type Error on line " line
          ", column " column (zero-or-more not-newline) ":\n"
          (message
           (one-or-more " ") (one-or-more not-newline)
           "\n" "\n"
           (one-or-more " ")
           (one-or-more not-newline)
           "\n"
           (one-or-more " ")
           (one-or-more not-newline))
          line-end)
   (error line-start "Type Error between lines " line
          (zero-or-more not-newline) ":\n"
          (+? (or any "\n"))
          "\n"
          (message
           (one-or-more " ")
           "Expected Type:"
           (one-or-more not-newline)
           "\n"
           (one-or-more " ")
           (one-or-more not-newline))
          line-end)
   )
  :modes elm-mode)

(add-to-list 'flycheck-checkers 'elm-elm)


(provide 'custom-elm)
;;; custom-elm.el ends here
