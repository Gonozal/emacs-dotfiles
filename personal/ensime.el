;; load the ensime lisp code...
(add-to-list 'load-path "/Users/arne/Development/Scala/ensime/dist/elisp/")
(require 'ensime)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
