;; load emmet mode for every html file
(add-hook 'sgml-mode-hook 'emmet-mode)

;; indent to 2 spaces
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
