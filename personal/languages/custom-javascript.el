;;; package --- JavaScript mode customizations

;;; Commentary:
;;; JavaScript customizations for my Emacs environment


;;; Code:

;; require additional JavaScript packages
(prelude-require-packages '(js2-mode js2-refactor ac-js2 tern company-tern))

;; always use js2-mode in js files
(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))

(rename-modeline "js2-mode" js2-mode "JS2")


(eval-after-load 'js2-mode
  '(progn
     (require 'js2-refactor)
     (require 'tern)
     (require 'company-tern)
     (require 'ac-js2)
     (add-to-list 'company-backends 'company-tern)
     (add-to-list 'company-backends 'ac-js2-company)
     (setq js-indent-level 4)
     (setq-default js2-basic-offset 4)
     (setq js2-basic-offset 4)
     ;; (define-key js2-mode-map (kbd "RET") 'js2-electric-return)
     (custom-set-variables
      '(js2-bounce-indent-p t)
      '(js2-pretty-multiline-declarations 'all)
      )
     (setq ac-js2-evaluate-calls t)
     (setq httpd-port 9090)
     (tern-mode 1)
     (flycheck-mode 1)
     (setq-default js2-global-externs
                   '("module" "require" "buster" "sinon" "assert" "refute"
                     "setTimeout" "clearTimeout" "setInterval" "clearInterval"
                     "location" "__dirname" "console" "JSON", "confirm"))

     (setq-default js2-auto-indent-p t)

     ;; (setq-default js2-show-parse-errors nil)
     ;; (setq-default js2-strict-missing-semi-warning nil)
     (setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

     ;; JS refactoring
     (js2r-add-keybindings-with-prefix "C-c C-m")
     (font-lock-add-keywords
      'js2-mode `(("\\(function\\) *("
                   (0 (progn (compose-region (match-beginning 1)
                                             (match-end 1) "\u0192")
                             nil)))))
     (font-lock-add-keywords
      'js2-mode `(("function *([^)]*) *{ *\\(return\\) "
                   (0 (progn (compose-region (match-beginning 1)
                                             (match-end 1) "\u2190")
                             nil)))))
     ))


(defun javascript-unicode ()
  (interactive)
  (substitute-patterns-with-unicode
   (list (cons "\\(function\\) *(" 'function)
         (cons "function *([^)]*) *{ *\\(return\\) " 'left-arrow)
         (cons "\\(<-\\)" 'left-arrow)
         (cons "\\(->\\)" 'right-arrow)
         (cons "\\(Math.sqrt\\)" 'square-root))))

(add-hook 'js2-mode-hook 'javascript-unicode)

(provide 'custom-javascript)
;;; custom-javascript.el ends here
