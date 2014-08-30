;;; package --- Ruby mode customizations

;;; Commentary:
;;; Ruby customizations for my Emacs environment


;;; Code:

;; Require additional ruby packages
(prelude-require-packages '(rvm robe rinari ruby-electric ruby-block))

;;; activate ruby mode in several additianal files
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile.lock\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Podfile\\'" . ruby-mode))

(eval-after-load 'ruby-mode
  '(progn
     (require 'rvm)
     (require 'rinari)
     (require 'robe)
     (require 'ruby-block)
     (require 'ruby-electric)
     (rvm-use-default)
     (setq ruby-deep-indent-paren nil)
     (ruby-block-mode t)
     (setq ruby-block-highlight-toggle t)
     (custom-set-variables '(ruby-electric-expand-delimiters-list '(?\|)))
     )
  )

;; ruby
;; (add-hook 'ruby-mode-hook #'(lambda () (smartparens-mode -1)))
;; (add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode nil)))

;; activate robe
(add-hook 'ruby-mode-hook 'robe-mode)
;; (add-hook 'robe-mode-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-robe)
;;             (setq completion-at-point-functions '(auto-complete))))


(provide 'custom-ruby)
;;; custom-ruby.el ends here
