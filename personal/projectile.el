(require 'projectile)
(require 'fiplr)

(projectile-global-mode)

(global-set-key (kbd "C-c h") 'helm-projectile)

(setq fiplr-root-markers '(".git"))
