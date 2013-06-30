(setq backup-by-copying t    ; Don't delink hardlinks
    backup-directory-alist '(("." . "~/.emacs.d/temps/backups"))
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5    ; and how many of the old

    auto-save-file-name-transforms `((".*" ,"~/.emacs.d/temps/autosaves" t))
    undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/temps/undotrees")))
    )


(global-undo-tree-mode 1)
(setq undo-tree-auto-save-history t)

