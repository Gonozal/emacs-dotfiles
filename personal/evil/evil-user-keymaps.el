; switch to file
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))



(evil-leader/set-leader ",")

;;; some evil keymaps
(evil-leader/set-key
  "e"  'file-file
  "bl"  'switch-to-buffer
  "bd" 'evil-delete-buffer
  "d"  'kill-this-buffer
  "sv" 'split-window-horizontally
  "sh" 'split-window-vertically
  "k"  'kill-this-buffer
  "n"  'nav-toggle
  "SPC" (lambda ()
          (interactive)
          (evil-insert-state)
          (yas-insert-snippet))

  "u"  'undo-tree

  ;; Git tools
  ;; REQUIRES Magit
  ; Open git status buffer
  "gs"  'magit-status
  "gh"  'magit-file-log
  "gb"  'magit-blame-mode
  "gg"  'vc-git-grep
  "v"   'find-user-init-file
  "t"   'align-regexp
  )
