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


;; Clear insert state bindings.
(setcdr evil-insert-state-map nil)

;; Don't wait for any other keys after escape is pressed.
(setq evil-esc-delay 0)

;; Make sure escape gets back to normal state and quits things.
(define-key evil-insert-state-map [escape] 'evil-normal-state)
(define-key evil-visual-state-map [escape] 'evil-normal-state)
(define-key evil-emacs-state-map [escape] 'evil-normal-state)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

;;; make ctrl-u scroll up as is in vim
(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

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
  )


(evil-ex-define-cmd "W" 'evil-write)
(evil-ex-define-cmd "b" 'switch-to-previous-buffer)

