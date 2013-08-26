(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defmacro evil-enclose-ace-jump (&rest body)
  `(let ((old-mark (mark))
         (ace-jump-mode-scope 'window))
     (remove-hook 'pre-command-hook #'evil-visual-pre-command t)
     (remove-hook 'post-command-hook #'evil-visual-post-command t)
     (unwind-protect
         (progn
           ,@body
           (recursive-edit))
       (if (evil-visual-state-p)
           (progn
             (add-hook 'pre-command-hook #'evil-visual-pre-command nil t)
             (add-hook 'post-command-hook #'evil-visual-post-command nil t)
             (set-mark old-mark))
         (push-mark old-mark)))))

;; Evil motions
(evil-define-motion evil-ace-jump-char-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (ace-jump-mode 5)))

(evil-define-motion evil-ace-jump-line-mode (count)
  :type line
  (evil-enclose-ace-jump
   (ace-jump-mode 9)))

(evil-define-motion evil-ace-jump-word-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (ace-jump-mode 1)))

(evil-define-motion evil-ace-jump-char-to-mode (count)
  :type exclusive
  (evil-enclose-ace-jump
   (ace-jump-mode 5)
   (forward-char -1)))
