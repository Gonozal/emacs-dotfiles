;;; package -- Summary

;;; Commentary:

;;; Code:
(require 'evil)
(require 'evil-leader)
(require 'ace-jump-mode)

(defun toggle-fullscreen ()
  "Toggle full screen."
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

;; pretty print json
(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

;; In a region, increment the first numbers in each row
;; to be ascending from top to bottom
(defun inc-num-region (p m)
  "Increments the numbers in a given region."
  (interactive "r")
  (save-restriction
    (save-excursion
      (narrow-to-region p m)
      (goto-char (point-min))
      (forward-line)
      (let ((counter 1))
        (while (not (eq (point)
                        (point-max)))
          (goto-char (point-at-eol))
          (search-backward-regexp "[0-9]+" (point-at-bol) t)
          (let* ((this-num (string-to-number (match-string 0)))
                 (new-num-str (number-to-string (+ this-num
                                                   counter))))
            (replace-match new-num-str)
            (incf counter)
            (forward-line)))))))

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

;; Open user init file (init.el in .emacs.d directory)
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file user-init-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax Guessing of files ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Guess syntax based on file content. Very primitive and error prone
(defvar guess-mode-syntax-rules
  '(("html-mode"     . "<.*?>")
    ("js2-mode"      . "var\\|[A-Za-z]+\\.[A-Za-z]+(")
    ("less-css-mode" . "[A-Za-z]+\s?{")
    ("php-mode"      . "$[A-Za-z]+"))
  "Rules for guessing a mode from some text.")

(defun guess-mode-of-region (beg end)
  "Guess the mode of region, in the dumbest possible way."
  (save-restriction
    (narrow-to-region beg end)
    (loop for match = major-mode
          for rule in guess-mode-syntax-rules
          until (save-excursion
                  (when (search-forward-regexp (cdr rule) nil t)
                    (setq match (intern (car rule)))))
          finally return match)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer management ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; Narrowing and guessing new filetype
(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil))
        (mode (guess-mode-of-region start end)))
    (with-current-buffer buf
      (narrow-to-region start end)
      (funcall mode))
    (switch-to-buffer buf)))

(evil-define-operator evil-narrow-indirect (beg end type)
  "Indirectly narrow the region from BEG to END."
  (interactive "<R>")
  (evil-normal-state)
  (narrow-to-region-indirect beg end))

(defun guess-mode-of-region-and-switch (beg end)
  "Guess the mode of region and switch to it."
  (interactive "r")
  (let ((mode (guess-mode-of-region beg end)))
    (funcall mode)))

;; Ido
(defun ido-disable-line-truncation ()
  (set (make-local-variable 'truncate-lines) nil))
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  )

;;;;;;;;;;;;;;;;;;;;;
;; File operations ;;
;;;;;;;;;;;;;;;;;;;;;
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun delete-current-buffer-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Text manipulation ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

;;;;;;;;;;;;;;;;;;;;;
;; Case conversion ;;
;;;;;;;;;;;;;;;;;;;;;
(defun mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun mixedcase  (s) (mapconcat 'capitalize (split-name s) ""))
(defun underscore (s) (mapconcat 'downcase   (split-name s) "_"))
(defun dasherize  (s) (mapconcat 'downcase   (split-name s) "-"))
(defun colonize   (s) (mapconcat 'capitalize (split-name s) "::"))
(defun uppercase  (s) (mapconcat 'upcase     (split-name s) "_"))
(defun camelcase  (s) (mapconcat 'identity   (mapcar-head
                                              '(lambda (word) (downcase word))
                                              '(lambda (word) (capitalize (downcase word)))
                                              (split-name s)) ""))

(defun mixedcase-word-at-point ()
      (interactive)
      (let* ((case-fold-search nil)
	     (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
	     (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
	     (txt (buffer-substring beg end))
	     (cml (mixedcase txt)) )
	(if cml (progn (delete-region beg end) (insert cml))) ))
(defun underscore-word-at-point ()
      (interactive)
      (let* ((case-fold-search nil)
	     (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
	     (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
	     (txt (buffer-substring beg end))
	     (cml (underscore txt)) )
	(if cml (progn (delete-region beg end) (insert cml))) ))
(defun camelcase-word-at-point ()
      (interactive)
      (let* ((case-fold-search nil)
	     (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
	     (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
	     (txt (buffer-substring beg end))
	     (cml (camelcase txt)) )
	(if cml (progn (delete-region beg end) (insert cml))) ))
(defun dasherize-word-at-point ()
      (interactive)
      (let* ((case-fold-search nil)
	     (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
	     (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
	     (txt (buffer-substring beg end))
	     (cml (dasherize txt)) )
	(if cml (progn (delete-region beg end) (insert cml))) ))
(defun uppercase-word-at-point ()
      (interactive)
      (let* ((case-fold-search nil)
	     (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
	     (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
	     (txt (buffer-substring beg end))
	     (cml (uppercase txt)) )
	(if cml (progn (delete-region beg end) (insert cml))) ))
(defun colonize-word-at-point ()
      (interactive)
      (let* ((case-fold-search nil)
	     (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
	     (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
	     (txt (buffer-substring beg end))
	     (cml (colonize txt)) )
	(if cml (progn (delete-region beg end) (insert cml))) ))

;;;;;;;;;;;;;;;;;;;;;;;
;; Window management ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun rotate-windows ()
  "Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Window restoration ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defvar stante-save-frame-parameters-file
  (locate-user-emacs-file ".frame-parameters" )
  "File in which to storce frame parameters on exit.")

(defconst stante-frame-parameters-to-save
  '(left top width height maximized fullscreen)
  "Frame parameters to save and restore for the initial frame.")

(defun stante-save-frame-parameters ()
  "Save frame parameters of the selected frame.

Save selected parameters (see `stante-frame-parameters-to-save')
to `stante-save-frame-parameters-file'."
  (condition-case nil
      (let ((params (--filter (memq (car it) stante-frame-parameters-to-save)
                              (frame-parameters))))
        (when (and params (display-graphic-p)) ; GUI frames only!
          (with-temp-file stante-save-frame-parameters-file
            (prin1 params (current-buffer))
            (terpri (current-buffer)))
          t))
    (file-error nil)))

(defun stante-restore-frame-parameters ()
  "Restore the frame parameters of the initial frame."
  (condition-case nil
      (-when-let* ((read-params
                    (with-temp-buffer
                      (insert-file-contents stante-save-frame-parameters-file)
                      (goto-char (point-min))
                      (read (current-buffer))))
                   (allowed-params
                    (--filter (memq (car it) stante-frame-parameters-to-save)
                              read-params)))
        (setq initial-frame-alist
              (append (--filter (assq (car it) allowed-params) initial-frame-alist)
                      allowed-params nil)))
    (error nil)))

;;;;;;;;;;;;;;;;;;
;; Evil motions ;;
;;;;;;;;;;;;;;;;;;

;; ace-jump-mode
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

;; Join lines
(evil-define-operator evil-join-unfill (beg end)
  "Join the selected lines. Uses fill-region so that
adaptive-fill-mode is effective when joining."
  :motion evil-line
  (evil-with-single-undo
    (end-of-line)
    (let* ((fill-column (point-max))
           (lines (count-lines beg end))
           (end (if (> lines 1)
                    end
                  (save-excursion
                    (goto-char end)
                    (forward-line)
                    (point)))))
      (fill-region beg end))))


(evil-define-motion evil-little-word (count)
  :type exclusive
  (let* ((case-fold-search nil)
         (count (if count count 1)))
    (while (> count 0)
      (forward-char)
      (search-forward-regexp "[_A-Z]\\|\\W" nil t)
      (backward-char)
      (decf count))))

(evil-define-operator evil-destroy (beg end type register yank-handler)
  (evil-delete beg end type ?_ yank-handler))

(evil-define-operator evil-destroy-replace (beg end type register yank-handler)
  (evil-destroy beg end type register yank-handler)
  (evil-paste-before 1 register))

(evil-define-operator evil-destroy-char (beg end type register)
  "Delete next character."
  :motion evil-forward-char
  (interactive "<R><x>")
  (evil-destroy beg end type register))

(provide 'setup-defuns)
;;; setup-defuns.el ends here
