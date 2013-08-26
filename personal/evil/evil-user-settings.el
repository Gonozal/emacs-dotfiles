(require 'evil)
;;; setup evil shift width
(setq evil-shift-width 2)

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

(defun guess-mode-of-region-and-switch (beg end)
  "Guess the mode of region and switch to it."
  (interactive "r")
  (let ((mode (guess-mode-of-region beg end)))
    (funcall mode)))

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

(defun inc-num-region (p m)
  "Increments the numbers in a given region"
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


(define-key evil-visual-state-map "+" 'inc-num-region)

(define-key evil-operator-state-map (kbd "lw") 'evil-little-word)

(define-key evil-normal-state-map "x" 'evil-destroy-char)
(define-key evil-visual-state-map "x" 'evil-destroy-char)

(define-key evil-normal-state-map "ü" 'evil-destroy-replace)
(define-key evil-visual-state-map "ü" 'evil-destroy-replace)

(define-key evil-normal-state-map "m" 'evil-narrow-indirect)
(define-key evil-visual-state-map "m" 'evil-narrow-indirect)

(define-key evil-normal-state-map "J" 'evil-join-unfill)
(define-key evil-visual-state-map "J" 'evil-join-unfill)
