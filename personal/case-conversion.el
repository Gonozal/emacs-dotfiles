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


(evil-leader/set-key
  "crm" 'Mixedcase::Word::At::Point
  "crc" 'camelcase-word-at-point
  "crs" 'underscore-word-at-point
  "cru" 'uppercase-word-at-point
  "crd" 'dasherize-word-at-point
  "cr:" 'colonize-word-at-point)
