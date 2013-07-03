;; Save and restore frame parameters
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

(unless noninteractive
  (add-hook 'kill-emacs-hook 'stante-save-frame-parameters)
  (add-hook 'after-init-hook 'stante-restore-frame-parameters))
