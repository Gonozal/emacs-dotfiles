;;; evil-maps.el --- Default keymaps

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.0-dev

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; code:

(require 'evil)

;; map multiple states at once (courtesy of Michael Markert;
;; http://permalink.gmane.org/gmane.emacs.vim-emulation/1674)
(defun set-in-all-evil-states (key def &optional maps)
  (unless maps
    (setq maps (list evil-normal-state-map
                     evil-visual-state-map
                     evil-insert-state-map
                     evil-emacs-state-map
		     evil-motion-state-map)))
  (while maps
    (define-key (pop maps) key def)))


(defun set-in-all-evil-states-but-insert (key def)
  (set-in-all-evil-states key def (list evil-normal-state-map
				   evil-visual-state-map
				   evil-emacs-state-map
				   evil-motion-state-map)))


(defun set-in-all-evil-states-but-insert-and-motion (key def)
  (set-in-all-evil-states key def (list evil-normal-state-map
				   evil-visual-state-map
				   evil-emacs-state-map)))

;;; left down up right motions
(set-in-all-evil-states-but-insert  "y" 'evil-backward-char)
(set-in-all-evil-states-but-insert  "n" 'evil-next-line)
(set-in-all-evil-states-but-insert  "i" 'evil-previous-line)
(set-in-all-evil-states-but-insert  "o" 'evil-forward-char)
(define-key evil-operator-state-map "o" 'evil-forward-char)

(set-in-all-evil-states-but-insert "y" 'evil-backward-char)
(set-in-all-evil-states-but-insert "o" 'evil-forward-char)

;;; text objects map
(define-key evil-visual-state-map   "k" evil-inner-text-objects-map)
(define-key evil-operator-state-map "k" evil-inner-text-objects-map)

;;; Normal state
(define-key evil-normal-state-map  "k" 'evil-insert)
(define-key evil-normal-state-map  "K" 'evil-insert-line)
(define-key evil-normal-state-map  "J" 'evil-join) ;;; notmodified
(define-key evil-normal-state-map  "l" 'evil-open-below)
(define-key evil-normal-state-map  "L" 'evil-open-above)
(define-key evil-normal-state-map  "O" 'evil-window-bottom)
(define-key evil-normal-state-map  "M" 'evil-window-middle) ;; notmodified
(define-key evil-normal-state-map  "Y" 'evil-window-top)
(define-key evil-normal-state-map  "gk" 'evil-insert-resume)
(define-key evil-normal-state-map  "gJ" 'evil-join-whitespace) ;;; notmodified
(set-in-all-evil-states-but-insert "h" 'evil-yank)
;; (set-in-all-evil-states-but-insert "H" 'evil-yank-line)

;; custom function commands
(defun codas-yank-to-end-of-line ()
  "Yank to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))
(define-key evil-normal-state-map "H" 'codas-yank-to-end-of-line)

(defun codas-shift-left-visual ()
  "Shift left and restore visual selection."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))
(defun codas-shift-right-visual ()
  "Shift right and restore visual selection."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))
(define-key evil-visual-state-map (kbd ">") 'codas-shift-right-visual)
(define-key evil-visual-state-map (kbd "<") 'codas-shift-left-visual)


;; window commands
(define-key evil-window-map "y" 'evil-window-left)
(define-key evil-window-map "Y" 'evil-window-move-far-left)
(define-key evil-window-map "n" 'evil-window-down)
(define-key evil-window-map "N" 'evil-window-move-very-bottom)
(define-key evil-window-map "i" 'evil-window-up)
(define-key evil-window-map "I" 'evil-window-move-very-top)
(define-key evil-window-map "o" 'evil-window-right)
(define-key evil-window-map "O" 'evil-window-move-far-right)
(define-key evil-window-map "j" 'evil-window-new)
(define-key evil-window-map "l" 'delete-other-windows)
(define-key evil-window-map "\C-Y" 'evil-window-move-far-left)
(define-key evil-window-map "\C-y" 'evil-window-left)
(define-key evil-window-map "\C-N" 'evil-window-move-very-bottom)
(define-key evil-window-map "\C-n" 'evil-window-down)
(define-key evil-window-map "\C-I" 'evil-window-move-very-top)
(define-key evil-window-map "\C-i" 'evil-window-up)
(define-key evil-window-map "\C-O" 'evil-window-move-far-right)
(define-key evil-window-map "\C-o" 'evil-window-right)
(define-key evil-window-map "\C-j" 'evil-window-new)
(define-key evil-window-map "\C-l" 'delete-other-windows)

;;; Motion state
(define-key evil-motion-state-map "Y" 'evil-window-top)
(define-key evil-motion-state-map "O" 'evil-window-bottom)
(define-key evil-motion-state-map "j" 'evil-search-next)
;; (define-key evil-motion-state-map "J" 'evil-search-previous)
(define-key evil-motion-state-map "gn" 'evil-next-visual-line)
(define-key evil-motion-state-map "gi" 'evil-previous-visual-line)

; ;;; Visual state
; (define-key evil-visual-state-map "K" 'evil-insert)
(define-key evil-visual-state-map "l" 'exchange-point-and-mark)
(define-key evil-visual-state-map "L" 'evil-visual-exchange-corners)
(define-key evil-visual-state-map "k" evil-inner-text-objects-map)
(define-key evil-visual-state-map "K" 'evil-insert)

;; switch 0 and ^
(set-in-all-evil-states-but-insert "^" 'evil-beginning-of-line)
(set-in-all-evil-states-but-insert "0" 'evil-first-non-blank)


; ;;; Minibuffer
; (define-key minibuffer-local-map "\C-p" 'evil-complete-next)
; (define-key minibuffer-local-map "\C-j" 'evil-complete-previous)
; (define-key minibuffer-local-map "\C-x\C-p" 'evil-complete-next-line)
; (define-key minibuffer-local-map "\C-x\C-j" 'evil-complete-previous-line)

; ;;; Ex Commands
  ; (define-key evil-ex-completion-map "\C-l" 'evil-ex-completion)
; (define-key evil-ex-completion-map "\C-p" #'next-complete-history-element)
; (define-key evil-ex-completion-map "\C-j" #'next-complete-history-element)

(provide 'evil-custom-maps)

;;; evil-custom-maps.el ends here
