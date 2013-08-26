;; AceJump is a nice addition to evil's standard motions.

;; The following definitions are necessary to define evil motions for ace-jump-mode (version 2).

;; ace-jump is actually a series of commands which makes handling by evil
;; difficult (and with some other things as well), using this macro we let it
;; appear as one.


(add-hook 'ace-jump-mode-end-hook 'exit-recursive-edit)

;; some proposals for binding:



;;; END ACE Jump Integration
