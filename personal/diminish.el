(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")

(diminish 'emmet-mode "")
(diminish 'flyspell-mode " ✓ ")
(diminish 'whitespace-mode " ☐ ")
(diminish 'undo-tree-mode " ⤺ ")
(diminish 'projectile-mode "")
(diminish 'tagedit-mode "<>")
