;; This file is for customizations that we want to protect from
;; re-writing by the "customize" system.

;; In an xterm, I want the current line highlighted in "white" since
;; that ANSI background color slot is occupied by the Solarized "base2"
;; color; but in full-color mode, I need to specify the actual RGB.

(custom-set-faces
 '(highlight ((((class color) (min-colors 88)) :background "#eee8d5")
              (t :background "white"))))
