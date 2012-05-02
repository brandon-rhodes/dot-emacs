;; Maintain at least 80 columns, even when the Emacs window is resized
;; Brandon Rhodes https://github.com/brandon-rhodes/dot-emacs

(setq maximum-font-height 180)
(setq minimum-font-height 60)
(setq previous-frame-width 0)

(setq window-size-change-functions
      '((lambda (frame)
          (if (/= previous-frame-width (frame-width frame))
              (let ((trial-size maximum-font-height))
                (set-face-attribute 'default frame :height trial-size)
                (while (and (> trial-size minimum-font-height)
                            (< (frame-width frame) 80))
                  (setq trial-size (- trial-size 10))
                  (set-face-attribute 'default frame :height trial-size))
                (setq previous-frame-width (frame-width frame))
                )))))
