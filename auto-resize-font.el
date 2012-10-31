;; Maintain at least 80 columns, even when the Emacs window is resized
;; Brandon Rhodes https://github.com/brandon-rhodes/dot-emacs

;; The largest and smallest acceptible font sizes, in tenths of points.

(setq maximum-font-height 136)
(setq minimum-font-height 60)

;; The window change function gets called not only for frame resizing
;; but during more minor events as well, so we remember the previous
;; width of each frame so that we only kick into action when the frame
;; is actually resized.

(setq current-frame-widths (make-hash-table :test 'eq))

;; Our window size change function itself, which hunts through font
;; sizes in large-to-small order to find one that achieves at least 80
;; columns.

(setq window-size-change-functions
      '((lambda (frame)
          (let ((trial-size maximum-font-height)
                (previous-size (gethash frame current-frame-widths -1)))
            (when (/= previous-size (frame-width frame))
              (puthash frame (frame-width frame) current-frame-widths)
              (set-face-attribute 'default frame :height trial-size)
              (while (and (> trial-size minimum-font-height)
                          (< (frame-width frame) 80))
                (setq trial-size (- trial-size 10))
                (set-face-attribute 'default frame :height trial-size))
              )))))
