;; Per the straight.el README:
(setq package-enable-at-startup nil)

;; Flag that indicates whether we are running under Termux.
(setq this-is-termux (string-match-p "android" operating-system-release))

;; Per Bozhidar Batsov blog post 2025-03-28: temporarily pause GC during
;; startup, then GC every 50MB; and avoid flicker of GUI and theme.

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 50 1024 1024))))

(menu-bar-mode -1)
(setq inhibit-startup-screen t)
(unless this-is-termux
    (tool-bar-mode -1)
    (load-theme 'dichromacy t))
