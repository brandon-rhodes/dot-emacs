;; Start by re-invoking phase 1, in case it failed the first time (see
;; the comment there).

(load-file "setup/phase1.el")

;; Then proceed to install everything.

(package-install 'ag)
(package-install 'blacken)
(package-install 'browse-kill-ring)
;;(package-install 'counsel)
(package-install 'dtrt-indent)
(package-install 'fzf)
(package-install 'git-link)
(package-install 'go-mode)
(package-install 'ivy)
(package-install 'json-mode)
(if (>= emacs-major-version 25)
    (progn
      (package-install 'flycheck)
      (package-install 'lsp-mode)
      (package-install 'lsp-ui)
      (package-install 'magit)
      (package-install 'projectile)
      ))
(package-install 'multiple-cursors)
(package-install 'org)
(package-install 'typescript-mode)
