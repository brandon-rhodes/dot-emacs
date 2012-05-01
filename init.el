;; Emacs configuration for Brandon Rhodes, who does lots of Python and
;; also some JavaScript when it has to run in the browser.

(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Ctrl-Tab and Shift-Ctrl-Tab switch between tabs in my browser.
;; To re-use that muscle memory, make them switch buffers in Emacs.

(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-tab] (lambda () (interactive) (other-window -1)))
(global-set-key [backtab] (lambda () (interactive) (other-window -1)))

;; Flyspell to check my spelling and underline possible mistakes.

(defun turn-on-flyspell ()
  "Force flyspell-mode on, using a positive arg, for use in hooks."
  (interactive)
  (flyspell-mode 1))

(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'rst-mode-hook 'turn-on-flyspell)
(add-hook 'sgml-mode-hook 'turn-off-flyspell)
(add-hook 'text-mode-hook 'turn-off-flyspell)

;; Auto-fill for text and SGML modes.

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'sgml-mode-hook 'turn-off-auto-fill)

;; Pressing Enter should go ahead and indent the new line.

(global-set-key "\C-m" 'newline-and-indent)

;; Dedicated doctest mode.

(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))
(autoload 'doctest-mode "doctest-mode" nil t)

;; Run Pymacs and Ropemacs when we enter Python mode.
;; We always use the Python that lives in the virtualenv we
;; built beneath our ".emacs.d" directory.

(setenv "PYMACS_PYTHON" "~/.emacs.d/usr/bin/python")

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(defun set-up-rope ()
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  (ropemacs-mode)
  (remove-hook 'python-mode-hook 'set-up-rope))

(add-hook 'python-mode-hook 'set-up-rope)

;; I want Tab to do variable-name completion, but Ctrl-I - which in
;; ASCII also means "Tab" - to indent the current line.  So I have to
;; convince Emacs to treat synonymous keystokes as different.

(defun set-up-tabbing ()
  (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
  (define-key python-mode-map (kbd "<tab>") 'dabbrev-expand)
  (define-key python-mode-map (kbd "C-i") 'indent-for-tab-command)
  (remove-hook 'python-mode-hook 'set-up-tabbing))

(add-hook 'python-mode-hook 'set-up-tabbing)

;; Typing open-paren or -bracket should auto-insert the closing one.

(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t)

;; Handle triple-quotes in Python; from http://code.google.com/p/autopair/

(add-hook 'python-mode-hook
          (lambda ()
            (setq autopair-handle-action-fns
                  (list #'autopair-default-handle-action
                        #'autopair-python-triple-quote-action))
            ))

;; In JavaScript, also complete on Tab but indent on Ctrl-I.

(defun one-shot-js-hook ()
  (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
  (define-key js-mode-map (kbd "<tab>") 'dabbrev-expand)
  (define-key js-mode-map (kbd "C-i") 'indent-for-tab-command)
  (remove-hook 'js-mode-hook 'one-shot-js-hook))

(add-hook 'js-mode-hook 'one-shot-js-hook)

;; Org mode should activate for files that end in ".org".

(load-library "org")
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Set up Flymake to use PyFlakes.

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/home/brandon/.emacs.d/usr/bin/pyflakes" (list local-file))))

  (defun flymake-gjslint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/home/brandon/.emacs.d/usr/bin/gjslint"
            (list "--nojsdoc" "--unix_mode" local-file))))

  (setq flymake-allowed-file-name-masks
        (list (list (concat (expand-file-name "~") "/.*\\.py$")
                    'flymake-pyflakes-init)
              (list (concat (expand-file-name "~") "/.*\\.js$")
                    'flymake-gjslint-init)
              ))

  (add-hook 'find-file-hook 'flymake-find-file-hook))

;; A few file extensions.

(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))

;; Load any local Emacs directives (such as extra packages, and
;; passwords that should not be stored in version control).

(if (file-exists-p "~/.emacs.d/local.el")
    (load-library "~/.emacs.d/local.el"))
