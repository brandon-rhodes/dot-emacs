;; Emacs configuration for Brandon Rhodes, who does lots of Python and
;; also some JavaScript when it has to run in the browser.

(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Ctrl-Tab and Shift-Ctrl-Tab switch between tabs in my browser.
;; To re-use that muscle memory, make them switch buffers in Emacs.

(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-tab] (lambda () (interactive) (other-window -1)))
(global-set-key [backtab] (lambda () (interactive) (other-window -1)))

;; Other convenient keys.

(global-set-key "\M-?" 'describe-char)

;; Flyspell to check my spelling and underline possible mistakes.

(add-hook 'message-mode-hook (lambda () (flyspell-mode)))
(add-hook 'rst-mode-hook (lambda () (flyspell-mode)))
(add-hook 'text-mode-hook (lambda () (flyspell-mode)))

(add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'js-mode-hook (lambda () (flyspell-prog-mode)))

;; Auto-fill for text and SGML modes.

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'sgml-mode-hook 'turn-off-auto-fill)

;; Ispell should stop suggesting that it can cure a misspelling by
;; splitting a word into two quite different words.

(defun contains-space-p (s)
  "Determine whether a given string contains spaces."
  (string-match-p " " s))

(defadvice ispell-parse-output (after remove-multi-words activate)
  "Remove multi-word suggestions from ispell-style output."
  (if (listp ad-return-value)
      (setq ad-return-value
            (list (nth 0 ad-return-value) ;; original word
                  (nth 1 ad-return-value) ;; offset in file
                  (remove-if 'contains-space-p (nth 2 ad-return-value))
                  (remove-if 'contains-space-p (nth 3 ad-return-value))
                  ))))

;; Spell-check the whole buffer upon entry (thanks, Ryan McGuire!)
;; unless it is really huge.

(defadvice flyspell-mode
  (after advice-flyspell-check-buffer-on-start activate)
  (if (< (buffer-size) 40000)
      (flyspell-buffer)))

;; Use an alternative dictionary, if available (see ./SETUP-spell.sh).
;; The "sug-mode" suggested by http://emacswiki.org/emacs/InteractiveSpell

(if (file-exists-p "~/.emacs.d/aspell-huge")
    (progn
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args
            (list
             (concat "--master=" (expand-file-name "~/.emacs.d/aspell-huge"))
             " --sug-mode=ultra"))))

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
  (define-key (current-local-map) (kbd "<tab>") 'dabbrev-expand)
  (define-key (current-local-map) (kbd "C-i") 'indent-for-tab-command))

(add-hook 'css-mode-hook 'set-up-tabbing)
(add-hook 'html-mode-hook 'set-up-tabbing)
(add-hook 'js-mode-hook 'set-up-tabbing)
(add-hook 'python-mode-hook 'set-up-tabbing)
(add-hook 'sh-mode-hook 'set-up-tabbing)

;; Typing open-paren or -bracket should auto-insert the closing one.

;; (require 'autopair)
;; (autopair-global-mode)
;; (setq autopair-autowrap t)

;; Close-brace should be electric in CSS mode, which the CSS modes
;; themselves do not support in Emacs 23 or 24.

(defun css-electric-brace ()
  "Insert a close brace and re-indent."
  (interactive)
  (insert "}")
  (indent-for-tab-command))

(defun css-setup ()
  (define-key css-mode-map (kbd "}") 'css-electric-brace))

(add-hook 'css-mode-hook 'css-setup)

;; Handle triple-quotes in Python; from http://code.google.com/p/autopair/

(add-hook 'python-mode-hook
          (lambda ()
            (setq autopair-handle-action-fns
                  (list #'autopair-default-handle-action
                        #'autopair-python-triple-quote-action))
            ))

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

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))

;; When I run "git commit", I always do it through an alias "git ci".
;; (Which is what "commit" was always called when I used RCS, then CVS,
;; then Subversion, then Mercurial, so I have quite a bit of muscle
;; memory invested - and who would use a six-letter subcommand for
;; something so critical, anyway?)  And my "git ci" is in fact aliased
;; to "git commit -v" so that the buffer contains a diff of what I am
;; about to commit, since only by quickly reviewing the diff can I
;; reliably make sure that I do not, at the last moment of typing the
;; command, accidentally committing more than I intended.  Anyway, the
;; diffs were very hard to read, until I got the idea to turn on Emacs
;; "diff" mode when editing a git "COMMIT_EDITMSG" file (see above), and
;; now they look wonderful and are very easy to scan.  But it did cause
;; one final problem: diff mode defines M-q as "quit window", kicking me
;; out of the commit message every time I press the button to re-format
;; the paragraph that I am writing about my commit.  Hence the following
;; fix, which maps M-q back to where it belongs!

(defun fix-meta-q ()
  (define-key (current-local-map) (kbd "M-q") 'fill-paragraph))

(add-hook 'diff-mode-hook 'fix-meta-q)

;; Have dired hide irrelevant files by default; this can be toggled
;; interactively with M-o.

(require 'dired-x)
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
              (seq "~" eol)                 ;; backup-files
              (seq ".pyc" eol)
              (seq ".pyo" eol)
              )))
(setq dired-omit-extensions
      (append dired-latex-unclean-extensions
              dired-bibtex-unclean-extensions
              dired-texinfo-unclean-extensions))
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(put 'dired-find-alternate-file 'disabled nil)

;; I sometimes write presentations right in an Emacs buffer, with "^L"
;; separating the slides.  By turning on "page-mode", I can move between
;; slides while staying in the same buffer with the "PageUp" and
;; "PageDn" keys.

(autoload 'page-mode "page-mode" nil t)

;; M-i and Shift-M-i indent currently highlighted region in and out,
;; like Command-[ and Command-] do for my friends who use TextMate.

(defun sustainable-shift-left ()
  "Shift left, leaving the region active."
  (interactive)
  (let (deactivate-mark)
    (call-interactively 'python-shift-left)))

(defun sustainable-shift-right ()
  "Shift right, leaving the region active."
  (interactive)
  (let (deactivate-mark)
    (call-interactively 'python-shift-right)))

(defun set-up-sustainable-shifts ()
  (define-key python-mode-map (kbd "M-I") 'sustainable-shift-left)
  (define-key python-mode-map (kbd "M-i") 'sustainable-shift-right)
  (remove-hook 'python-mode-hook 'set-up-sustainable-shifts))

(add-hook 'python-mode-hook 'set-up-sustainable-shifts)

;; Load any local Emacs directives (such as extra packages, and
;; passwords that should not be stored in version control).

(if (file-exists-p "~/.emacs.d/local.el")
    (load-library "~/.emacs.d/local.el"))

;; Load the solarized color theme, if present.

(when (and
       default-minibuffer-frame  ; is nil on a plain terminal
       (file-accessible-directory-p "~/.emacs.d/emacs-color-theme-solarized"))
  (add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
  (require 'color-theme-solarized)
  (eval-after-load "color-theme"
    '(progn
       (color-theme-initialize)
       (color-theme-solarized-light))))

;; Fix the fact that Emacs misinterprets Shift-Up from an xterm.
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2011-05/msg00211.html
;; I cannot get the above solution to work. A subsequent initialization
;; setup seems to erase my input-decode-map adjustment, even if I run it
;; in term-setup-hook! So I bind the offending key itself to the routine
;; that replaces its definition. I should learn how to also make itself
;; re-invoke the keystroke, but have spent enough time on this problem
;; for the evening.

(defun repair-shift-up ()
  (interactive)
  (define-key input-decode-map "\e[1;2A" [S-up]))

(global-set-key [select] 'repair-shift-up)

;; Variables set through M-x customize-apropos.

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(column-number-mode t)
 '(fill-column 72)
 '(global-hl-line-mode t)
 '(ido-enable-flex-matching t)
 '(ido-enable-last-directory-history nil)
 '(ido-enable-tramp-completion nil)
 '(ido-mode (quote both) nil (ido))
 '(ido-rotate-file-list-default t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(kill-do-not-save-duplicates t)
 '(line-number-mode t)
 '(longlines-show-hard-newlines t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(mouse-yank-at-point t)
 '(python-honour-comment-indentation nil)
 '(ropemacs-guess-project t)
 '(safe-local-variable-values (quote ((encoding . utf-8))))
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position t)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(quote (mode-line-highlight ((t nil)))))
