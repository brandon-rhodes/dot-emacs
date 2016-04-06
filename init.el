;; Emacs configuration for Brandon Rhodes, who does lots of Python, and
;; also some JavaScript for things that have to run in the browser.

(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Essential Mac OS X keybindings, put here at the top so that they
;; get installed even if something later in this file fails.  My
;; wrists would not survive my career if I had to twist my fingers
;; away from home row for every Ctrl or Meta, so I put them at my
;; thumbs instead.  Thumbs are orthoganal to the home-row fingers.

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'control)
  (setq mac-right-command-modifier 'meta)
  )

;; Ctrl-Tab and Shift-Ctrl-Tab switch between tabs in my browser.
;; To re-use that muscle memory, make them switch buffers in Emacs.

(global-set-key [C-tab] 'other-window)
(global-set-key [C-S-tab] (lambda () (interactive) (other-window -1)))
(global-set-key [backtab] (lambda () (interactive) (other-window -1)))

;; I have spent far too much of my life answering "y" to the prompt
;; "Active processes exist; kill them and exit anyway?"

(add-hook 'shell-mode-hook
          (lambda ()
            (process-kill-without-query
             (get-buffer-process (current-buffer)) nil)))

;; I have also spent too much of my life selecting the name of the
;; buffer that I want to kill, depsite the fact that it is always,
;; without exception, the current buffer.

(global-set-key [(control x) (k)] 'kill-this-buffer)

;; C-z should never iconify Emacs, only suspend it when in a terminal.
;; I mean, who even iconifies programs any more?  Not me.

(if (eq window-system 'x)
    (global-set-key [(control z)] 'suspend-emacs))

;; Make it easy to ask about a character.  Useful for obscure Unicode.

(global-set-key "\M-?" 'describe-char)

;; Get ready for (require...) calls to third-party libraries.

(package-initialize)

;; Third-party major mode for browsing the kill ring.

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; Jedi completion and pop-up documentation for Python.  This hook needs
;; to be installed before the Flyspell hook below, so that Jedi wins the
;; race to define important keys like the C-. key.

(require 'auto-complete-config)
(ac-config-default)

(add-hook 'python-mode-hook
          (lambda ()
            (jedi:setup)
            (define-key jedi-mode-map (kbd "<C-tab>") nil)  ;; let global win
            ))
(setq jedi:setup-keys t)  ; so Jedi wins the C-. and C-, keys
(setq jedi:complete-on-dot t)
(setq jedi:get-in-function-call-delay 360000)

;; Quickly jump up or down to the previous or next use of the name
;; sitting under point.

(defun search-forward-symbol-at-point ()
  (interactive)
  (end-of-thing 'symbol)
  (let ((case-fold-search nil))
    (re-search-forward (concat "\\_<" (thing-at-point 'symbol) "\\_>"))))

(defun search-backward-symbol-at-point ()
  (interactive)
  (beginning-of-thing 'symbol)
  (let ((case-fold-search nil))
    (re-search-backward (concat "\\_<" (thing-at-point 'symbol) "\\_>"))))

(global-set-key [(meta r)] 'search-backward-symbol-at-point)
(global-set-key [(meta s)] 'search-forward-symbol-at-point)

;; Jedi is wonderful for finding a definition, but what about the
;; opposite: finding everywhere that something is mentioned?

(require 'thingatpt)

(defun ag-current-word ()
  (interactive)
  (let ((text (if (use-region-p)
                  (buffer-substring (region-beginning) (region-end))
                (thing-at-point 'symbol))))
    (ag-project-regexp (concat "(?-i)\\b" text "\\b")))
  (other-window 1))

(global-set-key (kbd "M-a") 'ag-current-word)
(global-set-key (kbd "M-C-a") 'ag-project)

(eval-after-load 'coffee-mode
  '(define-key coffee-mode-map (kbd "M-C-a") 'ag-project))

;; Never auto-split a frame into a left and right window.

(setq split-width-threshold nil)
(setq split-height-threshold 0)

;; Multiple cursors.

(require 'multiple-cursors)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(global-set-key (kbd "M-p") 'mc/unmark-next-like-this)

;; I am beginning to conclude that Guido is simply not going to fix this
;; miserable dunder-main situation any time soon.  Good thing Emacs did
;; not pre-define the Control-underscrore key sequence!

(defun insert-dunder-main ()
  (interactive)
  (insert "import argparse\nif __name__ == '__main__':\n    "))

(global-set-key [(control _)] 'insert-dunder-main)

;; When I press enter in rst-mode, the *previous* line gets wrongly
;; re-indented.  This was supposed to have been fixed, says the docs, by
;; the setting of electric-indent-inhibit in rst.el, but the problem is
;; now happening for me on Emacs 24.5.1 so:

(defun set-up-rst-mode ()
  (make-local-variable 'electric-indent-inhibit)
  (setq electric-indent-inhibit t))

(add-hook 'rst-mode-hook 'set-up-rst-mode)

;; Flyspell to check my spelling and underline possible mistakes.
;; Thanks to http://stackoverflow.com/questions/8332163/

(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'rst-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'js-mode-hook (lambda () (flyspell-prog-mode)))

;; Only 2 space indents for JSON.  It is just data, after all.

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;; Auto-fill for text and SGML modes.

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'sgml-mode-hook 'turn-off-auto-fill)

;; Ispell should stop suggesting that it can cure a misspelling by
;; splitting a word into two quite different words.

(require 'cl)                       ; for Common List remove-if function

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
             ;; The --local-data-dir option is necessary because of:
             ;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=772415
             "--local-data-dir=/usr/lib/aspell"
             (concat "--master=" (expand-file-name "~/.emacs.d/aspell-huge"))
             " --sug-mode=ultra"))))

;; Pressing Enter should go ahead and indent the new line.

(global-set-key "\C-m" 'newline-and-indent)

;; Dedicated doctest mode.

(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))
(autoload 'doctest-mode "doctest-mode" nil t)

;; I want Tab to do variable-name completion, but Ctrl-I - which in
;; ASCII also means "Tab" - to indent the current line.  So I have to
;; convince Emacs to treat synonymous keystokes as different.

(defun set-up-tabbing ()
  (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
  (define-key (current-local-map) (kbd "<tab>") 'dabbrev-completion)
  (define-key (current-local-map) (kbd "C-i") 'indent-for-tab-command))

(add-hook 'css-mode-hook 'set-up-tabbing)
(add-hook 'html-mode-hook 'set-up-tabbing)
(add-hook 'js-mode-hook 'set-up-tabbing)
(add-hook 'sh-mode-hook 'set-up-tabbing)

(defun set-up-tabbing-for-python ()
  (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
  (define-key (current-local-map) (kbd "<tab>") 'jedi:complete)
  (define-key (current-local-map) (kbd "C-i") 'indent-for-tab-command))

(add-hook 'python-mode-hook 'set-up-tabbing-for-python)

;; Assume that .dat files belong to Ledger and support tab completion
;; for account names, that can contain internal spaces and colons.

(defun set-up-ledger ()
  (interactive)
  (text-mode)
  (make-variable-buffer-local 'dabbrev-case-fold-search)
  (make-variable-buffer-local 'dabbrev-abbrev-char-regexp)
  (setq dabbrev-case-fold-search nil)
  (setq dabbrev-abbrev-char-regexp "\\w\\|:\\| \\b")
  (set-up-tabbing))

(add-to-list 'auto-mode-alist '("\\.dat$" . (lambda () (set-up-ledger))))

;; Close-brace should be electric in CSS mode, which the CSS modes
;; themselves do not support in Emacs 23 or 24.

(defun electric-close-brace ()
  "Insert a close brace and re-indent."
  (interactive)
  (insert "}")
  (indent-for-tab-command))

(defun setup-electric-close-brace ()
  (define-key (current-local-map) (kbd "}") 'electric-close-brace))

(add-hook 'css-mode-hook 'setup-electric-close-brace)
(add-hook 'js-mode-hook 'setup-electric-close-brace)

;; Org mode should activate for files that end in ".org".

(load-library "org")
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Org mode should display totals in hours, not days-and-hours.
;; http://stackoverflow.com/questions/17929979/

(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Set up Flymake to use PyFlakes.

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-copy))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))

  (defun flymake-gjslint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-copy))
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

  (add-hook 'find-file-hook 'flymake-find-file-hook)
  )

;; Tell Flymake to use a temporary directory instead of spamming the
;; current directory with its temporary files, since the current
;; directory is often inside of my Dropbox or on a remote network
;; through sshfs.  This new setting is supported because we have
;; github.com/illusori/emacs-flymake/ in our "site-lisp" directory.

(setq flymake-run-in-place nil)
(setq temporary-file-directory "/tmp/")

;; Magit

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x v g") 'magit-blame)
(setq magit-last-seen-setup-instructions "1.4.0")
(add-hook 'magit-mode-hook 'turn-on-visual-line-mode) ; wrap long commit msgs

;; Colorize the diff that "git commit -v" (which I alias as "git ci")
;; includes when it asks me for a commit message by turning on Emacs
;; "diff-mode", and properly display the already-colorized ANSI
;; festooned diff that "hg ci" includes in the commit-message buffer.

(define-derived-mode hg-commit-mode text-mode
  (setq mode-name "Hg-Commit")
  (auto-fill-mode)
  (flyspell-mode)
  (ansi-color-buffer))

(add-to-list 'auto-mode-alist '("msg$" . hg-commit-mode))

(defun ansi-color-buffer ()
  "Replace all ANSI escape sequences in the current buffer with real colors."
  (interactive)
  (ansi-color-apply-on-region (point-min-marker) (point-max-marker)))

;; A few other file extensions.

(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . text-mode))

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

;; Make it easy to jump between files inside a project.

(defun fzf-repository ()
  "Run the fzf file selection tool in the current repository."
  (interactive)
  (fzf-directory (vc-git-root default-directory)))

(autoload 'vc-git-root "vc-git")
(global-set-key (kbd "C-x C-r") 'fzf-repository)

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

;; Allow for hand-written calls to customization functions; they must
;; live in a separate file, since the Customize sub-system sometimes
;; re-writes the custom-set-* calls here in init.el.

(load-library "~/.emacs.d/customizations.el")

;; Load any local Emacs directives (such as extra packages, and
;; passwords that should not be stored in version control).

(if (file-exists-p "~/.emacs.d/local.el")
    (load-library "~/.emacs.d/local.el"))

;; It may be obsolete, but it provides a combination of features I need.

(load-library "longlines.el")

;; Prevent Emacs from constantly creating and deleting ".#filename"
;; symlinks (requires Emacs 24.3, which is not yet the Ubuntu default).

;;(setq create-lockfiles nil)

;; Variables set through M-x customize-apropos.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments (quote ("--numbers" "--noheading" "--column" "--")))
 '(auto-save-default nil)
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(display-time-mode t)
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
 '(safe-local-variable-values (quote ((jedi:server-args "--sys-path" "/home/brhodes/src/server/metaserver") (encoding . utf-8))))
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
 '(highlight ((((class color) (min-colors 88)) :background "#eee8d5") (t :background "white")))
 '(magit-item-highlight ((t nil)))
 '(quote (mode-line-highlight ((t nil)))))
