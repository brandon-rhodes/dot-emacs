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

;; Emacs had started hanging every time I started it, and I found this
;; recommendation at http://spacemacs.org/doc/FAQ#orgheadline14 - it
;; stops tramp mode (why does tramp mode, which I never use, get to run
;; at startup?) from invoking SSH on an unknown hostname that my ISP
;; apparently intercepts.

(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

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

;;(global-set-key "\C-?" 'describe-char)

;; Get ready for (require...) calls to third-party libraries.

(package-initialize)

;; "M-x date", which adds a simple header for diary-style text files.

(defun date ()
  (interactive)
  (insert (concat "======== "
                  (format-time-string "%Y %B %d %A")
                  " ========\n\n")))

;; Third-party major mode for browsing the kill ring.

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)

;; Third-party mode for bracketed paste.

(require 'bracketed-paste)
(bracketed-paste-enable)

;; Language Server support, so I can jump-to-definition in languages
;; beyond just Python.

(if (>= emacs-major-version 25)
    (progn
      (require 'lsp-mode)

      (lsp-define-stdio-client
       lsp-python
       "python"
       (lambda () default-directory)
       (list (expand-file-name "~/.emacs.d/usr/bin/pyls")))

      (add-hook 'python-mode-hook #'lsp-python-enable)

      (lsp-define-stdio-client
       lsp-go
       "go"
       (lambda () default-directory)
       ;; go get -u github.com/sourcegraph/go-langserver
       '("~/go/bin/go-langserver"))

      (add-hook 'go-mode-hook #'lsp-go-enable)
      ))

;; Quickly jump up or down to the previous or next use of the name
;; sitting under point.

(defun alphanum-bounds-of-alphanum-at-point ()
  "Return the start and end points of an alphanum at the current point.
   The result is a paired list of character positions for an alphanum
   located at the current point in the current buffer.  An alphanum is any
   decimal digit 0 through 9 with an optional starting minus symbol
   \(\"-\")."
  (save-excursion
    (skip-chars-backward "A-Za-z_")
    (if (looking-at "[A-Za-z_]+")
        (cons (point) (match-end 0)) ; bounds of alphanum-1
      nil))) ; no alphanum at point

(put 'alphanum 'bounds-of-thing-at-point
     'alphanum-bounds-of-alphanum-at-point)

(defun search-forward-symbol-at-point ()
  (interactive)
  (end-of-thing 'alphanum)
  (let ((case-fold-search nil))
    (re-search-forward (concat "\\(^\\|[^A-Za-z_]\\)"
                               (thing-at-point 'alphanum)
                               "\\($\\|[^A-Za-z_]\\)"))
    (backward-char)))

(defun search-backward-symbol-at-point ()
  (interactive)
  (beginning-of-thing 'alphanum)
  (let ((case-fold-search nil))
    (re-search-backward (concat "\\(^\\|[^A-Za-z_]\\)"
                                (thing-at-point 'alphanum)
                                "\\($\\|[^A-Za-z_]\\)"))
    (forward-char)))

(global-set-key [(meta r)] 'search-backward-symbol-at-point)
(global-set-key [(meta s)] 'search-forward-symbol-at-point)

;; Search with "ag".

(require 'thingatpt)

(defun pcre-quote (string)
  (let* ((s string)
         (s (replace-regexp-in-string "(" "\\(" s 'fixedcase 'literal))
         (s (replace-regexp-in-string ")" "\\)" s 'fixedcase 'literal)))
    s))

;; (pcre-quote "(a)")"\\(a\\)"

(defun pcre-word-delimit (string)
  (let* ((s string)
         (s (if (string-match-p "^[A-Za-z]" s)
                (concat "\\b" s)
              s))
         (s (if (string-match-p "[A-Za-z]$" s)
                (concat s "\\b")
              s)))
    s))

;; (pcre-word-delimit "a")"\\ba\\b"
;; (pcre-word-delimit ".a")".a\\b"
;; (pcre-word-delimit "a.")"\\ba."
;; (pcre-word-delimit ".a.")".a."

(defun ag-current-word ()
  (interactive)
  (let* ((text (if (use-region-p)
                   (buffer-substring (region-beginning) (region-end))
                 (thing-at-point 'symbol)))
         (text (pcre-quote text))
         (text (pcre-word-delimit text)))
    (ag-project-regexp text))
  (other-window 1))

(global-set-key (kbd "M-a") 'ag-current-word)
(global-set-key (kbd "M-C-a") 'ag-project-regexp)

(eval-after-load 'coffee-mode
  '(define-key coffee-mode-map (kbd "M-C-a") 'ag-project))

(eval-after-load 'js-mode
  '(define-key js-mode-map (kbd "M-C-a") 'ag-project))

;; I like jumping automatically to compilation errors (see the stanza
;; later in this file that mentions "recompile"), but I don't like
;; jumping to whatever happens to be the first search result when "ag"
;; uses compilation mode to present search results.  So in that case
;; let's turn the setting off.

(add-hook 'ag-mode-hook
          (lambda ()
            (make-local-variable 'compilation-auto-jump-to-first-error)
            (setq compilation-auto-jump-to-first-error nil)
            ))

(require 'compile)

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
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'indent-relative)
  (make-local-variable 'electric-indent-inhibit)
  (setq electric-indent-inhibit t))

(add-hook 'rst-mode-hook 'set-up-rst-mode)

;; Flyspell to check my spelling and underline possible mistakes.
;; Thanks to http://stackoverflow.com/questions/8332163/

(if (executable-find "aspell")
    (progn
      (add-hook 'message-mode-hook 'turn-on-flyspell)
      (add-hook 'rst-mode-hook 'turn-on-flyspell)
      (add-hook 'text-mode-hook 'turn-on-flyspell)

      ;; (add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))
      (add-hook 'js-mode-hook
                (lambda ()
                  (when (string-match-p "^  [A-Za-z]" (buffer-string))
                    (make-variable-buffer-local 'js-indent-level)
                    (set-variable 'js-indent-level 2))
                  (flyspell-prog-mode)))))

;; Only 2 space indents for JSON.  It is just data, after all.

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

;;; https://www.emacswiki.org/emacs/UnfillParagraph
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(define-key global-map "\M-Q" 'unfill-paragraph)

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

(if (and (executable-find "aspell")
         (file-exists-p "~/.emacs.d/aspell-huge"))
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
  (define-key (current-local-map) (kbd "C-i") 'indent-for-tab-command))

(add-hook 'python-mode-hook 'set-up-tabbing-for-python)

;; Use Python mode for Bazel .bzl files.

(defun bzl-mode ()
  (python-mode)
  ;; (setq-local python-indent 2)
  ;; (setq-local indent-tabs-mode nil)
  ;; (setq-local tab-width 2)
  ;; (setq-local py-indent-offset 2)
  )

(add-to-list 'auto-mode-alist '("BUILD\\'" . python-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bzl\\'" . bzl-mode))
(add-to-list 'auto-mode-alist '("\\.pyst\\'" . python-mode))

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

;; Org mode reports should not use \emsp as their indent symbol.
;; http://emacs.stackexchange.com/questions/9528/

(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str ". "))
      (while (> level 2)
        (setq level (1- level)
              str (concat str ". ")))
      (concat str "» "))))

(advice-add 'org-clocktable-indent-string
            :override #'my-org-clocktable-indent-string)

;; Custom keywords.

(setq org-todo-keywords
       '((sequence "TODO" "|" "DONE" "WONT")))

(setq org-todo-keyword-faces
      '(("WONT" . "orange")))

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
      (list (expand-file-name "~/.emacs.d/usr/bin/gjslint")
            (list "--nojsdoc" "--unix_mode" local-file))))

  (setq flymake-allowed-file-name-masks
        (list (list (concat (expand-file-name "~") "/.*\\.py$")
                    'flymake-pyflakes-init)
              (list (concat (expand-file-name "~") "/.*\\.js$")
                    'flymake-gjslint-init)
              ))

  (if (executable-find "pyflakes")
      (add-hook 'find-file-hook 'flymake-find-file-hook))
  )

;; Tell Flymake to use a temporary directory instead of spamming the
;; current directory with its temporary files, since the current
;; directory is often inside of my Dropbox or on a remote network
;; through sshfs.  This new setting is supported because we have
;; github.com/illusori/emacs-flymake/ in our "site-lisp" directory.

(setq flymake-run-in-place nil)
(setq temporary-file-directory "/tmp/")

;; Magit

(if (not (string= system-name "brhodes-dbx"))
    (progn
      (require 'magit)
      (global-set-key (kbd "C-x g") 'magit-status)
      (global-set-key (kbd "C-x v g") 'magit-blame)
      (setq magit-last-seen-setup-instructions "1.4.0")
      (add-hook 'magit-mode-hook 'turn-on-visual-line-mode) ; wrap long commit msgs
      ))

;; Colorize the diff that "git commit -v" (which I alias as "git ci")
;; includes when it asks me for a commit message by turning on Emacs
;; "diff-mode", and properly display the already-colorized ANSI
;; festooned diff that "hg ci" includes in the commit-message buffer.

(define-derived-mode hg-commit-mode text-mode
  (setq mode-name "Hg-Commit")
  (auto-fill-mode)
  (if (executable-find "aspell")
      (flyspell-mode))
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
(setq dired-auto-revert-buffer t)

;; Make it easy to jump between files inside a project.

(defun fzf-repository ()
  "Run the fzf file selection tool in the current repository."
  (interactive)
  (fzf/start (vc-git-root default-directory)))

(setenv "FZF_DEFAULT_COMMAND" "ag -g ''")
(autoload 'vc-git-root "vc-git")
(global-set-key (kbd "C-x C-r") 'fzf-repository)

(load-library "fzf.el")

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

;; Turn on TypeScript mode for .tsx files.

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;; Load any local Emacs directives (such as extra packages, and
;; passwords that should not be stored in version control).

(if (file-exists-p "~/.emacs.d/local.el")
    (load-library "~/.emacs.d/local.el"))

;; It may be obsolete, but it provides a combination of features I need.

(load-library "longlines")

;; Smart quotes.

(load-library "smart-quotes")
(add-hook 'text-mode-hook 'smart-quotes-mode)
(add-hook 'html-mode-hook 'turn-off-smart-quotes)

;; Except that I so often need to insert straight quotes, so TODO

(defun smart-quotes-insert-single (&optional arg)
  "Insert U+2018 LEFT SINGLE QUOTATION MARK if point is preceded
by `smart-quotes-left-context'; U+2019 RIGHT SINGLE QUOTATION MARK
otherwise.  If point is preceded by a single left or right quote,
insert a straight quote instead."
  (interactive "P")
  (insert-char
   (or (if (or (= (preceding-char) #x2018)
               (= (preceding-char) #x2019))
           (progn (delete-char -1) #x0027))
       (if (looking-back smart-quotes-left-context) #x2018 #x2019))))

(defun smart-quotes-insert-double (&optional arg)
  "Insert U+201C LEFT DOUBLE QUOTATION MARK if point is preceded
by `smart-quotes-left-context'; U+201D RIGHT DOUBLE QUOTATION
MARK otherwise.  If point is preceded by a double left or right quote,
insert straight double quotes instead."
  (interactive "P")
  (insert-char
   (or (if (or (= (preceding-char) #x201C)
               (= (preceding-char) #x201D))
           (progn (delete-char -1) #x0022))
       (if (looking-back smart-quotes-left-context) #x201C #x201D))))

;; Prevent Emacs from constantly creating and deleting ".#filename"
;; symlinks (requires Emacs 24.3, which is not yet the Ubuntu default).

;;(setq create-lockfiles nil)

;; Support Go.

(autoload 'go-mode "go-mode" "Supports the Go language." t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'before-save-hook #'gofmt-before-save)

;; Bind "recompile" to F5 and jump to the first error.  If compilation
;; is producing paths relative to a directory, also set this locally:
;; (add-to-list 'compilation-search-path "/home/brhodes/livegrep")

(global-set-key (kbd "<f5>") 'recompile)
(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output 'first-error)

;; Variables set through M-x customize-apropos.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-arguments
   (quote
    ("--hidden" "--smart-case" "--stats" "--width" "240")))
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
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
 '(org-clock-mode-line-total (quote current))
 '(package-selected-packages
   (quote
    (lsp-mode multiple-cursors magit json-mode go-mode git-timemachine fzf edit-server coffee-mode browse-kill-ring ag)))
 '(python-honour-comment-indentation nil)
 '(recenter-positions (quote (middle)))
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
 '(escape-glyph ((t (:foreground "#d9c072"))))
 '(highlight ((((class color) (min-colors 88)) :background "#eee8d5") (t :background "white")))
 '(magit-item-highlight ((t nil)))
 '(quote (mode-line-highlight ((t nil)))))

;; The escape-gylph color (above) was designed to make longlines mode
;; easier for me to read by making the visible newline characters blend
;; into the background instead of standing visually in the way of the
;; text.  It was produced by chroma.js acting on Solarized colors:
;; > chroma.mix('#b58900', '#fdf6e3').hex()
;; < "#d9c072"

;; When selecting a file from dired, I'm usually there to just read, so
;; "view" mode is far more convenient (it's like less(1): the spacebar
;; pages down instead of adding a space to the file, et cetera).  I can
;; press "e" to start editing if I really mean to modify the file.

(define-key dired-mode-map (kbd "RET") 'dired-view-file)

;; Set the executable bit automatically.  This is going to save me SO
;; MUCH TIME!

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
