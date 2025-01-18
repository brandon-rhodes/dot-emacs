;; Emacs configuration for Brandon Rhodes, who does lots of Python.

(setq straight-use-package-by-default t)

;; It feels unhappy to possibly have network use taking place merely by
;; invoking Emacs on a new laptop, so, TODO: figure out how to replace
;; this with something that won't ever do I/O, and then move this into a
;; 'setup.el' that I can call only when I'm expecting to install packages.

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Let's start with all the variables set through M-x customize-apropos.
;; I keep them here at the top of the file so that if a later error
;; prevents the rest of this file from being loaded, I at least get to
;; enjoy these settings while I fix things.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(blacken-executable "~/.emacs.d/usr/bin/black")
 '(blacken-fast-unsafe t)
 '(blacken-skip-string-normalization t)
 '(blink-cursor-mode nil)
 '(c-default-style
   '((c-mode . "k&r")
     (c++-mode . "k&r")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu")))
 '(column-number-mode t)
 '(eldoc-echo-area-use-multiline-p nil)
 '(fill-column 72)
 '(help-at-pt-display-when-idle t nil (help-at-pt))
 '(help-at-pt-timer-delay 0)
 '(help-window-select t)
 '(image-file-name-extensions
   '("png" "jpeg" "jpg" "gif" "tiff" "tif" "xbm" "xpm" "pbm" "pgm" "ppm" "pnm"))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(kill-do-not-save-duplicates t)
 '(line-number-mode t)
 '(longlines-show-hard-newlines t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(mode-line-format
   '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position "  " mode-name mode-line-misc-info mode-line-end-spaces))
 '(mouse-yank-at-point t)
 '(org-clock-into-drawer nil)
 '(org-clock-mode-line-total 'current)
 '(org-duration-format 'h:mm)
 '(org-startup-folded nil)
 '(org-startup-truncated nil)
 '(recenter-positions '(middle))
 '(safe-local-variable-values '((encoding . utf-8)))
 '(scroll-preserve-screen-position t)
 '(search-default-mode 'char-fold-to-regexp)
 '(show-paren-delay 0)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(use-file-dialog nil)
 '(vc-handled-backends nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(escape-glyph ((t (:foreground "#d9c072"))))
 '(highlight ((((class color) (min-colors 88)) :background "#eee8d5") (t :background "white")))
 '(magit-item-highlight ((t nil)))
 '(quote (mode-line-highlight ((t nil)))))

;; The escape-glyph color (above) was designed to make longlines mode
;; easier for me to read by making the visible newline characters blend
;; into the background instead of standing visually in the way of the
;; text.  It was produced by chroma.js acting on Solarized colors:
;; > chroma.mix('#b58900', '#fdf6e3').hex()
;; < "#d9c072"

;; (add-to-list 'load-path "~/.emacs.d/site-lisp")

;; TODO: look back over the packages I previously had selected:
;; '(package-selected-packages
;;   '(git-link importmagic multiple-cursors json-mode go-mode edit-server browse-kill-ring))

;; Essential Mac OS X keybindings, put here at the top so that they
;; get installed even if something later in this file fails.  My
;; wrists would not survive my career if I had to twist my fingers
;; away from home row for every Ctrl or Meta, so I put them at my
;; thumbs instead.  Thumbs are orthogonal to the home-row fingers.

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'control)
  (setq mac-right-command-modifier 'meta)
  )

;; An experiment: it rarely seems like I want fundamental mode.
;; Instead, I'm always typing a quote character, and it comes out
;; straight instead of curly, and I have to manually switch into text
;; mode.  So let's make text mode the default for un-extensioned files
;; and see how annoying the result is.
;;
;; This follows the original practice of Unix, where all files not
;; marked executable were text files (directories were identified by
;; starting with capital letters).

(setq-default major-mode 'text-mode)

;; Ctrl-Tab and Shift-Ctrl-Tab switch between tabs in my browser.
;; To re-use that muscle memory, make them switch buffers in Emacs.

(global-set-key [C-tab] 'other-window)
(global-set-key [(control iso-lefttab)]  ; not C-S-tab or C-backtab?
                (lambda () (interactive) (other-window -1)))

;; I have spent far too much of my life answering "y" to the prompt
;; "Active processes exist; kill them and exit anyway?"

(add-hook 'shell-mode-hook
          (lambda ()
            (process-kill-without-query
             (get-buffer-process (current-buffer)) nil)))

;; C-z should never iconify Emacs, only suspend it when in a terminal.
;; I mean, who even iconifies programs any more?  Not me.

(if (eq window-system 'x)
    (global-set-key [(control z)] 'suspend-emacs))

;; "M-x date", which adds a simple header for diary-style text files.

(defun date ()
  (interactive)
  (insert (concat "======== "
                  (format-time-string "%Y %B %d %A")
                  " ========\n\n")))

;; Python

(add-hook 'python-mode-hook
  (lambda ()
    (define-key python-mode-map (kbd "M-[") #'python-indent-shift-left)
    (define-key python-mode-map (kbd "M-]") #'python-indent-shift-right)
    (setq outline-regexp " *\\(class\\|def\\) ")))

;; The "Eglot" language server library is now built-in to Emacs 29, so
;; let's try out "pylsp" from "python-lsp-server" (see SETUP.sh).

(add-hook 'python-mode-hook 'eglot-ensure)
(add-to-list 'exec-path "~/.emacs.d/venv/bin")  ; so it finds `pylsp`

;; To debug `pylsp`, for instance after editing `~/.config/pycodestyle`:

;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(python-mode . ("pylsp" "-vv"))))

;; But "Eglot" has a terrible behavior: every time you pause while
;; navigating the buffer, it computes and displays the documentation for
;; the symbol at point, which is both distracting and also burns your
;; laptop's CPU and battery to the ground.  So let's disable its timers
;; and switch to manually invoking it via a keystroke.

;;(setq eldoc-display-functions '(eldoc-display-in-buffer)  ; buffer only

(defun bcr-update-eglot-key-map ()
  (define-key eglot-mode-map (kbd "C-h .")
              'eldoc-print-current-symbol-info))

(defun bcr-remove-eldoc-hooks ()
  (remove-hook 'post-command-hook #'eldoc-schedule-timer t)
  (remove-hook 'pre-command-hook #'eldoc-pre-command-refresh-echo-area t))

(add-hook 'eglot-managed-mode-hook 'bcr-update-eglot-key-map)
(add-hook 'eldoc-mode-hook 'bcr-remove-eldoc-hooks)

;; Another very poor behavior of Eldoc: after splitting the window and
;; displaying its documentation buffer, it leaves your cursor stranded
;; over in the source code, so that closing the documentation requires
;; several keystrokes - `C-x o` then `q`.  Let's instead move focus to
;; the buffer, so simply typing `q` will dismiss the docs.

(defun bcr-switch-to-eldoc-window (docs interactive)
  (other-window 1))

(advice-add 'eldoc-display-in-buffer :after #'bcr-switch-to-eldoc-window)

;; Support routines for some "ripgrep" search functions below.

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

;; Never auto-split a frame into a left and right window.

(setq split-width-threshold nil)
(setq split-height-threshold 0)

;; Multiple cursors.

;; (require 'multiple-cursors)
;; (global-set-key (kbd "M-n") 'mc/mark-next-like-this)
;; (global-set-key (kbd "M-p") 'mc/unmark-next-like-this)

;; Recognize Unicode curly apostrophe during spell checks, instead of
;; assuming it breaks contractions into two separate words.

(setq ispell-local-dictionary-alist
      '(("english" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t ("-d" "en") nil utf-8)
        ))

(setq ispell-dictionary "english")

;; I'm not sure why the following didn't work for recognizing curly
;; single quotes in ispell; the change it makes never seems to show up
;; in the alist?  Leaving it here as a mystery so that I don't forget
;; the syntax:

;; (setf (alist-get "english" ispell-dictionary-alist nil nil 'equal)
;;       '("[[:alpha:]]" "[^[:alpha:]]" "'’" t ("-d" "en") nil utf-8))

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

;; Make the Tab key do something separate from C-i, which I want to keep
;; at its default definition of "indent".  I used to do it this way:

;; (defun set-up-tabbing ()
;;   (setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
;;   (define-key (current-local-map) (kbd "<tab>") 'dabbrev-completion)
;;   (define-key (current-local-map) (kbd "C-i") 'indent-for-tab-command))
;; (add-hook 'css-mode-hook 'set-up-tabbing) ...

;; --- but with modern Emacs it seems this is now sufficient?

(defun set-up-tab-for-python ()
  (define-key (current-local-map) [tab] 'completion-at-point))

(add-hook 'python-mode-hook 'set-up-tab-for-python)

;; Org mode should display totals in hours, not days-and-hours.
;; http://stackoverflow.com/questions/17929979/

(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

;; Custom keywords.

(setq org-todo-keywords
       '((sequence "TODO" "|" "DONE" "WONT")))

(setq org-todo-keyword-faces
      '(("WONT" . "orange")))

;; Git blame.

(defun my-blame ()
  (interactive)
  (start-process "blame" "*Messages*" ",blame"
                 (file-name-nondirectory (buffer-file-name))
                 (number-to-string (line-number-at-pos))))

(global-set-key (kbd "C-x v g") 'my-blame)

;; Colorize the diff that "git commit -v" (alias "git ci") includes in
;; the "COMMIT_EDITMSG" file; and, prevent "M-q" (fill-paragraph) from
;; mixing my text together with the git-generated comment that follows.

(defface commit-plus-face
  '((((class color)) :foreground "dark green"))
  "Diff lines preceded with a plus.")

(setq commit-highlights
      '(("^+.*" . 'commit-plus-face)
        ("^-.*" . 'error)))

(define-derived-mode commit-mode fundamental-mode "Commit Mode"
  (setq-local font-lock-defaults '(commit-highlights))
  (setq-local paragraph-start "#")
  (setq-local show-trailing-whitespace nil)
  (activate-smart-quotes)
  (auto-fill-mode))

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . commit-mode))

;; A few other file extensions.

(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.mako\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . text-mode))

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

;; Another dired-x feature: how to jump into dired at the current file,
;; which makes it easy to move or rename it.

(global-set-key (kbd "C-x d") 'dired-jump)
(global-set-key (kbd "C-x C-d") 'dired-jump)

;; Make it easy to jump between files inside a project.

(use-package fzf
  :bind (("C-x C-r" . fzf-git-files))
  )

;; I sometimes write presentations right in an Emacs buffer, with "^L"
;; separating the slides.  By turning on "page-mode", I can move between
;; slides while staying in the same buffer with the "PageUp" and
;; "PageDn" keys.

(autoload 'page-mode "page-mode" nil t)

;; Allow for hand-written calls to customization functions; they must
;; live in a separate file, since the Customize sub-system sometimes
;; re-writes the custom-set-* calls here in init.el.

(load "~/.emacs.d/customizations.el")

;; Turn on TypeScript mode for .tsx files.

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

;; Load any local Emacs directives (such as extra packages, and
;; passwords that should not be stored in version control).

(if (file-exists-p "~/.emacs.d/local.el")
    (load "~/.emacs.d/local.el"))

;; It may be obsolete, but it provides a combination of features I need.

(require 'longlines)

;; My own smart-quotes approach, based on Gareth Rees `smart-quotes.el`.

(setq smart-quotes-left-context "^\\|\\s-\\|\\s(\\|[‘“]")

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

(defun activate-smart-quotes ()
  (interactive)
  (local-set-key "'" 'smart-quotes-insert-single)
  (local-set-key "\"" 'smart-quotes-insert-double))

(add-hook 'text-mode-hook 'activate-smart-quotes)

;; Prevent Emacs from constantly creating and deleting ".#filename"
;; symlinks (requires Emacs 24.3, which is not yet the Ubuntu default).

(setq create-lockfiles nil)

;; Format Go on each save.

;; TODO: for the moment I no longer install "go-mode".  The next time I
;; need to develop in Go, I should try go-ts-mode, and figure out how to
;; get go-fmt hooked back up somehow as a save hook.
;;(add-hook 'before-save-hook #'gofmt-before-save)

;; Bind "recompile" to F5 and jump to the first error.  If compilation
;; is producing paths relative to a directory, also set this locally:
;; (add-to-list 'compilation-search-path "/home/brhodes/livegrep")

(require 'compile)  ;; otherwise, F5 prints "Wrong type argument: commandp"
(global-set-key (kbd "<f5>") 'recompile)
(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output 'first-error)

;; Hide the build buffer again immediately if it succeeds.

(add-hook 'compilation-finish-functions
          (lambda (buf str)
            (if (not (string-match-p ".*exited abnormally.*" str))
                (progn
                  (delete-windows-on
                   (get-buffer-create "*compilation*"))
                  (message "Success")))))

;; Make it easy for me to edit projects that use Black.

;; (require 'blacken)
(defun black (&optional arg)
  "Turn on blacken-mode in all Python buffers"
  (interactive "P")
  (add-hook 'python-mode-hook 'blacken-mode)
  )

;; Set the executable bit automatically.  This is going to save me SO
;; MUCH TIME!

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; JavaScript

(setq js-indent-level 4)

;; With much thanks to: https://www.emacswiki.org/emacs/RenumberList

(defun renumber-list (start end &optional num)
      "Renumber the list items in the current START..END region.
    If optional prefix arg NUM is given, start numbering from that number
    instead of 1."
      (interactive "*r\np")
      (save-excursion
        (goto-char start)
        (setq num (or num 1))
        (save-match-data
          (while (re-search-forward "^[0-9]+" end t)
            (replace-match (number-to-string num))
            (setq num (1+ num))))))

;; Make it easy to delete all trailing whitespace.  Based on M-\ key,
;; which is already by default bound to delete-horizontal-space.

(global-set-key (kbd "C-M-\\") 'delete-trailing-whitespace)

;; Prevent Emacs from scrolling the buffer in Org Mode when I press Tab
;; to cycle an item between hidden and visible.
;; https://emacs.stackexchange.com/questions/31276/

(remove-hook 'org-cycle-hook
             #'org-optimize-window-after-visibility-change)

;; Auto-detect C identation per-file (useful when working maintanence
;; work in the fairly heterogeneous XEphem and PyEphem code bases).

(use-package dtrt-indent)
(dtrt-indent-global-mode)

;; Inline evaluation of math expressions, without my having to fill my
;; text files with the Emacs Lisp math syntax, of which I tire (and
;; which is also less powerful math-wise than the "units" tool).

(defun evaluate-region-using-units (start end)
  "Evaluate the region as a 'units' expression and insert the result."
  (interactive "r")
  (let* ((region (buffer-substring start end))
         ;; Allow thousands commas in input, without "units" throwing an error.
         (region (replace-regexp-in-string "," "" region))
         ;; Allow dollar signs (usually copy and pasted from financials).
         (region (replace-regexp-in-string "\\$" "" region))
         (command (concat "units -t '(" region ")'"))
         (output-plus-newline (shell-command-to-string command))
         (output (substring output-plus-newline 0 -1)))
    (if (< (point) end) (exchange-point-and-mark))
    (insert " = " output)))

(global-set-key (kbd "C-=") 'evaluate-region-using-units)

;; I noticed while writing my PyTexas talk that I do this a lot, so
;; let's make it an Emacs macro.

(defun comment-line-and-duplicate ()
  "Comment out the current line and duplicate it on the next line."
  (interactive)
  (beginning-of-line)
  (let ((b (point)))
    (end-of-line)
    (copy-region-as-kill b (point)))
  (back-to-indentation)
  (insert "#")
  (end-of-line)
  (insert "\n")
  (yank)
  (current-kill 1)
  (back-to-indentation))

(global-set-key (kbd "M-#") 'comment-line-and-duplicate)

;; https://emacs.stackexchange.com/questions/54979/

(define-key global-map (kbd "C-x k")
  (lambda () (interactive) (kill-buffer (current-buffer))))

;; Experiment with the built-in completion techniques before trying
;; something external like "orderless".

(setq completion-styles '(basic substring partial-completion flex)
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

;; See the Vertico README.

(use-package vertico
  :init
  (vertico-mode))

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Adapted from the example configuration from the Consult README.

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-ripgrep)               ;; bcr: was consult-grep
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file ;;consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  (setq consult-project-function
        (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
)

;; The point of this is the "--sort path" at the end; the rest is pasted
;; from the default value. (TODO: can I append it programmatically, so
;; that the rest of the string doesn't go out of date?)

(setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --search-zip --sort path")

;; Use `consult-completion-in-region' if Vertico is enabled.
;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

;; A random setting that I'm trying out: leave highlights sitting around
;; when a search is done, as they provide a bit of visual context about
;; how I 'got here' in the code.  I used to find it intolerable to have
;; dangling highlighting, so I might have become less rigid over time.

(setq lazy-highlight-cleanup nil)

;; Don't create `~/.emacs.d/auto-save-list/`.

(setq auto-save-list-file-prefix nil)

;; I can never remember that `l` is the Back button in a *Help* buffer,
;; so this lets me instead use M-. and M-, to visit a link and then pop
;; back out to where I was reading.

(define-key help-mode-map (kbd "M-.") 'push-button)
(define-key help-mode-map (kbd "M-,") 'help-go-back)

;; Consult defines many "M-s" search actions, but leaves the crucial
;; keystroke "M-s M-s" free for the end user.  Let's try using it for a
;; project-wide ripgrep of the symbol at point.

(defun consult-ripgrep-symbol-at-point (&optional dir)
  (interactive "P")
  (consult-ripgrep dir (pcre-word-delimit (thing-at-point 'symbol))))

(global-set-key (kbd "M-s M-s") 'consult-ripgrep-symbol-at-point)

;; I should come up with a keystroke that will let me try out Embark.

;;(define-key global-map (kbd "C-c a") 'embark-act)

;; Since "M-x" isn't useful in the minibuffer anyway, let's make it a
;; shortcut to the Embark "export" command.

(define-key minibuffer-local-map (kbd "M-x") 'embark-export)

;; Have F5 commit my collection of text files.

(dir-locals-set-class-variables 'auto-committing-directory
   '((nil . ((compile-command . "$(git rev-parse --show-toplevel)/bin/commit")
             ))))

(dir-locals-set-directory-class
   "/home/brandon/Plain" 'auto-committing-directory)

;; I'm always opening a new file, adding a shebang line at the top, then
;; having to somehow get the buffer into the correct mode for the shell
;; or language on the shebang line.  So, a 'M = Mode' I/O command.

(global-set-key (kbd "C-x C-m") 'normal-mode)

;; For providing links to source code in online discussions.

(use-package git-link)

;; Uncomment this line to receive a traceback on error:

;;(setq debug-on-error t)
