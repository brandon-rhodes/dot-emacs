;; Copyright (C) 2008 Bart Massey
;; ALL RIGHTS RESERVED
;; 
;; [This program is licensed under the GPL version 3 or later.]
;; Please see the file COPYING in the source
;; distribution of this software for license terms.

;; "Page Mode"
;; Minor mode for displaying or working on just
;; a page of content at a time in Emacs, where pages
;; are user-defined subsets of a buffer.
;; Bart Massey 2008/4/17

(defun page-mode-build-keymap ()
  "Build the mode keymap for page mode."
  (let  ((map (make-keymap)))
    (define-key map "\C-c\C-n" 'next-page)
    (define-key map "\C-c\C-p" 'prev-page)
    (define-key map "\C-cq" 'page-mode)
    (define-key map "\C-cs" 'split-page)
    (define-key map "\C-cn" 'new-page)
    (define-key map "\C-ci" 'insert-page)
    (define-key map "\C-c<" 'first-page)
    (define-key map "\C-c>" 'last-page)
    (define-key map "\C-cg" 'goto-page)
    (define-key map "\C-c=" 'what-page)
    (define-key map "\C-xu" 'page-global-undo)
    ;; Two more keys by Brandon Craig Rhodes
    (define-key map [next] 'next-page)
    (define-key map [prior] 'prev-page)
    ;; And, back to the original code.
    map))

(defun page-mode-setup ()
  "Setup for entering or leaving page mode."
  (if page-mode
      (progn
	(require 'page)
	(widen)
	(narrow-to-page))
      (widen)))

;; Define the mode.
;; When mode is activated,
;; narrow on current page.
;; When mode is deactivated,
;; widen.
(define-minor-mode page-mode
  "Toggle minor mode for display of pages."
  nil " Page" (page-mode-build-keymap)
  (page-mode-setup))

;; move to the next page
(defun next-page ()
  "Go to next page."
  (interactive)
  (widen)
  (forward-page)
  (narrow-to-page))

;; move to the previous page
(defun prev-page ()
  "Go to previous page."
  (interactive)
  (widen)
  (backward-page 2)
  (narrow-to-page))

(defun insert-page-split ()
  "Insert a page split at point.
The page-delimiter variable is assumed to point at a regexp
consisting of a string with a preceding ^.  The page split
is assumed to be that string on a line by itself. Leaves
point at the start of the new page."
  (let ((ipoint (point)))
    (forward-line 0)
    (if (not (= ipoint (point)))
	(progn
	  (goto-char ipoint)
	  (insert "\n"))))
  (insert (substring page-delimiter 1) "\n"))

(defun establish-page-mode ()
  "Enter page mode if not already there."
  (if (not (page-mode))
      (page-mode)))

(defun split-page ()
  "Split page at point.
Leaves point at start of new page."
  (interactive)
  (establish-page-mode)
  (insert-page-split)
  (widen)
  (narrow-to-page))

(defun new-page ()
  "Append a new page after the current page and enter it."
  (interactive)
  (establish-page-mode)
  (goto-char (point-max))
  (insert-page-split)
  (insert "\n")
  (forward-line -1)
  (widen)
  (narrow-to-page))

(defun insert-page ()
  "Insert a new page before the current page and enter it."
  (interactive)
  (establish-page-mode)
  (goto-char (point-min))
  (insert-page-split)
  (goto-char (point-min))
  (insert "\n")
  (forward-line -1)
  (widen)
  (narrow-to-page))

(defun first-page ()
  "Go to the first page."
  (interactive)
  (establish-page-mode)
  (widen)
  (goto-char (point-min))
  (narrow-to-page))

(defun last-page ()
  "Go to the last page."
  (interactive)
  (establish-page-mode)
  (widen)
  (goto-char (point-max))
  (narrow-to-page)
  (goto-char (point-min)))

(defun goto-page (PAGENUMBER)
  "Go to page PAGENUMBER."
  (interactive "p")
  (establish-page-mode)
  (widen)
  (goto-char (point-min))
  (if (> PAGENUMBER 1)
      (forward-page (- PAGENUMBER 1)))
  (narrow-to-page))

(defun page-global-undo (&optional ARG)
  "Undo even actions taken on other pages."
  (interactive "P")
  (widen)
  (advertised-undo ARG)
  (narrow-to-page))
