;; Try downloading package lists from the major archives.  This may fail
;; the first time on a system whose ELPA GPG key is expired, so this
;; whole file is called again from phase2.el after the master shell
;; script SETUP.sh has had the chance to update the key.

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org-elpa" . "https://orgmode.org/elpa/")
        ))

(package-initialize)
(package-refresh-contents)
