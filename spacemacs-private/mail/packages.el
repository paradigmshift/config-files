;;; packages.el --- mail Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar mail-packages
  '(
    ;; package mails go here
    bbdb
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar mail-excluded-packages '()
  "List of packages to exclude.")

(defun mail/init-bbdb ()
  (use-package bbdb
    :config
    (progn
      (setq bbdb-pop-up-layout t           ;; allow popups for addresses
            bbdb-electric t                ;; be disposable with SPC
            bbdb-popup-window-size 1       ;; very small
            bbdb-mail-avoid-redundancy t   ;; always use full name
            bbdb-add-name 2                ;; show name-mismatches 2 secs
            bbdb-add-mails t ;; add new addresses to existing contacts automatically
            bbdb-canonicalize-redundant-mails t  ;; x@foo.bar.cx => x@bar.cx
            bbdb-completion-list nil             ;; complete on anything
            bbdb-complete-mail-allow-cycling t   ;; cycle through matches this
            ;; only works partially
            bbbd-message-caching-enabled t ;; be fast
            bbdb-use-alternate-names t     ;; use AKA
            bbdb-elided-display t          ;; single-line addresses
            ;; auto-create addresses from mail
            bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
            bbdb-ignore-message-alist ;; don't ask about fake addresses NOTE: there
            ;; can be only one entry per header (such as To, From)
            ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

            '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))
      (defun gnus-dired-mail-buffers ()
        "Return a list of active message buffers."
        (let (buffers)
          (save-current-buffer
            (dolist (buffer (buffer-list t))
              (set-buffer buffer)
              (when (and (derived-mode-p 'message-mode)
                         (null message-sent-message-via))
                (push (buffer-name buffer) buffers))))
          (nreverse buffers))))))

(defun mail/init-gnus-dired ()
  (use-package gnus-dired
      :config
      (progn
        (setq gnus-dired-mail-mode 'mu4e-user-agent)
        (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))))

;; For each package, define a function mail/init-<package-mail>
;;
;; (defun mail/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
