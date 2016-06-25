;;; extensions.el --- mail Layer extensions File for Spacemacs
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

(defvar mail-pre-extensions
  '(
    ;; pre extension mails go here
    mu4e
    )
  "List of all extensions to load before the packages.")

(defvar mail-post-extensions
  '(
    ;; post extension mails go here
    )
  "List of all extensions to load after the packages.")

(defun mail/init-mu4e ()
  (use-package mu4e
      :config (progn
                (when (fboundp 'imagemagick-register-types)
                  (imagemagick-register-types))

                (require 'gnus-dired)

                (setq
                 mu4e-maildir "~/Mail"
                 mu4e-sent-messages-behavior 'delete
                 mu4e-get-mail-command "offlineimap" ;; or fetchmail, or ...
                 mu4e-update-interval 600
                 message-kill-buffer-on-exit t
                 mu4e-view-show-images t
                 mu4e-image-max-width 800
                 mu4e-html2text-command "html2text -utf8 -width 72"
                 mu4e-html2text-command "w3m -dump -T text/html"
                 w3m-command "/usr/bin/w3m"
                 mu4e-view-prefer-html t
                 mu4e-view-show-addresses t)
                ;; list of mail account folders
                (setq mo-mu4e-account-alist
                      '(("urban"
                         (mu4e-sent-folder "/urban/[Gmail].Sent Mail")
                         (mu4e-drafts-folder "/urban/[Gmail].Drafts")
                         (user-mail-address "urban.yoga.journeys@gmail.com")
                         (user-full-name "Mo Reina")
                         (mu4e-trash-folder "/urban/[Gmail].Trash"))
                        ("momo"
                         (mu4e-sent-folder "/momo/[Gmail].Sent Mail")
                         (mu4e-drafts-folder "/momo/[Gmail].Drafts")
                         (user-mail-address "momo.reina@gmail.com")
                         (user-full-name "momo reina")
                         (mu4e-trash-folder "/momo/[Gmail].Trash"))
                        ("opensource"
                         (mu4e-sent-folder "/opensource/[Gmail].Sent Mail")
                         (mu4e-drafts-folder "/opensource/[Gmail].Drafts")
                         (user-mail-address "mo.opensource@gmail.com")
                         (user-full-name "Mozart Reina")
                         (mu4e-trash-folder "/opensource/[Gmail].Trash"))
                        ("mozart"
                         (mu4e-sent-folder "/mozart/[Gmail].Sent Mail")
                         (mu4e-drafts-folder "/mozart/[Gmail].Drafts")
                         (user-mail-address "mozart@mozartreina.com")
                         (user-full-name "Mozart Reina")
                         (mu4e-trash-folder "/mozart/[Gmail].Trash"))
                        ("yogamomo"
                         (mu4e-sent-folder "/yogamomo/[Gmail].Sent Mail")
                         (mu4e-drafts-folder "/yogamomo/[Gmail].Drafts")
                         (user-mail-address "info@yoga-momo.com")
                         (user-full-name "Ashtanga Yoga Center Osaka")
                         (mu4e-trash-folder "/yogamomo/[Gmail].Trash"))))

                (setq message-send-mail-function 'message-send-mail-with-sendmail
                      sendmail-program "/usr/bin/msmtp")
                (add-hook 'message-send-mail-hook 'choose-msmtp-account)

                (setq gnus-dired-mail-mode 'mu4e-user-agent)
                (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

                (setq mu4e-refile-folder
                      (lambda (msg)
                        (if msg
                            (cond
                              ((string-match "urban" (mu4e-message-field msg :maildir))
                               "/urban/[Gmail].All Mail")
                              ((string-match "momo" (mu4e-message-field msg :maildir))
                               "/momo/[Gmail].All Mail")
                              ((string-match "opensource" (mu4e-message-field msg :maildir))
                               "/opensource/[Gmail].All Mail")
                              ((string-match "mozart" (mu4e-message-field msg :maildir))
                               "/mozart/[Gmail].All Mail")
                              ((string-match "yogamomo" (mu4e-message-field msg :maildir))
                               "/yogamomo/[Gmail].All Mail"))
                            "/urban/[Gmail].All Mail")))

                (setq mu4e-maildir-shortcuts
                      '(("/urban/INBOX" . ?u)
                        ("/momo/INBOX" . ?M)
                        ("/mozart/INBOX" .?p)
                        ("/yogamomo/INBOX" .?y)))

                (setq message-sendmail-envelope-from 'header)
                (add-hook 'mu4e-compose-pre-hook 'mo-mu4e-set-account)
                (add-hook 'mu4e-compose-mode-hook
                          (defun my-add-cc ()
                            "Add a Bcc: header."
                            (save-excursion (message-add-header "Cc: \n"))))

                (add-hook 'mu4e-compose-mode-hook
                          (defun my-add-bcc ()
                            "Add a Bcc: header."
                            (save-excursion (message-add-header "Bcc: \n")))))))

;; For each extension, define a function mail/init-<extension-mail>
;;
;; (defun mail/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
