;;; extensions.el --- env Layer extensions File for Spacemacs
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

(defvar env-pre-extensions
  '(
    ;; pre extension envs go here
    ssl
    )
  "List of all extensions to load before the packages.")

(defvar env-post-extensions
  '(
    ;; post extension envs go here
    )
  "List of all extensions to load after the packages.")

(defun env/init-ssl ()
  (use-package ssl
      :init (progn
              (add-to-list 'load-path "/home/mo/dev/elisp"))
      :config (progn
                (defvar *ssl-cert* "/etc/ssl/certs/ca-certificates.crt"))))

;; For each extension, define a function env/init-<extension-env>
;;
;; (defun env/init-my-extension ()
;;   "Initialize my extension"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
