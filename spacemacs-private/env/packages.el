;;; packages.el --- env Layer packages File for Spacemacs
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

(defvar env-packages
  '(
    ;; package envs go here
    twittering-mode
    tree-mode
    markdown-mode
    nodejs-repl
    bongo
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar env-excluded-packages '()
  "List of packages to exclude.")

(defun env/init-twittering-mode ()
  (use-package twittering-mode
      :config (progn
                (setq twittering-cert-file *ssl-cert*
                      twittering-icon-mode t
                      twittering-timer-interval 180
                      twittering-url-show-status nil
                      twittering-use-master-password t))))

(defun env/init-bongo ()
  (use-package bongo
    :defer t))

(defun env/init-tree-mode ()
  (use-package tree-mode))

(defun env/init-markdown-mode ()
  (use-package markdown-mode))

(defun env/init-nodejs-repl ()
  (use-package nodejs-repl
    :defer t
    :config (progn
              (require 'nodejs-repl-eval))))
;; For each package, define a function env/init-<package-env>
;;
;; (defun env/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
