;;; packages.el --- lisp Layer packages File for Spacemacs
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

(defvar lisp-packages
  '(
    ;; package lisps go here
    slime
    ac-slime
    paredit
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar lisp-excluded-packages '()
  "List of packages to exclude.")

(defun lisp/init-paredit ()
  (use-package paredit))

(defun lisp/init-slime ()
  (use-package slime
      :defer t
      :init (progn
              (setq inferior-lisp-program "sbcl -K full"))
      :config
      (progn
        (add-hook 'slime-mode-hook (lambda ()
                                     (slime-mode t)))
        (add-hook 'inferior-lisp-mode-hook (lambda ()
                                             (inferior-slime-mode t)))
        (setq lisp-indent-function 'common-lisp-indent-function)
        (slime-setup '(slime-repl))
        (autoload 'enable-paredit-mode "paredit" t)
        (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
        (add-hook 'lisp-mode-hook #'enable-paredit-mode)
        (add-hook 'slime-mode-hook #'enable-paredit-mode))
      :bind (("RET" . newline-and-indent)
             ("C-t" . transpose-sexps))))

(defun lisp/init-ac-slime ()
  (use-package ac-slime
      :defer t
      :config
      (progn
        (add-hook 'slime-mode-hook 'set-up-slime-ac))))
;; For each package, define a function lisp/init-<package-lisp>
;;
;; (defun lisp/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
