;;;; Elisp
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (local-set-key (kbd "RET") 'newline-and-indent)))

;;;; CL
;; load SLIME 
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(setq lisp-indent-function 'common-lisp-indent-function)

(slime-setup '(slime-repl))

;; set lisp-specific auto-complete
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)

(add-hook 'slime-mode-hook (lambda()
  (local-set-key (kbd "RET") 'newline-and-indent)))

(defun slimekbd-mode-hook ()
  (define-key slime-mode-map (kbd "C-t") 'transpose-sexps)
  (define-key slime-mode-map (kbd "C-M-t") 'transpose-chars)
  )

(add-hook 'slime-mode-hook 'slimekbd-mode-hook)

;; paredit
(autoload 'enable-paredit-mode "paredit" t)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)
(add-hook 'slime-mode-hook #'enable-paredit-mode)
(define-key slime-mode-map [(?\()] 'paredit-open-list)
(define-key slime-mode-map [(?\))] 'paredit-close-list)
