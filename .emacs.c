;;;; C
(defun c-mode-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (mapc #'(lambda (d)
            (add-to-list 'achead:include-directories d))
        *c-headers*))

(add-hook 'c-mode-hook (lambda ()
                         (c-mode-init)
                         ( c-set-style "k&r")
                         (electric-pair-mode 1)
                         (local-set-key (kbd "RET") 'newline-and-indent)))
