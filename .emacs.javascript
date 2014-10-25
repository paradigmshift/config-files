(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(add-hook 'js-mode-hook 'js2-minor-mode)

;; syntax highlight amount
(setq js2-highlight-level 3)

;; initalize tern and auto-complete
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
     (tern-ac-setup)))

;; fix Tern refresh bug
(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

;; nodejs path
(setenv "PATH" (concat "/home/mo/.nvm/v0.11.14/bin:" (getenv "PATH")))
(setq exec-path (append exec-path '("/home/mo/.nvm/v0.11.14/bin")))
