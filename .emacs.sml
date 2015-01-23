(require 'smart-mode-line-powerline-theme)

(sml/setup)

;; change matching directory names to prefixes
(mapcar #'(lambda (regexp)
            (add-to-list 'sml/replacer-regexp-list regexp t))
        '(("^~/dev/lisp/" ":LispDev:")
          ("^~/dev/js/" ":JSDev:")
          ("^~/dev/python/" ":PyDev:")
          ("^~/dev/C/" ":CDev:")
          ("^~/dev/elisp/" ":Elisp:")
          ("^~/" ":Home:")))

;; hide following minor modes from modeline
(mapcar #'(lambda (min-mode)
            (add-to-list 'rm-blacklist min-mode))
        '(" Undo-Tree"
          " adoc"
          " AC"))
