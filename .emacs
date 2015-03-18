(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-delay 0.01)
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(custom-safe-themes
   (quote
    ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(dired-bind-jump nil)
 '(ecb-options-version "2.40")
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp track)))
 '(inhibit-startup-screen t)
 '(js2-basic-offset 4)
 '(python-python-command "python")
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(win-switch-idle-time 0.75))

(defun osx ()
  (setq browse-url-browser-function 'browse-url-default-macosx-browser) ; open links in google-chrome
  (setq stack-trace-on-error t) ; strange ecb requirement
  (load-theme 'zenburn t)
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (add-to-list 'load-path "/Users/mozartreina/.emacs.d/ace-jump-mode") ; ace-jump-mode (ELPA version outdated)

  ;;;; C header files
  (defvar *c-headers*
    '("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/5.1/include"))
  ;; flyspell
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin")
                          '("/Library/Frameworks/Python.framework/Versions/3.3/bin")
                          '("~/bin/")))
  (setq slime-js-swank-command "/usr/local/bin/swank-js") ; osx path
                                        ; for swank-js
  (setq magit-emacsclient-executable "/usr/local/bin/emacsclient") ; osx path for working emacsclient

  ;;;; SSL cert
  (defvar *ssl-cert* "")
  )

(defun linux ()
  (add-to-list 'load-path "/home/mo/dev/elisp") ; loading elisp files (should consoldiate location for OSx and linux)
  (require 'ssl)
  (setq x-select-enable-clipboard t) ; cut and copy enable X clipboard
  (setq default-directory "/home/mo/") ; set default directory
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "conkeror") ; open links in conkeror

  ;;;; C header files
  (defvar *c-headers*
    '("/usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.2/include"
      "/usr/lib/gcc/x86_64-unknown-linux-gnu/4.8.2/include-fixed"))

  ;;;; SSL cert
  (defvar *ssl-cert* "/etc/ssl/certs/ca-certificates.crt")
  
  ;;;; bbdb
  (setq bbdb-file "~/.emacs.d/bbdb")
  (require 'bbdb)
  (bbdb-initialize)

  (setq 
   bbdb-pop-up-layout t                        ;; allow popups for addresses
   bbdb-electric t                        ;; be disposable with SPC
   bbdb-popup-window-size  1               ;; very small
   
   bbdb-mail-avoid-redundancy t ;; always use full name
   bbdb-add-name 2       ;; show name-mismatches 2 secs

   bbdb-add-mails t ;; add new addresses to existing...

   ;; ...contacts automatically
   bbdb-canonicalize-redundant-mails t     ;; x@foo.bar.cx => x@bar.cx

   bbdb-completion-list nil                 ;; complete on anything

   bbdb-complete-mail-allow-cycling t       ;; cycle through matches
   ;; this only works partially

   bbbd-message-caching-enabled t           ;; be fast
   bbdb-use-alternate-names t               ;; use AKA


   bbdb-elided-display t                    ;; single-line addresses

   ;; auto-create addresses from mail
   bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook   
   bbdb-ignore-message-alist ;; don't ask about fake addresses
   ;; NOTE: there can be only one entry per header (such as To, From)
   ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

   '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))
  ;; color theme
  (add-to-list 'load-path "~/dev/elisp/color-theme-6.6.0")
  (require 'zenburn-theme)
  (setq inferior-lisp-program "sbcl -K full"))

  ;;;; Add attachment through dired C-c RET C-a
  (require 'gnus-dired)
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
      (set-buffer buffer)
      (when (and (derived-mode-p 'message-mode)
          (null message-sent-message-via))
        (push (buffer-name buffer) buffers))))
      (nreverse buffers)))

  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(eval-after-load "eww"
  '(progn (define-key eww-mode-map "f" 'eww-lnum-follow)
    (define-key eww-mode-map "F" 'eww-lnum-universal)))

;;;; Default environment settings
(load "~/.emacs.environment")

;;;; Smart Mode line
(load "~/.emacs.sml")

;;;; MODE SPECIFIC SETTINGS

;;;; Org-mode
;; (load "~/.emacs.org")

;;;; mu4e
(load "~/.emacs.mail")

;;;; Python
(load "~/.emacs.python")

;;;; C
(load "~/.emacs.c")

;;;; HTML
(load "~/.emacs.html")

;;;; Lisp
(load "~/.emacs.lisp")

;;;; JS
(load "~/.emacs.javascript")

;;;; ERC
(load "~/.emacs.erc")

;;;; w3m
(load "~/.emacs.w3m")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(sml/filename ((t (:inherit sml/global :background "#383838" :foreground "gold" :weight bold))))
 '(sml/folder ((t (:inherit sml/global :background "#383838" :foreground "light gray" :weight bold))))
 '(sml/prefix ((t (:inherit sml/global :background "#383838" :foreground "deep sky blue" :weight extra-bold))))
 '(sml/vc ((t (:inherit sml/git :background "steel blue" :foreground "snow" :weight extra-bold :width semi-expanded))))
 '(sml/vc-edited ((t (:inherit sml/prefix :background "orange" :foreground "#330000" :weight ultra-bold :width semi-expanded)))))
