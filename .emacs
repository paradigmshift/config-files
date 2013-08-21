(defun osx ()
  ;; open links in google-chrome
  (setq browse-url-browser-function 'browse-url-default-macosx-browser)
  ;; strange ecb requirement
  (setq stack-trace-on-error t)
  ;; load zenburn on init
  (load-theme 'zenburn t)
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  ;; ace-jump-mode
  (add-to-list 'load-path "/Users/mozartreina/.emacs.d/ace-jump-mode")
  ;; flyspell
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin")))
  
  (setq slime-js-swank-command "/usr/local/bin/swank-js") ; osx path for swank-js
  )

(defun linux ()
  ;; loading elisp files 
  (add-to-list 'load-path "/home/mo/dev/elisp")
  (require 'ssl)
  ;; cut and copy enable X clipboard
  (setq x-select-enable-clipboard t)
  ;; set default directory
  (setq default-directory "/home/mo/")
  ;; ace-jump-mode
  (add-to-list 'load-path "~/dev/elisp/ace-jump-mode")
  ;; open links in conkeror
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "run-conkeror")
      ;;;; bbdb
  (add-to-list 'load-path "~/dev/elisp/bbdb")
  (require 'bbdb)
  (bbdb-initialize)

  (setq 
   bbdb-offer-save 1                        ;; 1 means save-without-asking

   
   bbdb-use-pop-up t                        ;; allow popups for addresses
   bbdb-electric-p t                        ;; be disposable with SPC
   bbdb-popup-target-lines  1               ;; very small
   
   bbdb-dwim-net-address-allow-redundancy t ;; always use full name
   bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs

   bbdb-always-add-address t                ;; add new addresses to existing...
   ;; ...contacts automatically
   bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx

   bbdb-completion-type nil                 ;; complete on anything

   bbdb-complete-name-allow-cycling t       ;; cycle through matches
   ;; this only works partially

   bbbd-message-caching-enabled t           ;; be fast
   bbdb-use-alternate-names t               ;; use AKA


   bbdb-elided-display t                    ;; single-line addresses

   ;; auto-create addresses from mail
   bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook   
   bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
   ;; NOTE: there can be only one entry per header (such as To, From)
   ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

   '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))
  ;; color theme
  (add-to-list 'load-path "~/dev/elisp/color-theme-6.6.0")
  (require 'color-theme)
  (autoload 'color-theme-djcb-dark "djcb-dark" "new dark theme" t)
  (require 'zenburn)
  (eval-after-load  "color-theme"
    '(progn
       (color-theme-initialize)
       (color-theme-zenburn)))

      ;;;; inferior lisp
  (setq inferior-lisp-program "sbcl -K full")

      ;;;; mu4e
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (require 'mu4e)

  (setq
   mu4e-maildir       "~/Mail"   ;; top-level Maildir
   mu4e-sent-messages-behavior 'delete
   mu4e-get-mail-command "email-update.sh"   ;; or fetchmail, or ...
   mu4e-update-interval 600
   message-kill-buffer-on-exit t
   mu4e-view-show-images t
   mu4e-image-max-width 800
   mu4e-html2text-command "html2text -utf8 -width 72"
   mu4e-html2text-command "w3m -dump -T text/html"
   w3m-command "/usr/bin/w3m"
   mu4e-view-prefer-html t
   )

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
           (mu4e-trash-folder "/mozart/[Gmail].Trash"))))

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
                "/mozart/[Gmail].All Mail")) 
            "/urban/[Gmail].All Mail")))


  (setq message-send-mail-function 'message-send-mail-with-sendmail
        sendmail-program "/usr/bin/msmtp")

  (defun choose-msmtp-account ()
    (if (message-mail-p)
        (save-excursion
          (let*
              ((from (save-restriction
                       (message-narrow-to-headers)
                       (message-fetch-field "from")))
               (account
                (cond
                 ((string-match "urban.yoga.journeys@gmail.com" from) "urban")
                 ((string-match "momo.reina@gmail.com" from) "momo")
                 ((string-match "mo.opensource@gmail.com" from) "opensource")
                 ((string-match "mozart@mozartreina.com" from) "mozart")))
               (account-vars (cdr (assoc account mo-mu4e-account-alist))))
            (mapc #'(lambda (var)
                      (set (car var) (cadr var)))
                  account-vars)
            (setq message-sendmail-extra-arguments (list '"-a" account))))))

  (defun mo-mu4e-set-account () 
    "Set the account for composing a message." 
    (let* ((account
            (if mu4e-compose-parent-message
                (let ((maildir (mu4e-msg-field mu4e-compose-parent-message :maildir))) 
                  (string-match "/\\(.*?\\)/" maildir) 
                  (match-string 1 maildir))
              (completing-read (format "Compose with account: (%s) " 
                                       (mapconcat #'(lambda (var) (car var)) mo-mu4e-account-alist "/")) 
                               (mapcar #'(lambda (var) (car var)) mo-mu4e-account-alist) 
                               nil t nil nil (caar mo-mu4e-account-alist))))
           (account-vars (cdr (assoc account mo-mu4e-account-alist))))
      (if account-vars 
          (mapc #'(lambda (var) 
                    (set (car var) (cadr var))) 
                account-vars))))

  (setq message-sendmail-envelope-from 'header)
  (add-hook 'mu4e-compose-pre-hook 'mo-mu4e-set-account)
  (add-hook 'message-send-mail-hook 'choose-msmtp-account)
  (global-set-key "\M-x"
   (lambda ()
     (interactive)
     (call-interactively
      (intern
       (ido-completing-read "M-x " (all-completions "" obarray 'commandp)))))) ; ido-mode for M-x


  
  (setq slime-js-swank-command "/usr/bin/swank-js") ; archlinux path for swank-js
  )

;;;; GENERIC SETTINGS
;; elpa settings
(require 'package)
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(if (eq system-type 'darwin)
    (osx)
  (linux))

;; loading auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(auto-complete-mode t)
    
;; open files over ssh
(require 'tramp)
(setq tramp-default-method "ssh")

;; transparent buffer
(set-frame-parameter (selected-frame) 'alpha '(85 50))
(add-to-list 'default-frame-alist '(alpha 85 50))

(eval-when-compile (require 'cl))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(85 50))))

(global-set-key (kbd "C-c t") 'toggle-transparency)

;; side by side windows
(defun th-display-buffer (buffer force-other-window)
  "If BUFFER is visible, select it.

If it's not visible and there's only one window, split the
current window and select BUFFER in the new window. If the
current window (before the split) is more than 165 columns wide,
split horizontally, else split vertically.

If the current buffer contains more than one window, select
BUFFER in the least recently used window.

This function returns the window which holds BUFFER.

FORCE-OTHER-WINDOW is ignored."
  (or (get-buffer-window buffer)
      (if (one-window-p)
          (let ((new-win (if (> (window-width) 40)
                             (split-window-horizontally)
                           (split-window-vertically))))
            (set-window-buffer new-win buffer)
            new-win)
        (let ((new-win (get-lru-window)))
          (set-window-buffer new-win buffer)
          new-win))))

(setq display-buffer-function 'th-display-buffer)

(ido-mode t) ; ido mode for buffer switching
(menu-bar-mode -1) ; hide menu bar

;; window-number for window swithcing
(autoload 'window-number-mode "window-number"
"A global minor mode that enables selection of windows according to
numbers with the C-x C-j prefix. Another mode,
`window-number-meta-mode` enables the use of the M- prefix."
t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (window-number-mode 1)))

(add-hook 'window-number-mode-hook
          (lambda ()
            (window-number-define-keys window-number-mode-map "C-x C-j ")))

;; insert line above current with proper indentation
(defun smart-open-line-above ()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-O") 'smart-open-line-above)

;; map top level folding to F1
(defun mo-toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(global-set-key [f1] 'mo-toggle-selective-display)

;; map window splitting to META 1, 2, 3, 0
(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit argument
(global-set-key (kbd "M-2") 'split-window-vertically) ; was digit argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit argument
(global-set-key (kbd "M-o") 'other-window) ; was facemenu-keymap

;; unmap C-x 0, 1, 2, 3, o
(global-unset-key (kbd "C-x 3"))
(global-unset-key (kbd "C-x 2"))
(global-unset-key (kbd "C-x 1"))
(global-unset-key (kbd "C-x 0"))
(global-unset-key (kbd "C-x o"))

;; map window switching to META <left> and <right>
(defun select-next-window()
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  (interactive)
  (select-window (previous-window)))

(global-set-key (kbd "C-.") 'select-next-window)
(global-set-key (kbd "C-,") 'select-previous-window)

;; map buffer switching to C-X C-, and C-.
(global-set-key (kbd "C-x C-.") 'next-buffer)
(global-set-key (kbd "C-x C-,") 'previous-buffer)

(global-set-key (kbd "C-x C-m") 'execute-extended-command) ; binding C-x C-m to META-x
(global-set-key (kbd "C-w") 'backward-kill-word) ; set C-w to 'backward-kill-word
(global-set-key (kbd "C-x C-k") 'kill-region) ; set C-x C-k to kill-region
(global-set-key (kbd "<f7>") 'toggle-truncate-lines) ; word-wrap

;; resize window on loading 
(setq initial-frame-alist
  `((left . 70) (top . 30)
    (width . 160) (height . 50)))

(global-hl-line-mode 1) ; highlight current line 
(setq scroll-step 1) ; set default scrolling step
(global-hl-line-mode 1) ; highlight current line 

(setq-default indent-tabs-mode nil) ; space for tabs
(setq-default tab-width 4) ; width for tab

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-delay 0.01)
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(dired-bind-jump nil)
 '(erc-modules (quote (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp track)))
 '(inhibit-startup-screen t)
 '(js2-basic-offset 4)
 '(python-python-command "python")
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(win-switch-idle-time 0.75))

(make-directory "~/.emacs.d/autosaves/" t)

;;;; MODE SPECIFIC SETTINGS
;;;; emacs lisp
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (local-set-key (kbd "RET") 'newline-and-indent)))

;;;; C
(add-hook 'c-mode-hook (lambda ()
                         ( c-set-style "k&r")
                         (electric-pair-mode 1)))

(add-hook 'c-mode-hook' (lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

;;;; html
;; bind RET to newline-and-indent in HTML
(add-hook 'html-mode-hook' (lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

;;;; LISP
;; load SLIME 
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(add-to-list 'load-path "~/.emacs.d/slime-js")
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
(setq lisp-indent-function 'common-lisp-indent-function)

(slime-setup '(slime-js slime-repl))

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

;;;; javascript
(require 'autopair)
(require 'js2-refactor)
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'load-path "~/.emacs.d/")

(require 'setup-slime-js)

(slime-setup '(slime-js slime-repl))

(setq slime-js-swank-args '())

(js2r-add-keybindings-with-prefix "C-c C-m")
(global-set-key [f5] 'slime-js-reload)

(add-hook 'js2-mode-hook (lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (slime-js-minor-mode 1)
  (setq slime-use-autodoc-mode nil)
  (autopair-mode 1)))

;;;; python
(setq py-install-directory "~/.emacs.d/python-mode.el-6.0.12")
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(setq-default py-indent-offset 4)

(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")

(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)

(add-hook 'python-mode-hook ' (lambda ()
                                (local-set-key (kbd "RET") 'newline-and-indent)
                                (autopair-mode 1)))

(setq py-set-complete-keymap-p t)

;;;; ace-jump-Mode
;; cloned from git because elpa version outdated

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; 
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;;;ERC emacs irc client
(require 'erc)
(add-to-list 'load-path "~/dev/elisp/erc-5.3-extras")
(setq erc-auto-query 'window-noselect)

;; autojoin
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net" "#lisp" "#sbcl" "#lispweb")))

(add-hook 'erc-after-connect
          '(lambda (SERVER NICK)
             (cond
              ((string-match "freenode\\.net" SERVER)
               (erc-message  "PRIVMSG" "NickServ identify forzaitalia")))))

(defun irc-connect ()
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "momo-reina"))

;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
