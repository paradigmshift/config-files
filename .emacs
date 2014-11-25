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
  ;; (require 'color-theme)
  (require 'zenburn-theme)
  ;; (eval-after-load  "color-theme"
  ;;   '(progn
  ;;      (color-theme-initialize)
  ;;      (zenburn-theme)))

  (setq inferior-lisp-program "sbcl -K full")

  ;;;; mu4e
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (require 'mu4e)

  (setq
   mu4e-maildir       "~/Mail"   ;; top-level Maildir
   mu4e-sent-messages-behavior 'delete
   mu4e-get-mail-command "offlineimap"   ;; or fetchmail, or ...
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
           (mu4e-trash-folder "/mozart/[Gmail].Trash"))
          ("yogamomo"
           (mu4e-sent-folder "/yogamomo/[Gmail].Sent Mail")
           (mu4e-drafts-folder "/yogamomo/[Gmail].Drafts")
           (user-mail-address "info@yoga-momo.com")
           (user-full-name "Ashtanga Yoga Center Osaka")
           (mu4e-trash-folder "/yogamomo/[Gmail].Trash"))))

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
                 ((string-match "mozart@mozartreina.com" from) "mozart")
                 ((string-match "info@yoga-momo.com" from) "yogamomo")))
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
  (add-hook 'mu4e-compose-mode-hook
            (defun my-add-cc ()
              "Add a Bcc: header."
              (save-excursion (message-add-header "Cc: \n"))))
  (add-hook 'mu4e-compose-mode-hook
            (defun my-add-bcc ()
              "Add a Bcc: header."
              (save-excursion (message-add-header "Bcc: \n"))))
  (add-hook 'message-send-mail-hook 'choose-msmtp-account)
  
  ;; (setq slime-js-swank-command "/usr/bin/swank-js") ; archlinux path for swank-js
  )

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

;;;; GENERIC SETTINGS
;; elpa settings
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; auto-install these on a fresh install
(defvar my-packages '(ace-jump-mode
                      ac-slime
                      twittering-mode
                      ac-c-headers
                      ecb
                      auto-complete
                      exec-path-from-shell
                      ido-ubiquitous
                      js2-mode
                      js2-refactor
                      magit
                      paredit
                      yasnippet
                      window-number
                      color-theme
                      autopair
                      bbdb))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; load appropriate platform settings
(if (eq system-type 'darwin)
    (osx)
  (linux))

;; loading auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(auto-complete-mode t)

;; key-chord
(add-to-list 'load-path "~/.emacs.d/keychord/")
(require 'key-chord)
(key-chord-mode 1)

;; evil-mode
(load-file "~/.emacs.evilmode")

;; load smex
(require 'smex)
(global-set-key (kbd "M-x") 'smex)

;;;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

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
(setq ido-enable-flex-matching t)
(ido-ubiquitous-mode 1) ; ido mode everywhere

(show-paren-mode 1) ; show matching
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

(global-set-key (kbd "C-x g") 'magit-status)

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
 '(ecb-options-version "2.40")
 '(erc-modules (quote (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp track)))
 '(inhibit-startup-screen t)
 '(js2-basic-offset 4)
 '(python-python-command "python")
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(win-switch-idle-time 0.75))

(make-directory "~/.emacs.d/autosaves/" t)

;;;; Twittering mode
(require 'twittering-mode)
(setq twittering-cert-file *ssl-cert*)
(setq twittering-icon-mode t)
(setq twittering-timer-interval 180)
(setq twittering-url-show-status nil)
(setq twittering-use-master-password t)

;;; autopair
(require 'autopair)
(defvar autopair-modes '(python-mode))
(defun turn-on-autopair-mode ()
  (autopair-mode 1))
(dolist (mode autopair-modes) (add-hook (intern (concat (symbol-name mode) "-hook")) 'turn-on-autopair-mode))

;;;; ace-jump-Mode
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
  "Ace jump back:-"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;;; Emacs Code Browser
(require 'ecb)
(require 'ecb-autoloads)

(setq ecb-layout-name "left3")
(setq ecb-show-sources-in-directories-buffer 'always)
(setq ecb-compile-window-height 12)

(global-set-key (kbd "C-x C-;") 'ecb-activate)
(global-set-key (kbd "C-x C-'") 'ecb-deactivate)

;;;; MODE SPECIFIC SETTINGS

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

