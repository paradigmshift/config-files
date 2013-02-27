;;; generic settings

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

;; loading elisp files 
(add-to-list 'load-path "/home/mo/dev/elisp")

(require 'ssl)

;; ido mode for buffer switching
(ido-mode t)

;; package manager
(package-initialize)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; emacs-nox specific
(global-set-key (kbd "^W") 'backward-kill-word)

;; hide menu bar
(menu-bar-mode -1)

;; cut and copy enable X clipboard
(setq x-select-enable-clipboard t)

;; set default directory
(setq default-directory "/home/mo/")

;; open links in rekonq
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "xulrunner ~/compiled/conkeror/application.ini")

;;;; global keymap settings
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

;; map window switching to <C-,> and <C-.>
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

;; binding C-x C-m to META-x
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; set C-w to 'backward-kill-word
(global-set-key (kbd "C-w") 'backward-kill-word)

;; set C-x C-k to kill-region
(global-set-key (kbd "C-x C-k") 'kill-region)

;; word-wrap 
(global-set-key (kbd "<f7>") 'toggle-truncate-lines)

;;;; global system settings

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

;; ibus settings 
;;(add-to-list 'load-path "~/dev/elisp/ibus-el-0.3.2")
;;(setq ibus-python-shell-command-name "python2")
;;(require 'ibus)
;;(add-hook 'after-init-hook 'ibus-mode-on)
;;(ibus-define-common-key ?\C-\s nil)
;;(ibus-define-common-key ?\C-/ nil)
;;(setq ibus-cursor-color '("red" "blue" "limegreen"))
;;(ibus-define-common-key ?\S-\s nil)
;;(global-set-key (kbd "S-SPC") 'ibus-toggle)
;;(ibus-define-common-key ?\C-j t)

;; loading auto-complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(auto-complete-mode t)

;; color theme
(add-to-list 'load-path "~/dev/elisp/color-theme-6.6.0")
(require 'color-theme)
(autoload 'color-theme-djcb-dark "djcb-dark" "new dark theme" t)
(require 'zenburn)
(eval-after-load  "color-theme"
  '(progn
    (color-theme-initialize)
    (color-theme-zenburn)))

;; resize window on loading 
(setq initial-frame-alist
  `((left . 70) (top . 30)
    (width . 500) (height . 100)))

;; highlight current line 
(global-hl-line-mode 1)

;; set default scrolling step
(setq scroll-step 1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default py-indent-offset 4)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-delay 0.01)
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(erc-modules (quote (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp track)))
 '(inhibit-startup-screen t)
 '(js2-basic-offset 2)
 '(python-python-command "python")
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(win-switch-idle-time 0.75))

(make-directory "~/.emacs.d/autosaves/" t)

;;;; mode specific settings
;;;; emacs lisp
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (local-set-key (kbd "RET") 'newline-and-indent)))

;;;; html
;; bind RET to newline-and-indent in HTML
(add-hook 'html-mode-hook' (lambda ()
  (local-set-key (kbd "RET") 'newline-and-indent)))

;;;; python
;; bind RET to py-newline-and-indent in Python 
(add-hook 'python-mode-hook ' (lambda ()
    (define-key python-mode-map "\C-m" 'newline-and-indent)))

;; Electric Pairs
(add-hook 'python-mode-hook
    (lambda ()
        (define-key python-mode-map "\"" 'electric-pair)
        (define-key python-mode-map "\'" 'electric-pair)
        (define-key python-mode-map "(" 'electric-pair)
        (define-key python-mode-map "[" 'electric-pair)
        (define-key python-mode-map "{" 'electric-pair)))
(defun electric-pair()
    "Insert character pair without sournding spaces"
    (interactive)
    (let (parens-require-spaces)
        (insert-pair)))

;;;; javascript
;; fixing indentation for JS2-mode
;; loading javascript2 mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(autoload 'espresso-mode "espresso")

(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq espresso-indent-level 2
        indent-tabs-mode nil
        c-basic-offset 2)
  (c-toggle-auto-state 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map [(meta control \;)] 
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map [(control meta q)] 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
    (js2-highlight-vars-mode))
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)

;;;; LISP
;; load SLIME 
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(require 'slime)
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;; Optionally, specify the lisp program you are using. Default is "lisp"
(setq inferior-lisp-program "sbcl -K full")
(slime-setup '(slime-repl))

;; set lisp-specific auto-complete
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)

;; bind RET to newline-and-indent
(add-hook 'slime-mode-hook (lambda()
  (local-set-key (kbd "RET") 'newline-and-indent)))

(add-hook 'slime-mode-hook 'forwardsexp-mode-hook)
(defun forwardsexp-mode-hook ()
  (define-key slime-mode-map (kbd "C-f") 'forward-sexp)
  (define-key slime-mode-map (kbd "C-M-f") 'forward-char)
  (define-key slime-mode-map (kbd "C-b") 'backward-sexp)
  (define-key slime-mode-map (kbd "C-M-b") 'backward-char))

;;;; org mode
;;Update the checkbox statistics in the current section.
;;This will find all statistic cookies like [57%] and [6/12] and update
;;them with the current numbers.  With optional prefix argument ALL,
;;do this for the whole buffer.
(require 'org-install)
(require 'org-publish)

(setq org-publish-project-alist
      '(
        ("org-mo"
         :base-directory "~/docs/mozartreina/"
         :base-extension "org"

         :publishing-directory "~/dev/jekyll/mozartreina/"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t)

        ("org-static-mo"
         :base-directory "~/docs/mozartreina/"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
         :publishing-directory "~/dev/jekyll/mozartreina/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("mo" :components ("org-mo" "org-static-mo"))))

(defun wicked/org-update-checkbox-count (&optional all)
  (interactive "P")
  (save-excursion
    (let* ((buffer-invisibility-spec (org-inhibit-invisibility))
	   (beg (condition-case nil
		    (progn (outline-back-to-heading) (point))
		  (error (point-min))))
	   (end (move-marker
		 (make-marker)
		 (progn (or (outline-get-next-sibling) ;; (1)
			    (goto-char (point-max)))
			(point))))
	   (re "\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)")
	   (re-box
	    "^[ \t]*\\(*+\\|[-+*]\\|[0-9]+[.)]\\) +\\(\\[[- X]\\]\\)")
	   b1 e1 f1 c-on c-off lim (cstat 0))
      (when all
	(goto-char (point-min))
	(or (outline-get-next-sibling) (goto-char (point-max))) ;; (2)
	(setq beg (point) end (point-max)))
      (goto-char beg)
      (while (re-search-forward re end t)
	(setq cstat (1+ cstat)
	      b1 (match-beginning 0)
	      e1 (match-end 0)
	      f1 (match-beginning 1)
	      lim (cond
		   ((org-on-heading-p)
		    (or (outline-get-next-sibling) ;; (3)
			(goto-char (point-max)))
		    (point))
		   ((org-at-item-p) (org-end-of-item) (point))
		   (t nil))
	      c-on 0 c-off 0)
	(goto-char e1)
	(when lim
	  (while (re-search-forward re-box lim t)
	    (if (member (match-string 2) '("[ ]" "[-]"))
		(setq c-off (1+ c-off))
	      (setq c-on (1+ c-on))))
	  (goto-char b1)
	  (insert (if f1
		      (format "[%d%%]" (/ (* 100 c-on)
					  (max 1 (+ c-on c-off))))
		    (format "[%d/%d]" c-on (+ c-on c-off))))
	  (and (looking-at "\\[.*?\\]")
	       (replace-match ""))))
      (when (interactive-p)
	(message "Checkbox statistics updated %s (%d places)"
		 (if all "in entire file" "in current outline entry")
		 cstat)))))
(defadvice org-update-checkbox-count (around wicked activate)
  "Fix the built-in checkbox count to understand headlines."
  (setq ad-return-value
	(wicked/org-update-checkbox-count (ad-get-arg 1))))

;; org refiling
(setq org-refile-targets (quote (("~/docs/notes/hack-notes" :maxlevel . 2))))

;; todo keywords
(setq org-todo-keywords
      '((sequence "In progress(p)" "|" "Done(d)")
        (sequence "Important(i)" "Priority(r)" "Sticky(s)")))

(setq org-todo-keyword-faces
      '(("Priority" . (:foreground "yellow" :weight bold))))


;; pdf to latex
(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")))  

;;;;ERC emacs irc client
(require 'erc)
(add-to-list 'load-path "~/dev/elisp/erc-5.3-extras")
(setq erc-auto-query 'window-noselect)
(require 'erc-nicklist)

;; autojoin
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net" "#lisp")))

(add-hook 'erc-after-connect
          '(lambda (SERVER NICK)
             (cond
              ((string-match "freenode\\.net" SERVER)
               (erc-message  "PRIVMSG" "NickServ identify forzaitalia")))))

(defun irc-maybe ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (erc :server "irc.freenode.net" :port 6667 :nick "momo-reina")))

;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;; Google chat
(setq jabber-account-list
    '(("momo.reina@gmail.com"
       (:network-server . "talk.google.com")
       (:connection-type . ssl))
      ("urban.yoga.journeys@gmail.com"
       (:network-server . "talk.google.com")
       (:connection-type . ssl))))

(global-set-key (kbd "C-x C-j C-c") 'jabber-connect-all)

;;;; Ace-Jump-mode
;; cloned from github repo because elpa version outdated
(add-to-list 'load-path "/home/mo/dev/elisp/ace-jump-mode/")

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; you can select the key you prefer to
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;;; DirTree
;; cloned from git, not available in elpa
(add-to-list 'load-path "/home/mo/dev/elisp/emacs-dirtree")
(require 'windata)
(require 'tree-mode)
(require 'dirtree)
(global-set-key (kbd "C-x t") 'dirtree)

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
              "/opensource/[Gmail].All Mail")) 
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
