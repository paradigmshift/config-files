;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     env
     (erc :variables
          erc-autojoin-mode t)
     ;lisp
     common-lisp
     javascript
     sql
     sqlup
     haskell
     (ruby :variables
           ruby-enable-ruby-on-rails-support t)
     ruby-on-rails 
     ;; --------------------------------------------------------
     ;; Example of useful layers you may want to use right away
     ;; Uncomment a layer name and press C-c C-c to install it
     ;; --------------------------------------------------------
     (auto-completion :variables
                      auto-completion-enable-company-help-tooltip t
                      global-auto-complete-mode t
                      auto-complete-mode t)
     ;; better-defaults
     (git :variables
          git-gutter-use-fringe t
          git-enable-github-support t)
     ;; markdown
     (org :variables
          org-use-fast-todo-selection t
          org-agenda-compact-blocks t
          org-startup-indented t
          org-treat-S-cursor-todo-selection-as-state-change nil
          org-outline-path-complete-in-steps nil
          org-refile-allow-creating-parent-nodes 'confirm
          org-src-fontify-natively t
          org-latex-listings 'minted
          org-agenda-start-on-weekday nil)
     ;; syntax-checking
     mail
     )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progess in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to a .PNG file.
   ;; If the value is nil then no banner is displayed.
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner 'nil
   ;; t if you always want to see the changelog at startup
   dotspacemacs-always-show-changelog t
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         solarized-light
                         solarized-dark
                         leuven
                         monokai)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   ;; Default Ruby version manager
   ruby-version-manager 'rvm
   )
  ;; User initialization goes here
  (server-start)
  (add-to-load-path "/usr/share/emacs/site-lisp/mu4e")
  (add-to-list 'load-path "/home/mo/dev/elisp") ; loading elisp files (should
                                        ; consoldiate location for OSx
                                        ; and linux)
  (defun irc-connect ()
    (interactive)
    (erc :server "irc.freenode.net" :port 6667 :nick "momo-reina"))

  )

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  ;; ---------------------------------------------------------------------------
  ;; org-mode
  ;; ---------------------------------------------------------------------------
  (require 'ox-latex)

  (require 'org-protocol)

  (global-company-mode)

  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "conkeror")

  ;; (setq org-log-done t
  ;;       org-default-notes-file "~/notes/notes.org"
  ;;       org-agenda-files (list "~/notes/notes.org"
  ;;                              "~/notes/videos.org"
  ;;                              "~/notes/books.org"
  ;;                              "~/dev/lisp/shala-sys/shala-sys.org")
  ;;       org-refile-targets (quote ((nil :maxlevel . 9)
  ;;                                  ("~/notes/books.org" :maxlevel . 9)
  ;;                                  ("~/notes/videos.org" :maxlevel . 9)
  ;;                                  ("~/dev/lisp/shala-sys/shala-sys.org" :maxlevel . 9))))

  ;; (setq org-capture-templates
  ;;       '(("c" "Capture" entry (file+headline "~/notes/notes.org" "Captured")
  ;;          "* ⛬ %? %i %a %:url %T")
  ;;         ("n" "Note" entry (file+headline "~/notes/notes.org" "Captured")
  ;;          "* ⛬ %? %T")
  ;;         ("w" "" entry ;; 'w' for 'org-protocol'
  ;;          (file+headline "~/notes/notes.org" "Captured")
  ;;          "* ⛬ %? Source: %c, %u %i")))

  ;; (setq org-protocol-default-template-key "w")

  ;; (setq org-todo-keywords (quote ((sequence "⛬(c)" "►(n)" "◉(f)" "⌛(w)" "|" "✔(d)")))
  ;;       org-todo-keyword-faces '(("⛬" . "green")
  ;;                                ("⌛" . "orange")
  ;;                                ("◉" . "gray")
  ;;                                ("✔" . (:foreground "blue" :weight bold))))

  ;; (setq org-agenda-custom-commands
  ;;       '((" " "Agenda"
  ;;          ((agenda "" ((org-agenda-ndays 7)))
  ;;           (todo "⛬" ((org-agenda-overriding-header "Captured")
  ;;                      (org-agenda-files '("~/notes/notes.org"))))
  ;;           (tags "►/-@projects-✔" ((org-agenda-overriding-header "Next Actions")
  ;;                                   (org-agenda-files '("~/notes/notes.org"))
  ;;                                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "Next"))
  ;;                                   (org-tags-match-list-sublevels 'indented)
  ;;                                   (org-agenda-todo-list-sublevels 3)))
  ;;           (todo "⌛" ((org-agenda-overriding-header "Waiting")
  ;;                      (org-agenda-files '("~/notes/notes.org"))))
  ;;           (tags "@roby" ((org-agenda-overriding-header "Delegated")))
  ;;           (tags "-@projects/◉" ((org-agenda-overriding-header "Deferred")
  ;;                                 (org-agenda-files '("~/notes/notes.org"))))
  ;;           (tags "@projects" ((org-agenda-overriding-header "Projects")
  ;;                              (org-tags-match-list-sublevels 'indented)
  ;;                              (org-agenda-files '("~/notes/notes.org"))
  ;;                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "Projects"))
  ;;                              (org-agenda-todo-list-sublevels 2)))
  ;;           (tags "@projects/►" ((org-agenda-overriding-header "Shala-Sys Next Tasks")
  ;;                                (org-agenda-files '("~/dev/lisp/shala-sys/shala-sys.org")))))
  ;;          ((org-agenda-compact-blocks t))
  ;;          nil)))

  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


  (defun org-remove-headlines (backend)
    "Remove headlines with :no_title: tag."
    (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                     "no_title"))

  (add-hook 'org-export-before-processing-hook #'org-remove-headlines)
  ;; ---------------------------------------------------------------------------
  ;; erc
  ;; ---------------------------------------------------------------------------
  (add-hook 'erc-after-connect
            '(lambda (SERVER NICK)
               (cond
                ((string-match "freenode\\.net" SERVER)
                 (erc-message  "PRIVMSG" "NickServ identify manglavite")))))

  (setq erc-autojoin-channels-alist
        '((".*\\.freenode.net" "#lisp" "#sbcl" "#quicklisp" "#ruby")))

  (setq erc-prompt-for-nickserv-password nil)

  ;; ---------------------------------------------------------------------------
  ;; SQL
  ;; ---------------------------------------------------------------------------

  (add-hook 'sql-mode-hook 'sqlup-mode)
  ;; ---------------------------------------------------------------------------
  ;; javascript
  ;; ---------------------------------------------------------------------------

  (add-hook 'js2-mode 'nodejs-repl)
  (add-hook 'js2-mode-hook (lambda ()
                             (define-key js2-mode-map (kbd "C-x C-e") 'nodejs-repl-eval-dwim)))

  ;; paredit navigation
  (add-hook 'paredit-mode-hook (lambda ()
                                 (define-key evil-normal-state-map (kbd "C-l") 'paredit-forward)
                                 (define-key evil-insert-state-map (kbd "C-l") 'paredit-forward)
                                 (define-key evil-normal-state-map (kbd "C-h") 'paredit-backward)
                                 (define-key evil-insert-state-map (kbd "C-h") 'paredit-backward)
                                 (define-key evil-insert-state-map (kbd "C-t") 'transpose-sexps)
                                 (define-key evil-normal-state-map (kbd "C-j") 'paredit-forward-down)
                                 (define-key evil-insert-state-map (kbd "C-j") 'paredit-forward-down)
                                 (define-key evil-normal-state-map (kbd "M-.") 'slime-edit-definition-other-frame)
                                 (define-key evil-insert-state-map (kbd "M-.") 'slime-edit-definition-other-frame))))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
