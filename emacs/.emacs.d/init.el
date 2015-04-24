;; -*-emacs-lisp-*-
;; TODO auto add comment leaders
;; TODO highlight 'TODO' and 'FIXME'
;; TODO get evil outline mode navigation
;; TODO bindings clean em up (symbol-value map)
;;; setup
;;;; custom vars
(defconst my-winp (eq system-type 'windows-nt) "Running windows?")
(defvar my-user-dir "~/.emacs.d/.user" "Temporary user storage dir.")
;;;; package init
(require 'package)
(setq
 package-enable-at-startup nil          ; we will manually initialize
 package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                    ("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("elpy" . "http://jorgenschaefer.github.io/packages/"))
 )
(package-initialize)

;; install use-package if not already
(unless (package-installed-p 'use-package)
  (package-refresh-contents) (package-install 'use-package))
(require 'use-package)
;; diminish builtin modes
(use-package diminish :ensure t :config (diminish 'visual-line-mode))

;;; general emacs config
;;;; interface
(setq
 frame-title-format "%b - emacs"        ; buffer name as frame title
 echo-keystrokes 0.1                    ; echo commands unfinished 1s
 x-underline-at-descent-line t          ; draw underline lower
 inhibit-splash-screen t                ; no splash screen
 initial-scratch-message nil            ; no scratch message
 inhibit-startup-echo-area-message t    ; no startup message
 window-combination-resize t            ; use proportional window resize
 ring-bell-function 'ignore             ; no alarms
 )
(global-font-lock-mode t)               ; syntax highlight everywhere
(global-linum-mode t)                   ; line numbers
(line-number-mode t)                    ; line num in modeline
(column-number-mode t)                  ; col num in modeline
(show-paren-mode)                       ; highlight matching parens
(hl-line-mode t)                        ; highlight current line
(file-name-shadow-mode t)               ; dim expanded filename parts
(tool-bar-mode -1)                      ; no toolbar
(scroll-bar-mode -1)                    ; no scroll bar
(blink-cursor-mode -1)                  ; no blinking cursor
;; manually set font for windows
(when my-winp (set-frame-font "Input-9"))
;; manually set headline font family
(set-face-attribute 'variable-pitch nil :family "Fantasque Sans Mono")
;;;; editing
(setq
 save-interprogram-paste-before-kill t  ; save clipboard contents to kill-ring
 initial-major-mode 'text-mode          ; start scratch in text mode
 ;; formatting
 require-final-newline t                ; newline at end of file
 sentence-end-double-space nil          ; double space is dumb
 comment-padding ""                     ; no padding in comments
 tab-width 4                            ; tabs with width 4
 tab-always-indent nil                  ; tapping tab inserts a tab (or spaces)
 ;; tab-stop locations
 tab-stop-list (number-sequence 4 100 4)
 )
(setq-default
 indent-tabs-mode nil                   ; turn tabs to spaces
 default-fill-column 80                 ; break lines beyond col
 )
(electric-indent-mode t)                ; auto indent
(abbrev-mode t)
;; indent on enter
(bind-key "RET" 'reindent-then-newline-and-indent)
;; word-based text wrapping for text modes only
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;;;;; autocorrect through abbrev
(setq abbrev-file-name (concat my-user-dir "/abbrev_defs.el" )
      save-abbrevs t                   ; save abbrevs when saving file
 )
(setq-default abbrev-mode t)
;;;; behavior
(setq
 delete-by-moving-to-trash t            ; use system trash for deletion
 find-file-visit-truename t             ; resolve symlinks
 ;; backups
 backup-by-copying t                    ; copy file into backup dir
 version-control t                      ; add version control numbers
 delete-old-versions t                  ; delete old backups silently
 kept-old-versions 5                    ; old versions to keep
 kept-new-versions 8                    ; new versions to keep
 ;; don't ask before allowing these variables
 safe-local-variable-values '((eval whitespace-mode nil) (buffer-read-only \.t))
 ;; backup directory
 backup-directory-alist `((".*" . (concat my-user-dir "/backups")))
 ;; auto save location
 auto-save-list-file-prefix (concat my-user-dir "/autosaves")
 auto-save-file-name-transforms `((".*" (concat my-user-dir "/autosaves" t)))
 ;; ignore case in completion
 read-file-name-completion-ignore-case t
 ;; specify own email address
 user-mail-address "stewart.g.kelly@gmail.com"
 )
(mouse-wheel-mode t)                    ; mouse wheel enabled
(semantic-mode t)                       ; language-aware editing cmds
(global-auto-revert-mode t)             ; auto refresh buffers
;; confirm before killing emacs in gui sessions
confirm-kill-emacs (if (window-system) 'yes-or-no-p nil)
;; always UTF-8 encoding
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
;;;; navigation
(fset 'yes-or-no-p 'y-or-n-p)           ; less annoying
(cd "~")                                ; start in home dir
(setq case-fold-search t                ; ignore case when searching
      line-move-visual t                ; visual line movement
      )
(goto-address-mode t)                   ; URL highlight and follow

;;;; custom commands
;;;;; because I'm lazy
(defun my-config ()
  "Opens local init.el file."
  (interactive) (find-file user-init-file))
;;;;; because other people are lazy
(defun my-cleanup ()
  "Perform a lot of stuff on whitespace."
  (interactive)
  (indent-region (point-min) (point-max))  ; indent properly
  (untabify (point-min) (point-max))       ; tabs are evil
  (whitespace-cleanup)                     ; whitespace stuff
  )
;;;;; open source files in read-only mode
(dir-locals-set-class-variables
 'emacs
 '((nil . ((buffer-read-only .t)
           (show-trailing-whitespace . nil)
           (tab-width . 8)
           (eval . (whitespace-mode nil))
           )))
 )
(dir-locals-set-directory-class (concat user-emacs-directory "elpa") 'emacs)
;;; packages
;;;; navigation
;;;;; dired
(use-package dired
  :config
  (use-package dired+ :ensure t)
  (setq dired-dwim-target t             ; target other dired window
        dired-recursive-copies 'always  ; recursive copy by default
        dired-recursive-deletes 'top    ; confirm recursive delete
        dired-listing-switches "-lha"   ; human-readable listing
        global-auto-revert-non-file-buffers t ; auto refresh buffer
        auto-revert-verbose nil         ; no warnings on refresh
        )
  ;; auto refresh dired buffer on changes
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  )
;;;;; saveplace
;; save cursor place in file and return to it
(use-package saveplace
  :init (setq-default save-place-file (concat my-user-dir "/places") save-place t))
;;;;; help-mode+
;; library names in *Help* buffers become links
(use-package help-mode+ :ensure t)
;;;;; nyan-mode
(use-package nyan-mode :ensure t
  :config
  ;; TODO get this working
  (defun nyan-start () "Start the nyan :D"
    (interactive)
    (nyan-start-animation)
    (setq nyan-wavy-trail t)
    ;; changed command to work with VLC player
    (start-process-shell-command "nyan-music" "nyan-music"
                                 (concat "vlc -L" +nyan-music+))
    )
  (defun nyan-stop () "Stop the nyan :("
    (interactive) (nyan-stop-animation) (nyan-stop-music))
  
  (setq nyan-wavy-trail t)
  (nyan-mode t)
  )
;;;;; popwin
;; uses popup window for minor buffers
(use-package popwin :ensure t
  :init (popwin-mode 1)
  :config
  (add-to-list 'popwin:special-display-config '("*Help*" :stick t :noselect t))
  (add-to-list 'popwin:special-display-config '("*Python Help*" :stick t :noselect t :height 20))
  )
;;;;; golden-ratio
;; resize windows according to golden ratio
(use-package golden-ratio :ensure t
  :config
  (setq golden-ratio-exclude-modes '("helm-mode"
                                     "magit-log-mode"
                                     "magit-reflog-mode"
                                     "magit-status-mode"
                                     ;;"eshell-mode"
                                     "dired-mode"))
  (golden-ratio-mode t)
  )
;;;;; projectile
;; project-based navigation
(use-package projectile :ensure t
  :config
  (setq projectile-indexing-method 'alien  ; elisp methods are slower
        projectile-enable-caching t        ; cache projectile indexes
        )
  (projectile-global-mode t)
  )
;;;;; helm
;; Fuzzy minibuffer completion.
(use-package helm :ensure t
  :diminish ""
  :bind (("C-c h" . helm-command-prefix)
         ;; helm replacements for builtins
         ("M-x" . helm-M-x)
         ("C-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         )
  :init
;;;;;; init
  (setq
   helm-move-to-line-cycle-in-source t     ; cycle on buffer end
   helm-ff-file-name-history-use-recentf t ; use recentf not history
   ;; fuzzy matching
   helm-M-x-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-recentf-fuzzy-match t
   helm-semantic-fuzzy-match t
   helm-imenu-fuzzy-match t
   helm-apropos-fuzzy-match t
  )
  :config
;;;;;; config
  (use-package helm-config)
  (use-package helm-projectile :ensure t
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)
    )

  (bind-keys :map helm-map
   ("\t" . helm-next-source)               ; complete selected cmd
   ("C-z" . helm-select-action)            ; list actions
   )
  
  (helm-autoresize-mode t)
  (helm-mode t)
  )
;;;; editing
;;;;; whitespace
;; highlight trailing whitespace and lines over certain length
(use-package whitespace
  :diminish ""
  :config
  (whitespace-mode t)
  (setq
   whitespace-line-column nil              ; use fill-column value
   ;; only highlight trailing whitespace and tails of long lines
   whitespace-style '(face trailing lines-tail)
   )
  )
;;;;; flyspell
;; automatic spell checking
(use-package flyspell
  :diminish ""
  :config
  ;; enables for appropriate modes
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  )
;;;;; highlight-numbers
;; highlight numbers in text
(use-package highlight-numbers :ensure t
  :init (highlight-numbers-mode t))
;;;;; highlight-quoted
;; highlight lisp quotes and quoted symbols
(use-package highlight-quoted :ensure t
  )
;;;;; rainbow-mode
;; Different from nyan mode. Highlight color codes with color code.
;;(use-package rainbow-mode :ensure :config (rainbow-mode t))
;;;;; smartparens
;; auto-add parenthesis
(use-package smartparens-config :ensure smartparens
  :diminish smartparens-mode
  :config
  (smartparens-global-mode)
  (show-smartparens-mode t)        ; highlight all matching pairs
  )
;;;;; auto-indent
;; automatically indent lines according to user settings
;; TODO: test out smarttab as adequate replacement
;;(use-package auto-indent-mode :ensure
;;  :init (setq auto-indent-on-visit-file t)
;;  :config (auto-indent-global-mode))
;;;;; org-mode
(use-package org :ensure t
  :config
  (setq
   org-startup-folded t                 ; start in folded view
   org-src-fontify-natively t           ; syntax highlight source in org buffer
   org-alphabetical-lists t             ; add alphabetical lists
   )
  )
;;;;; outshine
;; org-mode style using outline mode
(use-package outshine :ensure t
  :diminish outline-minor-mode
  :config
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  ;; modes to activate this for
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'vimrc-mode 'outline-minor-mode)
  (setq outshine-show-hidden-lines-cookies-p t)
  )
;;;;; undo-tree
;; branching tree for undo actions
(use-package undo-tree :ensure t
  :diminish ""
  :init (global-undo-tree-mode t)
  :config (bind-key "C-x u" 'undo-tree-visualize undo-tree-map)
  )
;;;;; flycheck
;; on-the-fly syntax checking
;; TODO disable checkdoc for elisp modes
(use-package flycheck :ensure t
  :config
  (global-flycheck-mode)
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  ;; display errors by popup
  (use-package flycheck-tip :ensure t :config (flycheck-tip-use-timer 'verbose))
  )
;;;;; company
;; autocompletion in code
(use-package company :ensure t
  :bind ("C-." . company-complete)
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config
  (setq
   company-idle-delay 0.2            ; delay before completions
   company-show-numbers t            ; show quick-access nums
   company-selection-wrap-around t   ; wrap back to start/finish when selecting
   )
  )
;;;;; language-specific packages
(use-package elpy :ensure t :config (elpy-enable))     ; python
(use-package vimrc-mode :ensure t
  :mode ".+\\.\\(vim\\|penta\\)$\\|[._]?\\(pentadactyl\\|vim\\)rc$")
;;;;;; emacs-lisp
(use-package elisp-slime-nav :ensure t
  :diminish ""
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()(elisp-slime-nav-mode t) (eldoc-mode t)))
  )
;;;; misc
;;;;; maylon
;; Z-machine text based adventure reader
;;(use-package maylon :ensure t)
;;;;; smart-mode-line
;; powerline isn't exactly stable
;;(use-package smart-mode-line :ensure t
;;  :config
;;  (setq sml/no-confirm-load-theme t) ; don't confirm themes
;;  (use-package smart-mode-line-powerline-theme :ensure t
;;    :config (setq sml/theme 'powerline))
;;  (sml/shorten-directory)            ; shorten dir name
;;  (sml/setup)
;;  )
;;;;; powerline
;; better status line
(use-package powerline :ensure t :config (powerline-evil-vim-theme))
;;;;; solarized color theme
;; only do this if solarized is installed
(load-theme 'solarized t)
;;(use-package zenburn-theme :ensure t :config (load-theme 'hc-zenburn t))

;;;;; eshell
;; emacs shell
(use-package eshell
  :bind (("<f3>" . eshell))
  :config
  (setenv "PAGER" "cat")                ; don't pause output
  ;;(defun (my-hl-disable (hl-line-mode -1)))
  ;;(add-hook 'eshell-mode-hook 'my-hl-disable)
  (setq eshell-highlight-prompt nil)
  ;;(use-package em-smart :ensure t
    ;;:config
    ;;(setq
     ;;eshell-where-to-jump 'begin
     ;;eshell-review-quick-commands nil
     ;;eshell-smart-space-goes-to-end t
     ;;)
    ;;)
  )
;;;;; magit
;; git command integration
(use-package magit :ensure t
  :bind (("<f12>" . magit-status))
  :config
  (setq magit-auto-revert-mode nil)     ; don't revert
  (setq magit-last-seen-setup-instructions "1.4.0") ; hide message
  )
;;;;; evil
;; vimlike keybindings and modal editing
(message "evil incoming")
(use-package evil :ensure t
  ;:bind (("C-h" . evil-window-left)
  ;       ("C-j" . evil-window-down)
  ;       ("C-k" . evil-window-up)
  ;       ("C-l" . evil-window-right)
  ;       )
;;;;;; config
  :config
;;;;;;; bindings
  ;; TODO multi-map bindings
  (dolist (map '(evil-normal-state-map evil-visual-state-map))
    (bind-keys :map (symbol-value map)
               ;; TODO get equivalent of map Y y$
               ([escape] . keyboard-quit)          ; esc quits
               ("SPC" . helm-M-x)                  ; helm
               ;; easier navigation
               ("L" . evil-last-non-blank) ("S" . evil-first-non-blank)
               ;; visual line movement
               ("j" . evil-next-visual-line) ("k" . evil-previous-visual-line)
               )
    )
;;;;;;;; jk to exit insert mode
  ;; TODO get source for this
  (bind-key "j" 'cofi/maybe-exit evil-insert-state-map) ; jk inserts input mode
  (evil-define-command cofi/maybe-exit ()
    :repeat change
    (interactive)
    (let ((modified (buffer-modified-p)))
      (insert "j")
      (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
                             nil 0.5)))
        (cond
         ((null evt) (message ""))
         ((and (integerp evt) (char-equal evt ?k))
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
         (t (setq unread-command-events (append unread-command-events
                                                (list evt))))
         ))))
;;;;;;;; esc quits
  ;; TODO get source for this
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivete it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)
      ))
  ;; bind the key to all minibuffer maps
  (dolist (map '(minibuffer-local-map
                 minibuffer-local-ns-map
                 minibuffer-local-completion-map
                 minibuffer-local-must-match-map
                 minibuffer-local-isearch-map))
    (bind-key "<escape>" 'minibuffer-keyboard-quit (symbol-value map)))
;;;;;;; evil-leader
  (use-package evil-leader :ensure t
    :init
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "w" 'save-buffer
      "q" 'kill-buffer-and-window
      "e" 'helm-find-files
      "b" 'helm-mini
      "x" 'eval-region
      "X" 'eval-buffer
      )
    :config (global-evil-leader-mode t)
    )
;;;;;;; evil-surround
  (use-package evil-surround :ensure t
    :config (global-evil-surround-mode t))
;;;;;;; evil-commentary
  (use-package evil-commentary :ensure t
    :diminish ""
    :config (evil-commentary-mode t))
;;;;;;; evil-org
  ;; evil org-mode bindings
  (use-package evil-org :ensure t)
;;;;;;; package
  (bind-keys
   :map package-menu-mode-map
   ("j" . next-line)
   ("k" . previous-line)
   ("/" . isearch-forward)
   ("?" . isearch-backward)
   ("n" . isearch-repeat-forward)
   ("N" . isearch-repeat-backward)
   ("<escape>" . isearch-cancel)
   )
;;;;;;; outline-mode bindings
;;  (evil-define-key 'normal outline-minor-mode-map
;;    "zh" 'outline-promote
;;    "zl" 'outline-demote
;;    "zk" 'outline-previous-visible-heading
;;    "zK" 'outline-previous-heading
;;    "zj" 'outline-next-visible-heading
;;    "zJ" 'outline-next-heading
;;    "za" 'outline-cycle
;;    "zA" 'outline-toggle-children
;;    "zm" 'outline-mark-subtree
;;    )
;;;;;;; elisp-slime-nav bindings
  (evil-define-key 'normal emacs-lisp-mode-map
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
;;;;;;; magit initial states
  (dolist (mode '(magit-mode magit-mode-status magit-mode-diff magit-mode-log))
    (evil-set-initial-state mode 'normal))
;;;;;;; misc
  (setq evil-want-fine-undo t           ; vim-style undo
   )
  (evil-mode t)
  )
