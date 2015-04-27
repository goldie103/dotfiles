;; -*-mode: emacs-lisp-*-
;; TODO auto add comment leaders
;;; setup
(require 'package)
(setq
 load-prefer-newer t                    ; don't load outdated byte code
 package-enable-at-startup nil          ; we will manually initialize
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("melpa" . "http://melpa.milkbox.net/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)

;; install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents) (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)             ; log message after loading packages

;; diminish builtin modes
(use-package delight :ensure t)
(use-package diminish :ensure t
  :config
  (diminish 'visual-line-mode))

;;; appearance
;;;; builtins
;;;;; text
(setq
 frame-title-format "%b - emacs"        ; buffer name as frame title
 echo-keystrokes 0.1                    ; echo commands unfinished 1s
 x-underline-at-descent-line t          ; draw underline lower
 )

;;;;; interface
(setq
 initial-buffer-choice user-init-file   ; open init file on startup
 window-combination-resize t)           ; use proportional window resize
(global-font-lock-mode t)               ; syntax highlight everywhere
(global-linum-mode t)                   ; line numbers
(line-number-mode t)                    ; modeline line num
(column-number-mode t)                  ; modeline col num
(show-paren-mode t)                     ; highlight matching parens
(hl-line-mode t)                        ; highlight current line
(file-name-shadow-mode t)               ; dim expanded filename parts
(which-function-mode t)                 ; show def name in modeline

;; disable stuff
(setq
 inhibit-splash-screen t
 initial-scratch-message nil
 inhibit-startup-echo-area-message t    ; minibuffer startup message
 ring-bell-function 'ignore)            ; alarms
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

;; fonts
(set-frame-font "Input Mono Condensed-9")
(set-face-attribute 'variable-pitch nil :family "Input Sans")
;;;; winner
;; restore previous window configurations
(use-package winner :ensure t :config (winner-mode t))

;;;; color theme
(use-package solarized-theme :ensure t
  :config (load-theme 'solarized-dark t))
;;(use-package zenburn-theme :ensure t :config (load-theme 'hc-zenburn t))
;;;; whitespace
;; highlights whitespace and other potential style issues
(use-package whitespace
  :diminish " ς"
  :config
  (setq whitespace-line-column nil              ; use fill-column value
        whitespace-style '(face trailing lines-tail) ; what to highlight
   )
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (whitespace-mode t)
  )
;;;; highlight-numbers
;; highlight numbers in text
;; TODO use `font-lock-constant-face' font face
(use-package highlight-numbers :ensure t
  :config (add-hook 'prog-mode-hook 'highlight-numbers--turn-on))
;;;; highlight-quoted
;; highlight lisp quotes and quoted symbols
(use-package highlight-quoted :ensure t :config (highlight-quoted-mode t))
;;;; fic-mode
;; highlight TODO and FIXME keywords

;;;;; fic-mode
(use-package fic-mode :ensure t
  :diminish fic-mode
  ;;:init (setq fic-foreground-color "#d33682"
  ;;            fic-background-color "gray20")
  :config (add-hook 'prog-mode-hook 'turn-on-fic-mode))

;;;;; thing
;; http://emacsredux.com/blog/2013/07/24/highlight-comment-annotations/
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

;;;; rainbow-mode
;; Different from nyan mode. Highlight color codes with color code.
(use-package rainbow-mode :ensure t
  :diminish rainbow-mode
  :config (rainbow-mode t))
;;;; relative-line-numbers
;; relative line numbers
;; FIXME see if this can be used without enormous lag issues
;;(use-package relative-line-numbers :ensure t
;;  :config
;;  (linum-mode -1)
;;  (setq
;;   relative-line-numbers-format (lambda (offset) (format "%3s" offset))
;;   relative-line-numbers-motion-function 'vertical-motion
;;   )
;;  (global-relative-line-numbers-mode t)
;;  )
;;;; smart-mode-line
;; better modeline
(use-package smart-mode-line :ensure t
  :config
  (setq sml/shorten-directory t
        sml/shorten-modes t
        sml/no-confirm-load-theme t
        sml/theme 'respectful)
  (add-to-list 'sml/replacer-regexp-list
               '("^~/dotfiles/emacs/\\.emacs\\.d/" ":ε:"))
  ;;(use-package smart-mode-line-powerline-theme :ensure t
  ;;  :config (setq sml/theme 'powerline))
  (sml/setup))

;;; editing
(defvar my-user-dir "~/.emacs.d/.user/" "Temporary user storage dir.")

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
;; indent on enter
(bind-key "RET" 'reindent-then-newline-and-indent)
;; word-based text wrapping for text modes only
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;;;; abbrev
(use-package abbrev
  :diminish ""
  :config
  (setq abbrev-file-name (concat my-user-dir "abbrev_defs.el" )
        save-abbrevs t)                  ; save abbrevs when saving file
  (setq-default abbrev-mode t))

;;; behavior
(setq
 delete-by-moving-to-trash t                   ; use system trash for deletion
 find-file-visit-truename t                    ; follow symlinks
 read-file-name-completion-ignore-case t       ; ignore case in completions
 user-mail-address "stewart.g.kelly@gmail.com" ; for org mode
 ;; backups
 backup-by-copying t                    ; copy file into backup dir
 version-control t                      ; add version numbers
 delete-old-versions t                  ; delete old backups silently
 kept-old-versions 5                    ; old versions to keep
 kept-new-versions 8                    ; new versions to keep
 ;; don't ask before allowing these variables
 safe-local-variable-values '((eval whitespace-mode nil) (buffer-read-only \.t))
 ;; locations for generated files
 backup-directory-alist `(("." . ,(expand-file-name(concat my-user-dir
                                                           "backups"))))
 auto-save-list-file-prefix (concat my-user-dir "autosaves")
 auto-save-file-name-transforms `((".*" "~/.emacs.d/.user/autosaves/" t))
 custom-file (concat my-user-dir "custom.el")
 ;; confirm before killing emacs in gui sessions
 confirm-kill-emacs (if (window-system) 'yes-or-no-p nil)
 )
(mouse-wheel-mode t)                    ; mouse wheel enabled
(semantic-mode t)                       ; language-aware editing cmds
(global-auto-revert-mode t)             ; auto refresh buffers

;;;; always UTF-8 encoding
;;(set-keyboard-coding-system 'utf-8)
;;(set-language-environment "UTF-8")
;;(prefer-coding-system 'utf-8)

;;;; org-mode
(use-package org
  :config
  (setq org-startup-folded t               ; start in folded view
        org-src-fontify-natively t         ; syntax highlight code in org buffer
        org-list-allow-alphabetical t           ; add alphabetical lists
        )
  )

;;; navigation
;;;; builtin
(fset 'yes-or-no-p 'y-or-n-p)           ; less annoying
(cd "~")                                ; start in home dir
(setq case-fold-search t                ; ignore case when searching
      line-move-visual t                ; visual line movement
      )
(goto-address-mode t)                   ; URL highlight and follow
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;;;; dired+
;; dired extension and highlighting
(use-package dired+ :ensure t)
;;;; saveplace
;; save cursor place in file and return to it
;;(use-package saveplace
(setq-default save-place-file (concat my-user-dir "places") save-place t)
;;)
;;;; recentf
;; recent file listing
(use-package recentf
  :config (setq recentf-save-file (concat my-user-dir "recentf")))

;;;; help-mode+
;; library names in *Help* buffers become links
(use-package help-mode+ :ensure t)
;;;; nyan-mode
;;(use-package nyan-mode :ensure t
;;  :config
;;  ;; FIXME get nyan-start-music working
;;  (defun nyan-start () "Start the nyan :D"
;;    (interactive)
;;    (nyan-start-animation)
;;    (nyan-start-music)
;;    (setq nyan-wavy-trail t)
;;    )
;;  (defun nyan-stop () "Stop the nyan :("
;;    (interactive) (nyan-stop-animation) (nyan-stop-music))

;;  (setq nyan-wavy-trail t)
;;  (nyan-mode t)
;;  )
;;;; popwin
;; uses popup window for minor buffers
(use-package popwin :ensure t
  :config
  (popwin-mode t)
  (add-to-list 'popwin:special-display-config '("*Help*" :stick t :noselect t))
  (add-to-list 'popwin:special-display-config '("*Python Help*" :stick t :noselect t :height 20))
  )
;;;; golden-ratio
;; resize windows according to golden ratio
(use-package golden-ratio :ensure t
  :diminish ""
  :config
  (setq golden-ratio-exclude-modes '("helm-mode"
                                     "magit-log-mode"
                                     "magit-reflog-mode"
                                     "magit-status-mode"))
  (golden-ratio-mode t)
  )
;;;; projectile
;; project-based navigation
(use-package projectile :ensure t
  :ensure t
  :config
  (setq projectile-indexing-method 'alien  ; use faster OS methods
        projectile-mode-line '(:eval (format " π:%s" (projectile-project-name)))
        projectile-enable-caching t)       ; cache projectile indexes
  (projectile-global-mode t)
  )
;;;; helm
;; Fuzzy minibuffer completion.
(use-package helm :ensure t
  :diminish ""
  :bind (;; helm replacements for builtins
         ("M-x" . helm-M-x)
         ("C-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files))
  :config
  (use-package helm-config)
  (use-package helm-projectile :ensure t
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on))

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
  (helm-autoresize-mode t)
  (helm-mode t))

;;;;  dired
(use-package dired
  :config
  (setq dired-dwim-target t             ; target other dired window
        dired-recursive-copies 'always  ; recursive copy by default
        dired-recursive-deletes 'top    ; confirm recursive delete
        dired-listing-switches "-lha"   ; human-readable listing
        )
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  )
(use-package autorevert
  :config
  (setq global-auto-revert-non-file-buffers t ; auto refresh buffer
        auto-revert-verbose nil         ; no warnings on refresh
        )
  )
;; auto refresh dired buffer on changes

;;; custom commands
;;;; check if online
;; http://stackoverflow.com/questions/21064916/auto-install-emacs-packages-with-melpa/21065704#21065704
(defvar my-onlinep
  (condition-case nil (delete-process (make-network-process
                                       :name "my-check-internet"
                                       :host "elpa.gnu.org"
                                       :service 80))
    (error t)))
;;;; because I'm lazy
(defun my-config ()
  "Opens local init.el file."
  (interactive) (find-file user-init-file))
;;;; because other people are lazy
(defun my-cleanup ()
  "Perform a lot of stuff on whitespace."
  (interactive)
  (indent-region (point-min) (point-max))  ; indent properly
  (untabify (point-min) (point-max))       ; tabs are evil
  (whitespace-cleanup)                     ; whitespace stuff
  )
;;;; open source files in read-only mode
(dir-locals-set-class-variables
 'emacs
 '((nil . ((buffer-read-only . t)
           (show-trailing-whitespace . nil)
           (tab-width . 8)
           (eval . (whitespace-mode nil))))))
;; disable when updating packages
;;(dir-locals-set-directory-class (concat user-emacs-directory "elpa") 'emacs)
;;;; editing
;;;;; flyspell
;; automatic spell checking
(use-package flyspell
  :diminish ""
  :config
  ;; enable for appropriate modes
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;;;;; smartparens
;; auto-add parenthesis
(use-package smartparens-config :ensure smartparens
  :diminish smartparens-mode
  :config
  (smartparens-global-mode)
  (show-smartparens-mode t))       ; highlight all matching pairs

;;;;; outline
(use-package outline
  :config (diminish 'outline-minor-mode " o"))

;;;;; outshine
;; org-mode style using outline mode
(use-package outshine :ensure t
  :config
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
  ;; modes to activate this for
  (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
  (add-hook 'vimrc-mode 'outline-minor-mode)
  (setq outshine-show-hidden-lines-cookies-p t))

;;;;; undo-tree
;; branching tree for undo actions
(use-package undo-tree :ensure t
  :diminish ""
  :config
  (global-undo-tree-mode t)
  (bind-key "C-x u" 'undo-tree-visualize undo-tree-map))

;;;;; flycheck
;; on-the-fly syntax checking
(use-package flycheck :ensure t
  ;;:diminish " φ"
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  ;; redefine with fancy greek lettering
  (defalias 'flycheck-mode-line-status-text 'flycheck-mode-line-status-text-slim)
  (defun flycheck-mode-line-status-text-slim (&optional status)
    "Get a text describing STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
    (let ((text (pcase (or status flycheck-last-status-change)
                  (`not-checked "")
                  (`no-checker "-")
                  (`running "*")
                  (`errored "!")
                  (`finished
                   (if flycheck-current-errors
                       (let-alist (flycheck-count-errors flycheck-current-errors)
                         (format ":%s/%s" (or .error 0) (or .warning 0)))
                     ""))
                  (`interrupted "-")
                  (`suspicious "?"))))
      (concat " Φ" text)))

  ;; display errors by popup
  (use-package flycheck-tip :ensure t :config (flycheck-tip-use-timer 'verbose))
  (flycheck-mode t))

;;;;; company
;; autocompletion in code
;; FIXME won't work (at least in elisp buffers anyway)
(use-package company :ensure t
  :config
  (setq company-idle-delay 0.2            ; delay before completions
        company-show-numbers t            ; show quick-access nums
        company-selection-wrap-around t)  ; wrap back around when selecting
  (global-company-mode t)
  )

;;;;; language-specific packages
(use-package elpy :ensure t :config (elpy-enable))     ; python
(use-package vimrc-mode :ensure t
  :mode ".+\\.\\(vim\\|penta\\)$\\|[._]?\\(pentadactyl\\|vim\\)rc$")

;;;;;; emacs-lisp
(eldoc-mode t)                          ; echo area display of lisp objects
(use-package elisp-slime-nav :ensure t
  :diminish ""
  :config (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode))

;;;; misc
;;;;; maylon
;; Z-machine text based adventure reader
;;(use-package maylon :ensure t)

;;;;; eshell
(use-package eshell
  :bind (("<f10>" . eshell))
  :config
  (setenv "PAGER" "cat")                ; show extended output in other buffer
  (defun hl-line-mode-turn-off (hl-line-mode -1))
  (add-hook 'eshell-mode-hook 'hl-line-mode-turn-off))

;;;;; magit
;; git command integration
(use-package magit :ensure t
  :bind (("<f12>" . magit-status))
  :config
  (setq
   magit-auto-revert-mode nil                   ; don't revert
   magit-last-seen-setup-instructions "1.4.0")) ; hide startup msg

;;;;; evil
;; vim keybindings and modal editing
(use-package evil :ensure t
;;;;;; config
  :config
;;;;;;; packages
;;;;;;;; evil-surround
  ;; manipulate surrounding elements
  (use-package evil-surround :ensure t :config (global-evil-surround-mode t))

;;;;;;;; evil-commentary
  ;; manipulate comments
  (use-package evil-commentary :ensure t
    :diminish ""
    :config (evil-commentary-mode t))

;;;;;;;; evil-org
  ;; evil org-mode bindings
  (use-package evil-org :ensure t)

;;;;;;; helper functions
;;;;;;;; copy to end of line
  (defun copy-to-end-of-line ()
    (interactive) (evil-yank (point) (point-at-eol)))

;;;;;;;; switch to previous buffer
  (defun other-buffer ()
    (interactive) (switch-to-buffer (other-buffer (current-buffer) t)))

;;;;;;;; jk to exit insert mode
  ;; Adapted from https://zuttobenkyou.wordpress.com/2011/02/15/some-thoughts-on-emacs-and-vim/
  (evil-define-command maybe-exit ()
    :repeat change
    (interactive)
    (let ((modified (buffer-modified-p)))
      (insert "j")
      (let ((evt (read-event nil nil 0.5)))
        (cond
         ((null evt) (message ""))
         ((and (integerp evt) (char-equal evt ?k))
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
         (t (setq unread-command-events (append unread-command-events
                                                (list evt))))))))

;;;;;;;; esc quits
  ;; From https://github.com/davvil/.emacs.d/blob/master/init.el
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

;;;;;;; settings and enable mode
  (setq evil-want-fine-undo t            ; undo tracks delete as well
        evil-want-change-word-to-end nil ; don't let \"cw\" behave like \"ce\"
        evil-ex-substitute-global t)     ; global substitutions by default
  (add-to-list 'evil-emacs-state-modes '(dired-mode))
  (evil-mode t)
;;;;;;; bindings
;;;;;;;; leader setup
  ;; FIXME leader bindings broken with Emacs24.5
  ;(define-key evil-normal-state-map "," nil)
  ;(define-key evil-motion-state-map "," nil)

;;;;;;;; regular evil-state maps
  (bind-key "j" #'maybe-exit evil-insert-state-map) ; jk exits insert state

;;;;;;;;; visual, normal and motion state bindings
  (dolist (map '(evil-normal-state-map
                 evil-visual-state-map
                 evil-motion-state-map))
    (defalias map (symbol-value map))               ; get around bind-key change
    (bind-keys :map map
               ("Y" . copy-to-end-of-line)          ; more consistent
               ([escape] . keyboard-quit)           ; esc quits
               ("SPC" . execute-extended-command)   ; easier to reach
               ;;(",,h" . helm-command-prefix)      ; better helm prefix
               ;; easier movements for common commands
               ("s" . evil-last-non-blank)
               ("a" . evil-first-non-blank)
               ("S" . evil-jump-item)
               ("gw" . nil)
               ("gwl" . evil-window-right)
               ("gwh" . evil-window-left)
               ("gwj" . evil-window-down)
               ("gwk" . evil-window-up)
               ("gB" . helm-mini)
               ("gbb" . other-buffer)
               ("gbl" . evil-next-buffer)
               ("gbh" . evil-previous-buffer)
               ;; visual line movement
               ("j" . evil-next-visual-line) ("k" . evil-previous-visual-line)
               ;; leader keys for common commands
               ;("," . nil)
               ;(",q" . kill-buffer-and-window)
               ;(",e" . helm-projectile-find-file)
               ;(",x" . eval-region)
					;(",X" . eval-buffer)
	       ))

;;;;;;;;; visual and normal state bindings
  (dolist (map '(evil-normal-state-map
                 evil-visual-state-map))
    (defalias map (symbol-value map))               ; get around bind-key change
    (bind-keys :map map
               (",w" . save-buffer)))

;;;;;;;; esc quits minibuffer
  (dolist (map '(minibuffer-local-map
                 minibuffer-local-ns-map
                 minibuffer-local-completion-map
                 minibuffer-local-must-match-map
                 minibuffer-local-isearch-map))
    (bind-key "<escape>" 'minibuffer-keyboard-quit (symbol-value map)))

;;;;;;;; elisp-slime-nav
  (evil-define-key 'normal emacs-lisp-mode-map
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point)
;;;;;;;; package-menu-mode
  (bind-keys :map package-menu-mode-map
             ("j" . next-line)
             ("k" . previous-line)
             ("/" . isearch-forward)
             ("?" . isearch-backward)
             ("n" . isearch-repeat-forward)
             ("N" . isearch-repeat-backward)
             ("<escape>" . isearch-cancel))
;;;;;;;; outline-mode
  ;; mostly adapted from evil-org
  (evil-define-key 'normal outline-minor-mode-map
    "gh" 'outline-up-heading
    "gj" 'outline-forward-same-level
    "gk" 'outline-backward-same-level
    "gl" 'outline-next-visible-heading
    "-t" 'outshine-todo
    "-o" 'outshine-insert-heading
    "-<" 'outline-promote
    "->" 'outline-demote
    (kbd "TAB") 'outline-toggle-children
    "--" 'outline-cycle
    "-v" 'outline-mark-subtree)
  )

