;; Kelly Stewart                        -*-outline-regexp: "[*\\f]+"-*-
;; TODO auto add comment leaders
;;; setup
;; TODO https://github.com/tarsius/auto-compile
(require 'package)
(setq
 load-prefer-newer t                   ; don't load outdated byte code
 package-enable-at-startup nil         ; we will manually initialize
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("melpa" . "http://melpa.milkbox.net/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)

;; install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents) (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)            ; log message after loading packages

;; here so later use-package declarations can use `:delight' keyword
(use-package delight :ensure t
  :config
  (delight '((visual-line-mode)
             (emacs-lisp-mode "Elisp" :major))))

;;; appearance
;;; text
;;;; builtin
(setq
 frame-title-format "%b - emacs"        ; buffer name as frame title
 window-combination-resize t            ; use proportional window resize
 echo-keystrokes 0.1                    ; echo unfinished commands faster
 x-underline-at-descent-line t)         ; draw underline lower
(global-font-lock-mode t)               ; syntax highlight everywhere
(hl-line-mode t)                        ; highlight current line
(show-paren-mode t)                     ; highlight matching parens
(goto-address-mode t)                   ; highlight URL and allow opening in eww
(global-prettify-symbols-mode t)        ; pretty Greek lettering
(file-name-shadow-mode t)               ; dim expanded filename parts

;; modeline
(line-number-mode t)                    ; line num
(column-number-mode t)                  ; col num
(which-function-mode t)                 ; definition name
(global-linum-mode t)                   ; line numbers

;; we don't need no stinkin gui
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
(set-face-attribute 'variable-pitch nil :family "Input Serif")

;;;; whitespace
;; highlights whitespace and other potential style issues
(use-package whitespace
  :delight whitespace-mode " ς"
  :config
  (setq whitespace-line-column nil                    ; use fill-column value
        whitespace-style '(face trailing lines-tail)) ; what to highlight
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (whitespace-mode t))

;;;; highlight-numbers
;; highlight numbers in text
;; TODO use `font-lock-constant-face' font face
(use-package highlight-numbers :ensure t
  :config (add-hook 'prog-mode-hook 'highlight-numbers--turn-on))

;;;; highlight annonations
;;;;; fic-mode
;;(use-package fic-mode :ensure t
  ;;:delight fic-mode
  ;;:init (setq fic-foreground-color "#d33682"
  ;;            fic-background-color "gray20")
  ;;:config (add-hook 'prog-mode-hook 'turn-on-fic-mode))

;;;;; thing
;; http://emacsredux.com/blog/2013/07/24/highlight-comment-annotations/
(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.

This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\) "
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

;;;; rainbow-mode
;; Highlight background of color codes with specified color.
(use-package rainbow-mode :ensure t
  :delight rainbow-mode " ρ"
  :config (add-hook 'prog-mode-hook 'rainbow-turn-on t))

;;;; (d)relative-line-numbers
;; relative line numbers
;; FIXME see if this can be used without enormous lag issues
;;(use-package relative-line-numbers :ensure t
;;  :config
;;  (global-linum-mode -1)
;;  (setq
;;   relative-line-numbers-format (lambda (offset) (format "%3s" offset))
;;   relative-line-numbers-motion-function 'vertical-motion)
;;  (global-relative-line-numbers-mode t))

;;;; winner
;; restore previous window configurations
(use-package winner :ensure t :config (winner-mode t))

;;;; delight


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
  ;; TODO tell powerline-theme to use better colors good god
  ;; (use-package smart-mode-line-powerline-theme :ensure t
  ;;   :config (setq sml/theme 'powerline))
  (sml/setup))

;;;; color theme
(use-package solarized-theme :ensure t :config (load-theme 'solarized-dark t))
;; (use-package zenburn-theme :ensure t :config (load-theme 'hc-zenburn t))

;;; behavior
;;;; builtin
;;;;; user info
(defvar my-user-dir "~/.emacs.d/.user/" "Temporary user storage dir.")
(setq
 user-full-name "Kelly Stewart"
 user-mail-address "stewart.g.kelly@gmail.com")

;;;;; system interaction
(cd "~")                                ; start in home dir
(setq
 custom-file (concat my-user-dir "custom.el") ; custom file location
 find-file-visit-truename t             ; follow symlinks
 delete-by-moving-to-trash t            ; use system trash for deletion
 smooth-scroll-margin 3)                ; fewer 3 lines visible at buffer ends

;;;;;; backup and autosave
(setq
 ;; backups
 backup-by-copying t                     ; copy file into backup dir
 version-control t                       ; add version numbers
 delete-old-versions t                   ; delete old backups silently
 kept-old-versions 5                     ; old versions to keep
 kept-new-versions 8                     ; new versions to keep
 ;; file locations
 backup-directory-alist `(("." . ,(expand-file-name (concat my-user-dir
                                                            "backups"))))
 auto-save-file-name-transforms `((".*" ,(expand-file-name
                                          (concat my-user-dir "autosaves")) t))
 auto-save-list-file-prefix (concat my-user-dir "autosaves"))


;;;;; interface
(fset 'yes-or-no-p 'y-or-n-p)           ; less annoying
(setq
 save-interprogram-paste-before-kill t  ; save clipboard contents to kill-ring
 case-fold-search t                     ; ignore case when searching
 read-file-name-completion-ignore-case t ; ignore case in completions
 initial-buffer-choice user-init-file   ; open init file on startup
 initial-major-mode 'text-mode          ; start scratch in text mode
 line-move-visual t                    ; visual line movement
 ;; don't ask before allowing these variables
 safe-local-variable-values '((eval whitespace-mode nil)
                              (buffer-read-only \.t)))
(mouse-wheel-mode t)                    ; mouse wheel enabled
;;(semantic-mode t)                       ; language-aware editing commands

;;;;; formatting
(setq
 require-final-newline t                ; newline at end of file
 sentence-end-double-space nil          ; double space is dumb
 tab-width 4                            ; tabs with width 4
 tab-always-indent nil                  ; tapping tab inserts a tab (or spaces)
 tab-stop-list (number-sequence 4 100 4)) ; tab stop locations

(setq-default
 indent-tabs-mode nil                   ; turn tabs to spaces
 default-fill-column 80)                ; recommended text width

(electric-indent-mode t)                             ; auto indent
(electric-pair-mode t)                               ; auto add parens
(bind-key "RET" 'reindent-then-newline-and-indent)   ; indent on enter
(add-hook 'text-mode-hook 'turn-on-visual-line-mode) ; wrap by word in text mode

;;;; abbrev
(use-package abbrev
  :delight abbrev-mode
  :config
  (setq abbrev-file-name (concat my-user-dir "abbrevs.el" )
        save-abbrevs t)                  ; save abbrevs when saving file
  (setq-default abbrev-mode t))

;;; editing
;;; major modes (editing)
;;;; org-mode
(use-package org
  :init
  (setq
   org-startup-folded t
   org-src-fontify-natively t           ; syntax highlight code in org buffer
   org-list-allow-alphabetical t))      ; allow single-char alphabetical lists

;;;; lisp
;;;;; eldoc-mode
(use-package eldoc
  :config
  (setq ;eldoc-minor-mode-string nil
        eldoc-idle-delay 0.3)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode t))

;;;;; highlight-quoted
;; highlight lisp quotes and quoted symbols
(use-package highlight-quoted :ensure t
  :config
  (add-hook 'lisp-mode-hook 'highlight-quoted--turn-on t)
  (add-hook 'emacs-lisp-mode-hook 'highlight-quoted--turn-on t))

;;;;; elisp-slime-nav
(use-package elisp-slime-nav :ensure t
  :diminish ""
  :config (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode))

;;;; python
(use-package elpy :ensure t :config (elpy-enable))

;;;; vimrc
(use-package vimrc-mode :ensure t
  :mode ".+\\.\\(vim\\|penta\\)$\\|[._]?\\(pentadactyl\\|vim\\)rc$")

;;; major modes (other)
;;;; dired
;; File browser
(use-package dired
  :config
  (use-package dired+)                  ; dired extensions and syntax highlight
  (setq dired-dwim-target t             ; target other dired window
        dired-recursive-copies 'always  ; recursive copy by default
        dired-recursive-deletes 'top    ; confirm recursive delete
        dired-listing-switches "-lha")  ; human-readable listing
  (add-hook 'dired-mode-hook 'auto-revert-mode))


;;;; eshell
;; Integrated Emacs shell
(use-package eshell
  :bind (("<f10>" . eshell))
  :config
  (setenv "PAGER" "cat")                ; show extended output in other buffer
  (setq eshell-directory-name (concat my-user-dir ".eshell"))
  (defun hl-line-mode-turn-off () (hl-line-mode -1))
  (add-hook 'eshell-mode-hook 'hl-line-mode-turn-off))

;;;; magit
;; git command integration
(use-package magit :ensure t
  :bind (("<f12>" . magit-status))
  :config
  (setq
   magit-auto-revert-mode nil                   ; don't revert
   magit-last-seen-setup-instructions "1.4.0")) ; hide startup msg

;;;; diff-hl-mode
;; diff with uncommited changes
(use-package diff-hl :ensure t)

;;;; maylon
;; Z-machine text based adventure reader
;;(use-package maylon :ensure t)

;;; navigation
;;;; saveplace
;; Save cursor place in file and return to it
(use-package saveplace
  :config
  (setq save-place-file (concat my-user-dir "places"))
  (setq-default save-place t))

;;;; recentf
;; List recent files
(use-package recentf
  :config (setq recentf-save-file (concat my-user-dir "recentf")
                recentf-menu-filter 'recentf-sort-ascending))

;;;; autorevert
;; Auto refresh buffers after outside changes
(use-package autorevert
  :config
  (setq global-auto-revert-non-file-buffers t  ; auto refresh buffer
                auto-revert-verbose nil)       ; no warnings on refresh
  (global-auto-revert-mode t))

;;;; help+
;; Enhancements to help
(use-package help+ :ensure t)

;;;; nyan-mode
;; Nyan cat displays file percentage.
(use-package nyan-mode :ensure t
  :config
  ;; FIXME get nyan-start-music working
  ;;(defun nyan-start () "Start the nyan :D"
  ;;  (interactive)
  ;;  (nyan-start-animation)
  ;;  (nyan-start-music)
  ;;  (setq nyan-wavy-trail t))

  ;;(defun nyan-stop () "Stop the nyan :("
  ;;  (interactive) (nyan-stop-animation) (nyan-stop-music))

  ;; FIXME nyan needs a wavy tail
  (setq-default nyan-wavy-trail t)
  (nyan-mode t))

;;;; popwin
;; Popup window for minor buffers.
(use-package popwin :ensure t
  :config
  (popwin-mode t)
  (add-to-list 'popwin:special-display-config '("*Python Help*"
                                                :stick t
                                                :noselect t :height 20))
  (add-to-list 'popwin:special-display-config '("*Help*" :stick t :noselect t)))

;;;; golden-ratio
;; Resize windows according to golden ratio
(use-package golden-ratio :ensure t
  :delight golden-ratio-mode
  :config
  (setq golden-ratio-exclude-modes '("helm-mode"
                                     "magit-log-mode"
                                     "magit-reflog-mode"
                                     "magit-status-mode")
        golden-ratio-auto-scale t)           ; auto scale with screen size
  (golden-ratio-mode t))

;;;; projectile
;; Project-based navigation
(use-package projectile :ensure t
  :config
  (setq projectile-indexing-method 'alien  ; use faster OS methods
        projectile-enable-caching t        ; cache projectile indexes
        projectile-mode-line '(:eval (format " π:%s" (projectile-project-name))))
  (projectile-global-mode t))

;;;; helm
;; Fuzzy minibuffer completion.
(use-package helm :ensure t
  :delight helm-mode
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
   helm-buffers-fuzzy-matching t
   helm-recentf-fuzzy-match t
   helm-M-x-fuzzy-match t
   helm-semantic-fuzzy-match t
   helm-imenu-fuzzy-match t
   helm-apropos-fuzzy-match t)

  (helm-autoresize-mode t)
  (helm-mode t))

;;; custom commands
;;;; check if online
;; http://stackoverflow.com/questions/21064916/auto-install-emacs-packages-with-melpa/21065704#21065704
(defvar my-onlinep
  (condition-case nil
      (delete-process (make-network-process
                       :name "my-check-internet"
                       :host "elpa.gnu.org"
                       :service 80))
    (error t))
  "Are we online?")

;;;; because I'm lazy
(defun my-config ()
  "Opens local init.el file."
  (interactive)
  (find-file user-init-file))

;;;; because other people are lazy
(defun my-cleanup ()
  "Perform a lot of stuff on whitespace."
  (interactive)
  (untabify (point-min) (point-max))       ; tabs are evil
  (indent-region (point-min) (point-max))  ; indent properly
  (whitespace-cleanup))                    ; whitespace stuff

;;;; open source files in read-only mode
(dir-locals-set-class-variables
 'source-readonly
 '((nil . ((buffer-read-only . t)
           (show-trailing-whitespace . nil)
           (tab-width . 8)
           (eval . (whitespace-mode nil))))))

(dir-locals-set-class-variables
 'source
 '((nil . ((show-trailing-whitespace . nil)
           (tab-width . 8)
           (eval . (whitespace-mode nil))))))

(dir-locals-set-directory-class
 (concat user-emacs-directory "elpa")
 'source)

;;;; copy to end of line
  (defun copy-to-end-of-line ()
    (interactive) (evil-yank (point) (point-at-eol)))

;;;; esc quits
  ;; From https://github.com/davvil/.emacs.d/blob/master/init.el
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivete it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

;;; editing
;;;; outline
;; Collapsible headings for organization
(use-package outline
  :delight outline-minor-mode " o"
  :config
  ;; org-mode style using outline mode
  (use-package outshine :ensure t
    :config
    (setq outshine-fontify-whole-heading-line t
          outshine-startup-folded-p t
          outshine-org-style-global-cycling-at-bob-p t
          outshine-preserve-delimiter-whitespace t)
    (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
    ;; modes to activate this for
    (add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
    (add-hook 'vimrc-mode 'outline-minor-mode)))

;;;; flyspell
;; Automatic spell checking when editing
(use-package flyspell
  :delight flyspell-mode
  :config
  ;; enable for appropriate modes
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;;;; smartparens
;; Better parenthesis motions and matching
(use-package smartparens-config :ensure smartparens
  :delight smartparens-mode
  :config
  (smartparens-global-mode t)
  (show-smartparens-mode t))

;;;; undo-tree
;; Branching tree for undo actions
(use-package undo-tree :ensure t
  :delight undo-tree-mode
  :config
  (global-undo-tree-mode t)
  (bind-key "C-x u" 'undo-tree-visualize undo-tree-map))

;;;; flycheck
;; Syntax check buffers while editing.
(use-package flycheck :ensure t
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  ;; modeline with fancy Greek lettering
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
  ;; FIXME clobbers company popups
  ;;(use-package flycheck-tip :ensure t :config (flycheck-tip-use-timer 'verbose))

  (global-flycheck-mode t))

;;;; company
;; Autocompletion in code
;; FIXME won't work (at least in elisp buffers anyway)
(use-package company :ensure t
  :config
  (setq company-idle-delay 0.2            ; delay before completions
        company-show-numbers t            ; show quick-access nums
        company-selection-wrap-around t)  ; wrap back around when selecting
  (global-company-mode t))

;;; evil
;; Vim keybindings and modal editing
(use-package evil :ensure t
  :config
;;;; packages
;;;;; evil-surround
  ;; Manipulate surrounding elements
  (use-package evil-surround :ensure t :config (global-evil-surround-mode t))

;;;;; evil-commentary
  ;; Manipulate comments
  (use-package evil-commentary :ensure t
    :delight evil-commentary-mode
    :config (evil-commentary-mode t))

;;;;; evil-org
  ;; Evil org-mode bindings
  (use-package evil-org :ensure t)

;;;; helper functions
;;;;; jk to exit insert mode
;; Adapted from https://zuttobenkyou.wordpress.com/2011/02/15/some-thoughts-on-emacs-and-vim/
  ;;(define-key evil-insert-state-map "j" #'cofi/maybe-exit)
  ;;(evil-define-command cofi/maybe-exit ()
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

;;;;; insert a single character
  ;; Consider this if dot repetition is needed. Otherwise \ is acceptable.
  (evil-define-command evil-execute-in-insert-state (&optional arg)
    "Execute the next command in Insert state."
    (cond
     ((called-interactively-p 'any)
      (add-hook 'post-command-hook #'evil-execute-in-insert-state t)
      (evil-insert 1)
      (evil-echo "Switched to Insert state for the next command ..."))
     ((not (eq this-command #'evil-execute-in-insert-state))
      (remove-hook 'post-command-hook 'evil-execute-in-insert-state)
      (evil-change-to-previous-state)
      ;; ensure the command is recorded if we return to normal state
      (when (evil-normal-state-p)
        (evil-repeat-stop)))))

;;;; bindings
;;;;; regular evil-state maps
;;;;;; global
  (bind-keys
   ;; window movement
   ("C-l" . evil-window-right)
   ("C-k" . evil-window-up)
   ("C-h" . evil-window-left)
   ("C-j" . evil-window-down))
;;;;;; visual, normal and motion
  (bind-keys :map (evil-normal-state-map
                   evil-visual-state-map
                   evil-motion-state-map)
             ("Y" . copy-to-end-of-line)          ; more consistent
             ;; for consistency with other Emacs buffers
             ("q" . kill-buffer-and-window)
             ;; use Q for macro recording since we remapped q
             ("Q" . evil-record-macro)
             ;; buffer movement
             ("gB" . helm-mini)
             ("gbb" . other-window)
             ("gbl" . evil-next-buffer)
             ("gbh" . evil-previous-buffer)
             ;; visual line movement
             ("j" . evil-next-visual-line)
             ("k" . evil-previous-visual-line)
             ;; leader keys for common commands
             ("s" . evil-last-non-blank)
             ("a" . evil-first-non-blank)
             ("\"" . evil-jump-item)
             ("," . nil)
             (",w" . save-buffer)
             (",hp" . helm-projectile-find-file)
             (",x" . eval-region)
             (",X" . eval-buffer)
             ([escape] . keyboard-quit)
             ("SPC" . helm-M-x))

;;;;;; insert
  (bind-key "j" #'maybe-exit evil-insert-state-map) ; jk exits insert state

;;;;; esc quits minibuffer
  (bind-keys :map (minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map)
             ("<escape>" . minibuffer-keyboard-quit))

;;;;; package-menu-mode
  (bind-keys :map package-menu-mode-map
             ("j" . next-line)
             ("k" . previous-line)
             ("/" . isearch-forward)
             ("?" . isearch-backward)
             ("n" . isearch-repeat-forward)
             ("N" . isearch-repeat-backward)
             ("<escape>" . isearch-cancel))
;;;;; outline-mode
  ;; mostly adapted from evil-org
  (evil-define-key 'normal outline-minor-mode-map
    "gh" 'outline-up-heading
    "gj" 'outline-forward-same-level
    "gk" 'outline-backward-same-level
    "gl" 'outline-next-visible-heading
    "\t" 'outline-cycle
    "za" 'outline-toggle-children
    "-t" 'outshine-todo
    "-o" 'outshine-insert-heading
    "-<" 'outline-promote
    "->" 'outline-demote
    "-v" 'outline-mark-subtree)
;;;;; elisp-slime-nav
  (evil-define-key 'normal emacs-lisp-mode-map
    "K" 'elisp-slime-nav-describe-elisp-thing-at-point
    "gd" 'elisp-slime-nav-find-elisp-thing-at-point)
;;;;; flycheck
  (evil-define-key 'normal flycheck-mode
    ",,fl" 'flycheck-list-errors
    ",,fk" 'flycheck-previous-error
    ",,fj" 'flycheck-next-error)
;;;; settings
  (setq evil-want-fine-undo t            ; undo tracks delete as well
        evil-want-change-word-to-end nil ; don't let \"cw\" behave like \"ce\"
        evil-echo-state nil              ; we have it in the modeline anyway
        evil-ex-substitute-global t)     ; global substitutions by default
  (add-to-list 'evil-emacs-state-modes '(dired-mode))
;;;;; set shift-width to major mode indent levels
  ;; TODO incorporate into a list to avoid repetition as this grows
  (defun my-evil-shiftwidth-lisp () (setq evil-shift-width 2))
  (add-hook 'emacs-lisp-mode-hook 'my-evil-shiftwidth-lisp)
;;;; enable
  (evil-mode t))


;;;; cleanup
;; Confirm before killing Emacs in GUI sessions.
;; At end of file to make init debugging easier.
(setq confirm-kill-emacs (if (window-system) 'yes-or-no-p nil))
