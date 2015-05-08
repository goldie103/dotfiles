;;; init.el --- Kelly Stewart's init file
;;; Commentary:
;; TODO company not working in elisp buffers
;;; Code:
;; * setup
;; ** package
(require 'package)
(setq package-enable-at-startup nil         ; we will manually initialize
      load-prefer-newer t)                  ; don't load outdated byte code

;; add package archives
(dolist (archive '(("melpa" . "http://melpa.milkbox.net/packages/")
                   ("org" . "http://orgmode.org/elpa/")
                   ("elpy" . "http://jorgenschaefer.github.io/packages/")))
  (add-to-list 'package-archives archive))

(package-initialize)                    ; manually initialize

;; non-MELPA packages
(defconst my/dir-elisp (concat user-emacs-directory "elisp/"))
(add-to-list 'load-path my/dir-elisp)

;; ** use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(with-eval-after-load 'use-package
  (setq use-package-verbose t             ; log message after loading packages
        use-package-always-ensure t))     ; install all packages if necessary

;; ** bind-key
(use-package bind-key :bind ("RET" . newline-and-indent))

;; ** delight
(use-package delight
  :config
  (delight '((emacs-lisp-mode "Elisp" :major)))
  (delight visual-line-mode))

;; ** user info
(defconst my/dir (concat user-emacs-directory ".user/"))
(defconst my/dir-my-elisp (concat my/dir "elisp/"))
;; TODO why this not work
(load (concat my/dir-my-elisp "private.el") t) ; passwords and sensitive info
(setq user-full-name "Kelly Stewart")

;; * basic settings
;; ** settings

(setq
 read-file-name-completion-ignore-case t ; ignore case in completions
 sentence-end-base "[.?!][]\"'”)}\\n]*"  ; count newlines as sentence endings
 sentence-end-double-space nil          ; double space is dumb
 tab-always-indent nil                  ; tab inserts a character
 smooth-scroll-margin 3                 ; fewer lines visible at buffer ends
 save-interprogram-paste-before-kill t  ; save clipboard to kill-ring
 kill-do-not-save-duplicates t          ; no duplicates in kill-ring
 line-move-visual t                     ; visual line movement
 ;; builtin
 user-mail-address "stewart.g.kelly@gmail.com" ; manually define email address
 frame-title-format "%b - emacs"        ; buffer name as frame title
 window-combination-resize t            ; use proportional window resize
 echo-keystrokes 0.1                    ; echo unfinished commands faster
 x-underline-at-descent-line t          ; draw underline lower
 ring-bell-function 'ignore             ; alarms
 delete-by-moving-to-trash t            ; use system trash for deletion
 tab-width 4                            ; tabs with width 4
 ;; startup
 inhibit-startup-screen t               ; no start screen
 initial-scratch-message nil            ; no scratch message
 initial-major-mode 'text-mode)         ; scratch text mode

(setq-default indent-tabs-mode nil      ; turn tabs to spaces
              fill-column 80)           ; recommended text width

(fset 'yes-or-no-p #'y-or-n-p)          ; less annoying

(set-frame-font "Input Mono Condensed-9")
(set-face-attribute 'variable-pitch nil :family "Input Serif") ; headline font

(defun display-startup-echo-area-message () "Remove dumb start message." nil)

;; ** modes
(mouse-wheel-mode t)                    ; Mouse wheel enabled
(goto-address-mode t)                   ; Highlight and buttonize URLs
(file-name-shadow-mode t)               ; Dim expanded filename parts
(show-paren-mode t)                     ; Highlight matching parens
(electric-indent-mode t)                ; Auto indent
(electric-pair-mode t)                  ; Auto add parens
(global-prettify-symbols-mode t)        ; Pretty symbols
(add-hook 'prog-mode-hook #'semantic-mode) ; Language-aware manipulations
(add-hook 'prog-mode-hook (lambda()(line-number-mode -1))) ; Modeline line num
(add-hook 'prog-mode-hook (lambda()(column-number-mode -1))) ; Modeline col num

;; we don't need no stinkin GUI
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
;; * custom commands
;; ** my/cleanup
(defun my/cleanup ()
  "Perform a lot of stuff on whitespace."
  (interactive)
  (untabify (point-min) (point-max))       ; tabs are evil
  (indent-region (point-min) (point-max))  ; indent properly
  (whitespace-cleanup))                    ; whitespace stuff

;; ** my/source-class
(defun my/source-class-apply-readonly (readonlyp)
  "Create and apply a directory class for source code.
Set `buffer-read-only' to the value of READONLYP."

  (dir-locals-set-class-variables
   'package
   '((nil . `((buffer-read-only . ,readonlyp)
              (tab-width . 8)
              (eval . (whitespace-mode -1))
              (eval . (linum-mode -1))
              (eval . (real-auto-save-mode -1))))))

  (dir-locals-set-directory-class package-user-dir 'package))

;; ** my/handwrite
(defun my/make-handwrite-frame ()
  "Create a frame using handwriting faces and regular-sized org headlines."
  (interactive)

  (let ((new-family "Kelly Normal")
        (new-height 220)
        (default-family (internal-get-lisp-face-attribute 'default :family nil))
        (default-height (internal-get-lisp-face-attribute 'default :height nil)))

    ;; Apply handwriting frame settings
    (set-face-attribute 'default t :family new-family :height new-height)
    (set-face-attribute 'variable-pitch t :height default-height)

    ;; Create frame with current frame height and absolute width
    (make-frame `((height . ,(frame-height)) (width . 60)))

    ;; Reset to previous default frame settings
    (set-face-attribute 'default t :family default-family :height default-height)
    (set-face-attribute 'variable-pitch t :height 'unspecified)))

;; ** esc quits
;; From https://github.com/davvil/.emacs.d/blob/master/init.el
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; ** text-mode
;; *** my/prose-based-hook
(defun my/prose-based-hook ()
  "Apply modes and settings for editing text meant for humans to read."
  (interactive)
  (visual-line-mode t)
  (flyspell-mode t)
  (typo-mode t)
  (writegood-mode t)
  (hl-sentence-mode t)
  (wc-goal-mode t)
  (line-number-mode -1)
  (column-number-mode -1)
  (linum-mode -1)
  (hl-line-mode -1))

(add-hook 'text-mode-hook #'my/prose-based-hook)

;; *** my/writeroom-effect
(defun my/writeroom-effect ()
  "Apply effects designed for distraction-free writing for human beings to read.
Can be used outside writeroom mode."
  (interactive)
  (display-time-mode t)
  (display-battery-mode t)
  (nyan-mode nil)
  (setq mode-line-format
        '("%e"
          mode-line-front-space
          mode-line-frame-identification
          mode-line-buffer-identification
          sml/pre-modes-separator
          mode-line-modes
          mode-line-misc-info
          mode-line-end-spaces)))

;; * evil
(use-package evil                       ; Vim keybindings and modal editing
  :demand t
  :functions maybe-exit
  :commands (evil-set-command-properties
             evil-refresh-cursor
             evil-normal-state-p
             evil-motion-state-p
             evil-ace-jump-exit-recursive-edit)
  ;; ** evil/init
  :init
  (setq evil-want-fine-undo nil    ; undo insertions in single steps
        evil-want-change-word-to-end nil ; don't let cw behave like ce
        evil-echo-state nil              ; state is in the modeline anyway
        evil-ex-substitute-global t)     ; global substitutions by default

  ;; ** evil/config
  :config
  ;; *** evil/packages
  ;; **** evil-surround
  (use-package evil-surround :config (global-evil-surround-mode t))

  ;; **** evil-commentary
  (use-package evil-commentary          ; Manipulate comments
    :delight evil-commentary-mode
    :config (evil-commentary-mode t))

  ;; **** evil-args
  (use-package evil-args                ; Manipulate function arguments
    ;; cia - change inner argument
    ;; daa - delete an argument
    :init
    (add-hook 'emacs-lisp-mode-hook (lambda()(setq evil-args-delimiters '(" "))))
    (bind-keys :map (evil-normal-state-map evil-motion-state-map)
               ("L" . evil-forward-arg)
               ("H" . evil-backward-arg)
               ("ga" . evil-jump-out-args))

    (bind-key "a" #'evil-inner-arg evil-inner-text-objects-map)
    (bind-key "a" #'evil-outer-arg evil-outer-text-objects-map))

  ;; **** evil-matchit
  (use-package evil-matchit             ; Manipulate tags
    :defines evilmi-may-jump-percentage
    :init (setq evilmi-may-jump-percentage nil) ; allow count usage
    :config (global-evil-matchit-mode t))

  ;; **** evil-org
  (use-package evil-org                 ; Evil org-mode bindings
    :delight evil-org-mode
    :init (bind-key "\t" 'org-back-to-heading evil-normal-state-map))

  ;; *** evil/helper functions
  ;; **** maybe-exit
  ;; Adapted from https://zuttobenkyou.wordpress.com/2011/02/15/some-thoughts-on-emacs-and-vim/
  (evil-define-command maybe-exit ()
    "Exit insert mode and mark buffer modified on insertion of a two-key
command. Uses jk as default combination."
    :repeat change
    (interactive)
    (let ((modified (buffer-modified-p))
          (first-char "j")
          (second-char ?k))
      (insert first-char)
      (let ((evt (read-event nil nil 0.5)))
        (cond
         ((null evt) (message ""))
         ((and (integerp evt) (char-equal evt second-char))
          (delete-char -1)
          (set-buffer-modified-p modified)
          (push 'escape unread-command-events))
         (t (setq unread-command-events (append unread-command-events
                                                (list evt))))))))

  ;; *** evil/bindings
  ;; **** evil/bindings/state
  (bind-key "j" #'maybe-exit evil-insert-state-map) ; jk exits insert state

  (bind-keys
   :map global-map
   ;; window movement
   ("C-l" . evil-window-right)
   ("C-k" . evil-window-up)
   ("C-h" . evil-window-left)
   ("C-j" . evil-window-down)
   ;; scrolling
   ("M-n" . evil-scroll-down)
   ("M-p" . evil-scroll-up))

  (bind-keys
   :map (evil-normal-state-map evil-motion-state-map)
   ;; In a different mapping to exclude visual state
   ("Y" . (lambda()(evil-yank (point) (point-at-eol))))) ; more consistent

  (bind-keys
   :map (evil-normal-state-map
         evil-visual-state-map
         evil-motion-state-map)
   ("q" . kill-buffer-and-window)       ; consistency with other Emacs buffers
   ("Q" . evil-record-macro)            ; Q replaces old q action
   ;; visual line movement
   ("j" . evil-next-visual-line)
   ("k" . evil-previous-visual-line)
   ;; easier mappings for commands I use a lot
   ("SPC" . execute-extended-command)
   ("a" . evil-last-non-blank)
   ("s" . evil-first-non-blank)
   ("\"" . evil-jump-item)
   ("," . nil)
   (",f" . find-file)
   (",b" . list-buffers)
   (",w" . save-buffer)
   (",x" . eval-defun)
   (",X" . eval-region)
   ("<escape>" . keyboard-quit))

  ;; **** evil/bindings/minibuffer
  ;; ESC quits the minibuffer
  (bind-keys :map (minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map)
             ("<escape>" . minibuffer-keyboard-quit))

  ;; *** evil/enable
  (add-to-list 'evil-motion-state-modes 'package-menu-mode)
  (evil-mode t))

;; * tools
;; ** cus-edit
(use-package cus-edit :ensure nil       ; Customize user variables
  :init
  (setq custom-file (expand-file-name "custom.el" my/dir-my-elisp)
        custom-buffer-done-kill t       ; kill buffer when closing
        custom-raised-buttons nil       ; use brackets for buttons
        ;; show real variable names
        custom-unlispify-tag-names nil
        custom-unlispify-menu-entries nil)
  (load custom-file 'no-error 'no-message) ; load custom file
  (add-to-list 'evil-motion-state-modes 'Custom-mode)) ; Start in motion state

;; ** flycheck
(use-package flycheck                   ; On-the-fly syntax checking
  :demand t
  :config
  ;; *** flycheck/modeline
  (setq flycheck-mode-line
        '(:eval (replace-regexp-in-string
                 "FlyC" "Φ" (flycheck-mode-line-status-text))))

  ;; *** flycheck-tip
  (use-package flycheck-tip       ; display errors by popup
    ;; FIXME may or may not clobber company popups
    :commands flycheck-tip-use-timer
    :config (flycheck-tip-use-timer 'verbose))

  ;; *** flycheck/evil
  (bind-keys :map evil-normal-state-map
             (",,F" . helm-flycheck)
             (",,fj" . flycheck-next-error)
             (",,fl" . flycheck-previous-error))

  ;; *** flycheck/hooks
  (add-hook 'prog-mode-hook #'flycheck-mode))


;; ** lorem-ipsum
(use-package lorem-ipsum)              ; Insert filler text

;; ** magit
(use-package magit             ; Git version control management
  :delight magit-auto-revert-mode
  :bind (("C-x m" . magit-status))
  :init (setq
         magit-save-some-buffers 'dontask           ; don't ask before saving
         magit-last-seen-setup-instructions "1.4.0" ; clear startup message
         magit-diff-options '("-b")))               ; ignore whitespace in diffs

;; ** org
(use-package org
  :delight org-indent-mode
  :defines (org-export-in-background org-odt-preferred-output-format)
  :init (setq
         org-modules '(docview info inlinetask)
         org-export-backends '(ascii html odt)
         org-export-in-background t
         org-odt-preferred-output-format 'doc
         org-startup-folded t
         org-src-fontify-natively t     ; syntax highlight code in org buffer
         org-fontify-done-headline t    ; fontify whole done headline
         org-list-allow-alphabetical t) ; allow single-char alphabetical lists

  :config
  (set-face-attribute 'org-done nil
                      :inherit 'variable-pitch
                      :height 'unspecified
                      :strike-through t)
  (set-face-attribute 'org-headline-done nil
                      :inherit 'org-done)
  (set-face-attribute 'org-todo nil
                      :inherit 'variable-pitch
                      :height 'unspecified))

;; * faces
;; ** fic-ext
(use-package fic-ext-mode               ; Highlight annotations in comments
  :load-path "~/.emacs.d/elisp/" :ensure nil
  :commands fic-ext-mode
  :diminish fic-ext-mode
  :config
  (add-to-list 'fic-highlighted-words "FIXME?")
  (add-hook 'prog-mode-hook #'fic-ext-mode))

;; ** golden-ratio
(use-package golden-ratio               ; Resize windows to golden ratio
  ;; TODO get this to play nice with Helm
  :delight golden-ratio-mode
  :init (golden-ratio-mode t)
  :config (setq golden-ratio-exclude-modes '("helm-mode"
                                             "magit-log-mode"
                                             "magit-reflog-mode"
                                             "magit-status-mode")
                golden-ratio-auto-scale t))     ; auto scale with screen size

;; ** hl-line
(use-package hl-line :ensure nil        ; Highlight current cursor line
  :functions my/turn-off-hl-line-mode
  :config
  (add-hook 'prog-mode-hook #'hl-line-mode)
  (defun my/turn-off-hl-line-mode () (hl-line-mode -1))
  (add-hook 'eshell-mode-hook #'my/turn-off-hl-line-mode))

;; ** hl-sentence
(use-package hl-sentence                ; Highlight current sentence
  :config (set-face-attribute 'hl-sentence-face nil :inherit 'hl-line))

;; ** highlight-numbers
(use-package highlight-numbers          ; Highlight numbers
  :commands highlight-numbers--turn-on
  :config (add-hook 'prog-mode-hook #'highlight-numbers--turn-on))

;; ** page-break-lines
(use-package page-break-lines          ; Horizontal lines instead of ^L
  :delight page-break-lines-mode
  :functions global-page-break-lines-mode
  :config (global-page-break-lines-mode t))

;; ** nyan-mode
(use-package nyan-mode         ; Nyan cat scroll bar
  :demand t
  :commands (nyan-start
             nyan-stop
             nyan-start-music
             nyan-stop-music
             nyan-start-animation
             nyan-stop-animation
             nyan-mode)
  :functions my/turn-off-nyan-mode
  :config
  (defun nyan-start ()
    (interactive) (nyan-start-music) (nyan-start-animation)
    "Start the Nyan ☻.")

  (defun nyan-stop ()
    (interactive) (nyan-stop-music) (nyan-stop-animation)
    "Stop the Nyan ☹.")

  (defun my/turn-off-nyan-mode () (nyan-mode nil))

  (setq-default nyan-wavy-trail t)      ; TODO wavy nyan trail all the time
  (nyan-mode t))

;; ** rainbow-mode
(use-package rainbow-mode               ; Highlight color codes
  :delight rainbow-mode
  :commands rainbow-turn-on
  :config (add-hook 'prog-mode-hook #'rainbow-turn-on))

;; ** whitespace
(use-package whitespace :ensure nil     ; Faces for whitespace characters
  :demand t
  :delight whitespace-mode
  :init (setq
         whitespace-action '(auto-cleanup) ; clean bogus whitespace on write
         whitespace-line-column nil        ; use fill-column value
         whitespace-style '(face trailing lines-tail))
  :config (add-hook 'prog-mode-hook #'whitespace-mode))

;; ** writegood-mode
(use-package writegood-mode             ; Highlight poor forms in writing
  :delight writegood-mode
  :bind (("C-c C-g" . writegood-grade-level)
         ("C-c C-S-g" . writegood-reading-ease)))

;; ** color theme
(use-package solarized-theme
  :init (setq
         solarized-scale-org-headlines nil
         solarized-height-plus-1 1.05
         solarized-height-plus-2 1.05
         solarized-height-plus-3 1.05
         solarized-height-plus-4 1.05)
  :config (load-theme 'solarized-dark))
(use-package zenburn-theme :disabled t :config (load-theme 'hc-zenburn))

;; * interface
;; ** -anzu
(use-package anzu :disabled t)          ; Search match count in modeline

;; ** helm
(use-package helm-config :ensure helm   ; Fuzzy minibuffer completion
  :demand t
  :commands helm-info-emacs
  :defines (helm-semantic-fuzzy-match
            helm-completion-in-region-fuzzy-match
            helm-imenu-fuzzy-match
            helm-M-x-always-save-history
            helm-M-x-fuzzy-match)
  :delight helm-mode
  ;; *** helm/global bindings
  :bind ( ;; Helm replacements
         ("M-x" . helm-M-x)
         ("M-s o" . helm-occur)
         ("C-y" . show-kill-ring)
         ("C-x r i" . helm-register)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("<help> l" . helm-locate-library)
         ("<help> C-a" . helm-apropos)
         ("<help> C-l" . view-lossage)
         ;; Additional Helm functions
         ("<help> C-r" . helm-info-at-point)
         ("<help> C-w" . helm-man-woman)
         ("<help> i" . helm-info-emacs)
         ("<help> I" . helm-info-elisp)
         ("<help> M-i" . helm-info-at-point))
  ;; *** helm/init
  :init (setq
         helm-command-prefix-key "C-c h"
         helm-move-to-line-cycle-in-source t ; cycle on buffer end
         helm-display-header-line nil        ; no header line
         helm-scroll-amount 5                ; scroll amount in other window
         helm-split-window-in-side-p t       ; split inside current window
         helm-M-x-always-save-history t      ; save history even on fail
         helm-ff-auto-update-initial-value t ; auto update when only one match
         helm-ff-file-name-history-use-recentf t ; use recentf
         helm-ff-search-library-in-sexp t        ; get library from functions
         helm-ff-skip-boring-files t             ; skip irrelevant files
         helm-findutils-search-full-path t  ; search in full path with shell
         helm-findutils-skip-boring-files t ; skip irrelevant files in shell
         helm-adaptive-history-file (concat my/dir "helm-adaptive-history")
         ;; fuzzy matching everywhere
         helm-semantic-fuzzy-match t
         helm-completion-in-region-fuzzy-match t
         helm-locate-fuzzy-match t
         helm-buffers-fuzzy-matching t
         helm-M-x-fuzzy-match t
         helm-apropos-fuzzy-match t
         helm-lisp-fuzzy-completion t
         helm-imenu-fuzzy-match t
         helm-file-cache-fuzzy-match t
         helm-recentf-fuzzy-match t)
  ;; *** helm/config
  :config
  ;; silence byte compiler
  (use-package helm :commands helm-autoresize-mode)
  (use-package helm-adaptive :ensure nil :commands helm-adaptive-mode)
  (use-package helm-info :ensure nil :commands helm-info-emacs helm-info-elisp)

  ;; **** helm/third-party
  ;; ***** helm-company
  (with-eval-after-load 'company
    (use-package helm-company
      :init (bind-keys :map (company-mode-map company-active-map)
                       ("C-:" . helm-company))))

  ;; ***** helm-descbinds
  (use-package helm-descbinds           ; Describe bindings
    :init (helm-descbinds-mode t)
    :config (setq helm-descbinds-window-style 'split-window))

  ;; ***** helm-flycheck
  (with-eval-after-load 'flycheck
    (use-package helm-flycheck
      :config (bind-key "C-c ! l" #'helm-flycheck flycheck-mode-map)))

  ;; ***** helm-flyspell
  (use-package helm-flyspell)

  ;; ***** helm-projectile
  (with-eval-after-load 'projectile
    (use-package helm-projectile
      :config
      (setq projectile-completion-system 'helm
            projectile-switch-project-action #'helm-projectile
            helm-projectile-fuzzy-match t)
      (helm-projectile-on)))

  ;; ***** -helm-swoop
  (use-package helm-swoop :disabled t   ; Find things among multiple buffers
    :bind ("C-M-s" . helm-swoop)
    :config
    (bind-key "M-i" #'helm-swoop-from-isearch isearch-mode-map)
    (bind-key "M-i" #'helm-multi-swoop-from-helm-swoop helm-swoop-map))

  ;; **** helm/bindings
  (bind-keys
   :map helm-map
   ("<C-S-up>" . helm-scroll-other-window)
   ("<C-S-down>". helm-scroll-other-window-down)
   ([tab] . helm-execute-persistent-action) ; execute action without closing
   ("C-z" . helm-select-action))            ; list actions

  ;; **** helm/evil
  (bind-keys :map (evil-normal-state-map
                   evil-motion-state-map)
             ("SPC" . helm-M-x)
             (",b" . helm-mini)
             (",f" . helm-find-files)
             (",,hl" . helm-locate)
             (",,ho" . helm-occur))

  (bind-keys :map (evil-normal-state-map
                   evil-motion-state-map
                   evil-insert-state-map)
             ("C-c C-h" . helm-resume)
             ("C-y" . helm-show-kill-ring)
             ("C-/" . helm-semantic-or-imenu))

  ;; **** helm/enable
  (helm-adaptive-mode t)
  (helm-autoresize-mode t)
  (helm-mode t))

;; ** linum
(use-package linum :ensure nil          ; Line numbers
  :config
  (defun my/turn-off-linum-mode () (linum-mode -1))
  (add-hook 'prog-mode-hook #'linum-mode))

;; ** popwin
(use-package popwin                     ; Popup window for minor buffers
  :demand t
  :commands popwin-mode
  :config
  (setq popwin:popup-window-position 'right) ; show popup windows to right
  (dolist (config '(("*Python Help*" :stick t)
                    ("*Help*" :stick t)
                    ("*Backtrace*" :noselect t)))
    (add-to-list 'popwin:special-display-config config))
  (global-set-key (kbd "C-z") popwin:keymap) ; activate bindings
  (popwin-mode t))

;; ** smart-mode-line
(use-package smart-mode-line            ; Better modeline
  :demand t
  :defines (sml/use-projectile-p sml/projectile-replacement-format)
  :init
  (setq
   sml/theme nil                        ; allow override with powerline
   sml/battery-format "%b%p[%t]"
   sml/full-mode-string " ⋯"            ; append this to modeline when full
   sml/shorten-mode-string ""           ; no indication for all modes displayed
   sml/mode-width 'right                ; move modes to right and expand to fill
   sml/mule-info nil                    ; don't show buffer encoding
   sml/no-confirm-load-theme t          ; TODO fix so this isn't required
   sml/use-projectile-p t               ; projectile file prefix takes precedent
   sml/projectile-replacement-format "[π:%s]") ; format for projectile prefixes
  :config
  (setq sml/replacer-regexp-list
        (append sml/replacer-regexp-list
                '(("^~/dotfiles/" ":.:")
                  ("^:\\.:emacs/\\.emacs\\.d/" ":.ε:"))))

  (sml/setup)
  (use-package powerline :config (powerline-default-theme)))

;; ** wc-mode
(use-package my/wc-mode                 ; TODO fix this
  :load-path "~/.emacs.d/elisp/" :ensure nil
  :functions my/wc-mode
  :config (my/wc-mode t))

;; ** which-func
(use-package which-func :ensure nil     ; Modeline definition name
  :demand t
  :config
  (defun my/which-func-current ()
    (let ((current (or (gethash (selected-window) which-func-table) "")))
      (truncate-string-to-width
       (concat
        " ➤ "
        (replace-regexp-in-string "%" "%%" current))
       20 nil nil "⋯")))

  (setq which-func-format
        `((:propertize (:eval (my/which-func-current))
                       local-map ,which-func-keymap
                       face which-func
                       mouse-face mode-line-highlight)))
  (which-function-mode t))

;; ** winner
(use-package winner                     ; Window configuration undo
  ;; FIXME? may have to get super bindings working on MS-Windows
  :bind (("s-j" . winner-undo)
         ("s-k" . winner-redo))
  :config (winner-mode t))

;; ** writeroom-mode
(use-package writeroom-mode             ; Distraction-free writing mode
  :delight writeroom-mode " Σ"
  :bind ("C-q" . writeroom-mode)
  :config
  (setq writeroom-width 100)
  ;; Apply additional effects
  (add-hook 'writeroom-mode #'my/writeroom-effect))

;; * navigation
;; ** ace-jump-mode
;; Consider as possible alternative to relative line numbers with evil-mode
(use-package ace-jump-mode              ; Jump to specific points with marks
  :bind ("C-SPC" . ace-jump-mode))

;; ** -desktop
(use-package desktop :disabled t        ; Save and restore Emacs sessions state
  :init (setq desktop-path `(,(concat my/dir "desktops")))
  :config (desktop-save-mode t))

;; ** discover-my-major
(use-package discover-my-major          ; List current major mode bindings
  :bind ("<help> M-m" . discover-my-major))

;; ** -guide-key
(use-package guide-key :disabled t      ; Delayed completion for possible keys
  :config (guide-key-mode t))

;; ** help+
(use-package help+)                     ; Enhancements to builtin help
(use-package help-fns+)                 ; Add help functions

;; ** ibuffer
(use-package ibuffer
  :bind ("C-x b" . ibuffer)
  :init
  (setq ibuffer-default-shrink-to-minimum-size t ; minimize window size
        ibuffer-old-time 5           ; hours before a buffer is old
        ibuffer-read-only-char ?R    ; char for read-only
        ibuffer-use-other-window t)) ; display ibuffer in another window

;; ** savehist
(use-package savehist                   ; Save command history
  :init (setq savehist-file (concat my/dir "savehist")
              history-delete-duplicates t
              savehist-save-minibuffer-history t)
  :config (savehist-mode t))

;; ** saveplace
(use-package saveplace                  ; Save and restore cursor place in file
  :init (setq-default save-place-file (concat my/dir "places")
                      save-place t))

;; ** projectile
(use-package projectile                 ; Project-based navigation
  :demand t
  :commands (projectile-current-project-dirs
             projectile-project-p
             projectile-project-root)
  ;; *** projectile/init
  :init
  (setq
   projectile-enable-caching t          ; cache projectile indexes
   projectile-indexing-method 'alien    ; use faster OS methods
   ;; don't clutter my .emacs.d please
   projectile-cache-file (expand-file-name "projectile.cache" my/dir)
   projectile-known-projects-file (expand-file-name
                                   "projectile-known.eld" my/dir)
   ;; pretty Greek symbols
   projectile-mode-line '(:eval (format " π:%s" (projectile-project-name))))
  ;; *** projectile/config
  :config
  (bind-keys :map (evil-normal-state-map
                   evil-motion-state-map)
             (",F" . helm-projectile-find-file-dwim)
             (",p" . helm-projectile)
             (",P" . projectile-dired))

  (projectile-global-mode t))

;; * editing
;; ** abbrev
(use-package abbrev :ensure nil          ; Auto-correct words after typing
  :delight abbrev-mode
  :init (setq abbrev-file-name (concat my/dir-my-elisp "abbrevs.el")
              save-abbrevs 'silently     ; save abbrevs when saving file
              abbrev-all-caps t)         ; expand in all-caps if written in caps
  :config
  (abbrev-mode t))

;; ** auto-indent-mode
(use-package auto-indent-mode           ; Automatic indentation
  ;; TODO get this working with indenting pasted code
  :delight auto-indent-mode
  :commands (auto-indent-delete-backward-char
             auto-indent-remove-advice-p
             auto-indent-global-mode
             auto-indent-is-bs-key-p)
  :config (auto-indent-global-mode t))

;; ** company
(use-package company                    ; Autocompletion in code
  :demand t
  :init
  (setq company-idle-delay nil            ; attempt completion immediately
        company-show-numbers t            ; show quick-access nums
        company-auto-complete t           ; auto complete on special char insert
        company-lighter-base "Aψ"
        company-selection-wrap-around t)  ; wrap back around when selecting
  :config (global-company-mode t))

;; ** expand-region
(use-package expand-region               ; Expand functions block at a time
  :bind ("C-=" . er/expand-region))

;; ** flyspell
(use-package flyspell                   ; On-the-fly spell checking
  :delight flyspell-mode
  :init (setq flyspell-issue-welcome-flag nil  ; no start message
              flyspell-issue-message-flag nil) ; no checking message
  :config
  ;; *** flyspell-lazy
  ;; Don't mark words less than 3 chars
  (use-package flyspell-lazy
    :config
    (add-hook 'flyspell-mode #'flyspell-lazy-mode)
    (add-hook 'flyspell-prog-mode #'flyspell-lazy-mode))

  ;; *** flyspell/hooks
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;; ** outline
(use-package outline :ensure nil        ; Hierarchical outlining support
  :delight outline-minor-mode
  :bind (("C-c o" . outline-insert-heading))
  ;; *** outline/init
  :init (bind-keys :map (evil-normal-state-map
                         evil-motion-state-map)
                   ("gh" . outline-up-heading)
                   ("gj" . outline-next-heading)
                   ("gk" . outline-previous-heading)
                   ("gl" . outline-forward-same-level)
                   ("za" . outline-toggle-children)
                   ("C-c v" . outline-mark-subtree))
  :config
  ;; *** outline/outshine
  (use-package outshine                 ; Org-mode style with outline-mode
    :demand t
    :commands outshine-hook-function
    :bind (("C-c t" . outshine-todo)
           ("C-c h" . outline-promote)
           ("C-c l" . outline-demote))
    :init
    (setq outshine-org-style-global-cycling-at-bob-p t)

    (bind-keys :map (evil-normal-state-map
                     evil-motion-state-map)
               ([tab] . outline-cycle))
    :config (add-hook 'outline-minor-mode-hook #'outshine-hook-function))

  ;; *** outline/hooks
  (add-hook 'prog-mode-hook #'outline-minor-mode))

;; ** smartparens
(use-package smartparens-config         ; Balenced paren management
  :ensure smartparens
  :delight smartparens-mode
  :bind ("M-=" . sp-indent-defun)
  :config
  ;; TODO add evil bindings for sp commands
  (electric-pair-mode -1)        ; disable so we don't get duplicate quote marks
  (smartparens-global-mode t)
  (show-smartparens-mode t))

;; ** typo
;; TODO change EM-DASH character to second mark
(use-package typo :delight typo-mode)  ; Insert typographical characters

;; ** undo-tree
(use-package undo-tree                  ; Branching undo tree
  :delight undo-tree-mode
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-history" my/dir))))
  (bind-key "C-x u" 'undo-tree-visualize undo-tree-map)
  :config (global-undo-tree-mode t))

;; ** yasnippet
(use-package yasnippet
  :defer t
  :init (setq yas-snippet-dirs
                `(,(concat my/dir "snippets/") yas-installed-snippets-dir)))

;; * files
;; ** autorevert
(use-package autorevert :ensure nil      ; Auto revert to external modifications
  :delight autorevert-mode
  :init (setq global-auto-revert-non-file-buffers t) ; auto refresh buffer
  :config (global-auto-revert-mode t))

;; ** dash
;; Elisp List API
(use-package dash :commands -filter -first-item)

;; ** dired
(use-package dired :ensure nil          ; Emacs file browser
  :init
  (setq
   dired-auto-revert-buffer t           ; auto revert dired buffer on visit
   dired-backup-overwrite 'always       ; always make backup before overwrite
   dired-dwim-target t                  ; target other dired directory if exists
   dired-isearch-filenames 'dwim        ; isearch filenames if point on filename
   dired-no-confirm '(copy move symlink) ; don't confirm these operations
   dired-recursive-copies 'always        ; recursive copy by default
   dired-recursive-deletes 'top          ; confirm recursive delete
   dired-listing-switches "-lha")        ; human-readable listing
  (add-to-list 'evil-emacs-state-modes 'dired-mode) ; Evil initial state
  :config
  (use-package dired+                   ; Dired extensions and syntax highlight
    :init (setq diredp-dwim-any-frame-flag t)) ; allow dwim target other frame
  (add-hook 'dired-mode-hook #'auto-revert-mode))

;; ** files
(use-package files :ensure nil          ; File-related actions
  :init
  (setq
   require-final-newline t                ; newline at end of file
   large-file-warning-threshold 20000000  ; only warn at 20MB files
   find-file-visit-truename t             ; silently follow symlinks
   view-read-only t                       ; view read-only files in view-mode
   ;; autosave file location
   auto-save-list-file-prefix (concat my/dir "autosaves/")
   auto-save-file-name-transforms `((".*" ,auto-save-list-file-prefix t))
   ;; backups
   backup-directory-alist `(("." . ,(expand-file-name "backups" my/dir)))
   backup-by-copying t                  ; copy file into backup dir
   version-control t                    ; add version numbers
   delete-old-versions t                ; delete old backups silently
   kept-old-versions 5                  ; old versions to keep
   kept-new-versions 8)                 ; new versions to keep
  :config (cd "~"))                     ; start in home dir

;; ** image
(use-package image :ensure nil          ; Emacs image viewing
  ;; Don't normally open images in Emacs, but when I do I don't want them to
  ;; open in an external process.
  :config
  (use-package image-dired :ensure nil  ; Image manipulation in dired
    :init (setq image-dired-dir (concat my/dir "image-dired/")))
  (auto-image-file-mode t))                  ; view images in Emacs
;; ** pandoc
(use-package pandoc-mode                ; Markup conversion tool
  :defer t
  :config (add-hook 'org-mode-hook 'pandoc-mode))

;; ** real-auto-save
(use-package real-auto-save    ; Auto save buffers
  :commands real-auto-save-mode
  :delight real-auto-save-mode " α"
  :config (real-auto-save-mode t))

;; ** recentf
(use-package recentf                    ; List recent files
  :init (setq recentf-save-file (concat my/dir "recentf")
              recentf-menu-filter 'recentf-sort-ascending
              recentf-max-saved-items 50)
  :config (recentf-mode t))

;; ** uniquify
(use-package uniquify :ensure nil           ; Distinguish buffers with same name
  :init (setq uniquify-buffer-name-style 'forward
              uniquify-trailing-separator-p t)) ; add separator to dired names

;; * applications
;; ** calc
(use-package calc                       ; Advanced calculator
  ;; don't clutter my .emacs.d
  :config (setq calc-settings-file (concat my/dir-my-elisp "calc.el")))

;; ** calendar
(use-package calendar
  ;; need to get used to using
  :config (setq calendar-date-style 'european)) ; logical date display please

;; ** doc-view
(use-package doc-view :ensure nil       ; In-buffer document viewer
  :config (setq doc-view-continuous t))        ; reaching page end advances page

;; ** -elim
(use-package garak :disabled t          ; ELIM messenger front-end
  :enabled nil
  :load-path (concat my/dir-elisp "elim"))

;; ** eshell
(use-package eshell                     ; Emacs shell
  :bind (("<f12>" . eshell))
  :config
  ;; *** esh-module
  (use-package esh-module :ensure nil   ; Eshell module loader
    :config
    (setq
     ;; Add eshell-smart, remove eshell-banner
     eshell-modules-list
     '(eshell-smart eshell-alias eshell-basic eshell-cmpl eshell-dirs
                    eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt
                    eshell-script eshell-term eshell-unix)))

  ;; *** eshell-cmpl
  (use-package em-cmpl :ensure nil            ; Eshell completion
    :config (setq eshell-cmpl-ignore-case t)) ; ignore case in file completion
  (add-to-list 'eshell-modules-list 'eshell-smart)

  ;; *** eshell-prompt
  (use-package em-prompt :ensure nil :disabled t
    :config
    (setq
     eshell-prompt-regexp "t"
     eshell-prompt-function "t"))

  ;; *** eshell settings
  (setenv "PAGER" "cat")                ; show extended output in other buffer
  (setq eshell-directory-name (concat my/dir "eshell/")))

;; ** fortune
(use-package fortune
  :config
  ;; Use UNIX system fortunes
  (setq fortune-dir "/USSR/share/games/fortunes"
        fortune-file "~/.local/share/fortunes"))

;; ** malyon
(use-package malyon :ensure nil)         ; Z-machine text-based-adventure reader

;; ** jabber
;; XMPP Chat client
(use-package jabber  :disabled t
  :config
  (setq jabber-account-list '(("kelly.stewart7737@chat.facebook.com"
                               (:network-server . "chat.facebook.com")
                               (:connection-type . starttls)
                               (:port . 5222)
                               (:password . ,my/ac-fb-pw)))))
;; ** ispell
(use-package ispell
  :config (setq ispell-silently-savep t       ; save without asking confirmation
                ispell-quietly t              ; no messages please
                ispell-personal-dictionary "~/.local/share/aspell/aspell.pws"))

;; * languages
;; ** BBCode
(use-package bbcode-mode )

;; ** generic-x
(use-package generic-x :ensure nil      ; Collection of generic modes
  :config (dolist (mode generic-mswindows-modes)            ; define extra modes
          (add-to-list 'generic-extras-enable-list mode)))

;; ** gitignore
(use-package gitignore-mode)

;; ** lisp
;; *** eldoc-mode
(use-package eldoc                      ; Documentation in echo area
  :config
  (setq eldoc-idle-delay 0.3)
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode t))

;; *** highlight-quoted
(use-package highlight-quoted  ; Faces for lisp quotes and quoted symbols
  :config (add-hook 'emacs-lisp-mode-hook 'highlight-quoted--turn-on t))

;; *** elisp-slime-nav
(use-package elisp-slime-nav   ; Navigate elisp documentation
  :delight elisp-slime-nav-mode
  :config
  (bind-keys :map (evil-normal-state-map
                   evil-motion-state-map)
             ("K" . elisp-slime-nav-describe-elisp-thing-at-point)
             ("gd" . elisp-slime-nav-find-elisp-thing-at-point))

  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode))


;; ** lua
(use-package lua-mode)

;; ** markdown
(use-package markdown-mode  :disabled t)

;; ** python
(use-package python
  :init (setq
         python-shell-interpreter "python3" ; use Python 3
         python-indent-offset 4)            ; TODO? is this being overridden
  :config
;; *** python/elpy
  (use-package elpy
    :config
    (setq elpy-modules (delete 'elpy-module-yasnippet elpy-modules))
    (elpy-enable)))

;; ** sh
(use-package sh-script
  :init (setq sh-learn-basic-offset t         ; guess indentation when obvious
              sh-basic-offset 2))             ; indent by 2

;; ** vimrc
;; VimScript major mode. Also for editing Pentadactyl config files.
(use-package vimrc-mode :mode "[._]pentadactylrc\\'" "\\.penta\\'")

;; ** web
(use-package web-mode :disabled t
  :mode "\\.html?\\'")

;; * unsorted
;; ** -handlebars
(use-package handlebars-mode :disabled t) ; What.

;; ** -try
(use-package try :disabled t)           ; No idea what this does

;; * cleanup
;; Confirm before killing Emacs in GUI sessions.
;; At end of file to make init debugging easier.
(setq confirm-kill-emacs (if (window-system) 'yes-or-no-p nil))

;;; init.el ends here
;; Local Variables:
;; outline-regexp: " *;; [*]"
;; End:
