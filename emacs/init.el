;;; Setup

;;;; Helper functions
(defun my-add-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR if not already in list.

Return new value of LIST-VAR. If LIST-VAR is not defined, then
define it as ELEMENTS."
  (let ((l (if (listp elements) elements (list elements))))
    (if (and (boundp list-var) (symbol-value list-var))
        (dolist (item l) (add-to-list list-var item))
      (set list-var l)))
  (symbol-value list-var))


;;;; Constants
(defconst my-dir
  (expand-file-name "emacs/" (getenv "XDG_CACHE_HOME"))
  "Personal elisp settings files and generated files.")

(defconst my-dir-elisp
  (expand-file-name "my-elisp/" user-emacs-directory)
  "Personal elisp files.")
(add-to-list 'load-path my-dir-elisp)

(defconst my-dir-packages
  (expand-file-name "elisp/" user-emacs-directory)
  "Directory for packages not available through a repository.")
(add-to-list 'load-path my-dir-packages)

(defvar my-win-p (eq system-type 'windows-nt) "Non-nil if using MS Windows.")
(defvar my-desktop-p (not (string= system-name "hraesvelgr")) "Non-nil if on desktop machine")

(setq custom-file (expand-file-name "custom.el" my-dir))
(load custom-file 'no-error 'no-message)

;;;; Package
(require 'package)
(setq package-enable-at-startup nil     ; we will manually initialize
      load-prefer-newer t)              ; don't load outdated byte code
(my-add-list 'package-archives
             '(("melpa" . "http://melpa.milkbox.net/packages/")
               ("org" . "http://orgmode.org/elpa/")
               ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)                    ; manually initialize

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  ;; required before use-package is loaded
  (defvar use-package-enable-imenu-support)
  (setq use-package-enable-imenu-support t)
  (require 'use-package))
(setq
 use-package-verbose t             ; log message after loading a package
 use-package-always-ensure t       ; ensure all packages are installed
 use-package-always-defer t)       ; defer all packages
(use-package general)


;;; Basic settings
(setq
 inhibit-startup-screen t               ; no startup screen
 initial-scratch-message nil            ; no scratch message
 disabled-command-function nil          ; no disabled commands
 ring-bell-function 'ignore             ; no bell
 initial-major-mode 'fundamental-mode   ; scratch in fundamental mode
 scroll-conservatively 101              ; don't recenter when scrlling
 scroll-margin 3                    ; lines of context to keep in view
 comment-auto-fill-only-comments t  ; only auto fill in comments
 frame-title-format "%b "           ; buffer name as frame title
 echo-keystrokes 0.1                ; faster command echo
 save-interprogram-paste-before-kill t ; don't overwrite clipboard contents
 kill-do-not-save-duplicates t     ; don't put duplicates in kill ring
 sentence-end-double-space nil
 read-file-name-completion-ignore-case t
 vc-handled-backends '(Git)           ; remove unnecessary vc backends
 delete-by-moving-to-trash t
 large-file-warning-threshold 20000000  ; larger warning threshold
 view-read-only t                       ; read-only in view-mode
 ;; backups and autosave
 auto-save-list-file-prefix (expand-file-name "autosaves/" my-dir)
 auto-save-file-name-transforms `((".*" ,auto-save-list-file-prefix t))
 backup-directory-alist `(("." . ,(expand-file-name "backups" my-dir)))
 backup-by-copying t                    ; always backup by copy
 version-control t                      ; add version numbers
 delete-old-versions t                  ; delete old backups silently
 kept-old-versions 5
 kept-new-versions 8
 ;; user info
 user-full-name "Kelly Stewart"
 user-mail-address "stewart.g.kelly@gmail.com")

(setq-default fill-column 100
              indent-tabs-mode nil
              tab-width 4)

(fset #'yes-or-no-p #'y-or-n-p)
(fset #'display-startup-echo-area-message #'ignore)


;; never kill scratch
(defun no-kill-scratch ()
  "Bury scratch buffer instead of killing"
  (if (equal (buffer-name) "*scratch*")
      (progn (bury-buffer) nil) t))
(add-to-list 'kill-buffer-query-functions #'no-kill-scratch)

;; auto close compilation buffer on success
(defun auto-close-compile (buf result)
  (let ((win (get-buffer-window buf 'visible)))
    (when (and win (not (string-match ".*exited abnormally.*" result)))
      (delete-window win))))
(add-to-list 'compilation-finish-functions #'auto-close-compile)

;; display last buffer on window split
(defun my-last-buf (&rest _args)
  "Switch to other window and display previous buffer."
  (other-window 1 nil)
  (switch-to-next-buffer))
(dolist (func '(split-window-horizontally
                split-window-vertically))
  (advice-add func :after-while #'my-last-buf))

;;;; Useful Functions
(defun new-emacs ()
  "Launch a new GUI emacs under X or Windows as necessary."
  (let* ((win-p (memq system-type '(windows-nt ms-dos)))
         (path (expand-file-name
                (if win-p "runemacs.exe" invocation-name)
                invocation-directory)))
    (if win-p
        (w32-shell-execute "open" path)
      (call-process "sh" nil 0 nil "-c" path))))

(defun restart-emacs ()
  "Restart emacs under X by first launching a new process, and then killing."
  (interactive)
  (let ((kill-emacs-hook (append kill-emacs-hook (list #'new-emacs))))
    (save-buffers-kill-emacs)))

(defun sudo-save ()
  "Save current buffer with sudo."
  (interactive)
  (write-file
   (concat
    "/sudo::"
    (cond (buffer-file-name)
          ((fboundp 'helm-read-file-name) (helm-read-file-name "File: "))
          ((fboundp 'ido-read-file-name) (ido-read-file-name "File: "))))))

(defun my-narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        (t (narrow-to-defun))))

(defun my-comment-dwim (orig-fun &rest args)
  "First toggle line comment if no active region and point is not at end of line."
  (interactive "*P")
  (if (and (not (region-active-p)) (not (eq (point) (line-end-position))))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (apply orig-fun args)))
(advice-add #'comment-dwim :around #'my-comment-dwim)

;;;; Modes and hooks
(show-paren-mode t)                     ; highlight matching parens
(electric-pair-mode t)                  ; auto add parens
(menu-bar-mode -1)                      ; no menu bar
(tool-bar-mode -1)                      ; no toolbar
(scroll-bar-mode -1)                    ; no scrollbar
(blink-cursor-mode -1)                  ; no blinking cursor
(unless (display-graphic-p) (menu-bar-mode -1)) ; no menu bar in terminal

;; Mode hooks
(dolist (hook '((prog-mode-hook
                 prettify-symbols-mode  ; replace words with symbols
                 auto-fill-mode         ; auto fill text past fill-col
                 goto-address-prog-mode ; buttonize URLs in comments and strings
                 line-number-mode       ; line number in modeline
                 )
                (text-mode-hook
                 goto-address-mode      ; buttonize URLs
                 visual-line-mode       ; wrap by word
                 variable-pitch-mode
                 )))
  (dolist (func (cdr hook)) (add-hook (car hook) func)))

;;;; Bindings
(defvar my-window-map "C-w" "Keymap for my window related commands")

(general-define-key
 "M-SPC" #'cycle-spacing
 "RET" #'newline-and-indent
 "C-x C-S-c" #'restart-emacs
 "C-x n" #'my-narrow-or-widen-dwim
 "<up>" #'scroll-down
 "<down>" #'scroll-up
 "M-/" #'grep
 "C-M-/" #'find-grep-dired
 [remap switch-to-buffer] #'ibuffer
 "C-w" nil)

(general-define-key
 :prefix my-window-map
 "C-w" #'other-window
 "w" #'other-window
 "s" #'split-window-vertically
 "C-s" #'split-window-horizontally
 "=" #'balance-windows
 "f" #'delete-frame
 "d" #'delete-window
 "C-d" #'delete-other-windows)

(general-define-key
 :keymaps 'help-mode-map
 "H" #'help-go-back
 "L" #'help-go-forward)
(general-define-key
 :keymaps 'help-map
 "<f1>" #'apropos
 "w" #'woman
 "k" #'describe-key-briefly
 "K" #'describe-key
 "C-k" #'describe-bindings
 "m" #'describe-mode
 "M-k" #'where-is
 "c" #'describe-face
 "l" #'locate-library
 "M-l" #'view-lossage
 "u" #'insert-char
 "i" #'info-lookup-symbol
 "C-i" #'info-emacs-manual)

(general-define-key :keymaps 'undo-tree-map "C-/" nil)
(general-define-key :keymaps 'undo-tree-map :states 'normal "U" #'undo-tree-redo)

(general-define-key :keymaps 'emacs-lisp-mode-map "C-c C-c" #'eval-defun)

;;; Major packages
;;;; Evil
(use-package evil
  :init
;;;;; Evil Packages
  (use-package evil-escape      ; escape from everything with two keys
    :init (add-hook 'evil-mode-hook #'evil-escape-mode))

  (use-package evil-surround            ; surround operator
    :init (add-hook 'prog-mode-hook #'evil-surround-mode))

  (use-package evil-matchit             ; more tag jumping support
    :general (:keymaps 'evil-matchit-mode-map [remap evil-jump-item] #'evilmi-jump-items)
    :init (add-hook 'evil-mode-hook #'global-evil-matchit-mode)
    :config
    (setq evilmi-may-jump-by-percentage nil)) ; allow count usage

;;;;; Evil Init
  (defvar my-evil-leader-map (make-sparse-keymap) "Evil leader bindings")
  (define-prefix-command 'my-evil-leader-map)
  (evil-mode t)
;;;;; Evil Bindings
  :general
  (:keymaps 'motion
            "TAB" nil
            "C-w" nil
            "<up>" nil
            "<down>" nil
            "SPC" #'execute-extended-command
            "q" #'kill-this-buffer
            "," #'my-evil-leader-map
            "\"" #'evil-jump-item
            "Q" #'evil-record-macro
            [remap evil-next-line] #'evil-next-visual-line
            [remap evil-previous-line] #'evil-previous-visual-line)
  (:keymaps 'normal
            "q" nil
            "\"" #'evil-jump-item)
  (:keymaps 'my-evil-leader-map
            "f" #'find-file
            "b" #'list-buffers
            "w" #'save-buffer
            "W" #'sudo-save
            "SPC" #'evil-ex)

;;;;; Evil Config
  :config
  (setq evil-echo-state nil             ; state is in modeline anyway
        evil-cross-lines t
        evil-symbol-word-search t
        evil-ex-substitute-global t)    ; global sed by default

  (my-add-list 'evil-emacs-state-modes
               '(shell-mode term-mode multi-term-mode package-menu-mode))
  ;; yank to EOL
  (evil-add-command-properties #'evil-yank-line :motion 'evil-end-of-line))

;;;; Helm
(use-package helm-config :ensure helm
  :init
;;;;; Helm Packages
  (use-package helm-dash :disabled t)   ; TODO language documentation

  (use-package helm-descbinds        ; `describe-bindings' replacement
    :init (add-hook 'helm-mode-hook #'helm-descbinds-mode)
    :config (setq helm-descbinds-window-style 'split-window))

  (use-package helm-swoop               ; fast search and navigation
    :general
    ([remap grep] #'helm-swoop)
    (:keymaps 'isearch-mode-map "M-/" #'helm-swoop-all-from-isearch)
    (:keymaps 'helm-swoop-map "M-/" #'helm-multi-swoop-all-from-helm-swoop)

    :config
    (setq helm-swoop-speed-or-color t
          helm-swoop-use-fuzzy-match t))

;;;;; Helm Init
  (helm-mode t)
  
;;;;; Helm Bindings
  :general
  ([remap execute-extended-command] #'helm-M-x
   [remap occur] #'helm-occur
   [remap find-file] #'helm-find-files
   [remap switch-to-buffer] #'helm-mini
   [remap list-buffers] #'helm-mini
   [remap ibuffer] #'helm-mini
   [remap evil-paste-pop] #'helm-show-kill-ring
   ;; help
   [remap describe-face] #'helm-colors
   [remap insert-char] #'helm-ucs
   [remap info-emacs-manual] #'helm-info-emacs
   [remap info-lookup-symbol] #'helm-info-at-point
   [remap locate-library] #'helm-locate-library
   [remap woman] #'helm-man-woman
   [remap manual-entry] #'helm-man-woman
   [remap apropos] #'helm-apropos
   [remap apropos-command] #'helm-apropos
   [remap apropos-documentation] #'helm-apropos)

  (:keymaps 'emacs-lisp-mode-map "C-/"#'helm-semantic-or-imenu)
  (:keymaps 'helm-buffer-map
            "C-d" #'helm-buffer-run-kill-buffer
            "<C-return>" #'helm-buffer-switch-other-window)
  (:keymaps 'helm-map
            "M-d" #'helm-scroll-other-window
            "M-u" #'helm-scroll-other-window-down)
;;;;; Helm Config
  :config
  (set-face-attribute 'helm-buffer-directory nil :inherit 'dired-directory
                      :foreground nil :background nil)
  (set-face-attribute 'helm-buffer-saved-out nil
                      :inherit 'font-lock-warning-face
                      :foreground nil :background nil :inverse-video nil)

  (use-package helm-mode :ensure nil
    :config
    (setq helm-completion-in-region-fuzzy-match t
          helm-mode-fuzzy-match t))
  (use-package helm-files :ensure nil
    :config
    (setq helm-ff-auto-update-initial-value t
          helm-ff-file-name-history-use-recentf t
          helm-ff-skip-boring-files t)) 

  (setq helm-move-to-line-cycle-in-source t     ; cycle on end
        helm-scroll-amount 5                    ; other window scroll
        helm-split-window-in-side-p t           ; split in current window
        helm-findutils-skip-boring-files t)
  (helm-autoresize-mode t))

;;; Help

(use-package discover-my-major      ; list current major mode bindings
  :general ([remap describe-mode] #'discover-my-major))

(use-package guide-key            ; delayed completion for prefix keys
  :init (guide-key-mode t)
  :config (setq guide-key/recursive-key-sequence-flag t
                guide-key/guide-key-sequence '("C-x" "C-c" "C-w" ",")))

(use-package info
  :config
  (add-to-list 'Info-directory-list (expand-file-name "info" (getenv "XDG_CONFIG_HOME"))))

;;; Appearance
;;;; Theme
(use-package gruvbox-theme
  :demand t
  :config (load-theme 'gruvbox)
  (set-face-attribute 'default nil :foreground "#ebdbb2")
  (set-face-attribute 'highlight nil :background "#3c3836")
  (set-face-attribute 'shadow nil :background "#1d2021"))

;;;; Modeline
(use-package smart-mode-line
  :commands sml/faces-from-theme
  :init
  (sml/setup)
  :config
  (setq
   rm-blacklist
   '(" Fly" " ARev" " Wrap" " BufFace" " Eldoc" " RAS" " Outl" " SliNav" " ws"
     " PgLn" " Guide" " Helm" " Undo-Tree" " Fill" " fd" " Abbrev")
   sml/replacer-regexp-list
   '(("^~/\\.emacs\\.d/elpa/" ":ELPA:")
     ("^~/dotfiles/" ":.:")
     ("^~/doc/" ":Doc:")
     ("^~/\\([^/]+\\)/" ":\\1:"))))

(use-package nyan-mode                  ; essential package
  :init (nyan-mode t)
  :config
  ;; FIXME this doesn't work
  (setq nyan-wavy-trail t))

(use-package which-func                 ; Modeline definition name
  :init (which-function-mode t)
  :config
  (set-face-attribute 'which-func nil :foreground nil :inherit 'font-lock-function-name-face))

(use-package wc-goal-mode               ; Modeline word count
  :init (add-hook 'text-mode-hook #'wc-goal-mode))

;;;; Faces
(use-package hl-line
  :init (add-hook 'prog-mode-hook #'hl-line-mode)
  :config (setq hl-line-face 'highlight))

(use-package hl-sentence
  :init (add-hook 'text-mode-hook #'hl-sentence-mode)
  :config (set-face-attribute 'hl-sentence-face nil :inherit 'highlight))

(use-package highlight-numbers
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package page-break-lines         ; Horizontal lines instead of ^L
  :init (global-page-break-lines-mode t))

(use-package rainbow-delimiters
  :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode)

(use-package fill-column-indicator
  :init (add-hook 'prog-mode-hook #'fci-mode)
  :config (setq fci-rule-color "#3c3836"
                fci-always-use-textual-rule t))

;;;; My faces
(set-fontset-font "fontset-default" nil (font-spec :size 20 :name "Symbola"))

(defvar my-font-faces
  (if my-desktop-p
      '((default nil :family "Iosevka" :height 100)
        (variable-pitch nil :family "InputSerifCompressed" :height 90)
        (font-lock-comment-face nil :slant italic)
        (font-lock-string-face nil :inherit variable-pitch))
    '((default nil :family "TamzenForPowerline" :height 109)
      (variable-pitch nil :family "Noto Sans" :height 110)))
  "Font faces dependant on the current machine")

(dolist (face my-font-faces) (apply #'set-face-attribute face))

(defface my-headline-face
  `((t ,@(if my-desktop-p '(:family "FabfeltScript Bold" :height 150)
           '(:family "Superscript" :height 150))))
  "Face for headlines"
  :group 'faces)


;; TODO make this a separate package
(defface font-lock-fic-face
  '((t (:underline t :bold nil :inherit error)))
  "Face to fontify FIXME/TODO words"
  :group 'faces)

(defun my-highlight-fic ()
  "Highlight FIXME and TODO keywords."
  (font-lock-add-keywords
   nil `((,(regexp-opt '("TODO" "TODO?" "FIXME" "FIXME?" "DOING" "REVIEW"
                         "HACK") t)
          1 'font-lock-fic-face prepend))))

(add-hook 'prog-mode-hook #'my-highlight-fic)

;;; Interface

(use-package simple :ensure nil
  :config (setq find-file-visit-truename t))

(use-package zone                       ; Screensaver
  :config
  (use-package zone-nyan
    :config (setq zone-programs [zone-nyan])))

(use-package golden-ratio :disabled t   ; Window resizing
  :init (golden-ratio-mode t))

(use-package hideshow                   ; Code folding
  :general
  (:keymaps 'hs-minor-mode-map
            "TAB" #'hs-toggle-hiding
            "<C-tab>" #'hs-hide-level)
  :init
  (use-package hideshowvis)
  (add-hook 'java-mode-hook #'hs-minor-mode)
  :config)

(use-package popwin :disabled t       ; Popup window for minor buffers
  :init (popwin-mode t))

(use-package uniquify                 ; Distinguish same-named buffers
  :ensure nil
  :config (setq uniquify-buffer-name-style 'forward
                uniquify-trailing-separator-p t))

(use-package winner     :disabled t     ; Window configuration undo
  :general (:prefix my-window-map
                    "u" #'winner-undo
                    "U" #'winner-redo)
  :init (winner-mode t))

(use-package writeroom-mode            ; Distraction-free writing mode
  :general (:keymaps 'text-mode-map "<C-f11>" #'writeroom-mode)
  :functions my-writeroom-effect
  :config
  (setq writeroom-width 100)
  (defun my-writeroom-effect (arg)
    (let ((off (if arg -1 t))
          (on (if arg t -1)))
      (when (fboundp #'display-time-mode) (display-time-mode on))
      (when (fboundp #'display-battery-mode) (display-battery-mode on))
      (when (fboundp #'which-function-mode) (which-function-mode off))
      (when (fboundp #'nyan-mode) (nyan-mode off))))
  (add-to-list 'writeroom-global-effects #'my-writeroom-effect))

(use-package visual-fill-column                                 ; Wrap text at fill col
  :init (add-hook 'text-mode-hook #'visual-fill-column-mode)
  :config (setq-default visual-fill-column-width 100))

;;; Navigation
(use-package avy                        ; Jump to specific points
  :general
  ("C-e" #'avy-goto-word-1 "C-S-e" #'avy-goto-line)
  (:keymaps 'evil-motion-state-map "C-e" nil)
  (:keymaps 'org-mode-map "C-e" nil)
  :config (setq avy-background t))

(use-package ace-window :disabled t
  :general ([remap other-window] #'ace-window)
  :config (setq aw-keys (or avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(use-package desktop :disabled t        ; Save opened buffers, windows, frames
  :init (desktop-save-mode t)
  :config
  (setq desktop-auto-save-timeout 60
        desktop-dirname my-dir)
  (add-to-list 'desktop-path desktop-dirname)
  (my-add-list 'desktop-modes-not-to-save
               '(magit-mode git-commit-mode)))

(use-package expand-region :disabled t  ; Expand selection by blocks at a time
  :general (:keymaps 'motion "zz" #'er/expand-region)
  :config (setq expand-region-contract-fast-key "x"))

(use-package savehist                   ; Save command history
  :init (savehist-mode t)
  :config (setq savehist-file (expand-file-name "savehist" my-dir)
                history-delete-duplicates t
                savehist-save-minibuffer-history t))

(use-package saveplace              ; Save and return to place in file
  :demand t
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "places" my-dir)))

(use-package projectile                 ; Project-based navigation
  :general (:keymaps 'my-evil-leader-map
                     "p" #'projectile-find-file-dwim
                     "P" #'projectile-switch-project)
  :init (projectile-global-mode t)
  :config
  (setq projectile-cache-file (expand-file-name "projectile.cache" my-dir)
        projectile-known-projects-file (expand-file-name "projectile.eld" my-dir)
        projectile-mode-line
        '(:eval (if (file-remote-p default-directory)
                    " Projectile"
                  (format " Projectile[%s]" (projectile-project-name)))))

  (use-package helm-projectile
    :general (:keymaps 'my-evil-leader-map
                       [remap projectile-find-file-dwim] #'helm-projectile
                       [remap projectile-switch-project] #'helm-projectile-switch-project)
    :init (helm-projectile-on)
    :config (setq projectile-completion-system 'helm
                  projectile-switch-project-action #'helm-projectile
                  helm-projectile-fuzzy-match t)))

;;; Editing

(use-package multiple-cursors :disabled t
  :init (multiple-cursors-mode t))

(use-package drag-stuff :disabled t
  :init (drag-stuff-global-mode t))

(use-package flycheck
  :general (:keymaps 'my-evil-leader-map
                     "cj" #'flycheck-next-error
                     "ck" #'flycheck-previous-error)
  :init (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (setq flycheck-keymap-prefix (kbd "C-c f")
        flycheck-indication-mode 'right-fringe)

  (use-package helm-flycheck
    :general
    (:keymaps 'flycheck-mode-map "C-c f c" #'helm-flycheck)
    (:keymaps 'my-evil-leader-map "cc" #'helm-flycheck))

  (use-package flycheck-tip             ; Display errors by popup
    :general
    ([remap flycheck-next-error] #'flycheck-tip-cycle
     [remap flycheck-previous-error] #'flycheck-tip-cycle-reverse)
    :config (flycheck-tip-use-timer 'normal)))

(use-package lorem-ipsum                ; Insert filler text
  :functions my-lorem-sgml-settings
  :config
  (defun my-lorem-sgml-settings ()
    (setq lorem-ipsum-paragraph-separator "\n<p>"
          lorem-ipsum-sentence-separator " "))
  (add-hook 'sgml-mode-hook #'my-lorem-sgml-settings)
  (unless sentence-end-double-space (setq lorem-ipsum-sentence-separator " ")))

(use-package writegood-mode          ; Highlight poor forms in writing
  :init (add-hook 'text-mode-hook #'writegood-mode)
  :config
  (add-hook 'writegood-mode-hook #'writegood-passive-voice-turn-off)
  (my-add-list 'writegood-weasel-words
               '("thing" "different" "probably" "really")))

(use-package abbrev                     ; Auto-correct
  :ensure nil
  :init (abbrev-mode t)
  :config (setq save-abbrevs 'silently
                abbrev-all-caps t
                abbrev-file-name (expand-file-name "abbrevs.el" my-dir)))

(use-package auto-indent-mode :disabled t
  :init (auto-indent-global-mode t))

(use-package autoinsert :disabled t     ; Auto insert buffer text
  :init (auto-insert-mode t))

(use-package company                    ; Autocompletion
  :general (:keymaps 'company-active-map
                     "M-j" #'company-select-next
                     "M-k" #'company-select-previous
                     "M-d" #'company-show-doc-buffer)
  :init (add-hook 'prog-mode-hook #'company-mode)
  :config

  ;; Make it work with `fci-mode'
  (defvar company-fci-mode-on-p nil "Whether `fci-mode' is on.")

  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

  (setq company-idle-delay 0            ; immediate completion attempt
        company-show-numbers t          ; allow M-num selection
        company-tooltip-align-annotations t
        company-selection-wrap-around t)

  (remove 'company-dabbrev 'company-backends))

(use-package ispell                     ; Spell checking
  :functions my-ispell-run-together
  :init (when my-win-p (add-to-list 'exec-path "C:\\Program Files\\Emacs\\Aspell\\bin"))
  :config
  (setq ispell-program-name "aspell"
        ispell-dictionary "british-ize"
        ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB")
        ispell-silently-savep t
        ispell-quietly t)

  ;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
  (defun my-ispell-run-together (orig-func &rest args)
    "Use ispell --run-together options while ORIG-FUNC is being called."
    (let ((old-ispell-extra-args ispell-extra-args)
          (run-together-args '("--run-together"
                               "--run-together-limit=5"
                               "--run-together-min=2")))
      (ispell-kill-ispell t)
      (setq ispell-extra-args (append ispell-extra-args run-together-args))
      (apply orig-func args)
      (setq ispell-extra-args old-ispell-extra-args)
      (ispell-kill-ispell t)))
  (advice-add #'ispell-word :around #'my-ispell-run-together)

  (use-package flyspell                 ; On-the-fly spell check
    :init
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)

    :config
    (use-package flyspell-popup
      :general ([remap ispell-word] #'flyspell-popup-correct))

    (use-package flyspell-lazy
      :init
      (add-hook 'flyspell-mode #'flyspell-lazy-mode)
      (add-hook 'flyspell-prog-mode #'flyspell-lazy-mode))

    (setq flyspell-issue-welcome-flag nil
          flyspell-issue-message-flag nil)
    (advice-add #'flyspell-auto-correct-word :around #'my-ispell-run-together)))

(use-package smartparens-config :disabled t     ; Balanced parenthesis management
  :ensure smartparens)

(use-package autorevert           ; Auto revert external modifications
  :init (global-auto-revert-mode t)
  :config (setq global-auto-revert-non-file-buffers t))

(use-package pandoc-mode                ; Markup conversion tool
  :general
  ("C-c C-p" #'pandoc-mode)
  (:keymaps 'pandoc-mode-map
            [remap pandoc-mode] #'pandoc-main-hydra/body
            "C-c C-S-p" #'pandoc-run-pandoc)
  :config (setq pandoc-data-dir (expand-file-name "pandoc" my-dir)))

(use-package real-auto-save             ; Auto save buffers
  :commands real-auto-save-mode
  :init (add-hook 'after-save-hook #'real-auto-save-mode)
  :config (setq real-auto-save-interval 60))

(use-package recentf                    ; List recent files
  :config
  (setq recentf-exclude '("COMMIT_EDITMSG") ; exclude commit messages
        recentf-save-file (expand-file-name "recentf" my-dir)))

(use-package compile
  :config (setq compilation-always-kill t ; kill old before starting new
                compilation-skip-threshold 2 ; skip warning and info messages
                compilation-context-lines 3
                compilation-scroll-output 'first-error
                compilation-auto-jump-to-first-error t
                compilation-ask-about-save nil))

(use-package paradox                    ; Better package management
  :config
  (use-package async)
  (setq paradox-execute-asynchronously t))

(use-package ediff                      ; Emacs diff utility
  :general (:keymaps 'ediff-mode
                     "j" #'ediff-next-difference
                     "k" #'ediff-previous-difference))

(use-package magit
  :general
  ("<f10>" #'magit-status)
  (:keymaps 'magit-status-mode-map
            "SPC" #'execute-extended-command
            "j" #'next-line
            "k" #'previous-line
            "C" #'magit-commit
            "C-=" #'magit-diff-working-tree)
  :config
  (defun my-git-commit-setup-fun ()
    (when (fboundp #'visual-line-mode) (visual-line-mode -1))
    (when (fboundp #'auto-fill-mode) (auto-fill-mode t)))
  (add-hook 'git-commit-setup-hook #'my-git-commit-setup-fun))

(use-package git-timemachine           ; Travel through commit history
  :general ("<C-f10>" #'git-timemachine-toggle))

;;; Languages
(use-package gitignore-mode)
(use-package arduino-mode)
(use-package ess)
(use-package vimrc-mode :mode "[._]?(pentadactyl|vimperator)rc$" "\\.(penta|vimp)$")
(use-package generic-x :disabled t)
(use-package elpy :init (elpy-enable))
(use-package lua-mode :config (setq lua-indent-level 2))

(use-package markdown-mode
  :config
  (setq markdown-header-face 'my-headline-face))

;;;; Org
(use-package org
  :general
  (:keymaps 'org-mode-map [remap my-narrow-or-widen-dwim] #'my-org-narrow-or-widen-dwim)

  :config
  (defun my-org-narrow-or-widen-dwim (p)
    "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.

Narrow to region, org-src-block (actually calls
`org-edit-src-code'), org-subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
    (interactive "P")
    (declare (interactive-only))
    (cond ((and (buffer-narrowed-p) (not p)) (widen))
          ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
          ((org-in-src-block-p) (org-edit-src-code))
          ((org-at-block-p) (org-narrow-to-block))
          (t (org-narrow-to-subtree))))

  (dolist (face '(org-table org-block org-code))
    (set-face-attribute face nil :family (face-attribute 'default :family)))
  (set-face-attribute 'org-table nil :foreground nil :inherit 'font-lock-string-face)

  (setq org-pretty-entities t))

;;;; Elisp
(use-package outline
  :init
  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)

  (defun my-outline-minor-mode ()
    "Add some minor functionality to `outline-minor-mode'"
    (font-lock-add-keywords
     nil
     '((";;[;]\\{1\\} \\(.*\\)" 1 'outline-1 t)
       (";;[;]\\{2\\} \\(.*\\)" 1 'outline-2 t)
       (";;[;]\\{3\\} \\(.*\\)" 1 'outline-3 t)
       (";;[;]\\{4\\} \\(.*\\)" 1 'outline-4 t)
       (";;[;]\\{5\\} \\(.*\\)" 1 'outline-5 t)
       (";;[;]\\{6\\} \\(.*\\)" 1 'outline-6 t)
       (";;[;]\\{7\\} \\(.*\\)" 1 'outline-7 t)
       (";;[;]\\{8\\} \\(.*\\)" 1 'outline-8 t)))
    (add-to-list 'imenu-generic-expression '("Heading" ";;[;]\\{1,8\\} \\(.*$\\)" 1)))
  (add-hook 'emacs-lisp-mode-hook #'my-outline-minor-mode)

  :config
  (use-package outline-magic
    :general (:keymaps 'outline-minor-mode-map "TAB" #'outline-cycle))

  (dolist (spec '((outline-1 . 150)
                  (outline-2 . 150)
                  (outline-3 . 150)
                  (outline-4 . 90)
                  (outline-5 . 90)
                  (outline-6 . 90)
                  (outline-7 . 90)
                  (outline-8 . 90)))
    (set-face-attribute
     (car spec) nil
     :foreground (let* ((face (car spec))
                        (fg (face-attribute face :foreground)))
                   (when (eq fg 'unspecified)
                     (setq fg (face-attribute (face-attribute face :inherit) :foreground)))
                   fg)
     :height (cdr spec)
     :inherit 'my-headline-face)))

(use-package eldoc                    ; Documentation in echo area
  :init (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  :config (setq eldoc-idle-delay 0.3))

(use-package highlight-quoted    ; Faces for lisp quotes and symbols
  :init (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))
(use-package elisp-slime-nav          ; Navigate elisp documentation
  :general (:keymaps '(elisp-slime-nav-mode-map help-mode-map) :states 'motion
                     "K" #'elisp-slime-nav-describe-elisp-thing-at-point
                     "gd" #'elisp-slime-nav-find-elisp-thing-at-point)
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode))

;;;; Web
(use-package css-mode
  :defines my-sass-output-dir
  :mode "\\.s[ac]ss$"
  :config
  (setq css-indent-offset 2)
  (defcustom my-sass-output-dir nil
    "Directory to store compiled sass files."
    :group 'css)

  (defun my-sass-watch ()
    "Start a sass process with `sass --watch' watching the current buffer."
    (interactive)
    (let ((sass-command "sass"))
      (shell-command
       (if my-sass-output-dir
           (format
            "%s --watch '%s':'%s%s.css'&"
            sass-command
            buffer-file-name
            (or my-sass-output-dir "")
            (file-name-nondirectory
             (file-name-sans-extension buffer-file-name)))
         (format
          ("%s --watch' %s' &" buffer-file-name)))
       )))

  ;; Run `prog-mode-hook' manually since `css-mode' doesn't derive from it
  (add-hook 'css-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

  (use-package css-eldoc                ; Basic minibuffer display help for CSS
    :init (add-hook 'css-mode-hook #'css-eldoc-enable)))

(use-package web-mode
  :mode "\\.html?$"
  :config
  (setq web-mode-enable-block-face t
        web-mode-enable-part-face t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2))

;;; Local Variables
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; byte-compile-warnings: (not noruntime unresolved)
;; End:
