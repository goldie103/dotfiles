;; -*- lexical-binding: t -*-

;;; Setup

;;;; Constants
(defconst my-dir
  (expand-file-name "emacs/" (or (getenv "XDG_CACHE_HOME") "~/.cache"))
  "Personal elisp settings files and generated files.")

(defvar my-win-p (eq system-type 'windows-nt) "Non-nil if using MS Windows.")
(defvar my-desktop-p (string= (system-name) "lyngbakr") "Non-nil if on desktop machine")

(setq custom-file (expand-file-name "custom.el" my-dir))
(load custom-file 'no-error 'no-message)

;;;; Package Management

;; Set up straight.el
(let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Set up use-package

(straight-use-package 'use-package)

;; deal with compiler issues
(defvar straight-use-package-by-default)
(defvar use-package-enable-imenu-support)
(defvar use-package-verbose)
(defvar use-package-always-defer)

(setq
 straight-use-package-by-default t  ; always ensure packages are installed
 use-package-enable-imenu-support t ; add imenu headings for use-package blocks
 use-package-verbose t              ; log message after loading a package
 use-package-always-defer t)        ; defer all packages

(use-package general)

(use-package delight
    :init
    (delight '((undo-tree-mode nil undo-tree)
               (auto-fill-function nil auto-fill-mode)))
    ;; This has to be done manually for some reason
    (eval-after-load 'auto-fill-mode
      (setcar (cdr (assq 'auto-fill-function minor-mode-alist)) nil)))

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

(general-def
 "M-SPC" #'cycle-spacing
 "RET" #'newline-and-indent
 "C-x n" #'my-narrow-or-widen-dwim
 "<up>" #'scroll-down
 "<down>" #'scroll-up
 "M-/" #'grep
 "C-M-/" #'find-grep-dired
 [remap switch-to-buffer] #'ibuffer
 "C-w" nil)

(general-def
 :prefix "C-w"
 :prefix-command 'my-window-command
 :prefix-map 'my-window-map
 "C-w" #'other-window
 "w" #'other-window
 "s" #'split-window-vertically
 "C-s" #'split-window-horizontally
 "=" #'balance-windows
 "k" #'enlarge-window
 "j" #'shrink-window
 "f" #'delete-frame
 "d" #'delete-window
 "C-d" #'delete-other-windows)

(general-def help-mode-map
 "H" #'help-go-back
 "L" #'help-go-forward)
(general-def help-map
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

(general-def undo-tree-map "C-/" nil)

;;; Major packages
;;;; Evil
(use-package evil :demand t
;;;;; Evil Bindings
  :general
  ("<f3>" #'evil-record-macro)
  (:states 'normal :keymaps 'undo-tree-map "U" #'undo-tree-redo)
  (:keymaps '(motion normal)
           "C-w" nil
           "," nil
           "SPC" #'execute-extended-command
           "q" #'kill-this-buffer
           "\"" #'evil-jump-item
           "*" #'evil-use-register
           [remap evil-next-line] #'evil-next-visual-line
           [remap evil-previous-line] #'evil-previous-visual-line)
  (:keymaps '(motion normal)
           :prefix ","
           :prefix-command 'my-evil-leader-command
           :prefix-map 'my-evil-leader-map
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

  (nconc evil-emacs-state-modes '(shell-mode term-mode multi-term-mode package-menu-mode))
  (evil-add-command-properties #'evil-yank-line :motion 'evil-end-of-line)
  (evil-mode t))

;;;;; Evil Packages
(use-package evil-escape      ; escape from everything with two keys
  :after evil
  :delight
  :init (evil-escape-mode t))

(use-package evil-surround            ; surround operator
  :after evil
  :init (add-hook 'prog-mode-hook #'evil-surround-mode))

(use-package evil-matchit             ; more tag jumping support
  :after evil
  :general (:keymaps 'evil-matchit-mode-map [remap evil-jump-item] #'evilmi-jump-items)
  :config
  (setq evilmi-may-jump-by-percentage nil)
  (global-evil-matchit-mode t)) ; allow count usage

(use-package evil-goggles
  :after evil
  :config (evil-goggles-use-diff-faces))

;;;; Helm
(use-package helm-config :straight helm :demand t
  :delight helm-mode
  :defines (helm-completion-in-region-fuzzy-match
            helm-recentf-fuzzy-match
            helm-mode-fuzzy-match
            helm-ff-auto-update-initial-value
            helm-ff-skip-boring-files
            helm-ff-file-name-history-use-recentf
            helm-M-x-fuzzy-match
            helm-imenu-fuzzy-match
            helm-semantic-fuzzy-match
            helm-move-to-line-cycle-in-source
            helm-buffers-fuzzy-matching
            helm-apropos-fuzzy-match
            helm-scroll-amount
            helm-allow-mouse
            helm-split-window-inside-p)
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
   [remap apropos-documentation] #'helm-apropos
   "C-SPC" #'helm-resume)

  (:keymaps 'emacs-lisp-mode-map
            "C-/"#'helm-semantic-or-imenu)
  (:keymaps 'helm-buffer-map
            "C-d" #'helm-buffer-run-kill-persistent
            "<C-return>" #'helm-buffer-switch-other-window)
  (:keymaps 'helm-map
            "M-d" #'helm-scroll-other-window
            "M-u" #'helm-scroll-other-window-down)
  (:keymaps 'helm-find-files-map
            "C-d" #'helm-ff-persistent-delete
            "C-o" #'helm-ff-run-open-file-with-default-tool)
;;;;; Helm Config
  :config
  (setq helm-completion-in-region-fuzzy-match t
        helm-split-window-inside-p t
        helm-recentf-fuzzy-match t
        helm-mode-fuzzy-match t
        helm-ff-auto-update-initial-value t
        helm-ff-file-name-history-use-recentf t
        helm-ff-skip-boring-files t
        helm-M-x-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-semantic-fuzzy-match t
        helm-move-to-line-cycle-in-source t     ; cycle on end
        helm-buffers-fuzzy-matching t
        helm-apropos-fuzzy-match t
        helm-scroll-amount 5                    ; other window scroll
        helm-allow-mouse t
        helm-split-window-inside-p t)
  (helm-adaptive-mode t)
  (helm-popup-tip-mode t)
  (helm-autoresize-mode t)
  (helm-mode t))

;;;;; Helm Packages

(use-package helm-eshell :straight nil
  :after helm eshell
  :general ([remap eshell-list-history] #'helm-eshell-history))

(use-package helm-dash                ; Language documentation
  :after helm
  :config
  (setq helm-dash-docsets-path
        (expand-file-name "dash" (or (getenv "XDG_DATA_HOME") "~/.local/share"))))

(use-package helm-descbinds        ; `describe-bindings' replacement
  :after helm
  :config
  (setq helm-descbinds-window-style 'split-window)
  (helm-descbinds-mode t))

(use-package helm-swoop               ; fast search and navigation
  :after helm
  :general
  ([remap grep] #'helm-swoop)
  (:keymaps 'isearch-mode-map "M-/" #'helm-swoop-all-from-isearch)
  (:keymaps 'helm-swoop-map "M-/" #'helm-multi-swoop-all-from-helm-swoop)

  :config
  (setq helm-swoop-speed-or-color t
        helm-swoop-use-fuzzy-match t))

(use-package helm-flx
  :after helm
  :config
  (setq helm-flx-for-helm-locate t)
  (helm-flx-mode t))

(use-package helm-tramp
  :after helm)

;;;; Async
(use-package async :init (async-bytecomp-package-mode t))

;;; Help

(use-package discover-my-major      ; list current major mode bindings
  :general ([remap describe-mode] #'discover-my-major))

(use-package guide-key :demand t    ; delayed completion for prefix keys
  :delight
  :config
  (setq guide-key/recursive-key-sequence-flag t
        guide-key/guide-key-sequence '("C-x" "C-c" "C-w" "," "<f1>"))
  (guide-key-mode t))

(use-package info
  :config
  (add-to-list 'Info-directory-list (expand-file-name "info" (getenv "XDG_CONFIG_HOME"))))

;;; Appearance
;;;; Modeline

(use-package spaceline-config
  :demand t
  :straight spaceline
  :config
  (setq spaceline-responsive nil
        powerline-default-separator nil)
  (spaceline-helm-mode t))

(use-package spaceline-all-the-icons
  :demand t
  :after spaceline all-the-icons
  :config
  (spaceline-all-the-icons-theme)
  (spaceline-all-the-icons--setup-anzu)
  (spaceline-all-the-icons--setup-git-ahead)
  (spaceline-all-the-icons--setup-neotree)
  (spaceline-toggle-all-the-icons-git-status-on)
  (setq spaceline-all-the-icons-separator-type 'wave))

(use-package which-func                 ; Modeline definition name
  :config (which-function-mode t))

(use-package wc-goal-mode               ; Modeline word count
  :init (add-hook 'text-mode-hook #'wc-goal-mode))

(use-package anzu
  :config (global-anzu-mode t))


;;;; Theme

(defmacro my-load-theme (theme &rest body)
  "Load THEME and then execute BODY such that it applies correctly in the daemon.

This is a workaround to deal with themes not getting loaded when Emacs is run as a daemon.
Some themes rely on settings from the current frame, but when launching as a daemon, there
is not necessarily a current frame when the theme is loaded.

This can be circumvented by adding the theme loading code to `after-frame-make-functions',
so that it is only executed after the frame is loaded. However, we only want to run the
theme code once, not after every frame is opened.

We get around this by removing the entry in `after-frame-make-functions', so the function
should execute once and then remove itself.

This approach potentially still has the limitation of not behaving correctly in terminal
frames versus display frames. Further investigation is required."
  (if (daemonp)
      `(cl-labels ((my-load-theme-daemon (frame)
                                         (with-selected-frame frame (load-theme ',theme t) ,@body)
                                         (remove-hook 'after-make-frame-functions
                                                      #'my-load-theme-daemon)))
         (add-hook 'after-make-frame-functions #'my-load-theme-daemon))
    `(progn (load-theme ',theme t) ,@body)))

(use-package all-the-icons :demand t)

(use-package doom-themes :demand t
  :after all-the-icons spaceline-all-the-icons
  :defines (doom-themes-enable-bold
            doom-themes-enable-italic
            doom-neotree-file-icons)
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-neotree-file-icons t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config)

  (my-load-theme doom-dracula
   ;;(set-face-attribute 'mode-line nil :background nil)
   ;; (with-eval-after-load 'web-mode
   ;;   (set-face-attribute 'web-mode-block-face nil :background
   ;;                       (face-attribute 'helm-source-header :background))
   ;;   (set-face-attribute 'web-mode-current-column-highlight-face nil
   ;;                       :background (face-attribute 'helm-source-header :background))
   ;;   (set-face-attribute 'web-mode-current-element-highlight-face nil
   ;;                       :foreground nil
   ;;                       :background (face-attribute 'helm-source-header :background)))
   ))

(use-package solaire-mode
  :after doom-themes
  :config
  (add-hook 'after-change-major-mode-hook #'solaire-mode)
  (add-hook 'after-revert-hook #'solaire-mode)
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (solaire-mode-swap-bg))


;;;; Faces
(use-package highlight-numbers :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))
(use-package page-break-lines :config (global-page-break-lines-mode t) :delight)
(use-package rainbow-mode :init (add-hook 'css-mode-hook #'rainbow-mode))
(use-package rainbow-delimiters :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
(use-package rainbow-blocks)

(use-package hl-line
  :init (add-hook 'prog-mode-hook #'hl-line-mode))

(use-package whitespace
  :delight
  :init (add-hook 'prog-mode-hook #'whitespace-mode)
  :config
  (setq
   whitespace-line-column nil
   whitespace-style '(face trailing lines-tail)
   whitespace-action '(auto-cleanup warn-if-read-only)))

;;;; My faces

(defvar my-font-faces
  (if my-desktop-p
      '((default nil :family "Iosevka" :height 100)
        (variable-pitch nil :family "InputSerifCompressed" :height 90)
        (font-lock-comment-face nil :slant italic)
        (font-lock-string-face nil :inherit variable-pitch))
    '((default nil :family "TamzenForPowerline" :height 105)
      (fixed-pitch nil :family "TamzenForPowerline" :height 105)
      (variable-pitch nil :foundry "UKWN" :family "Bitter" :height 120)))
  "Font faces dependant on the current machine")

(dolist (face my-font-faces) (apply #'set-face-attribute face))

(defface my-headline-face
  `((t ,@(if my-desktop-p '(:family "FabfeltScript Bold" :height 150)
           '(:family "artwiz fkp"))))
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
(use-package nlinum
  :demand t
  :init (add-hook 'prog-mode-hook #'nlinum-mode))

(use-package simple :straight nil
  :config (setq find-file-visit-truename t))

(use-package zone)                      ; Screensaver
(use-package zone-nyan
  :after zone
  :config (setq zone-programs [zone-nyan]))

(use-package golden-ratio               ; Window resizing
  :delight
  :config
  (setq golden-ratio-auto-scale t)

  (defun my-helm-alive-p ()
    (if (boundp 'helm-alive-p) (symbol-value 'helm-alive-p)))
  (add-to-list 'golden-ratio-inhibit-functions #'my-helm-alive-p)
  (golden-ratio-mode t))

(use-package hideshow                   ; Code folding
  :general
  (:keymaps 'hs-minor-mode-map
            "TAB" #'hs-toggle-hiding
            "<C-tab>" #'hs-hide-level)
  :init
  (add-hook 'java-mode-hook #'hs-minor-mode))

(use-package uniquify                 ; Distinguish same-named buffers
  :straight nil
  :config (setq uniquify-buffer-name-style 'forward
                uniquify-trailing-separator-p t))

(use-package winner                     ; Window configuration undo
  :general (:keymaps 'my-window-map
                     "u" #'winner-undo
                     "U" #'winner-redo)
  :config (winner-mode t))

(use-package writeroom-mode            ; Distraction-free writing mode
  :general (:keymaps 'text-mode-map "<C-f11>" #'writeroom-mode)
  :functions my-writeroom-effect
  :config
  (setq writeroom-width 100))

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

(use-package ace-window
  :general ([remap other-window] #'ace-window)
  :config (setq aw-keys (or avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(use-package desktop                    ; Save opened buffers, windows, frames
  :config
  (setq desktop-auto-save-timeout 60
        desktop-dirname my-dir)
  (add-to-list 'desktop-path desktop-dirname)
  (nconc desktop-modes-not-to-save '(magit-mode git-commit-mode))
  (desktop-save-mode t))

(use-package expand-region :disabled t  ; Expand selection by blocks at a time
  :general (:keymaps 'motion "zz" #'er/expand-region)
  :config (setq expand-region-contract-fast-key "x"))

(use-package savehist                   ; Save command history
  :config
  (setq savehist-file (expand-file-name "savehist" my-dir)
        history-delete-duplicates t
        savehist-save-minibuffer-history t)
  (savehist-mode t))

(use-package saveplace              ; Save and return to place in file
  :demand t
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "places" my-dir)))

(use-package tramp                      ; Edit remote files
  :config
  (setq tramp-default-method "ssh"))
;;; File navigation
;;;; Google Drive

;;;; Dired
(use-package dired
  :straight nil
  :defines ls-lisp-dirs-first
  :config
  (defun my-silence-auto-revert ()
    "Set `auto-revert-verbose' to nil in the current buffer"
    (defvar auto-revert-verbose)
    (setq auto-revert-verbose nil))
  (add-hook 'dired-mode-hook #'my-silence-auto-revert)
  (setq ls-lisp-dirs-first t
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        dired-ls-F-marks-symlinks t
        delete-by-moving-to-trash t))
(use-package dired-x :after dired :straight nil :init (setq-default dired-omit-files-p t))
(use-package dired-aux :after dired :straight nil)
(use-package dired-async :straight async :after dired :config (dired-async-mode t))
(use-package dired+ :after dired)

;;;; Neotree
(use-package neotree
  :general
  (:keymaps 'my-evil-leader-map
            "F" #'neotree-toggle
            "M-f" #'neotree-find)
  (:states 'normal :keymaps 'neotree-mode-map
           "TAB" #'neotree-enter
           "SPC" #'neotree-quick-look
           "q" #'neotree-hide
           "RET" #'neotree-enter)
  :config (setq neo-smart-open t))

;;;; Ranger
;; The things I do to silence the byte compiler

;; (use-package wdired :after dired)

(use-package ranger
  :config (ranger-override-dired-mode t))

;;;; Projectile
(use-package projectile :demand t       ; Project-based navigation
  :general (:keymaps 'my-evil-leader-map
                     "p" #'projectile-command-map
                     "P" #'projectile-find-file-dwim)
  :config
  (setq projectile-cache-file (expand-file-name "projectile.cache" my-dir)
        projectile-known-projects-file (expand-file-name "projectile.eld" my-dir))
  (projectile-register-project-type 'python '("Pipfile"))
  (projectile-mode t))

(use-package helm-projectile
  :after projectile helm
  :general
  (:keymaps 'my-evil-leader-map
            [remap projectile-find-file-dwim] #'helm-projectile-find-file-dwim)
  (:keymaps 'projectile-command-map
            [remap projectile-switch-project] #'helm-projectile-switch-project
            [remap projectile-grep] #'helm-projectile-grep-or-ack
            [remap projectile-ag] #'helm-projectile-ag
            [remap projectile-find-file] #'helm-projectile-find-file)
  :config
  (setq projectile-completion-system 'helm
        projectile-switch-project-action #'helm-projectile
        helm-projectile-fuzzy-match t)
  (use-package neotree
    :config
    (defun helm-projectile-and-neotree ()
      "Helper function to call both helm-projectile and neotree-projectile-action"
      (neotree-projectile-action)
      (helm-projectile))
    (setq projectile-switch-project-action #'helm-projectile-and-neotree))
  (helm-projectile-on))

;;; Editing

(use-package eshell
  :general
  ("<f12>" #'eshell-here)
  (:keymaps 'eshell-mode-map
   [remap eshell-complete] #'helm-esh-pcomplete
   "M-p" #'helm-eshell-history)

  :init
  ;; from http://www.howardism.org/Technical/Emacs/eshell-fun.html
  (defun eshell-here ()
    "Open a new shell in the current buffer's directory"
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (height (/ (window-total-height) 3))
           (name (format "*eshell: %s*" (car (last (split-string parent "/" t))))))
      (split-window-vertically (- height))
      (if (get-buffer name)
          (switch-to-buffer name)
        (eshell "new")
        (rename-buffer name))
      (shrink-window height)))

  :config
  (add-hook 'eshell-mode-hook #'eshell-cmpl-initialize)
  (add-hook 'eshell-mode-hook #'company-mode))

(use-package pcomplete-extension :after eshell)

(use-package multiple-cursors :disabled t
  :config (multiple-cursors-mode t))

(use-package drag-stuff :disabled t
  :config (drag-stuff-global-mode t))

(use-package flycheck
  :general (:keymaps 'my-evil-leader-map
                     "cj" #'flycheck-next-error
                     "ck" #'flycheck-previous-error)
  :init (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (setq flycheck-keymap-prefix (kbd "C-c f")
        flycheck-indication-mode 'right-fringe))

(use-package flycheck-tip             ; Display errors by popup
  :after flycheck
  :general
  ([remap flycheck-next-error] #'flycheck-tip-cycle
   [remap flycheck-previous-error] #'flycheck-tip-cycle-reverse))

(use-package flycheck-pos-tip
  :after flycheck
  :init (add-hook 'flycheck-mode-hook #'flycheck-pos-tip-mode))

(use-package helm-flycheck
  :after helm flycheck
  :general
  (:keymaps 'flycheck-mode-map "C-c f c" #'helm-flycheck)
  (:keymaps 'my-evil-leader-map "cc" #'helm-flycheck))

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
  (nconc writegood-weasel-words '("thing" "different" "probably" "really")))

(use-package abbrev                     ; Auto-correct
  :straight nil
  :config
  (setq save-abbrevs 'silently
        abbrev-all-caps t
        abbrev-file-name (expand-file-name "abbrevs.el" my-dir))
  (add-hook 'text-mode-hook #'abbrev-mode))

(use-package auto-indent-mode :disabled t
  :config (auto-indent-global-mode t))

(use-package autoinsert :disabled t     ; Auto insert buffer text
  :config (auto-insert-mode t))

(use-package company                    ; Autocompletion
  :delight
  :general (:keymaps 'company-active-map
                     "M-j" #'company-select-next
                     "M-k" #'company-select-previous
                     "M-d" #'company-show-doc-buffer)
  :init (add-hook 'prog-mode-hook #'company-mode)
  :config
  (setq company-idle-delay 0            ; immediate completion attempt
        company-show-numbers t          ; allow M-num selection
        company-tooltip-align-annotations t
        company-selection-wrap-around t)

  (delete 'company-dabbrev company-backends))

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
  (advice-add #'ispell-word :around #'my-ispell-run-together))

(use-package flyspell                 ; On-the-fly spell check
  :after ispell
  :init
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil)
  (advice-add #'flyspell-auto-correct-word :around #'my-ispell-run-together))

(use-package flyspell-popup
  :after flyspell
  :general ([remap ispell-word] #'flyspell-popup-correct))

(use-package flyspell-lazy
  :after flyspell
  :init
  (add-hook 'flyspell-mode #'flyspell-lazy-mode)
  (add-hook 'flyspell-prog-mode #'flyspell-lazy-mode))

(use-package smartparens-config :disabled t     ; Balanced parenthesis management
  :straight smartparens)

(use-package autorevert           ; Auto revert external modifications
  :config
  (setq global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode t))

(use-package pandoc-mode                ; Markup conversion tool
  :general
  ("C-c C-p" #'pandoc-mode)
  (:keymaps 'pandoc-mode-map
            [remap pandoc-mode] #'pandoc-main-hydra/body
            "C-c C-S-p" #'pandoc-run-pandoc)
  :config (setq pandoc-data-dir (expand-file-name "pandoc" my-dir)))

(use-package real-auto-save             ; Auto save buffers
  :delight
  :commands real-auto-save-mode
  :init (add-hook 'after-save-hook #'real-auto-save-mode)
  :config (setq real-auto-save-interval 60))

(use-package recentf                    ; List recent files
  :config
  (setq recentf-exclude '("COMMIT_EDITMSG" ".*\\.png$") ; exclude commit messages
        recentf-save-file (expand-file-name "recentf" my-dir)))

(use-package compile
  :config (setq compilation-always-kill t ; kill old before starting new
                compilation-skip-threshold 2 ; skip warning and info messages
                compilation-context-lines 3
                compilation-scroll-output 'first-error
                compilation-auto-jump-to-first-error t
                compilation-ask-about-save nil))

(use-package ediff                      ; Emacs diff utility
  :general (:keymaps 'ediff-mode
                     "j" #'ediff-next-difference
                     "k" #'ediff-previous-difference))

(use-package magit
  :general
  ("<f10>" #'magit-status)
  (:keymaps 'magit-status-mode-map
            "SPC" #'execute-extended-command)
  :config
  (defun my-git-commit-setup-fun ()
    (when (fboundp #'visual-line-mode) (visual-line-mode -1))
    (when (fboundp #'auto-fill-mode) (auto-fill-mode t)))
  (add-hook 'git-commit-setup-hook #'my-git-commit-setup-fun))

(use-package evil-magit
  :after evil magit)

(use-package git-timemachine           ; Travel through commit history
  :general ("<C-f10>" #'git-timemachine-toggle))

(use-package git-gutter-fringe+
  :general ("<M-f10>" #'git-gutter+-toggle-fringe)
  :config (git-gutter+-mode))

;;; Languages
(use-package ein)
(use-package pdf-tools :disabled t      ; TODO installing this with straight.el
  :general (:keymaps 'pdf-view-mode-map
                     "SPC" #'helm-M-x
                     "," #'my-evil-leader-command
                     "q" #'image-kill-buffer
                     "y" #'my-pdf-view-page-as-text
                     "j" #'pdf-view-next-line-or-next-page
                     "k" #'pdf-view-previous-line-or-previous-page
                     "h" #'pdf-view-previous-page-command
                     "l" #'pdf-view-next-page-command
                     "g" #'pdf-view-first-page
                     "G" #'my-pdf-view-goto-page
                     "m" #'pdf-view-position-to-register
                     "`" #'pdf-view-jump-to-register
                     "/" #'pdf-occur
                     "o" #'pdf-outline
                     "f" #'pdf-links-action-perform
                     "b" #'pdf-view-midnight-minor-mode
                     "C-o" #'pdf-history-backwards
                     "C-i" #'pdf-history-forwards)
  :config
  (defun my-pdf-view-goto-page (count)
    "Go to page COUNT in pdf-view-mode. If COUNT is not supplied, go to last page"
    (interactive "P")
    (if count (pdf-view-goto-page count) (pdf-view-last-page)))
  (defun my-pdf-view-page-as-text ()
    (interactive)
    (pdf-view-mark-whole-page)
    (pdf-view-kill-ring-save)
    (switch-to-buffer (make-temp-name (concat (buffer-name) " text")))
    (save-excursion (yank)))
  (pdf-tools-install))

(use-package doc-view
  :straight nil
  :general (:keymaps 'doc-view-mode-map
                     "SPC" #'helm-M-x
                     "," #'my-evil-leader-command
                     "q" #'image-kill-buffer
                     "j" #'doc-view-next-line-or-next-page
                     "k" #'doc-view-previous-line-or-previous-page
                     "h" #'doc-view-previous-page
                     "l" #'doc-view-next-page
                     "g" #'doc-view-first-page
                     "G" #'my-doc-view-goto-page
                     "/" #'doc-view-search)
  :config
  (defun my-doc-view-goto-page (count)
    "Go to page COUNT in pdf-view-mode. If COUNT is not supplied, go to last page"
    (interactive "P")
    (if count (doc-view-goto-page count) (doc-view-last-page))))

(use-package gitignore-mode)
(use-package arduino-mode)
(use-package ess)
(use-package vimrc-mode :mode "[._]?(pentadactyl|vimperator)rc$" "\\.(penta|vimp)$")
(use-package generic-x :disabled t)
(use-package lua-mode :config (setq lua-indent-level 2))

(defun my-run-python-script ()
  "Run python script"
  (interactive)
  (compile (concat "pipenv run python " (buffer-name))))

(use-package elpy :after python
  :config
  (elpy-enable)
  (pyvenv-tracking-mode t))

(use-package js
  :config
  (setq-default js-indent-level 2))

(use-package markdown-mode
  :config
  (setq markdown-header-face 'my-headline-face))

(use-package tex
  :straight auctex
  :config
  (defun my-tex-watch ()
    "Start a latexmk process watching the current buffer"
    (interactive)
    (let ((latexmk-command "latexmk"))
      (shell-command
       (format "%s --pvc %s '%s' &"
               latexmk-command (if TeX-PDF-mode "-pdf" "") buffer-file-name))))

  (setq TeX-auto-save t
        TeX-auto-local nil
        TeX-source-correlate-mode t
        TeX-parse-self t)
  (add-hook 'TeX-mode-hook #'company-mode)
  (add-hook 'TeX-mode-hook #'TeX-PDF-mode)
  (add-hook 'TeX-mode-hook #'TeX-interactive-mode)
  (add-hook 'LaTeX-mode-hook #'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))

(use-package bibtex :config (setq bibtex-dialect 'biblatex))

(use-package auctex-latexmk
  :after auctex
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  (auctex-latexmk-setup))

(use-package company-auctex
    :after company auctex
    :config
    (company-auctex-init))

(use-package go-mode)
(use-package company-go
  :after company go-mode)
(use-package go-eldoc
  :init (add-hook 'go-mode-hook #'go-eldoc-setup))
;;;; Org
(use-package qml-mode)

(use-package org
  :general
  (:keymaps 'org-mode-map [remap my-narrow-or-widen-dwim] #'my-org-narrow-or-widen-dwim)
  (:keymaps 'org-mode-map :states 'normal "!" #'org-toggle-latex-fragment)

  :custom-face
  (org-table ((t (:inherit fixed-pitch))))
  (org-block ((t (:inherit fixed-pitch))))
  (org-code ((t (:inherit fixed-pitch))))
  (org-verbatim ((t (:inherit fixed-pitch))))
  (org-formula ((t (:inherit fixed-pitch))))
  (org-block-begin-line ((t (:inherit fixed-pitch))))
  (org-block-end-line ((t (:inherit fixed-pitch))))
  (org-special-keyword ((t (:inherit fixed-pitch))))
  (org-meta-line ((t (:inherit fixed-pitch))))
  (font-lock-comment-face ((t (:inherit fixed-pitch))))

  :config

  ;; Workaround hack for getting org to work with straight.el
  (require 'subr-x)
  (use-package git)
  (defun org-git-version ()
    "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
    (require 'git)
    (let ((git-repo (expand-file-name
                     "straight/repos/org/" user-emacs-directory)))
      (string-trim
       (git-run "describe"
                "--match=release\*"
                "--abbrev=6"
                "HEAD"))))
  (defun org-release ()
    "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
    (require 'git)
    (let ((git-repo (expand-file-name
                     "straight/repos/org/" user-emacs-directory)))
      (string-trim
       (string-remove-prefix
        "release_"
        (git-run "describe"
                 "--match=release\*"
                 "--abbrev=0"
                 "HEAD")))))
  (provide 'org-version)

  ;; Auto export on save

  (defcustom my-org-default-export nil
    "Default function to use when exporting org documents.
Used by `my-org-export'. If nil, then don't export at all."
    :safe (lambda (fun) (member fun '(org-latex-export-to-pdf))))

  (defcustom my-org-export-async-p t
    "If non-nil, then `my-org-export-async' will pass the t argument."
    :risky nil)

  (defun my-org-export ()
    "Call `my-org-default-export' function with `my-org-export-async' as an argument."
    (interactive)
    (when my-org-default-export
      (funcall my-org-default-export my-org-export-async-p)))

  (defun my-org-export-on-save ()
    "Add `my-org-export' to `after-save-hook'."
    (add-hook 'after-save-hook #'my-org-export t t))

  (add-hook 'org-mode-hook #'my-org-export-on-save)

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

  (setq org-pretty-entities t
        org-entities-user '(("vDash" "\\vDash" nil "&#8872;" "|=" "|=" "⊨"))
        org-hide-emphasis-markers t
        org-preview-latex-image-directory (expand-file-name "ltximg" my-dir)
        org-highlight-latex-and-related '(latex script entities)
        org-export-in-background t
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-latex-pdf-process '("latexmk -pdflatex='%latex -shell-escape -interaction nonstopmode' -pdf -output-directory=%o -f %f")
        org-export-async-init-file (expand-file-name "org-export.el"
                                                     (file-name-directory user-init-file))))

(use-package org-ref :after org :demand t)

(use-package ob-ipython :after org
  :init
  (org-babel-do-load-languages 'org-babel-load-languages '((ipython . t)))
  :config
  (use-package company
    :config (add-to-list 'company-backends 'company-ob-ipython))
  (setq org-babel-async-python t
        ob-ipython-command "jupyter")
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images))

(use-package ox-ipynb
  :demand t
  :straight (ox-ipynb :type git :host github :repo "jkitchin/ox-ipynb"))

(use-package ox-latex
  :straight nil
  :config
  (add-to-list 'org-latex-logfiles-extensions "tex")
  (add-to-list 'org-latex-classes
               '("problemset"
                 "\\documentclass[10pt]{article}
[DEFAULT-PACKAGES]
\\usepackage{fancyhdr}
[PACKAGES]
[EXTRA]

\\makeatletter
\\pagestyle{fancy}
\\fancyhf{}
\\lhead{\\@title}
\\rhead{\\@author}
\\makeatother

\\def \\circledtwo {\\raisebox{.5pt}{\\textcircled{\\raisebox{-.9pt} {2}}}}"
                 ("\\section{%s}". "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package evil-org
  :after evil org
  :config
  (setq org-special-ctrl-a/e t)
  (add-hook 'org-mode-hook #'evil-org-mode)
  (add-hook 'evil-org-mode-hook #'evil-org-set-key-theme))

;;;; Lisp
(use-package outline
  :delight outline-minor-mode
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
  (when my-desktop-p
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
       :inherit 'my-headline-face))))

(use-package outline-magic
  :demand t
  :after outline
  :general (:keymaps 'outline-minor-mode-map :states 'normal "TAB" #'outline-cycle))

(use-package eldoc                    ; Documentation in echo area
  :delight
  :init (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  :config (setq eldoc-idle-delay 0.3))

(use-package highlight-quoted    ; Faces for lisp quotes and symbols
  :init
  (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))

(use-package elisp-slime-nav          ; Navigate elisp documentation
  :delight
  :general (:keymaps '(elisp-slime-nav-mode-map help-mode-map) :states 'motion
                     "K" #'elisp-slime-nav-describe-elisp-thing-at-point
                     "gd" #'elisp-slime-nav-find-elisp-thing-at-point)
  :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode))

(general-def emacs-lisp-mode-map "C-c C-c" #'eval-defun)

(use-package slime
  :config
  (load (expand-file-name "~/.roswell/helper.el"))

  (slime-setup '(slime-fancy
                 slime-compiler-notes-tree
                 slime-highlight-edits
                 slime-hyperdoc
                 slime-quicklisp))

  (setq slime-lisp-implementations
        '((sbcl ("sbcl" "--dynamic-space-size" "2000"))
          (roswell ("ros" "-Q" "run")))
        slime-default-lisp 'roswell))

;;;; Web
(use-package css-mode
  :defines my-sass-output-dir
  :mode "\\.s[ac]ss$"
  :config
  (setq css-indent-offset 2)
  (defcustom my-sass-output-dir nil
    "Directory to store compiled sass files."
    :type 'variable
    :group 'css)

  (defun my-sass-watch ()
    "Start a sass process with `sass --watch' watching the current buffer."
    (interactive)
    (let ((sass-command "sass"))
      (shell-command
       (if my-sass-output-dir
           (format "%s --watch '%s':'%s%s.css'&"
                   sass-command buffer-file-name
                   (or my-sass-output-dir "")
                   (file-name-nondirectory
                    (file-name-sans-extension buffer-file-name)))
         (format "%s --watch' %s' &" sass-command buffer-file-name)))))

  ;; Run `prog-mode-hook' manually since `css-mode' doesn't derive from it
  (add-hook 'css-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

(use-package css-eldoc                ; Basic minibuffer display help for CSS
  :after css-mode
  :init (add-hook 'css-mode-hook #'css-eldoc-enable))

(use-package web-mode
  :mode "\\.html?$" "\\.tpl$"
  :config

  (setq web-mode-enable-block-face t
        web-mode-enable-part-face t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2)
  (nconc web-mode-engines-alist '(("mako" . "\\.tpl\\'"))))

;;; Local Variables
;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; byte-compile-warnings: (not noruntime unresolved)
;; End:
