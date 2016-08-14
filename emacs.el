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
  (expand-file-name
   "my-elisp/" (file-name-directory (file-truename user-init-file)))
  "Personal elisp files.")
(add-to-list 'load-path my-dir-elisp)

(defconst my-dir-packages
  (expand-file-name "elisp/" user-emacs-directory)
  "Directory for packages not available through a repository.")
(add-to-list 'load-path my-dir-packages)

(defvar my-win-p (eq system-type 'windows-nt) "Non-nil if using MS Windows.")
(defvar my-laptop-p (string= system-name "hraesvelgr")
  "Non-nil if on desktop machine")

(setq custom-file (expand-file-name "custom.el" my-dir))
(load custom-file 'no-error 'no-message)

;;;; package
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
(eval-when-compile (require 'use-package))
(setq
 use-package-enable-imenu-support t			; view packages in imenu
 use-package-verbose t             ; log message after loading a package
 use-package-always-ensure t       ; ensure all packages are installed
 use-package-always-defer t)       ; defer all packages
(use-package bind-key)

;;; Basic settings
(setq
 inhibit-startup-screen t								; no startup screen
 initial-scratch-message nil						; no scratch message
 disabled-command-function nil					; no disabled commands
 ring-bell-function 'ignore							; no bell
 initial-major-mode 'fundamental-mode		; scratch in fundamental mode
 scroll-conservatively 101							; don't recenter when scrlling
 scroll-margin 3												; lines of context to keep in view
 comment-auto-fill-only-comments t			; only auto fill in comments
 frame-title-format "%b "								; buffer name as frame title
 echo-keystrokes 0.1										; faster command echo
 save-interprogram-paste-before-kill t	; don't overwrite clipboard contents
 kill-do-not-save-duplicates t					; don't put duplicates in kill ring
 sentence-end-double-space nil
 read-file-name-completion-ignore-case t
 vc-handled-backends '(Git)							; remove unnecessary vc backends
 delete-by-moving-to-trash t
 large-file-warning-threshold 20000000	; larger warning threshold
 find-file-visit-truname t							; silently follow symlinks
 view-read-only t												; read-only in view-mode
 ;; backups and autosave
 auto-save-list-file-prefix (expand-file-name "autosaves/" my-dir)
 auto-save-file-name-transforms `((".*" ,auto-save-list-file-prefix t))
 backup-directory-alist `(("." . ,(expand-file-name "backups" my-dir)))
 backup-by-copying t										; always backup by copy
 version-control t											; add version numbers
 delete-old-versions t									; delete old backups silently
 kept-old-versions 5
 kept-new-versions 8
 ;; user info
 user-full-name "Kelly Stewart"
 user-mail-address "stewart.g.kelly@gmail.com")

(setq-default
 tab-width 2
 fill-column 90)

;; UTF-8 everywhere
;;(set-language-environment 'utf-8)
;;(setq locale-coding-system 'utf-8)
;;(set-default-coding-systems 'utf-8)
;;(set-terminal-coding-system 'utf-8)
;;(unless my-win-p (set-selection-coding-system 'utf-8))
;;(prefer-coding-system 'utf-8)

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

;; http://www.writequit.org/org/settings.html#sec-1-38
(defun my-narrow-or-widen-dwim (p)
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
        ((derived-mode-p 'org-mode)
         (cond ((org-in-src-block-p)
                (org-edit-src-code) (delete-other-windows))
               ((org-at-block-p) (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))

(defun my-comment-dwim (arg)
  "Like `comment-dwim' but first toggle line comment if no active region and point is not at end of line."
  (interactive "*P")
  (if (and (not (region-active-p))
           (not (or (eq (point) (line-end-position))
                    ;; compensate for evil-mode cursor being one point ahead
                    (and (eq (point) (- (line-end-position) 1))
                         (bound-and-true-p evil-normal-state-p)))))
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; (defun minibuffer-keyboard-quit ()
;;   "Abort recursive edit.
;; In Delete Selection mode, if the mark is active, just deactivate it;
;; then it takes a second \\[keyboard-quit] to abort the minibuffer."
;;   (interactive)
;;   (if (and delete-selection-mode transient-mark-mode mark-active)
;;       (setq deactivate-mark  t)
;;     (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
;;     (abort-recursive-edit)))

;;;; Modes and hooks
(show-paren-mode t)											; highlight matching parens
(electric-pair-mode t)									; auto add parens
(tool-bar-mode -1)											; no toolbar
(scroll-bar-mode -1)										; no scrollbar
(blink-cursor-mode -1)									; no blinking cursor
(unless (display-graphic-p) (menu-bar-mode -1)) ; no menu bar in terminal

;; Mode hooks
(defvar my-hooks
	'((prog-mode-hook
		 prettify-symbols-mode							; replace words with symbols
		 auto-fill-mode											; auto fill text past fill-col
		 goto-address-prog-mode							; buttonize URLs in comments and strings
		 line-number-mode										; line number in modeline
		 )
		(text-mode-hook
		 goto-address-mode									; buttonize URLs
		 visual-line-mode										; wrap by word
		 )))
(dolist (hook my-hooks)
	(dolist (func (cdr hook)) (add-hook (car hook) func)))

;;;; Bindings

;; oopsies
;;(global-unset-key (kbd "C-x ESC ESC"))

(bind-keys
 ("M-SPC" . cycle-spacing)
 ("RET" . newline-and-indent)
 ("C-x n" . my-narrow-or-widen-dwim)
 ("M-/" . grep)
 ("C-M-/" . find-grep-dired)
 ([remap comment-dwim] . my-comment-dwim)
 ([remap switch-to-buffer] . ibuffer))

;; (bind-keys
;;  :prefix-map my-window-map
;;  :prefix "C-w"
;;  :prefix-docstring "My window related commands"
;;  ("C-w" . other-window)
;;  ("w" . other-window)
;;  ("v" . split-window-vertically)
;;  ("h" . split-window-horizontally)
;;  ("=" . balance-windows)
;;  ("f" . delete-frame)
;;  ("d" . delete-window)
;;  ("D" . delete-other-windows))

;; (dolist (map '(minibuffer-local-map
;; 							 minibuffer-local-ns-map
;; 							 minibuffer-local-completion-map
;; 							 minibuffer-local-must-match-map
;; 							 minibuffer-local-isearch-map))
;; 	(bind-key "<escape>" #'minibuffer-keyboard-quit (symbol-value map)))

;;; Major packages
;;;; Evil
(use-package evil
	:demand t
	:init (evil-mode t)
	;;;;; evil functions
  (defvar my-evil-leader-map (make-sparse-keymap)
    "Map for personal evil leader bindings.")
  (define-prefix-command 'my-evil-leader-map)

  (defmacro evil-unbind (key &optional states)
    "Unbind KEY, in STATES only if STATES is non-nil, otherwise unbind all."
    (macroexp-progn
     (mapcar
      (lambda (state)
        `(unbind-key
          ,key ,(intern (concat "evil-" (symbol-name state) "-state-map"))))
      (or states '(normal insert visual replace operator motion emacs)))))

	(defmacro evil-bind-key (&rest args)
    "Bind multiple keys to multiple evil states and maps.

Accepted forms:
KEY DEF
KEY DEF ARG
KEY DEF STATE MAP
KEY DEF MAP STATE
ARGS

KEY is the key to be bound to in the form of a string or vector,
DEF is the function to bind.
ARG can be either MAP or STATE.
MAP is the keymap to bind to. If nil, use `global-map'.
STATE is a symbol state to be passed to `evil-define-key'. If nil, use `normal'

If the last form is used, ARGS are of form (KEY . DEF) where DEF is
the unquoted function to bind to. In this form, keyword arguments are accepted:
:map - A MAP  or list of keymaps to bind to
:state - A STATE or list of states to be passed to `evil-define-key'"
    (declare (indent 1))
    (if (stringp (car args))
        (let ((key (pop args))
              (def (pop args))
              (state (or (cond ((symbolp (car args)) (pop args))
                               ((symbolp (nth 1 args))
                                (nth 1 args) (setq args (car args)))
                               (t nil)) 'normal))
              (map (if (keymapp args) args 'global-map)))
          `(evil-define-key ,state ,map
             ,(if (vectorp key) key (read-kbd-macro key)) ,def))

      (let* ((map (or (plist-get args :map) 'global-map))
             (maps (if (listp map) map (list map)))
             (state (or (plist-get args :state) 'normal))
             (states (if (listp state) state (list state)))
             (binds
              (apply
               #'nconc
               (mapcar
                ;; alist of (key . func) passing to `read-kbd-macro' if needed
                (lambda (k) (let ((key (car k)))
                         `(,(if (vectorp key) key (read-kbd-macro key))
                           ',(cdr k))))
                ;; get bindings from ARGS
                (progn (while (keywordp (car args)) (pop args) (pop args))
                       args)))))

        (macroexp-progn
         (mapcar
          (lambda (item)
            `(evil-define-key ',(car item) ,(cdr item) ,@binds))
          ;; build a list of (state . map) for each `evil-define-key' command
          (apply
           #'nconc
           (mapcar
            (lambda (s) (mapcar (lambda (m) (cons s m)) maps)) states)))))))

  (evil-define-command my-evil-yank-to-eol ()
    "Call `evil-yank' with point to end of line."
    (evil-yank (point) (point-at-eol)))

	;;;;; Bindings
	(evil-unbind "TAB")

	(evil-bind-key
		:state (normal motion visual)
		("SPC" . execute-extended-command)
		("Y" . my-evil-yank-to-eol)
		("q" . kill-this-buffer)
		("," . my-evil-leader-map)
		("Q" . evil-record-macro)
		("\"" . evil-jump-item)
		;; why is this default
		("j" . evil-next-visual-line)
		("k" . evil-previous-visual-line))

	(bind-keys
	 :map my-evil-leader-map
	 ("f" . find-file)
	 ("b" . list-buffers)
	 ("w" . save-buffer)
	 ("W" . sudo-save)
	 ("SPC" . evil-ex))

	;;;;; Settings
	(setq evil-echo-state nil							; state is in modeline anyway
				evil-ex-substitute-global t)		; global sed by default

	;;;;; Modes
	(my-add-list 'evil-emacs-state-modes
							 '(shell-mode term-mode multi-term-mode))
	;;;;; Packages
	(use-package evil-escape							; escape from everything with two keys
		:init (evil-escape-mode t))

	(use-package evil-surround						; surround operator
		:init (add-hook 'prog-mode-hook #'evil-surround-mode))

	(use-package evil-commentary :disabled t ; comment operator
		:init (evil-commentary-mode t))

	(use-package evil-matchit							; more tag jumping support
		:bind (:map evil-matchit-mode-map
								([remap evil-jump-item] . evilmi-jump-items))
		:init (global-evil-matchit-mode t)
		:config
		(setq evilmi-may-jump-by-percentage nil)); allow count usage
	)

;;;;; Helm
(use-package helm												; fuzzy minibuffer completion
	:demand t
	:init (helm-mode t)
  :bind (([remap execute-extended-command] . helm-M-x)
				 ([remap occur] . helm-occur)
				 ([remap find-file] . helm-find-files)
				 ([remap switch-to-buffer] . helm-mini)
				 ([remap list-buffers] . helm-mini)
				 ([remap ibuffer] . helm-mini)
				 ([remap evil-paste-pop] . helm-show-kill-ring)
				 ;; help
				 ([remap describe-face] . helm-colors)
				 ([remap ucs-insert] . helm-ucs)
				 ([remap info-emacs-manual] . helm-info-emacs)
				 ([remap info-lookup-symbol] . helm-info-at-point)
				 ([remap locate-library] . helm-locate-library)
				 ([remap woman] . helm-man-woman)
				 ([remap manual-entry] . helm-man-woman)
				 ([remap apropos] . helm-apropos)
				 ([remap apropos-command] . helm-apropos)
				 ([remap apropos-documentation] . helm-apropos)
				 :map emacs-lisp-mode-map
				 ("C-/". helm-semantic-or-imenu)
				 :map helm-buffer-map
				 ("C-d" . helm-buffer-run-kill-buffer)
				 ("<C-return>" . helm-buffer-switch-other-window)
				 :map helm-map
				 ("M-d" . helm-scroll-other-window)
				 ("M-u" . helm-scroll-other-window-down))
	:config

	;;;;; Fiddly bits
	;; Add use-package declarations to colors
  (defun my-helm-imenu-transformer (candidates)
    "Custom imenu transformer to add colouring for headings."
    (cl-loop
     for (k . v) in candidates
     for types = (or (helm-imenu--get-prop k) (list "Function" k))
     collect (cons (mapconcat
                    (lambda (x)
                      (propertize
                       x 'face
                       (cond
                        ((string= x "Variables") 'font-lock-variable-name-face)
                        ((string= x "Function") 'font-lock-function-name-face)
                        ((string= x "Types") 'font-lock-type-face)
                        ((string= x "Packages") 'font-lock-doc-face)
                        ((string= x "Headings") 'font-lock-keyword-face))))
                    types helm-imenu-delimiter)
                   (cons k v))))
	(advice-add #'helm-imenu-transformer :override #'my-helm-imenu-transformer)
	;;;;; Faces
	(set-face-attribute 'helm-buffer-directory nil :inherit 'dired-directory
											:foreground nil :background nil)
	(set-face-attribute 'helm-buffer-saved-out nil
											:inherit 'font-lock-warning-face
											:foreground nil :background nil :inverse-video nil)

	;;;;; Settings
	(setq
	 helm-move-to-line-cycle-in-source t	; cycle on end
	 helm-scroll-amount 5									; other window scroll
	 helm-split-window-in-side-p t				; split in current window
	 helm-ff-auto-update-initial-value t	; auto update when only one match
	 helm-ff-file-name-history-use-recentf t ; use recentf
	 ;;helm-ff-search-library-in-sexp t			; get library from functions
	 helm-ff-skip-boring-files t					; skip irrelevant files
	 ;;helm-findutils-search-full-path t		; search in full path with shell
	 helm-findutils-skip-boring-files t		; skip irrelevant files in shell
	 helm-mode-fuzzy-match t							; fuzzy matching
	 helm-completion-in-region-fuzzy-match t ; more fuzzy
	 )
	(helm-autoresize-mode t)

	;;;;; Packages
	(use-package helm-dash :disabled t)		; TODO language documentation

	(use-package helm-descbinds						; `describe-bindings' replacement
		:init (helm-descbinds-mode t)
		:config (setq helm-descbinds-window-style 'split-window))

	(use-package helm-swoop								; fast search and navigation
		:bind (([remap grep] . helm-swoop)
					 :map isearch-mode-map
					 ("M-/" . helm-swoop-all-from-isearch)
					 :map helm-swoop-map
					 ("M-/" . helm-multi-swoop-all-from-helm-swoop))
		:config
		(setq helm-swoop-speed-or-color t
					helm-swoop-use-fuzzy-match t)))

;;; Help

(use-package discover-my-major					; list current major mode bindings
	:bind ([remap describe-mode]. discover-my-major))

(use-package guide-key									; delayed completion for prefix keys
	:init (guide-key-mode t)
	:config (setq guide-key/recursive-key-sequence-flag t
								guide-key/guide-key-sequence '("C-x" "C-c" "C-w" ",")))

(use-package help-mode
	:ensure nil
	:bind (:map help-mode-map
              ("H" . help-go-back)
              ("L" . help-go-forward)
              :map help-map
              ("<f1>" . apropos)
              ("w" . woman)
              ("k" . describe-key-briefly)
              ("K" . describe-key)
              ("C-k" . describe-bindings)
              ("m" . describe-mode)
              ("M-k" . where-is)
              ("c" . describe-face)
              ("l" . locate-library)
              ("M-l" . view-lossage)
              ("u" . ucs-insert)
              ("i" . info-lookup-symbol)
              ("C-i" . info-emacs-manual)))

(use-package info
	:config (add-to-list 'Info-directory-list (expand-file-name "info" (getenv "XDG_CONFIG_HOME"))))

;;; Appearance
;;;; Font
(defconst my-font-text '(:family "Monoid"))
(defconst my-font-prog '(:family "Tamzen"))

(defun my-font-use-text ()
	"Use `my-font-text' for current buffer."
	(interactive)
	(setq buffer-face-mode-face my-font-text))

(defun my-font-use-prog ()
	"Use `my-font-prog' for current buffer."
	(interactive)
	(setq buffer-face-mode-face my-font-prog))

(defun my-font-frame-prog ()
	"Set frame font to `my-font-prog'."
	(interactive)
	(set-face-attribute 'default nil :font (car (cdr my-font-prog)) :height 109))

(add-hook 'after-init-hook #'my-font-frame-prog)
(add-hook 'text-mode-hook #'my-font-use-text)

;;;; Highlight fic
;; TODO make this a separate package

(defface font-lock-fic-face
  '((t (:slant italic :inherit error)))
  "Face to fontify FIXME/TODO words"
  :group 'faces)

(defun my-highlight-fic ()
  "Highlight FIXME and TODO keywords."
  (font-lock-add-keywords
   nil `((,(regexp-opt '("TODO" "TODO?" "FIXME" "FIXME?" "DOING" "REVIEW"
                         "HACK") t)
          1 'font-lock-fic-face prepend))))

(add-hook 'prog-mode-hook #'my-highlight-fic)

;;;; Theme
(use-package gruvbox-theme :demand t :config (load-theme 'gruvbox))

;;;; Modeline
;; TODO alllll of this
(use-package delight :disabled t)
(use-package powerline :disabled t)
(use-package smart-mode-line :disabled t)

(use-package nyan-mode									; essential package
	:init (nyan-mode t)
	:config
	;; FIXME this doesn't work
	(setq nyan-wavy-trail t))

(use-package which-func									; Modeline definition name
	:init (which-function-mode t))

(use-package wc-goal-mode								; Modeline word count
	:init (add-hook 'text-mode-hook #'wc-goal-mode))

;;;; Faces
(use-package hl-line
	:init (add-hook 'prog-mode-hook #'hl-line-mode))

(use-package hl-sentence
	:init (add-hook 'text-mode-hook #'hl-sentence-mode)
	:config (setq hl-sentence-face 'highlight))

(use-package highlight-numbers
	:init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package page-break-lines						; Horizontal lines instead of ^L
	:init (global-page-break-lines-mode t))

(use-package rainbow-delimiters
	:init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package whitespace
	:init (add-hook 'prog-mode-hook #'whitespace-mode)
	:config
	(setq
	 whitespace-line-column nil
	 whitespace-style '(face trailing lines-tail)
	 whitespace-action '(auto-cleanup warn-if-read-only)))

;;; Interface

(use-package zone 											; Screensaver
	:config
	(use-package zone-nyan)
	(setq zone-programs [zone-nyan]
				zone-timer (run-with-idle-timer 120 t 'zone)))

(use-package golden-ratio :disabled t		; Window resizing
	:init (golden-ratio-mode t))

(use-package hideshow :disabled t				; Code folding
	(use-package hideshowvis))

(use-package popwin	:disabled t					; Popup window for minor buffers
	:init (popwin-mode t))

(use-package uniquify										; Distinguish same-named buffers
	:ensure nil
	:config (setq uniquify-buffer-name-style 'forward
								uniquify-trailing-separator-p t))

(use-package winner	:disabled t					; Window configuration undo
	:bind (("C-w u" . winner-undo)
				 ("C-w U" . winner-redo))
	:init (winner-mode t))

(use-package writeroom-mode						; Distraction-free writing mode
	:bind (:map text-mode-map ("<C-f11>" . writeroom-mode))
	:config
	(setq writeroom-width 100)
	(defun my-writeroom-effect (arg)
		(let ((off (if arg -1 t))
					(on (if arg t -1)))
			(display-time-mode on)
			(display-battery-mode on)
			(which-function-mode off)
			(nyan-mode off)))
	(add-to-list 'writeroom-global-effects #'my-writeroom-effect))

(use-package visual-fill-column					; Wrap text at fill col
	:init (add-hook 'text-mode-hook #'visual-fill-column-mode)
	:config (setq-default visual-fill-column-width 100))

;;; Navigation
(use-package avy												; Jump to specific points
	:bind (("C-e" . avy-goto-word-1)
				 ("C-S-e" . avy-goto-line))
	:init (evil-unbind "C-e")
	:config (setq avy-background t))

(use-package ace-window :disabled t
	:bind ([remap other-window] . ace-window)
	:config (setq aw-keys (or avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(use-package ag :disabled t							; Fast search and file nav
	(use-package helm-ag))

(use-package desktop										; Save opened buffers, windows, frames
	:init (desktop-save-mode t)
	:config
	(setq desktop-auto-save-timeout 60
				desktop-dirname my-dir)
	(add-to-list 'desktop-path desktop-dirname)
	(my-add-list 'desktop-modes-not-to-save
							 '(magit-mode git-commit-mode)))

(use-package expand-region :disabled t	; Expand selection by blocks at a time
	:init (evil-bind-key :state (normal motion visual)
											 ("zz" . er/expand-region))
	:config (setq expand-region-contract-fast-key "x"))

(use-package savehist										; Save command history
	:init (savehist-mode t)
	:config (setq savehist-file (expand-file-name "savehist" my-dir)
								history-delete-duplicates t
								savehist-save-minibuffer-history t))

(use-package saveplace									; Save and return to place in file
	:demand t
	:config
	(setq-default save-place t)
	(setq save-place-file (expand-file-name "places" my-dir)))

(use-package projectile									; Project-based navigation
	:bind (:map my-evil-leader-map
							("p" . projectile-find-file-dwim)
							("P" . projectile-switch-project))
	:init (projectile-global-mode t)
	:config
	(setq projectile-cache-file (expand-file-name "projectile.cache" my-dir)
				projectile-known-projects-file (expand-file-name "projectile.eld" my-dir))

	(use-package helm-projectile
		:bind (:map my-evil-leader-map
								([remap projectile-find-file-dwim] . helm-projectile)
								([remap projectile-switch-project] . helm-projectile-switch-project))
		:init (helm-projectile-on)
		:config (setq projectile-completion-system 'helm
									projectile-switch-project-action #'helm-projectile
									helm-projectile-fuzzy-match t)))

;;; Editing
(use-package undo-tree
	:config (unbind-key "C-/" undo-tree-map))

(use-package multiple-cursors :disabled t
	:init (multiple-cursors-mode t))

(use-package drag-stuff :disabled t
	:init (drag-stuff-global-mode t))

(use-package flycheck
	:bind (:map my-evil-leader-map
							("cj" . flycheck-next-error)
							("ck" . flycheck-previous-error))
	:init (add-hook 'prog-mode-hook #'flycheck-mode)
	:config
	(setq flycheck-keymap-prefix (kbd "C-c f"))

	(use-package helm-flycheck
		:bind (:map flycheck-mode-map
								("C-c f c" . helm-flycheck)
								:map my-evil-leader-map
								("cc" . helm-flycheck)))

	(use-package flycheck-tip							; Display errors by popup
		:bind (([remap flycheck-next-error] . flycheck-tip-cycle)
					 ([remap flycheck-previous-error] . flycheck-tip-cycle-reverse))
		:config (flycheck-tip-use-timer 'normal)))

(use-package lorem-ipsum								; Insert filler text
	:config
	(defun my-lorem-sgml-settings ()
		(setq lorem-ipsum-paragraph-separator "\n<p>"
					lorem-ipsum-sentence-separator " "))
	(add-hook 'sgml-mode-hook #'my-lorem-sgml-settings)
	(unless sentence-end-double-space (setq lorem-ipsum-sentence-separator " ")))

(use-package writegood-mode							; Highlight poor forms in writing
	:init (add-hook 'text-mode-hook #'writegood-mode)
	:config
	(add-hook 'writegood-mode-hook #'writegood-passive-voice-turn-off)
	(my-add-list 'writegood-weasel-words
							 '("thing" "different" "probably" "really")))

(use-package abbrev											; Auto-correct
	:ensure nil
	:init (abbrev-mode t)
	:config (setq save-abbrevs 'silently
								abbrev-all-caps t
								abbrev-file-name (expand-file-name "abbrevs.el" my-dir)))

(use-package auto-indent-mode :disabled t
	:init (auto-indent-global-mode t))

(use-package autoinsert :disabled t			; Auto insert buffer text
	:init (auto-insert-mode t))

(use-package company										; Autocompletion
	:bind (:map company-active-map
							("M-j" . company-select-next)
							("M-k" . company-select-previous)
							("M-d" . company-show-doc-buffer))
	:init (add-hook 'prog-mode-hook #'company-mode)
	:config
	(setq company-idle-delay 0						; immediate completion attempt
				company-show-numbers t					; allow M-num selection
				company-tooltip-align-annonations t
				company-selection-wrap-around t)

	(use-package helm-company
		:bind (:map (company-mode-map company-active-map)
								("C-:" . helm-company)))

	(use-package company-statistics				; Sort candidates by statistics
		:init (company-statistics-mode t)
		:config
		(setq company-statistics-file (expand-file-name "company-stats.el" my-dir))))

(use-package ispell											; Spell checking
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

	(use-package flyspell									; On-the-fly spell check
		:init
		(add-hook 'text-mode-hook #'flyspell-mode)
		(add-hook 'prog-mode-hook #'flyspell-prog-mode)

		:config
		(use-package flyspell-popup
			:bind ([remap ispell-word] . flyspell-popup-correct))

		(use-package flyspell-lazy
			:init
			(add-hook 'flyspell-mode #'flyspell-lazy-mode)
			(add-hook 'flyspell-prog-mode #'flyspell-lazy-mode))

		(setq flyspell-issue-welcome-flag nil
					flyspell-issue-message-flag nil)
		(advice-add #'flyspell-auto-correct-word :around #'my-ispell-run-together)))

(use-package outshine										; Outline-mode improvements
	:bind (:map outline-minor-mode-map ("C-c o" . outline-insert-heading))
	:init
	(add-hook 'outline-minor-mode-hook #'outshine-hook-function)
	(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
	;; :config
  ;; (evil-bind-key
  ;;     :state (normal motion visual)
  ;;     :map outline-minor-mode-map
	;; 		("TAB" . outline-cycle)
	;; 		("gh" . outline-up-heading)
	;; 		("gj" . outline-next-heading)
	;; 		("gk" . outline-previous-heading)
	;; 		("gl" . outline-forward-same-level)
	;; 		("<" . outline-promote)
	;; 		(">" . outline-demote))
	)

(use-package smartparens-config :disabled t ; Balanced parenthesis management
	:ensure smartparens)

(use-package autorevert									; Auto revert external modifications
	:init (global-auto-revert-mode t)
	:config (setq global-auto-revert-non-file-buffers t))

(use-package pandoc-mode								; Markup conversion tool
	:bind (("C-c C-p" . pandoc-mode)
				 :map pandoc-mode-map
				 ([remap pandoc-mode]. pandoc-run-pandoc))
	:config (setq pandoc-data-dir (expand-file-name "pandoc" my-dir)))

(use-package real-auto-save							; Auto save buffers
	:commands real-auto-save-mode
	:init (add-hook 'after-save-hook #'real-auto-save-mode)
	:config (setq real-auto-save-interval 60))

(use-package recentf										; List recent files
	:config
	(setq recentf-exclude '("COMMIT_EDITMSG") ; exclude commit messages
				recentf-save-file (expand-file-name "recentf" my-dir)))

(use-package compile
	:config (setq compilation-always-kill t ; kill old before starting new
								comilation-skip-threshold 2 ; skip warning and info messages
								compilation-context-lines 3
								compilation-scroll-output 'first-error
								compilation-auto-jump-to-first-error t
								compilation-ask-about-save nil))

(use-package paradox										; Better package management
	:config
	(use-package async)
	(setq paradox-execute-asynchronously t))

(use-package ediff											; Emacs diff utility
	:bind (:map ediff-mode
							("j" . ediff-next-difference)
							("k" . ediff-previous-difference)))

(use-package maylon											; Z-machine text-based adventure reader
	:ensure nil)

(use-package magit
	:bind (("<f10>" . magit-status)
				 :map magit-status-mode-map
				 ("j" . next-line)
				 ("k" . previous-line)
				 ("<down>" . magit-goto-next-sibling-section)
				 ("<up>" . magit-goto-previous-sibling-section)
				 ("C" . magit-commit)
				 ("d" . magit-discard-item)
				 ("C-=" . magit-diff-working-tree))
	:config
	(setq magit-diff-options '("-b")))	; ignore whitespace in diffs

(use-package git-timemachine						; Travel through commit history
	:bind ("<C-f10>" . git-timemachine-toggle))

;;; Languages
(use-package lua-mode)
(use-package gitignore-mode)
(use-package markdown-mode)
(use-package vimrc-mode :mode "[._]?(pentadactyl|vimperator)rc$" "\\.(penta|vimp)$")
(use-package org :disabled t)
(use-package generic-x :disabled t)

(use-package lisp
	:ensure nil
	:bind (:map emacs-lisp-mode-map ("C-c C-c" . eval-defun))
	:init
	(use-package eldoc										; Documentation in echo area
		:init (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
		:config (setq eldoc-idle-delay 0.3))

	(use-package highlight-quoted					; Faces for lisp quotes and symbols
		:init (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))
	(use-package elisp-slime-nav					; Navigate elisp documentation
		:init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
		:config
		;;(evil-bind-key :map (elisp-slime-nav-mode-map help-mode-map)
		;;							 ("K" . elisp-slime-nav-describe-elisp-thing-at-point)
		;;							 ("gd" . elisp-slime-nav-find-elisp-thing-at-point))
		))

(use-package elpy
	:init (elpy-enable))

(use-package css-mode
  :defines my-sass-output-dir
  :mode "\\.s[ac]ss$"
  :config
  (defcustom my-sass-output-dir "../"
    "Directory to store compiled sass files."
    :group 'css)

  (defun my-sass-watch ()
    "Start a sass process with `sass --watch' watching the current buffer."
    (interactive)
    (let ((sass-command "sass"))
      (shell-command
       (format
        "%s --watch '%s':'%s%s.css'&"
        sass-command
        buffer-file-name
        (or my-sass-output-dir "")
        (file-name-nondirectory
         (file-name-sans-extension buffer-file-name))))))

  ;; Run `prog-mode-hook' manually since `css-mode' doesn't derive from it
  (add-hook 'css-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

  (use-package css-eldoc                ; Basic minibuffer display help for CSS
    :init (add-hook 'css-mode-hook #'css-eldoc-enable)))

(use-package web-mode
  :mode "\\.html?$"
  :config
  (setq web-mode-enable-block-face t
        web-mode-enable-part-face t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2))

(use-package java-mode
	:ensure nil
  :init
  (defun my-java-formats ()
    (interactive)
    (setq c-basic-offset 4
	  tab-width 4
	  indent-tabs-mode t
	  fill-column 100))
  :config
  (add-hook 'java-mode-hook #'my-java-formats))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

