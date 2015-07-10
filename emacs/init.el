;; -*- outline-regexp: ";;; \\*\\{1,8\\}"; -*-
;;; * init.el
;; TODO aspell messages
;; TODO eshell prompt
;; DONE? add :defer and :demand keywords correctly
;; DONE? fix evil bindings
;;; ** setup
(defconst my-dir (expand-file-name  ".user/" user-emacs-directory))
(defconst my-dir-elisp (expand-file-name "my-elisp" user-emacs-directory))
(defconst my-dir-packages (expand-file-name "elisp/" user-emacs-directory))

(add-to-list 'load-path my-dir-packages) ; location for non-MELPA packages
(add-to-list 'load-path my-dir-elisp) ; location for personal elisp files

;; load custom file
(setq custom-file (expand-file-name "custom.el" my-dir))
(load custom-file 'no-error 'no-message)


;;; *** functions

(defun my-add-hooks (hook functions)
  "Add each function in FUNCTIONS to HOOK if the function has been bound."
  (dolist (func functions)
    (when (fboundp func) (add-hook hook func))))


;; http://www.writequit.org/org/settings.html#sec-1-38
(defun my-narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode)
         (cond ((org-in-src-block-p)
                (org-edit-src-code)
                (delete-other-windows))
               ((org-at-block-p)
                (org-narrow-to-block))
               (t (org-narrow-to-subtree))))
        (t (narrow-to-defun))))


;; http://stackoverflow.com/questions/24356401/how-to-append-multiple-elements-to-a-list-in-emacs-lisp
(defun my-append-to-list (list-var elements)
  "Add to the end of LIST-VAR each item in ELEMENTS.

Return the new value of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))

  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))

  (symbol-value list-var))


(defun my-cleanup ()
  "Clean up the current buffer.
First untabify, then re-ident, and then if bound call `whitespace-cleanup'."
  (interactive)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max))
  (when (fboundp 'whitespace-cleanup) (whitespace-cleanup)))


(defun my-english-count-lines ()
  "Count screen lines in a region with handwriting font activated."
  (interactive)
  (let ((handwriting '(:family "Kelly Normal" :height 220)))
    (unless (member `(default ,handwriting default) face-remapping-alist)
      (face-remap-add-relative 'default handwriting))
    (message "%s" (count-screen-lines (region-beginning) (region-end)))
    (face-remap-remove-relative
     (face-remap-add-relative 'default handwriting))))


;;; *** package
(require 'package)
(setq package-enable-at-startup nil         ; we will manually initialize
      load-prefer-newer t)                  ; don't load outdated byte code

(my-append-to-list 'package-archives
                   '(("melpa" . "http://melpa.milkbox.net/packages/")
                     ("org" . "http://orgmode.org/elpa/")
                     ("elpy" . "http://jorgenschaefer.github.io/packages/")))

(package-initialize)                     ; manually initialize


;;; *** use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-verbose t            ; log message after loading a package
      use-package-always-ensure t)     ; ensure all packages are installed


(use-package delight)
(use-package bind-key)

;;; ** basic settings
;;; *** general
(setq
 disabled-command-function nil          ; no disabld
 comment-auto-fill-only-comments t
 smooth-scroll-margin 3                 ; fewer lines visible at buffer ends
 frame-title-format "%b emacs"          ; buffer name as frame title
 window-combination-resize t            ; use proportional window resize
 echo-keystrokes 0.1                    ; echo unfinished commands faster
 x-underline-at-descent-line t          ; draw underline lower
 ring-bell-function 'ignore             ; disable alarms
 initial-major-mode 'text-mode          ; scratch in text mode
 save-interprogram-paste-before-kill t  ; don't overwrite clipboard contents
 kill-do-not-save-duplicates t
 line-move-visual t
 ;; compilation
 compilation-always-kill t        ; kill old processes before starting new ones
 compilation-skip-threshold 2     ; skip warning and info messages
 compilation-context-lines 3      ; show 3 lines of context around message
 compilation-scroll-output 'first-error
 compilation-auto-jump-to-first-error t
 compilation-ask-about-save nil
 ;; formatting
 tab-always-indent nil                  ; tab inserts a character
 require-final-newline t
 tab-width 4
 sentence-end-double-space nil
 ;; files
 read-file-name-completion-ignore-case t
 vc-handled-backends '(SVN Git)         ; remove unnecessary vc backends
 delete-by-moving-to-trash t            ; use system trash for deletion
 large-file-warning-threshold 20000000  ; larger warning threshold
 find-file-visit-truename t             ; silently follow symlinks
 view-read-only t                       ; view read-only files in view-mode
 ;; autosave file location
 auto-save-list-file-prefix (expand-file-name "autosaves/" my-dir)
 auto-save-file-name-transforms `((".*" ,auto-save-list-file-prefix t))
 ;; backups
 backup-directory-alist `(("." . ,(expand-file-name "backups" my-dir)))
 backup-by-copying t
 version-control t                      ; add version numbers
 delete-old-versions t                  ; delete old backups silently
 kept-old-versions 5
 kept-new-versions 8
 ;; user info
 user-full-name "Kelly Stewart"
 user-mail-address "stewart.g.kelly@gmail.com")

(setq-default
 indent-tabs-mode nil               ; turn tabs to spaces
 fill-column 79)

;; UTF-8 everywhere
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(fset 'yes-or-no-p #'y-or-n-p)

;;; remove startup messages
(setq inhibit-startup-screen t
      initial-scratch-message nil)
(fset display-startup-echo-area-message #'ignore)

(defun my-bury-scratch ()
  "Bury the scratch buffer instead of killing it."
  (if (equal (buffer-name) "*scratch*") (progn (bury-buffer) nil) t))
(add-to-list 'kill-buffer-query-functions #'my-bury-scratch)

(defun my-compilation-close-on-success (buf result)
  "Close window if compilation did not exit abnormally."
  (let ((win (get-buffer-window buf 'visible)))
    (when (and win (not (string-match ".*exited abnormally.*" result)))
      (delete-window win))))
(add-to-list 'compilation-finish-functions #'my-compilation-close-if-success)

(defun my-comment-dwim-line (&rest args)
  "Advice for `comment-dwim'. Toggle current line comment if no active region."
  (when (and (not (region-active-p))
             (not (or (eq (point) (line-end-position))
                      ;; compensate for evil-mode cursor being one point ahead
                      (eq (point) (- (line-end-position) 1)))))
    (comment-or-uncomment-region
     (line-beginning-position) (line-end-position))))
(advice-add #'comment-dwim :before-until #'my-comment-dwim-line)

(defun my-change-word-case (arg)
  "Advice for functions to change case. First moves to beginning of word."
  (unless (looking-back "\\b") (backward-word)))
(dolist (func '(upcase-word downcase-word capitalize-word))
  (advice-add func :before #'my-change-word-case))


;;; *** modes and hooks

(mouse-wheel-mode t)                    ; Mouse wheel enabled
(show-paren-mode t)                     ; Highlight matching parens
(electric-indent-mode t)                ; Auto indent
(electric-pair-mode t)                  ; Auto add parens
(aut-insert-mode t)                     ; Auto insert text based on filetype

;; we don't need no stinkin GUI
(size-indication-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(unless (display-graphic-p) (menu-bar-mode -1)) ; no menu bar in terminal

(my-add-hooks 'prog-mode-hook
              '(hl-line-mode            ; Highlight current line
                prettify-symbols-mode   ; Replace words with symbols
                auto-fill-mode          ; Automatically fill text past fill-col
                goto-address-prog-mode  ; Buttonize URLs in comments and string
                column-number-mode      ; Numbers in modeline
                line-number-mode))

(my-add-hooks 'text-mode-hook
              '(visual-line-mode        ; Visually wrap text by words
                goto-address-mode       ; Buttonize URLs
                visual-fill-column-mode)) ; Wrap to fill-column

;;; *** bindings

;; Adapted from http://www.reddit.com/r/emacs/comments/25v0eo/you_emacs_tips_and_tricks/chldur
(defun my-vsplit-last-buffer (prefix)
  "Split window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (when (= prefix 1) (switch-to-next-buffer)))

(defun my-hsplit-last-buffer (prefix)
  "Split window horizontally and display previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (when (= prefix 1) (switch-to-next-buffer)))


(bind-keys
 ("S-SPC" . cycle-spacing)              ; M-SPC is taken in Linux Mint
 ("RET" . newline-and-indent)           ; indent new lines automatically
 ("C-x C-M-x" . revert-buffer)
 ([remap evil-window-split] . my-hsplit-last-buffer)
 ([remap evil-window-vsplit] . my-vsplit-last-buffer)
 ([remap evil-window-new] . my-vsplit-last-buffer))

(bind-key "q" #'kill-buffer package-menu-mode-map)


;; https://github.com/davvil/.emacs.d/blob/master/init.el
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(bind-keys :map (minibuffer-local-map
                 minibuffer-local-ns-map
                 minibuffer-local-completion-map
                 minibuffer-local-must-match-map
                 minibuffer-local-isearch-map)
           ("<escape>" . minibuffer-keyboard-quit))


;;; **** windows super key
(when (eq system-type 'windows-nt)
  (setq w32-apps-modifier 'hyper
        ;; Super is Windows key
        ;; Taken mappings: s-l s-r
        w32-lwindow-modifier 'super
        w32-pass-lwindow-to-system nil))

;;; *** delighted modes

(delight '((emacs-listp-mode "Elisp" :major)
           (visual-line-mode)
           (auto-fill-mode)))
;; WTF.
(delight #'auto-fill-mode)
(use-package simple
  :ensure nil :defer t
  :delight auto-fill-mode
  :init
  (delight #'auto-fill-mode)
  :config
  (delight #'auto-fill-mode))

;;; *** fonts
;; DONE? check this works properly on Windows
(require 'dash)

(defvar my-fonts
  '((proportional
     . ("Input Sans Condensed" "InputSansCondensed" "Input Sans" "InputSans"
        "DejaVu Sans" "Calibri" "Arial" "Sans Serif"))
    (mono
     . ("Input Mono Condensed" "InputMonoCondensed" "Input Mono" "InputMono"
        "DejaVu Sans Mono" "Consolas" "Courier" "Monospace" "Fixed")))
  "Alist of font types paired with an ordered list of preferences.
Used with `my-font' to get the first valid entry of each font pairing.")

(defun my-font (font)
  "Return the first valid entry for FONT in `my-fonts'."
  (--first (find-font (font-spec :name it)) (cdr (assoc font my-fonts))))

(add-to-list 'my-fonts
             `(header . ,(append '("Fantasque Sans Mono" "FantasqueSansMono")
                                  (cdr (assoc 'proportional my-fonts)))))


(defun my-font-use-proportional ()
  "Set current buffer's font to `my-font-proprortional'."
  (face-remap-add-relative 'default
                           :family (my-font 'proportional) :height 100))

(set-frame-font (concat (my-font 'mono) "-8"))
(set-face-attribute 'variable-pitch nil :family (my-font 'header))

;;; ** major packages
(use-package evil    ; Vim keybindings and modal editing
  :demand t
  :init
  (setq
   evil-want-fine-undo nil          ; undo insertions in single steps
   evil-want-C-w-in-emacs-state t   ; prefix window commands with C-w
   evil-want-change-word-to-end nil ; don't let cw behave like ce
   evil-echo-state nil              ; state is in the modeline anyway
   evil-ex-substitute-global t)     ; global substitutions by default
  (evil-mode t)

  :config
  (my-append-to-list 'evil-emacs-state-modes
                     '(shell-mode
                       term-mode
                       multi-term-mode))
  (add-to-list 'evil-insert-state-modes 'insert)
  (add-to-list 'evil-motion-state-modes 'motion)

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

  (evil-define-command my-evil-yank-to-eol ()
    "Call `evil-yank' with point to end of line."
    (evil-yank (point) (point-at-eol)))


  (bind-keys :map (evil-insert-state-map evil-replace-state-map)
             ("j" . maybe-exit)) ; jk exits insert state

  (bind-keys :map (evil-normal-state-map evil-motion-state-map)
             ("Y" . my-evil-yank-to-eol))         ; more consistent

  (bind-keys
   :map (evil-normal-state-map evil-motion-state-map evil-visual-state-map)
   (":" . comment-dwim)
   ("M-;" . evil-ex)
   ("SPC" . execute-extended-command)
   ("<escape>" . keyboard-quit)
   ("q" . kill-buffer-and-window)       ; consistency with other Emacs buffers
   ("Q" . evil-record-macro)            ; Q replaces old q action
   ;; movement
   ("j" . evil-next-visual-line)
   ("k" . evil-previous-visual-line)
   ("s" . evil-last-non-blank)
   ("a" . evil-first-non-blank)
   ("\"" . evil-jump-item)
   ;; leader bindings
   ("," . nil)
   (",f" . find-file)
   (",b" . list-buffers)
   (",w" . save-buffer))

  (use-package evil-surround :init (global-evil-surround-mode t))

  (use-package evil-matchit             ; Manipulate tags
    :defines evilmi-may-jump-percentage
    :init (setq evilmi-may-jump-percentage nil) ; allow count usage
    :config
    (global-evil-matchit-mode t)
    (evil-define-key 'normal evil-matchit-mode-map
      "\"" #'evilmi-jump-items)))


(use-package helm-config            ; Fuzzy minibuffer completion
  :ensure helm :demand t
  :delight helm-mode
  :defines (helm-M-x-always-save-history
            helm-ff-search-library-in-sexp
            helm-ff-skip-boring-files
            helm-locate-fuzzy-match
            helm-ff-auto-update-initial-value
            helm-ff-file-name-history-use-recentf
            helm-findutils-search-full-path
            helm-findutils-skip-boring-files
            helm-completion-in-region-fuzzy-match
            helm-semantic-fuzzy-match
            helm-buffers-fuzzy-matching
            helm-apropos-fuzzy-match
            helm-lisp-fuzzy-completion
            helm-file-cache-fuzzy-match
            helm-recentf-fuzzy-mtch
            helm-M-x-fuzzy-match
            helm-imenu-fuzzy-match)
  :init
  (setq
   helm-quick-update t
   helm-command-prefix-key "C-c h"
   helm-move-to-line-cycle-in-source t       ; cycle on buffer end
   helm-display-header-line nil              ; no header line
   helm-scroll-amount 5                      ; scroll amount in other window
   helm-split-window-in-side-p t             ; split inside current window
   helm-M-x-always-save-history t            ; save history even on fail
   helm-ff-auto-update-initial-value t       ; auto update when only one match
   helm-ff-file-name-history-use-recentf t   ; use recentf
   helm-ff-search-library-in-sexp t          ; get library from functions
   helm-ff-skip-boring-files t               ; skip irrelevant files
   helm-findutils-search-full-path t         ; search in full path with shell
   helm-findutils-skip-boring-files t        ; skip irrelevant files in shell
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

  (helm-mode t)
  :config
  (defalias #'ibuffer #'helm-mini)
  (helm-autoresize-mode t)

  (bind-keys*
   ;; Helm replacements
   ([remap execute-extended-command] . helm-M-x)
   ;; ("M-s o" . helm-occur)
   ;; ("C-x r i" . helm-register)
   ([remap list-buffers]. helm-mini)
   ([remap ibuffer]. helm-mini)
   ([remap find-files] . helm-find-files)
   ([remap locate-library] . helm-locate-library)
   ([remap apropos] . helm-apropos)
   ([remap manual-entry] . helm-man-woman)
   ;; Additional Helm functions
   ("M-/" . helm-do-grep)
   ("C-y" . helm-show-kill-ring)
   ("C-/" . helm-semantic-or-imenu)
   ("<help> C-r" . helm-info-at-point)
   ("<help> i" . helm-info-emacs)
   ("<help> I" . helm-info-elisp))

  (bind-keys
   :map helm-map
   ("<C-S-up>" . helm-scroll-other-window)
   ("<C-S-down>". helm-scroll-other-window-down)
   ("<tab>" . helm-execute-persistent-action) ; execute action without closing
   ("C-z" . helm-select-action))            ; list actions

  (bind-keys
   :map (evil-normal-state-map
         evil-motion-state-map
         evil-visual-state-map)
   (",hy" . helm-show-kill-ring)
   (",hs" . helm-do-grep)
   (",hu" . helm-ucs)
   (",hc" . helm-colors)
   (",H" . helm-resume))

  (use-package imenu-anywhere
    :bind ([remap helm-semantic-or-imenu] . imenu-anywhere))

  (use-package helm-dash            ; TODO Language documentation viewer
    :init (setq
           helm-dash-browser-func 'eww
           helm-dash-docsets-path "~/.emacs.d/.user/docset")
    ;; :config
    ;; (add-hook 'python-mode-hook (lambda()
    ;;                               (setq-local helm-dash-docsets '("Python"))))
    )

  (use-package helm-descbinds       ; Replacement for `describe-bindings'
    :init
    (setq helm-descbinds-window-style 'split-window)
    (helm-descbinds-mode t))

  (use-package helm-swoop :disabled t   ; Fast searching and navigation
    :bind (("M-/" . helm-swoop)
           ("C-c h s" . helm-swoop)
           ("C-c h S" . helm-multi-swoop)
           ("C-c h C-s" . helm-multi-swoop-all)
           ("C-c h M-s" . helm-multi-swoop-current-mode))
    :init
    (bind-key "M-i" #'helm-swoop-from-isearch)
    (bind-key ",hs" #'helm-swoop evil-normal-state-map)
    :config (bind-key "M-i" #'helm-multi-swoop-from-helm-swoop helm-swoop-map)))

;;; ** appearance

(use-package hl-line+ :disabled t)     ; TODO what.

;;; *** highlight fic
;; DONE? fixme todo highlight
(defface font-lock-fic-face
  '((((class color))
     (:background "white" :foreground "red" :weight bold))
    (t (:weight bold)))
  "Face to fontify FIXME/TODO words"
  :group 'faces)

(defun my-highlight-fic ()
  "Highlight FIXME and TODO keywords."
  (font-lock-add-keywords
   nil `(("\\(TODO\\??\\|FIXME\\??\\|DOING\\|DONE\\??\\)"
          1 'font-lock-fic-face prepend))))

(add-hook 'prog-mode-hook #'my-highlight-fic)

;;; *** modeline packages
(use-package smart-mode-line    ; Better modeline
  :demand t
  :defines (sml/use-projectile-p
            sml/projectile-replacement-format)
  :init
  (setq
   sml/theme 'respectful
   sml/battery-format "%b%p[%t]"
   sml/full-mode-string " ⋯"           ; append this to modeline when full
   sml/shorten-mode-string ""          ; no indication for all modes displayed
   sml/mule-info nil                   ; don't show buffer encoding
   sml/use-projectile-p t              ; projectile file prefix takes precedent
   sml/projectile-replacement-format "[π:%s]")

  :config
  (setq sml/replacer-regexp-list
	(append sml/replacer-regexp-list
		'(("^/media/user/" ":θ:")
		  (":θ:Documents/" ":Δ:")
		  (":Δ:work/" ":Σ:")
		  (":θ:dev/" ":δ:")
		  (":δ:dotfiles/" ":.:")
		  ("^:\\.:emacs/" ":.ε:"))))
  (sml/setup))


(use-package nyan-mode    ; Nyan cat scroll bar
  :demand t
  ;; TODO nyan music ☹
  :commands nyan-mode
  :functions my-turn-off-nyan-mode
  :init (setq-default nyan-wavy-trail t) ; TODO wavy nyan trail all the time
  :config
  (defun nyan-mode-turn-off () (nyan-mode -1))
  (nyan-mode t))


(use-package which-func    ; Modeline definition name
  :demand t
  :config
  (defun my-which-func-current ()
    "Return a formatted which-func string if possible, or nil if not."
    (-if-let (current (gethash (selected-window) which-func-table))
        (truncate-string-to-width
         (concat
          " ➤ "
          (replace-regexp-in-string "%" "%%" current))
         20 nil nil "⋯")
      nil))

  (setq which-func-format
        `((:propertize (:eval (my-which-func-current))
                       local-map ,which-func-keymap
                       face which-func
                       mouse-face mode-line-highlight)))
  (which-function-mode t))


(use-package wc-goal-mode
  :init (add-hook 'text-mode-hook #'wc-goal-mode)
  :config
  (defun my-wc-format-toggle ()
    (interactive)
    (let ((a "wc:%tw%w") (b "lc:%tl%l"))
      (setq wc-goal-modeline-format (if (eql wc-goal-modeline-format a)
                                        b a)))))

;;; *** face packages


(use-package hl-sentence                ; Highlight current sentence
  :defer t
  :init (add-hook 'text-mode-hook #'hl-sentence-mode)
  :config (set-face-attribute 'hl-sentence-face nil :inherit 'hl-line))


(use-package highlight-numbers          ; Highlight numbers
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))


(use-package page-break-lines          ; Horizontal lines instead of ^L
  :defer t
  :delight page-break-lines-mode
  :init (global-page-break-lines-mode t))


(use-package paren-face
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook #'paren-face-mode))


(use-package rainbow-mode               ; Highlight color codes
  :defer t
  :delight rainbow-mode
  :init
  (dolist (hook '(web-mode-hook
                  css-mode-hook))
    (add-hook hook #'rainbow-mode)))


(use-package whitespace             ; Faces for whitespace characters
  :defer t
  :delight whitespace-mode
  :init
  (setq
   whitespace-line-column nil       ; use fill-column value
   ; modes to disable whitespace-mode
   whitespace-global-modes
   '(not org-mode eshell-mode shell-mode web-mode dired-mode)
   ;; trailing whitespace and tails of long lines
   whitespace-style
   '(face trailing lines-tail)
   ;; clean whitespace on write and warn if readonly
   whitespace-action '(auto-cleanup warn-if-read-only))

  (add-hook 'prog-mode-hook #'whitespace-mode))

;;; *** color theme


(use-package solarized-theme
  :defer t
  :init (setq
         solarized-scale-org-headlines nil
         solarized-height-plus-1 1.2
         solarized-height-plus-2 1.2
         solarized-height-plus-3 1.2
         solarized-height-plus-4 1.2))


(use-package zenburn-theme
  :defer t)

(load-theme 'solarized-dark)

;;; ** interface


(use-package linum    ; Line numbers
  :defer t
  :init (add-hook 'prog-mode-hook #'linum-mode))


(use-package golden-ratio :disabled t   ; Resize windows to golden ratio
  :defer t
  ;; TODO get this to play nice with Helm
  :delight golden-ratio-mode
  :init
  ;; http://writequit.org/org/settings.html
  (defun my-helm-alive-p ()
    (when (boundp 'helm-alive-p)
      (symbol-value 'helm-alive-p)))

  (setq
   ;; exclude helm
   golden-ratio-inhibit-functions #'my-helm-alive-p
   golden-ratio-exclude-modes '(helm-mode
                                magit-log-mode
                                magit-reflog-mode
                                magit-status-mode)
   golden-ratio-auto-scale t)           ; auto scale with screen size
  (golden-ratio-mode t))


(use-package hideshow                 ; Code folding
  ;; TODO org-mode
  :init
  ;; enable in modes where folding marks have been defined
  (defvar hs-special-modes-alist '())
  (dolist (mode '(c-mode
                  c++-mode
                  java-mode
                  js-mode
                  css-mode
                  web-mode))
    (add-to-list 'hs-special-modes-alist `(,mode "{" "}" "/[*/]" nil nil))
    (add-hook mode #'hs-minor-mode)))


(use-package popwin    ; Popup window for minor buffers
  :demand t
  :commands popwin-mode
  :init (setq popwin:popup-window-position 'right)
  :config
  (my-append-to-list 'popwin:special-display-config
                     '(("*Backtrace*" :noselect t)
                       ("*Python Help*" :stick t :height 20)
                       ("*Help*" :stick t :noselect t :height 20)))
  (popwin-mode t))


(use-package uniquify    ; Distinguish buffers with same name
  :ensure nil :demand t
  :init (setq uniquify-buffer-name-style 'forward
              uniquify-trailing-separator-p t)) ; add separator to dired names


(use-package winner    ; Window configuration undo
  :demand t
  :bind (("s-j" . winner-undo)
         ("s-k" . winner-redo))
  :config (winner-mode t))


(use-package writeroom-mode         ; Distraction-free writing mode
  :delight writeroom-mode " Σ"
  :init
  (setq writeroom-width 100)
  (bind-key "C-q" #'writeroom-mode text-mode-map)
  :config
  ;; Apply additional effects
  (defun my-writeroom-effect ()
    "Apply additional functions for distraction-free writing.
Can be used outside writeroom mode."
    (display-time-mode t)
    (display-battery-mode t)
    (which-function-mode -1)
    ;; (nyan-mode nil) ; may not be needed with modeline format
    (setq mode-line-format
          '("%e"
            mode-line-front-space
            mode-line-frame-identification
            mode-line-buffer-identification
            mode-line-modes
            mode-line-misc-info
            mode-line-end-spaces)))
  (add-hook 'writeroom-mode #'my-writeroom-effect))


(use-package visual-fill-column
  :init
  (setq visual-fill-column-width 100)
  (add-hook 'text-mode-hook #'visual-fill-column-mode))

;;; ** navigation


(use-package ace-jump-mode              ; Jump to specific points with marks
  :bind ("C-SPC" . ace-jump-mode))


(use-package ace-window
  :bind ("M-o" . ace-window)
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


(use-package ag                         ; Fast search and navigation
  :config
  (use-package helm-ag
    :bind (("M-/" . helm-do-ag-this-file)
           ("C-M-/" . helm-do-ag-project-root)
           ("C-M-?" . helm-do-ag-buffers)
           ("C-c h s" . helm-do-ag-this-file)
           ("C-c h C-s" . helm-do-ag-project-root)
           ("C-c h M-s" . helm-do-ag-buffers))
    :init
    (bind-key ",hs" #'helm-do-ag-this-file evil-normal-state-map)
    (setq helm-ag-insert-at-point 'symbol  ; symbol at point as default pattern
          ;; helm-ag-source-type 'file-line
          helm-ag-fuzzy-match t)))

(use-package desktop                    ; Save buffers, windows and frames
  :defer t
  :init
  (setq desktop-auto-save-timeout 60
        desktop-dirname (expand-file-name "desktop" my-dir))
  (desktop-save-mode t)
  :config
  (my-append-to-list 'desktop-not-to-save '(magit-mode git-commit-mode)))


(use-package expand-region               ; Expand functions block at a time
  :bind ("C-=" . er/expand-region))


(use-package savehist                   ; Save command history
  :defer t
  :init
  (setq savehist-file (expand-file-name "savehist" my-dir)
        history-delete-duplicates t
        savehist-save-minibuffer-history t)
  (savehist-mode t))


(use-package saveplace
  :init
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "places" my-dir)))


(use-package projectile                 ; Project-based navigation
  :init
  (setq
   projectile-globally-ignored-files '("TAGS" "*.odt" "*.docx" "*.doc")
   projectile-indexing-method 'alien    ; use faster OS methods
   ;; don't clutter my .emacs.d please
   projectile-cache-file (expand-file-name "projectile.cache" my-dir)
   projectile-known-projects-file (expand-file-name
                                   "projectile-known.eld" my-dir)
   ;; pretty Greek symbols
   projectile-mode-line '(:eval (format " π:%s" (projectile-project-name))))
  (projectile-global-mode t)

  :config
  (dolist (state '(normal visual motion))
    (evil-define-key state projectile-mode-map
      ",p" #'projectile-find-file-dwim
      ",P" #'projectile-switch-project))

  (use-package helm-projectile
    :init
    (setq projectile-completion-system 'helm
          projectile-switch-project-action #'helm-projectile
          helm-projectile-fuzzy-match t)
    (helm-projectile-on)
    :config
    (dolist (state '(normal visual motion))
      (evil-define-key state projectile-mode-map
        ",p" #'helm-projectile
        ",P" #'helm-projectile-switch-project))))

;;; ** help


(use-package discover-my-major    ; List current major mode bindings
  :bind ("<help> m" . discover-my-major))


(use-package guide-key                  ; Delayed completion for possible keys
  :delight guide-key-mode
  :init
  (setq guide-key/recursive-key-sequence-flag t
        guide-key/guide-key-sequence '("C-x" "C-c" ","))
  (guide-key-mode t))


(use-package help-mode
  :ensure nil :defer t
  :config
  (use-package help+)
  (use-package help-fns+ :bind ("<help> M-m" . describe-mode))
  (bind-keys :map help-mode-map
             ("H" . help-go-back)
             ("L" . help-go-forward)))

;;; ** editing

(use-package adaptive-wrap)             ; Choose wrapping mode intelligently
(use-package flycheck                   ; On-the-fly syntax checking
  :defer t
  :init
  (setq flycheck-mode-line
        '(:eval (replace-regexp-in-string
                 "FlyC" "Φ" (flycheck-mode-line-status-text)))
        flycheck-disabled-checkers '(emacs-lisp-checkdoc)
        flycheck-indication-mode 'right-fringe)
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (add-hook 'sh-mode-hook (lambda() (flycheck-mode -1)))

  :config
  (evil-define-key 'normal flycheck-mode-map
    ",!j" #'flycheck-next-error
    ",!k" #'flycheck-previous-error)

  (use-package helm-flycheck
    :init
    (bind-key "C-c ! h" #'helm-flycheck flycheck-mode-map)
    (evil-define-key 'normal flycheck-mode-map ",!h" #'helm-flycheck))

  (use-package flycheck-tip             ; display errors by popup
    :config (flycheck-tip-use-timer 'verbose)))


(use-package lorem-ipsum            ; Insert filler text
  :defer t
  :commands lorem-ipsum-insert-paragraphs
  :init
  ;; double spaces are still dumb
  (unless sentence-end-double-space (setq lorem-ipsum-sentence-separator " "))
  ;; overwrite default entries
  (add-hook 'sgml-mode-hook (lambda ()
                              (setq lorem-ipsum-paragraph-separator "/n<p>"
                                    lorem-ipsum-sentence-separator " "))))


(use-package writegood-mode    ; Highlight poor forms in writing
  :defer t
  :delight writegood-mode
  :init (add-hook 'text-mode-hook #'writegood-mode)
  :config (my-append-to-list 'writegood-weasel-words
                             '("thing" "different" "probably" "really")))


(use-package abbrev    ; Auto-correct words after typing
  :ensure nil
  :delight abbrev-mode
  :init
  (setq abbrev-file-name (expand-file-name "abbrevs.el" my-dir)
        save-abbrevs 'silently          ; save abbrevs when saving file
        abbrev-all-caps t)              ; expand in all-caps if written in caps
  (abbrev-mode t))


(use-package auto-indent-mode           ; Automatic indentation
  ;; TODO get this working with indenting pasted code ;;      probably has something to do with Evil command hijacking
  :commands auto-indent-global-mode
  :delight auto-indent-mode
  :init (auto-indent-global-mode t))


(use-package company                    ; Autocompletion in code
  :defer t
  :init
  (setq company-idle-delay 0            ; attempt completion immediately
        company-show-numbers t          ; allow M-num selection
        company-tooltip-align-annonation t
        company-lighter-base "ψ"
        company-selection-wrap-around t)
  (add-hook 'prog-mode-hook #'company-mode)
  :config
  (bind-keys :map company-active-map
             ("M-j" . company-select-next)
             ("M-k" . company-select-previous)
             ("M-d" . company-show-doc-buffer))

  (use-package helm-company
    :defer t
    :init (bind-keys :map (company-mode-map company-active-map)
                     ("C-:" . helm-company)))

  (use-package company-web-html
    :ensure nil)

  (use-package company-statistics       ; Sort candidates by statistics
    :init (company-statistics-mode)))


(use-package ispell
  :defer t
  :init
  (setq
   ispell-dictionary "british-ize"
   ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB")
   ;; no messages please
   ispell-silently-savep t
   ispell-quietly t)

  :config
  ;; regions not to spell check
  (my-append-to-list 'ispell-skip-region-alist
                     '((":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:")
                       ("#\\+BEGIN_SRC" . "#\\+END_SRC")
                       ("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE")))

  ;; Adapted from http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
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

  (use-package flyspell    ; On-the-fly spell checking
    :defer t
    :delight flyspell-mode " σ"
    :init
    ;; no messages
    (setq flyspell-issue-welcome-flag nil
          flyspell-issue-message-flag nil)

    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)
    :config
    (advice-add #'flyspell-auto-correct-word :around #'my-ispell-run-together)

    (use-package flyspell-lazy    ; Lazier checking for words
      :defer t
      :init
      (add-hook 'flyspell-mode #'flyspell-lazy-mode)
      (add-hook 'flyspell-prog-mode #'flyspell-lazy-mode))

    (use-package helm-flyspell
      :init (evil-define-key 'normal flyspell-mode-map
              "z=" 'helm-flyspell-correct))))


(use-package outline                    ; Hierarchical outlining support
  :ensure nil
  :delight outline-minor-mode
  :init

  (defun my-outline-fontify-headlines ()
    "Add font-lock-keywords for each outline level face and re-fontify buffer."

    (let ((level-regexp "\\\\{\\(1,8\\)\\\\}")
          (faces '(outline-1 outline-2 outline-3 outline-4
                             outline-5 outline-6 outline-7 outline-8)))
      (when (string-match level-regexp outline-regexp)
        (font-lock-add-keywords
         nil
         (mapcar
          (lambda (face)
            `(,(concat
                (replace-regexp-in-string
                 level-regexp (substring (symbol-name face) -1)
                 outline-regexp nil nil 1)
                " \\(.+\\)$")
              1 (quote ,face) t))
          faces))))
    (font-lock-fontify-buffer))
  (add-hook 'outline-minor-mode-hook #'my-outline-fontify-headlines)

  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)

  :config
  (bind-key "C-c o" #'outline-insert-heading outline-minor-mode-map)
  (evil-define-key 'normal outline-minor-mode-map
    "gh" #'outline-up-heading
    "gj" #'outline-next-heading
    "gk" #'outline-previous-heading
    "gl" #'outline-forward-same-level
    "<" #'outline-promote
    ">" #'outline-demote)

  (use-package outline-magic
    :defer t
    :init
    (evil-define-key 'normal outline-minor-mode-map
      "\t" #'outline-cycle)))


(use-package smartparens-config         ; Balanced paren management
  :ensure smartparens :defer t
  :delight smartparens-strict-mode "⎶"
  :init
  (delight #'smartparens-mode)
  (setq sp-show-pair-from-inside t)     ; highlight pair when point on bracket
  ;; disable lesser versions to avoid doubling
  (add-hook 'smartparens-enabled-hook (lambda() (electric-pair-mode -1)))
  (add-hook 'show-smartparens-mode-hook (lambda() (show-paren-mode -1)))
  ;; enable the modes
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook #'smartparens-strict-mode)
  :config
  (bind-key "M-=" #'sp-indent-defun smartparens-mode-map)
  (sp-local-pair 'html-mode "<" ">")

  (use-package evil-smartparens         ; Evil smartparen bindings
    :delight evil-smartparens-mode "ε"
    :init
    (add-hook 'prog-mode-hook #'smartparens-strict-mode)
    (add-hook 'smartparens-strict-mode-hook #'evil-smartparens-mode)))


(use-package typo    ; Insert typographical characters
  :defer t
  :delight typo-mode
  :init (add-hook 'text-mode-hook #'typo-mode))


(use-package undo-tree                  ; Branching undo tree
  :delight undo-tree-mode
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-history" my-dir))))
  (global-undo-tree-mode t)
  :config (bind-key "C-x u" #'undo-tree-visualize undo-tree-map))


(use-package yasnippet
  :defer t
  :init (setq yas-snippet-dirs
              `(,(expand-file-name "snippets/" my-dir)
                yas-installed-snippets-dir)))


(use-package autorevert                 ; Auto revert to external modifications
  :delight autorevert-mode
  :init
  (setq global-auto-revert-non-file-buffers t) ; auto refresh buffer
  (global-auto-revert-mode t))


(use-package pandoc-mode                ; Markup conversion tool
  :delight pandoc-mode '(:eval (concat " Π:" (pandoc--get 'write)))
  :bind ("C-c C-p" . pandoc-mode))


(use-package real-auto-save    ; Auto save buffers
  :delight real-auto-save-mode " α"
  :commands real-auto-save-mode
  :init (add-hook #'after-save-hook #'real-auto-save-mode))


(use-package recentf                    ; List recent files
  :defer t
  :init
  (setq
   recentf-max-saved-items 300          ; increase history size
   recentf-auto-cleanup 600             ; cleanup files after 10 minutes
   recentf-exclude '("COMMIT_EDITMSG")
   recentf-save-file (expand-file-name "recentf" my-dir))
  (recentf-mode t))

;;; ** applications

(use-package paradox
  :init
  (setq paradox-execute-asynchronously t)
  (use-package async))


(use-package calendar
  :defer t
  :init (setq calendar-date-style 'european))


(use-package dired                      ; Emacs file browser
  :ensure nil
  :bind (("C-x d" . dired-jump)
         ("C-x C-d" . list-directory))
  :init
  (setq
   dired-auto-revert-buffer t          ; auto revert dired buffer on visit
   dired-backup-overwrite 'always      ; always make backup before overwrite
   dired-dwim-target t                 ; target other dired directory if exists
   dired-isearch-filenames 'dwim       ; isearch filenames if point on filename
   dired-no-confirm '(copy move symlink) ; don't confirm these operations
   dired-recursive-copies 'always        ; recursive copy by default
   dired-recursive-deletes 'top          ; confirm recursive delete
   dired-listing-switches "-lhaF")       ; human-readable listing
  (add-to-list 'evil-emacs-state-modes 'dired-mode) ; Evil initial state
  :config

  (use-package dired+                   ; Dired extensions and syntax highlight
    :init
    (setq diredp-dwim-any-frame-flag t  ; allow dwim target other frame
          dired-omit-verbose nil)
    (add-hook 'dired-mode-hook #'dired-omit-mode)  ; omit uninteresting files
    :config
    ;; https://github.com/lunaryorn/.emacs.d/blob/master/init.el
    ;; Hack for diminishing mode lighter. Can't just use `:delight' because
    ;; lighter isn't there yet after `dired-omit-mode' is loaded.
    ;; (advice-add #'dired-omit-startup :after
    ;;             (lambda () (delight 'dired-omit-mode)))
    )

  (bind-keys :map dired-mode-map
             ("q" . kill-buffer)
             ("C-M-u" . dired-up-directory)
             ("C-w" . wdired-change-to-wdired-mode)))


(use-package doc-view    ; In-buffer document viewer
  :ensure nil :defer t
  :init (setq doc-view-continuous t)
  :config
  (bind-keys :map doc-view-mode-map
             ("j" . doc-view-next-line-or-next-page)
             ("k" . doc-view-previous-line-or-previous-page)
             ("q" . kill-this-buffer)))


(use-package ediff
  :defer t
  :init (setq ediff-diff-options "-w")  ; ignore whitespace
  :config
  (evil-define-key 'normal ediff-mode
    "j" #'ediff-next-difference
    "k" #'ediff-previous-difference))


(use-package garak :disabled t   ; ELIM messenger front-end
  :enabled nil :load-path (concat my-dir-packages "elim"))


(use-package comint
  :ensure nil :defer t
  :init (setq
         comint-completion-addsuffix t  ; add space/slash after file completion
         comint-input-ignoredups t      ; ignore duplicates in command history
         comint-scroll-to-bottom-on-input t
         comint-completion-autolist t)
  :config

  (defun my-comint-evil-insert ()
    "Enter insert state after the process mark."
    (interactive)
    (comint-goto-process-mark)
    (evil-append 1))

  (evil-define-key 'normal comint-mode-map
    "I" #'my-comint-evil-insert
    "A" #'my-comint-evil-insert)

  (setenv "PAGER" "cat")

  (defun my-comint-suppress-message (orig-func &rest args)
    "Suppress 'History item:' messages."
    (let ((old-message (symbol-function 'message)))
      (unwind-protect
          (progn (fset 'message 'ignore)
                 (apply orig-func args))
        (fset 'message old-message))))

  (advice-add #'comint-previous-matching-input
              :around #'my-comint-suppress-message))


(use-package eshell                     ; Emacs shell
  :bind (("<f12>" . eshell))
  :functions my-eshell-prompt
  :defines (eshell-cmpl-ignore-case
            eshell-highlight-prompt
            eshell-banner-message
            eshell-directory-name
            eshell-prompt-regexp
            eshell-prompt-function)
  :init (setq
         ;; eshell-prompt-regexp "^[^#$\n]* [#$] "
         eshell-buffer-shorthand t      ; buffer shorthand: echo foo > #'buffer
         eshell-highlight-prompt nil
         ;; eshell-prompt-function #'my-eshell-prompt
         eshell-directory-name (expand-file-name my-dir "eshell/")
         eshell-cmpl-ignore-case t
         eshell-banner-message (propertize (shell-command-to-string "fortune")
                                           'face '(:foreground "#b58900")))
  :config
  (add-to-list 'eshell-modules-list 'eshell-smart)

  (use-package helm-eshell
    :ensure nil
    :init (bind-key "<C-return>" #'helm-eshell-history eshell-mode-map))

  (use-package eshell-prompt-extras
    :init
    (use-package virtualenvwrapper      ; Show Python venv info in prompt
      :config (venv-initialize-interactive-shells))
    (setq
     eshell-highlight-prompt nil
     epe-git-dirty-char "δ"
     epe-git-untracked-char "θ"
     epe-git-detached-HEAD-char "Η")
    :config
    (setq eshell-prompt-function #'epe-theme-geoffgarside)))


(use-package malyon
  :ensure nil :defer t); Z-machine text-based-adventure reader


(use-package magit                      ; Git version control management
  :delight magit-auto-revert-mode
  :bind ("<f10>" . magit-status)
  :init (setq
         magit-save-some-buffers 'dontask           ; don't ask before saving
         magit-last-seen-setup-instructions "1.4.0" ; clear startup message
         magit-diff-options '("-b"))    ; ignore whitespace in diffs
  :config

  ;; http://writequit.org/org/settings.html
  (defun my-git-browse-url ()
    "Browse to the project's github URL, if available"
    (interactive)
    (let ((url (with-temp-buffer
                 (unless (zerop (call-process-shell-command
                                 "git remote -v" nil t))
                   (error "Failed: 'git remote -v'"))
                 (goto-char (point-min))
                 (when (re-search-forward
                        "github\\.com[:/]\\(.+?\\)\\.git" nil t)
                   (format "https://github.com/%s" (match-string 1))))))
      (unless url
        (error "Can't find repository URL"))
      (browse-url url)))

  (use-package git-wip-mode :disabled t)          ; TODO what does this do

  (bind-keys :map magit-status-mode-map
             ("SPC" . helm-M-x)
             ("<C-tab>" . magit-section-cycle)
             ("j" . next-line)
             ("k" . previous-line)
             ("<down>" . magit-goto-next-sibling-section)
             ("<up>" . magit-goto-previous-sibling-section)
             ("K" . magit-discard-item)
             (",b" . ibuffer))

  (use-package git-timemachine
    :bind (("C-<f10>" . git-timemachine)
           ("S-<f10>" . git-timemachine-toggle))))


;;; ** languages

;; various unconfigured language packages
(use-package bbcode-mode :defer t)
(use-package lua-mode :defer t)
(use-package gitignore-mode :defer t)
(use-package markdown-mode :defer t)
(use-package vimrc-mode :mode "[._]?pentadactylrc$" "\\.penta$" :defer t)


(use-package git-commit-mode            ; TODO Git commit messages
  :defer t
  :delight server-buffer-clients
  :config
  (evil-define-key 'normal git-commit-mode-map
    ",w" #'git-commit-commit
    "q" #'git-commit-abort))


(use-package org
  :delight org-indent-mode
  :defines (org-export-in-background
            org-clock-persist
            org-clock-in-resume
            org-odt-preferred-output-format)
  :init
  (setq
   org-edit-src-content-indentation 0   ; no initial indent for source code
   org-src-preserve-indentation t       ; preserve source block indentation
   org-src-strip-leading-and-trailing-blank-lines t
   org-clock-persist t                  ; save clock and clock history on exit
   org-clock-in-resume t                ; resume if clocking in with open clock
   org-cycle-separator-lines 1          ; let one line be a separator
   org-adapt-indentation nil            ; don't adapt indentation
   org-imenu-depth 3                    ; larger imenu depth
   org-special-ctrl-a/e t               ; begin/end of line skips tags & stars
   org-special-ctrl-k t                 ; kill lines depending on org context
   org-return-follows-link t            ; follow links with RET
   org-catch-invisible-edits 'smart     ; smart editing of hidden things
   org-todo-keywords '((sequence "☐" "☒"))
   org-modules '(org-docview org-info org-gnus org-inlinetask)
   org-export-backends '(ascii html odt taskjuggler)
   org-odt-preferred-output-format 'doc
   org-startup-folded t                 ; start buffers with folded headers
   org-src-fontify-natively t           ; syntax highlight code in org buffer
   org-list-allow-alphabetical t)       ; allow single-char alphabetical lists

  :config
  (set-face-attribute 'org-table nil :family (my-font 'mono))
  (set-face-attribute 'org-todo nil
                      :height 'unspecified
                      :inherit 'variable-pitch)
  (set-face-attribute 'org-done nil :inherit 'org-todo)

  (bind-keys :map org-mode-map
             ("C-1" . org-clock-in)
             ("C-2" . org-clock-out)
             ("S-<return>" . org-insert-heading-after-current))
  (evil-define-key 'normal org-mode-map "RET" #'org-insert-heading)

  (use-package org-plus-contrib
    :config
    (use-package ox-taskjuggler
      :ensure nil
      :init (setq
             org-taskjuggler-default-reports "\nmacro TaskTip [\n  tooltip istask() -8<-\n    '''Start: ''' <-query attribute='start'->\n    '''End: ''' <-query attribute='end'->\n    '''Precursors: '''\n    <-query attribute='precursors'->\n\n    '''Followers: '''\n    <-query attribute='followers'->\n    ->8-\n]\n\ntextreport report \"Plan\" {\n  formats html\n  center -8<-\n    <[report id=\"plan\"]>\n  ->8-\n}\n\ntaskreport plan \"\" {\n  headline \"Project Plan\"\n  columns name, start, end, effort, chart { scale day width 1500 ${TaskTip} }\n}"))
    :config
    (defun my-org-tj-add-project-options (project)
      "Add various properties to the taskjuggler PROJECT declaration."
      (concat
       (string-join
        `(;; declaration header excluding ending bracket and newline
          ,(substring project 0 -2)
          ;; properties to add
          "timeformat \"%H-%d\""
          "timingresolution 15 min")
        "\n  ")
       "\n}\n"))
    (advice-add #'org-taskjuggler--build-project
                :filter-return #'my-org-tj-add-project-options))

  (use-package evil-org                 ; Evil org-mode bindings
    :delight evil-org-mode
    :init (add-hook 'org-mode-hook #'evil-org-mode)
    :config
    (evil-define-key 'normal evil-org-mode-map
      "\t" #'org-back-to-heading))

  (defun org-dblock-write:git-log-view (params)
    "Display a git commit log according to PARAMS."
    (let ((repo (expand-file-name
                 ".git" (or (plist-get params :repo)
                            (file-name-directory (buffer-file-name)))))
          (format (or (format "--format='%s'" (plist-get params :format)) ""))
          (trunc-p (plist-get params :trunc))
          (args (or (string-join (plist-get params :args) " ") "")))
      (let ((output (shell-command-to-string
                     (format "git --git-dir='%s' log %s %s" repo format args))))
        (when trunc-p
          (setq output (replace-regexp-in-string "\\.\\." "" output)))
        (insert output)))))


(use-package generic-x                  ; Collection of generic modes
  :ensure nil :defer t
  :config
  (my-append-to-list 'generic-extras-enable-list generic-mswindows-modes)

  (use-package conkyrc-mode :disabled t   ; System monitor setup language
    :load-path "~/.emacs.d/elisp/" :ensure nil))


(use-package elisp-mode
  :defer t :ensure nil
  :config
  (bind-key "C-c C-c" #'eval-defun emacs-lisp-mode-map)

  (defun my-imenu-decls ()
    "Add custom declarations to `imenu-generic-expression'."
    (setq imenu-generic-expression
          (append imenu-generic-expression
                  `(("Packages" "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)
                    (nil ,(concat outline-regexp "\\(.+\\)$") 1)))))
  (add-hook 'emacs-lisp-mode-hook #'my-imenu-decls)

  (use-package eldoc                      ; Documentation in echo area
    :defer t
    :delight eldoc-mode
    :init
    (setq eldoc-idle-delay 0.3)
    (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))

  (use-package highlight-quoted  ; Faces for lisp quotes and quoted symbols
    :defer t
    :init (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))

  (use-package elisp-slime-nav            ; Navigate elisp documentation
    :defer t
    :delight elisp-slime-nav-mode
    :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
    :config
    (evil-define-key 'normal elisp-slime-nav-mode-map
      "K" #'elisp-slime-nav-describe-elisp-thing-at-point
      "gd" #'elisp-slime-nav-find-elisp-thing-at-point)))


(use-package python
  :defer t
  :init (setq python-shell-interpreter "python3") ; use Python 3
  :config

  (use-package elpy
    :defer t
    :init (elpy-enable)))


(use-package sh-script
  :defer t
  :init (setq sh-indentation 2
              sh-basic-offset 2))


(use-package vbnet-mode
  :defer t :ensure nil
  ;; use sensible faces instead of package-defined ones
  :defines (vbnet-funcall-face vbnet-namespace-face)
  :init (setq vbnet-funcall-face 'font-lock-function-name-face
              vbnet-namespace-face 'font-lock-preprocessor-face))


(use-package js2-mode :disabled t
  :defer t
  :mode "\\.js$"
  :init
  (defalias 'javascript-generic-mode #'js2-mode)
  (setq-default js-indent-level 2))


(use-package css-mode
  :defer t
  :mode "\\.s?css$"
  :config
  (defvar my-sass-output-dir "../"
    "Directory to store compiled sass files.")

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
  :defer t
  :mode "\\.html?$"
  :init (setq
         web-mode-enable-block-face t
         web-mode-enable-part-face t
         web-mode-enable-comment-keywords t
         web-mode-enable-current-element-highlight t
         web-mode-enable-current-column-highlight t
         web-mode-css-indent-offset 2
         web-mode-code-indent-offset 2
         web-mode-markup-indent-offset 2)
  :config (add-hook 'web-mode-hook #'rainbow-mode))

