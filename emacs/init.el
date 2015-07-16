;;; init.el --- Kelly Stewart's init file
;;; Commentary:
;; TODO silence byte-compiler
;; REVIEW :init and :config keyword reorder
;; REVIEW add :defer and :demand keywords correctly
;; REVIEW fix evil bindings
;;; Code:
;;;; helper functions

;; Adapted from http://stackoverflow.com/a/24357106/3912814
(defun my-append (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR if not already in list.

Return new value of LIST-VAR."
  (if (and (boundp list-var) (symbol-value list-var))
      (dolist (item elements) (add-to-list list-var item))
    (set list-var elements))
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

;;;; setup
(defconst my-dir (expand-file-name  ".user/" user-emacs-directory))
(defconst my-dir-elisp (expand-file-name "my-elisp" user-emacs-directory))
(defconst my-dir-packages (expand-file-name "elisp/" user-emacs-directory))

(add-to-list 'load-path my-dir-packages) ; location for non-MELPA packages
(add-to-list 'load-path my-dir-elisp)    ; location for personal elisp files

;; load custom file
(setq custom-file (expand-file-name "custom.el" my-dir))
(load custom-file 'no-error 'no-message)

;;;;; package
(require 'package)
(setq package-enable-at-startup nil     ; we will manually initialize
      load-prefer-newer t)              ; don't load outdated byte code

(my-append 'package-archives
           '(("melpa" . "http://melpa.milkbox.net/packages/")
             ("org" . "http://orgmode.org/elpa/")
             ("elpy" . "http://jorgenschaefer.github.io/packages/")))

(package-initialize)                    ; manually initialize


;;;;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(setq use-package-verbose t             ; log message after loading a package
      use-package-always-ensure t)      ; ensure all packages are installed

(use-package delight)
(use-package bind-key)

;;;; basic settings
;;;;; general
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

(fset #'yes-or-no-p #'y-or-n-p)

;; remove startup messages
(setq inhibit-startup-screen t
      initial-scratch-message nil)
(fset #'display-startup-echo-area-message #'ignore)

;; never kill *scratch*
(defun my-bury-scratch ()
  "Bury the scratch buffer instead of killing it."
  (if (equal (buffer-name) "*scratch*") (progn (bury-buffer) nil) t))
(add-to-list 'kill-buffer-query-functions #'my-bury-scratch)

;; auto close compilation buffer on success
(defun my-compilation-close-on-success (buf result)
  "Close window if compilation did not exit abnormally."
  (let ((win (get-buffer-window buf 'visible)))
    (when (and win (not (string-match ".*exited abnormally.*" result)))
      (delete-window win))))
(add-to-list 'compilation-finish-functions #'my-compilation-close-if-success)

;; toggle line comment
(defun my-comment-dwim-line (&rest args)
  "Advice for `comment-dwim'. Toggle current line comment if no active region."
  (when (and (not (region-active-p))
             (not (or (eq (point) (line-end-position))
                      ;; compensate for evil-mode cursor being one point ahead
                      (eq (point) (- (line-end-position) 1)))))
    (comment-or-uncomment-region
     (line-beginning-position) (line-end-position))))
(advice-add #'comment-dwim :before-until #'my-comment-dwim-line)

;; make `upcase-word' `downcase-word' and `capitalize-word' act on whole word
(dolist (func '(upcase-word downcase-word capitalize-word))
  (advice-add func :before
              (lambda (arg) (unless (looking-back "\\b") (backward-word)))))

;;;;; modes and hooks

(mouse-wheel-mode t)                    ; Mouse wheel enabled
(show-paren-mode t)                     ; Highlight matching parens
(electric-indent-mode t)                ; Auto indent
(electric-pair-mode t)                  ; Auto add parens
(auto-insert-mode t)                    ; Auto insert text based on filetype
;; we don't need no stinkin GUI
(size-indication-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(unless (display-graphic-p) (menu-bar-mode -1)) ; no menu bar in terminal

(dolist (func '(hl-line-mode            ; Highlight current line
                prettify-symbols-mode   ; Replace words with symbols
                auto-fill-mode          ; Automatically fill text past fill-col
                goto-address-prog-mode  ; Buttonize URLs in comments and string
                ;; numbers in modelines
                column-number-mode
                line-number-mode))
  (add-hook 'prog-mode-hook func))

(add-hook 'text-mode-hook #'goto-address-mode) ; Buttonize URLs

;;;;; delighted modes

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

;;;;; fonts
;; DONE? check this works properly on Windows
(require 'dash)

(defvar my-fonts
  '((proportional
     . ("Fira Sans" "FiraSans"
        "Input Sans Condensed" "InputSansCondensed" "Input Sans" "InputSans"
        "DejaVu Sans" "Calibri" "Arial" "Sans Serif"))
    (mono
     . ("Input Mono Condensed" "InputMonoCondensed" "Input Mono" "InputMono"
        "DejaVu Sans Mono" "Consolas" "Courier" "Monospace" "Fixed")))
  "Alist of font types paired with an ordered list of preferences.
Used with `my-font' to get the first valid entry of each font pairing.")

(add-to-list 'my-fonts
             `(header . ,(append '("Fantasque Sans Mono" "FantasqueSansMono")
                                  (cdr (assoc 'proportional my-fonts)))))

(defun my-font (font)
  "Return the first valid entry for FONT in `my-fonts'."
  (--first (find-font (font-spec :name it)) (cdr (assoc font my-fonts))))

(defun my-font-use-proportional ()
  "Set current buffer's font to proportional.'"
  (interactive)
  (face-remap-add-relative
   'default :family (my-font 'proportional) :height 100))
(add-hook 'text-mode-hook #'my-font-use-proportional)

(set-frame-font (concat (my-font 'mono) "-8"))
(set-face-attribute 'variable-pitch nil :family (my-font 'header))

;;;; bindings

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

(global-unset-key (kbd "C-x ESC ESC"))

(bind-keys
 ("S-SPC" . cycle-spacing)              ; M-SPC is taken in Linux Mint
 ("RET" . newline-and-indent)           ; indent new lines automatically
 ("C-x C-M-x" . revert-buffer)
 ("C-x n" . my-narrow-or-widen-dwim)
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


;;;;;; windows super key
(when (eq system-type 'windows-nt)
  (setq w32-apps-modifier 'hyper
        ;; Super is Windows key
        ;; Taken mappings: s-l s-r
        w32-lwindow-modifier 'super
        w32-pass-lwindow-to-system nil))

;;;; major packages

(use-package evil                       ; Vim keybindings and modal editing
  :demand t
  :init (evil-mode t)
  :config
  (setq
   evil-want-fine-undo nil              ; undo insertions in single steps
   evil-want-C-w-in-emacs-state t       ; prefix window commands with C-w
   evil-want-change-word-to-end nil     ; don't let cw behave like ce
   evil-echo-state nil                  ; state is in the modeline anyway
   evil-ex-substitute-global t)         ; global substitutions by default
  (my-append 'evil-emacs-state-modes
             '(shell-mode term-mode multi-term-mode))
  (my-append 'evil-insert-state-modes '(git-commit-mode))

  (defmacro evil-bind-keys (state map &rest bindings)
    "Bind BINDINGS to evil STATE and keymap MAP.

STATE can be either a symbol specifying an evil state or a list
of symbols. MAP can be a keymap or a list of keymaps.

The rest of the arguments are conses of keybinding string, which
will be passed to `read-kbd-macro' if necessary, and an unquoted
function symbol."
    (let* ((maps (if (listp map) map (list map)))
           (state (car (cdr state)))
           (states (if (listp state) state (list state)))
           (binds (apply
                   #'nconc
                   (mapcar
                    (lambda (k)
                      (let ((key (car k)))
                        ;; pass to `read-kbd-macro' if necessary
                        `(,(if (vectorp key) key (read-kbd-macro key))
                          (quote ,(cdr k)))))
                    bindings))))

      (macroexp-progn
       (mapcar
        (lambda (item)
          `(evil-define-key (quote ,(car item)) ,(cdr item) ,@binds))
        ;; build a list of (state . map) for each `evil-define-key' command
        (apply
         #'nconc
         (mapcar
          (lambda (s) (mapcar (lambda (m) (cons s m)) maps)) states))))))

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
         (t (setq unread-command-events
                  (append unread-command-events (list evt))))))))

  (evil-define-command my-evil-yank-to-eol ()
    "Call `evil-yank' with point to end of line."
    (evil-yank (point) (point-at-eol)))

  (bind-keys :map (evil-insert-state-map evil-replace-state-map)
             ("j" . maybe-exit))        ; jk exits insert state

  (bind-key "y" #'evil-yank evil-motion-state-map) ; Add yanking to motion map

  (bind-keys :map evil-window-map
             ("d" . delete-window)
             ("D" . delete-other-windows))

  (bind-key "C-S-SPC" #'evil-ex)

  (bind-keys
   :map (evil-normal-state-map evil-motion-state-map evil-visual-state-map)
   ("SPC" . execute-extended-command)
   ("<escape>" . keyboard-quit)
   (";" . comment-dwim)
   (":" . evil-repeat-find-char)
   ("Y" . my-evil-yank-to-eol)          ; more consistent
   ("q" . kill-buffer-and-window)
   ("Q" . evil-record-macro)            ; Q replaces old q action
   ;; movement
   ("j" . evil-next-visual-line)
   ("k" . evil-previous-visual-line)
   ("\"" . evil-jump-item)
   ;; REVIEW try removing this and using other motions to get around instead
   ;; a-s is more mnemonic but on my keyboard a is first so fingers get
   ;; confused any other way
   ;; ("s" . evil-last-non-blank)
   ;; ("a" . evil-first-non-blank)
   ;; leader bindings
   ("," . nil)
   (",f" . find-file)
   (",b" . list-buffers)
   (",w" . save-buffer))

  (use-package evil-surround            ; Operators for surrounding elements
    :defer t
    :init (global-evil-surround-mode t))

  (use-package evil-matchit             ; Manipulate tags
    :defines evilmi-may-jump-percentage
    :init (global-evil-matchit-mode t)
    :config
    (setq evilmi-may-jump-percentage nil) ; allow count usage
    (evil-bind-keys 'normal evil-matchit-mode-map
      ("\"" . evilmi-jump-items))))

(use-package evil-commentary
    :init (evil-commentary-mode t)
    :config
    (evil-bind-keys 'normal evil-commentary-mode-map
                    (":" . evil-commentary)))
(use-package helm                       ; TODO Fuzzy minibuffer completion
  :demand t
  :delight helm-mode
  :init (helm-mode t)
  :functions my-helm-imenu-transformer
  :config
  (setq
   helm-quick-update t
   helm-command-prefix-key "C-c h"
   helm-move-to-line-cycle-in-source t     ; cycle on buffer end
   helm-display-header-line nil            ; no header line
   helm-scroll-amount 5                    ; scroll amount in other window
   helm-split-window-in-side-p t           ; split inside current window
   helm-M-x-always-save-history t          ; save history even on fail
   helm-ff-auto-update-initial-value t     ; auto update when only one match
   helm-ff-file-name-history-use-recentf t ; use recentf
   helm-ff-search-library-in-sexp t        ; get library from functions
   helm-ff-skip-boring-files t             ; skip irrelevant files
   helm-findutils-search-full-path t       ; search in full path with shell
   helm-findutils-skip-boring-files t      ; skip irrelevant files in shell
   ;; fuzzy matching everywhere
   helm-semantic-fuzzy-match t
   helm-completion-in-region-fuzzy-match t
   helm-buffers-fuzzy-matching t
   helm-M-x-fuzzy-match t
   helm-apropos-fuzzy-match t
   helm-lisp-fuzzy-completion t
   helm-imenu-fuzzy-match t
   helm-file-cache-fuzzy-match t
   helm-recentf-fuzzy-match t)

  (defalias #'ibuffer #'helm-mini)
  (helm-autoresize-mode t)

  ;; load packages manually to avoid byte-compiler warnings
  (use-package helm-config :ensure nil)
  (use-package helm-files :ensure nil)
  (use-package helm-mode :ensure nil)
  (use-package helm-semantic :ensure nil)
  (use-package helm-buffers :ensure nil)
  (use-package helm-command :ensure nil)
  (use-package helm-elisp :ensure nil)
  (use-package helm-imenu :ensure nil)

  (defun my-helm-imenu-transformer (candidates)
    "Custom imenu transformer with added headings and faces."
    (cl-loop for (k . v) in candidates
             for types = (or (helm-imenu--get-prop k) (list "Function" k))
             collect
             (cons (mapconcat (lambda (x)
                                (propertize
                                 x 'face (cond ((string= x "Variables")
                                                'font-lock-variable-name-face)
                                               ((string= x "Function")
                                                'font-lock-function-name-face)
                                               ((string= x "Types")
                                                'font-lock-type-face)
                                               ((string= x "Packages")
                                                'font-lock-doc-face)
                                               ((string= x "Headings")
                                                'font-lock-keyword-face))))
                              types helm-imenu-delimiter)
                   (cons k v))))
  (defalias #'helm-imenu-transformer #'my-helm-imenu-transformer)

  ;; less ugly colors for helm-buffer items
  (set-face-attribute 'helm-buffer-directory nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inherit 'dired-directory)
  (set-face-attribute 'helm-buffer-saved-out nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :inverse-video 'unspecified
                      :inherit 'font-lock-warning-face)

  ;; TODO helm-mini C-d delete buffer, new binding for other window
  (bind-keys*
   ([remap execute-extended-command] . helm-M-x)
   ([remap occur]. helm-occur)
   ([remap find-files] . helm-find-files)
   ([remap list-buffers]. helm-mini)
   ([remap ibuffer]. helm-mini)

   ;; help
   ([remap info-emacs-manual] . helm-info-emacs)
   ([remap locate-library] . helm-locate-library)
   ([remap manual-entry] . helm-man-woman)
   ([remap apropos] . helm-apropos)
   ([remap apropos-command] . helm-apropos)
   ([remap apropos-documentation] . helm-apropos)
   ("<help> I" . helm-info-at-point))

  (bind-keys
   ("M-/" . helm-do-grep)
   ("C-y" . helm-show-kill-ring))

  (bind-key "C-/" #'helm-semantic-or-imenu emacs-lisp-mode-map)

  (bind-keys
   :map helm-map
   ("C-M-u" . helm-scroll-other-window)
   ("C-M-d". helm-scroll-other-window-down)
   ("<tab>" . helm-execute-persistent-action) ; execute action without closing
   ("C-z" . helm-select-action))              ; list actions

  (bind-keys
   :map (evil-normal-state-map
         evil-motion-state-map
         evil-visual-state-map)
   (",hy" . helm-show-kill-ring)
   (",hs" . helm-do-grep)
   (",hu" . helm-ucs)
   (",hc" . helm-colors)
   (",H" . helm-resume))

  (use-package helm-dash                ; TODO Language documentation viewer
    :init (setq
           helm-dash-browser-func 'eww
           helm-dash-docsets-path "~/.emacs.d/.user/docset")
    ;; :config
    ;; (add-hook 'python-mode-hook
    ;;           (lambda()
    ;;             (setq-local helm-dash-docsets '("Python"))))
    )

  (use-package helm-descbinds           ; Replacement for `describe-bindings'
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

;;;; help

(use-package discover-my-major          ; List current major mode bindings
  :bind ("<help> m" . discover-my-major))


(use-package guide-key                  ; Delayed completion for possible keys
  :delight guide-key-mode
  :init (guide-key-mode t)
  :config
  (setq guide-key/recursive-key-sequence-flag t
        guide-key/guide-key-sequence '("C-x" "C-c" ",")))


(use-package help-mode
  :ensure nil :defer t
  :config
  (use-package help+)
  (use-package help-fns+ :bind ("<help> M-m" . describe-mode))
  (bind-keys :map help-mode-map
             ("H" . help-go-back)
             ("L" . help-go-forward))

  (bind-keys :map help-map
             ("k" . describe-key-briefly)
             ("K" . describe-key)
             ("C-k" . describe-bindings)
             ("M-k" . view-lossage)
             ("l" . locate-library)
             ("i" . info-lookup-symbol)
             ("C-i" . info-emacs-manual)))

;;;; appearance

;;;;;; highlight fic
;; REVIEW fixme todo highlight
(defface font-lock-fic-face
  '((((class color))
     (:inherit 'font-lock-warning-face :slant italic))
    (t (:slant italic)))
  "Face to fontify FIXME/TODO words"
  :group 'faces)

(defun my-highlight-fic ()
  "Highlight FIXME and TODO keywords."
  (font-lock-add-keywords
   nil `(("\\(TODO\\??\\|FIXME\\??\\|INPROGRESS\\|REVIEW\\)"
          1 'font-lock-fic-face prepend))))

(add-hook 'prog-mode-hook #'my-highlight-fic)

;;;;;; modeline packages
(use-package smart-mode-line            ; Better modeline
  :demand t
  :config
  (setq
   sml/theme 'respectful
   sml/battery-format "%b%p[%t]"
   sml/full-mode-string " ⋯"           ; append this to modeline when full
   sml/shorten-mode-string ""          ; no indication for all modes displayed
   sml/mule-info nil                   ; don't show buffer encoding
   sml/use-projectile-p t              ; projectile file prefix takes precedent
   sml/projectile-replacement-format "[π:%s]")
  (my-append 'sml/replacer-regexp-list
             '(("^/media/user/" ":θ:")
               (":θ:Documents/" ":Δ:")
               (":Δ:work/" ":Σ:")
               (":θ:dev/" ":δ:")
               (":δ:dotfiles/" ":.:")
               ("^:\\.:emacs/" ":.ε:")))
  (sml/setup))


(use-package nyan-mode                  ; Nyan cat scroll bar
  :defer t
  ;; TODO nyan music ☹
  :commands nyan-mode
  :init (nyan-mode t)
  :config
  (setq-default nyan-wavy-trail t) ; TODO wavy nyan trail all the time
  (defun nyan-mode-off () (nyan-mode -1)))

(use-package which-func                 ; Modeline definition name
  :defer t
  :init (which-function-mode t)
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
                       mouse-face mode-line-highlight))))


(use-package wc-goal-mode
  :defer t
  :init (add-hook 'text-mode-hook #'wc-goal-mode)
  :config
  (defun my-wc-format-toggle ()
    (interactive)
    (let ((a "wc:%tw%w") (b "lc:%tl%l"))
      (setq wc-goal-modeline-format (if (eql wc-goal-modeline-format a)
                                        b a)))))

;;;;;; face packages

(use-package hl-sentence                ; Highlight current sentence
  :defer t
  :init (add-hook 'text-mode-hook #'hl-sentence-mode)
  :config (set-face-attribute 'hl-sentence-face nil :inherit 'hl-line))


(use-package highlight-numbers          ; Highlight numbers
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))


(use-package page-break-lines           ; Horizontal lines instead of ^L
  :defer t
  :delight page-break-lines-mode
  :init (global-page-break-lines-mode t))


(use-package paren-face                 ; Faces for parens
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook #'paren-face-mode))


(use-package rainbow-mode               ; Highlight color codes
  :defer t
  :delight rainbow-mode
  :init
  (dolist (hook '(web-mode-hook css-mode-hook))
    (add-hook hook #'rainbow-mode)))


(use-package whitespace                 ; Faces for whitespace characters
  :defer t
  :delight whitespace-mode
  :init (add-hook 'prog-mode-hook #'whitespace-mode)
  :config
  (setq
   whitespace-line-column nil           ; use fill-column value
   ;; modes to disable
   whitespace-global-modes
   '(not org-mode eshell-mode shell-mode web-mode dired-mode read-only-mode)
   ;; trailing whitespace and tails of long lines
   whitespace-style '(face trailing lines-tail)
   ;; clean whitespace on write and warn if readonly
   whitespace-action '(auto-cleanup warn-if-read-only)))

;;;;;; color theme

(use-package solarized-theme :disabled t
  :defer t
  :config
  (setq solarized-scale-org-headlines nil
        solarized-height-plus-1 1.1
        solarized-height-plus-2 1.1
        solarized-height-plus-3 1.1
        solarized-height-plus-4 1.1)
  (load-theme 'solarized-dark))


(use-package zenburn-theme :disabled t
  :defer t
  :config (load-theme 'zenburn-hc))

(use-package color-theme-sanityinc-tomorrow
  :defer t
  :config (load-theme 'sanityinc-tomorrow-night))


;;;; interface

(use-package linum                      ; Line numbers
  :defer t
  :init (add-hook 'prog-mode-hook #'linum-mode))


(use-package golden-ratio :disabled t   ; Resize windows to golden ratio
  :defer t
  ;; TODO get this to play nice with Helm
  :delight golden-ratio-mode
  :init (golden-ratio-mode t)
  :config

  (setq
   golden-ratio-exclude-modes '(helm-mode
                                magit-log-mode
                                magit-reflog-mode
                                magit-status-mode)
   golden-ratio-auto-scale t))


(use-package hideshow                   ; Code folding
  :delight hideshow-minor-mode "+"
  :init
  (use-package hideshowvis :disabled t  ; Visualize hidden blocks
    :defer t
    :init
    (hideshowvis-symbols)
    (add-hook 'hs-minor-mode-hook #'hideshowvis-minor-mode))
  (add-hook 'prog-mode-hook #'hs-minor-mode)

  :config

  ;; add folding for other modes with curly bracket delimiters
  (mapc
   (lambda (mode)
     (add-to-list 'hs-special-modes-alist `(,mode "{" "}" "/[*/]" nil nil)))
   '(css-mode web-mode))

  ;; REVIEW expand collapsed blocks when jumping to them
  (defun my-hs-expand-advice (&rest _args)
    (save-excursion (if (and (boundp 'outline-minor-mode)
                             outline-minor-mode
                             (boundp 'outline-cycle))
                        ()
                        (hs-show-block))))
  (advice-add #'imenu :after #'my-hs-expand-advice)
  (advice-add #'goto-line :after #'my-hs-expand-advice)

  ;; fallback indentation-based hiding
  (defun toggle-hiding (column)
    "Toggle a block based on indentation level or defined markers."
    (interactive "P")
    (if hs-minor-mode
        (when (condition-case nil (hs-toggle-hiding) (error t))
          (hs-show-all))
      (set-selective-display
       (or column
           (unless selective-display (1+ (current-column))))))))


(use-package popwin                     ; Popup window for minor buffers
  :defer t
  :commands popwin-mode
  :init (popwin-mode t)
  :config
  (setq popwin:popup-window-position 'right)
  (my-append 'popwin:special-display-config
             '(("*Backtrace*" :noselect t)
               ("*Python Help*" :stick t :height 20)
               ("*Help*" :noselect t :height 20))))


(use-package uniquify                   ; Distinguish buffers with same name
  :ensure nil :demand t
  :config (setq uniquify-buffer-name-style 'forward
                uniquify-trailing-separator-p t))


(use-package winner                     ; Window configuration undo
  :defer t
  :bind (("s-j" . winner-undo)
         ("s-k" . winner-redo))
  :init (winner-mode t))


(use-package writeroom-mode             ; Distraction-free writing mode
  :defer t
  :delight writeroom-mode " Σ"
  :init (bind-key "<C-f11>" #'writeroom-mode text-mode-map)
  :config
  (setq writeroom-width 100)
  (defun my-writeroom-effect ()
    "Apply additional functions for distraction-free writing."
    (display-time-mode t)
    (display-battery-mode t)
    (which-function-mode -1)
    (nyan-mode -1)                     ; may not be needed with modeline format
    (setq mode-line-format
          '("%e"
            mode-line-front-space
            mode-line-frame-identification
            mode-line-buffer-identification
            mode-line-modes
            mode-line-misc-info
            mode-line-end-spaces)))
  (add-to-list 'writeroom-global-effects #'my-writeroom-effect))


(use-package visual-fill-column
  :defer t
  :init (add-hook 'text-mode-hook #'visual-fill-column-mode)
  :config (setq-default visual-fill-column-width 100))

;;;; navigation

(use-package ace-jump-mode              ; Jump to specific points with marks
  :bind ("C-SPC" . ace-jump-mode))


(use-package ace-window                 ; Quick window jumping
  :bind (("M-w" . ace-window)
         ("M-o" . ace-window))
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


(use-package ag                         ; Fast search and navigation
  :config
  (setq ag-reuse-buffers t
        ag-reuse-window t)

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
  :init (desktop-save-mode t)
  :config
  (setq desktop-auto-save-timeout 60
        desktop-dirname (expand-file-name "desktop" my-dir))
  (my-append 'desktop-modes-not-to-save '(magit-mode git-commit-mode)))


(use-package expand-region              ; Expand functions block at a time
  :bind ("C-z" . er/expand-region)
  :init
  (bind-keys :map (evil-normal-state-map
                   evil-visual-state-map)
             ("zz" . er/expand-region))
  :config (setq expand-region-contract-fast-key "x"))


(use-package savehist                   ; Save command history
  :defer t
  :init (savehist-mode t)
  :config (setq savehist-file (expand-file-name "savehist" my-dir)
                history-delete-duplicates t
                savehist-save-minibuffer-history t))


(use-package saveplace                  ; Save place in file and return to it
  :demand t
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "places" my-dir)))


(use-package projectile                 ; Project-based navigation
  :defer t
  :init (projectile-global-mode t)
  :config
  (setq
   projectile-globally-ignored-files '("TAGS" "*.odt" "*.docx" "*.doc")
   projectile-indexing-method 'alien    ; use faster OS methods
   ;; don't clutter my .emacs.d please
   projectile-cache-file (expand-file-name "projectile.cache" my-dir)
   projectile-known-projects-file (expand-file-name
                                   "projectile-known.eld" my-dir)
   ;; pretty Greek symbols
   projectile-mode-line '(:eval (format " π:%s" (projectile-project-name))))

  (evil-bind-keys '(normal visual motion) projectile-mode-map
                  (",p" . projectile-find-file-dwim)
                  (",P" . projectile-switch-project))

  (use-package helm-projectile
    :defer t
    :init (helm-projectile-on)
    :config
    (setq projectile-completion-system 'helm
          projectile-switch-project-action #'helm-projectile
          helm-projectile-fuzzy-match t)

    (evil-bind-keys '(normal visual motion) projectile-mode-map
                    (",p" . helm-projectile)
                    (",P" . helm-projectile-switch-project))))

;;;; editing

(use-package adaptive-wrap              ; Choose wrapping mode intelligently
  :defer t
  :init (adaptive-wrap-prefix-mode t))


(use-package flycheck                   ; On-the-fly syntax checking
  :defer t
  :init
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (add-hook 'sh-mode-hook (lambda() (flycheck-mode -1)))

  :config
  (setq flycheck-mode-line
        '(:eval (replace-regexp-in-string
                 "FlyC" "Φ" (flycheck-mode-line-status-text)))
        flycheck-indication-mode 'right-fringe)
  (evil-bind-keys 'normal flycheck-mode-map
    (",!j" . flycheck-next-error)
    (",!k" . flycheck-previous-error))

  (use-package helm-flycheck
    :init
    (bind-key "C-c ! h" #'helm-flycheck flycheck-mode-map)
    (evil-bind-keys 'normal flycheck-mode-map
                    (",!h" . helm-flycheck)))

  (use-package flycheck-tip             ; display errors by popup
    :config (flycheck-tip-use-timer 'verbose)))


(use-package lorem-ipsum                ; Insert filler text
  :defer t
  :init
  ;; overwrite default `sgml-mode' entries
  (add-hook 'sgml-mode-hook
            (lambda () (setq lorem-ipsum-paragraph-separator "/n<p>"
                        lorem-ipsum-sentence-separator " ")))
  :config
  ;; double spaces are still dumb
  (unless sentence-end-double-space (setq lorem-ipsum-sentence-separator " ")))


(use-package writegood-mode             ; Highlight poor forms in writing
  :defer t
  :delight writegood-mode
  :init (add-hook 'text-mode-hook #'writegood-mode)
  :config (my-append 'writegood-weasel-words
                     '("thing" "different" "probably" "really")))

(use-package abbrev                     ; Auto-correct words after typing
  :ensure nil
  :delight abbrev-mode
  :init (abbrev-mode t)
  :config
  (setq save-abbrevs 'silently          ; save abbrevs when saving file
        abbrev-all-caps t               ; expand in all-caps if written in caps
        abbrev-file-name (expand-file-name "abbrevs.el" my-dir)))


(use-package auto-indent-mode           ; Automatic indentation
  ;; TODO get this working with indenting pasted code
  ;;      probably has something to do with Evil command hijacking
  :defer t
  :commands auto-indent-global-mode
  :delight auto-indent-mode
  :init (auto-indent-global-mode t))


(use-package company                    ; Autocompletion in code
  :defer t
  :init (add-hook 'prog-mode-hook #'company-mode)
  :config
  (setq company-idle-delay 0            ; attempt completion immediately
        company-show-numbers t          ; allow M-num selection
        company-tooltip-align-annonations t
        company-lighter-base "ψ"
        company-selection-wrap-around t)

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


(use-package ispell                     ; TODO Spell-checking
  ;; TODO stop messages showing in minibuffer when starting a process
  :defer t
  :config
  (setq
   ispell-dictionary "british-ize"
   ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB")
   ;; no messages please
   ispell-silently-savep t
   ispell-quietly t)
  (my-append 'ispell-skip-region-alist
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
    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)
    :config
    ;; no messages
    (setq flyspell-issue-welcome-flag nil
          flyspell-issue-message-flag nil)
    (advice-add #'flyspell-auto-correct-word :around #'my-ispell-run-together)

    (use-package flyspell-lazy    ; Lazier checking for words
      :defer t
      :init
      (add-hook 'flyspell-mode #'flyspell-lazy-mode)
      (add-hook 'flyspell-prog-mode #'flyspell-lazy-mode))

    (use-package helm-flyspell
      :init (evil-bind-keys 'normal flyspell-mode-map
                            ("z=" . helm-flyspell-correct)))))


(use-package outline                    ; Hierarchical outlining support
  :ensure nil
  :delight outline-minor-mode
  :init
  ;; fontify outline headers
  (defun my-outline-fontify-headlines ()
    "Add font-lock-keywords for each outline level face."
    (font-lock-add-keywords
     nil
     (mapcar
      (lambda (face)
        `(,(replace-regexp-in-string "\\\\{\\(1,8\\)\\\\}"
                                     (substring (symbol-name face) -1)
                                     ";;;\\{1,8\\} \\(.+\\)$"
                                     ;; (concat outline-regexp " \\(.+\\)$")
                                     nil nil 1)
          1 (quote ,face) t))
      '(outline-1 outline-2 outline-3 outline-4
                  outline-5 outline-6 outline-7 outline-8))))
  (add-hook 'outline-minor-mode-hook #'my-outline-fontify-headlines)

  (use-package outline-magic
    :defer t
    :init
    (evil-define-key 'normal outline-minor-mode-map
      ("\t" . outline-cycle)))

  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)

  :config
  (bind-key "C-c o" #'outline-insert-heading outline-minor-mode-map)
  (evil-bind-keys 'normal outline-minor-mode-map
                  ("gh" . outline-up-heading)
                  ("gj" . outline-next-heading)
                  ("gk" . outline-previous-h)eading
                  ("gl" . outline-forward-sa)me-level
                  ("<" . outline-promote)
                  (">" . outline-demote))

)(use-package smartparens-config         ; FIXME Balanced paren management
  ;; FIXME autopairing quotes and backticks
  ;; FIXME hooks not being run correctly
  ;; TODO delight modeline lighters
  :ensure smartparens :defer t
  :delight smartparens-mode '(:eval
                              (concat " " (when smartparens-strict-mode "⒮")))
  :init
  ;; disable lesser versions to avoid doubling
  (add-hook 'smartparens-enabled-hook (lambda() (electric-pair-mode -1)))
  (add-hook 'show-smartparens-mode-hook (lambda() (show-paren-mode -1)))
  ;; enable the modes
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook #'smartparens-strict-mode)

  (use-package evil-smartparens         ; Evil smartparen bindings
    :delight evil-smartparens-mode
    :init
    (add-hook 'prog-mode-hook #'smartparens-strict-mode)
    (add-hook 'smartparens-strict-mode-hook #'evil-smartparens-mode))

  :config
  (setq sp-show-pair-from-inside t)     ; highlight pair when point on bracket
  (bind-key "M-=" #'sp-indent-defun smartparens-mode-map)
  (sp-local-pair 'html-mode "<" ">"))


(use-package typo                       ; Insert typographical characters
  :defer t
  :delight typo-mode
  :init (add-hook 'text-mode-hook #'typo-mode))


(use-package undo-tree                  ; REVIEW Branching undo tree
  :delight undo-tree-mode
  :init (global-undo-tree-mode t)
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        ;; undo-tree-auto-save-history t   ; may be causing corrupted history
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-history" my-dir))))

  (defun undo-tree-mode-off () (undo-tree-mode -1))

  (unbind-key "C-/" undo-tree-map))     ; REVIEW check this actually does thing


(use-package yasnippet                  ; Snippet insertion
  :defer t
  :config (setq yas-snippet-dirs
                `(,(expand-file-name "snippets/" my-dir)
                  yas-installed-snippets-dir)))


(use-package autorevert                 ; Auto revert to external modifications
  :defer t
  :delight autorevert-mode
  :init (global-auto-revert-mode t)
  :config (setq global-auto-revert-non-file-buffers t))


(use-package pandoc-mode                ; Markup conversion tool
  :delight pandoc-mode '(:eval (concat " Π:" (pandoc--get 'write)))
  :bind ("C-c C-p" . pandoc-mode))


(use-package real-auto-save             ; TODO Auto save buffers
  ;; TODO change interval
  :delight real-auto-save-mode " α"
  :commands real-auto-save-mode
  :init (add-hook #'after-save-hook #'real-auto-save-mode))


(use-package recentf                    ; List recent files
  :defer t
  :init (recentf-mode t)
  :config
  (setq
   recentf-max-saved-items 300          ; increase history size
   recentf-auto-cleanup 600             ; cleanup files after 10 minutes
   recentf-exclude '("COMMIT_EDITMSG")
   recentf-save-file (expand-file-name "recentf" my-dir)))

;;;; applications

(use-package compile                    ; Compilation
  :defer t
  :config
  (setq
   compilation-always-kill t      ; kill old processes before starting new ones
   compilation-skip-threshold 2   ; skip warning and info messages
   compilation-context-lines 3    ; show 3 lines of context around message
   compilation-scroll-output 'first-error
   compilation-auto-jump-to-first-error t
   compilation-ask-about-save nil))

(use-package paradox                    ; Better package management
  :init (use-package async)
  :config
  (setq paradox-execute-asynchronously t))


(use-package calendar                   ; Calendar
  :defer t
  :config (setq calendar-date-style 'european))


(use-package dired                      ; Emacs file browser
  :ensure nil
  :bind (("C-x d" . dired-jump)
         ("C-x C-d" . list-directory))
  :config
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


(use-package doc-view                   ; In-buffer document viewer
  :ensure nil :defer t
  :init (add-hook 'doc-view-minor-mode-hook #'undo-tree-mode-off)
  :config
  (setq doc-view-continuous t)
  (bind-keys :map doc-view-mode-map
             ("SPC" . execute-extended-command)
             ("g" . nil)
             ("gg" . doc-view-first-page)
             ("gp" . doc-view-goto-page)
             ("G" . doc-view-last-page)
             ("/" . doc-view-search)
             ("?" . doc-view-search-backward)
             ("n" . doc-view-search-next-match)
             ("j" . doc-view-next-line-or-next-page)
             ("k" . doc-view-previous-line-or-previous-page)
             ("q" . kill-this-buffer)))


(use-package ediff                      ; Emacs diff utility
  :defer t
  :config
  (setq ediff-diff-options "-w")  ; ignore whitespace
  (evil-define-key 'normal ediff-mode
    ("j" . ediff-next-difference)
    ("k" . ediff-previous-difference)))


(use-package garak :disabled t          ; ELIM messenger front-end
  :enabled nil :load-path (concat my-dir-packages "elim"))


(use-package comint                     ; Emacs terminal emulator
  :ensure nil :defer t
  :config
  (setq comint-completion-addsuffix t  ; add space/slash after file completion
        comint-input-ignoredups t      ; ignore duplicates in command history
        comint-scroll-to-bottom-on-input t
        comint-completion-autolist t)
  (defun my-comint-evil-insert ()
    "Enter insert state after the process mark."
    (interactive)
    (comint-goto-process-mark)
    (evil-append 1))

  (evil-define-key 'normal comint-mode-map
    ("I" . my-comint-evil-insert)
    ("A" . my-comint-evil-insert))

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


(use-package eshell                     ; TODO Emacs shell
  :bind (("<f12>" . eshell))
  :functions my-eshell-prompt
  :config
  (setq
   ;; eshell-prompt-regexp "^[^#$\n]* [#$] "
   eshell-buffer-shorthand t      ; buffer shorthand: echo foo > #'buffer
   eshell-highlight-prompt nil
   ;; eshell-prompt-function #'my-eshell-prompt
   eshell-directory-name (expand-file-name my-dir "eshell/")
   eshell-cmpl-ignore-case t
   eshell-banner-message (propertize (shell-command-to-string "fortune")
                                     'face '(:foreground "#b58900")))
  (add-to-list 'eshell-modules-list 'eshell-smart)

  (use-package em-prompt :ensure nil)
  (use-package em-cmpl :ensure nil)
  (use-package em-banner :ensure nil)

  (use-package helm-eshell
    :ensure nil
    :init (bind-key "<C-return>" #'helm-eshell-history eshell-mode-map))

  (use-package eshell-prompt-extras     ; TODO this. all of this.
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


(use-package malyon                     ; Z-machine text-based adventure reader
  :ensure nil :defer t)


(use-package magit                      ; Git version control management
  :delight magit-auto-revert-mode
  :bind ("<f10>" . magit-status)
  :init (setq magit-last-seen-setup-instructions "1.4.0")
  :config
  (setq magit-diff-options '("-b")           ; ignore whitespace in diffs
        magit-save-some-buffers 'dontask)

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


  (bind-keys :map magit-status-mode-map
             ("<C-tab>" . magit-section-cycle)
             ("j" . next-line)
             ("k" . previous-line)
             ("<down>" . magit-goto-next-sibling-section)
             ("<up>" . magit-goto-previous-sibling-section)
             ("d" . magit-discard-item)
             ("C-=" . magit-diff-working-tree)
             (",b" . ibuffer))

  (use-package git-timemachine          ; Travel through commit history
    :bind (("C-<f10>" . git-timemachine)
           ("S-<f10>" . git-timemachine-toggle))))


(use-package git-wip-mode :disabled t)  ; TODO what does this do

;;;; languages

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
    (",w" . git-commit-commit)
    ("q" . git-commit-abort)))

(use-package org
  :config
  (setq
   org-edit-src-content-indentation 0   ; no initial indent for source code
   org-src-preserve-indentation t       ; preserve source block indentation
   org-src-strip-leading-and-trailing-blank-lines t
   org-adapt-indentation nil            ; don't adapt indentation
   org-imenu-depth 3                    ; larger imenu depth
   org-special-ctrl-a/e t               ; begin/end of line skips tags & stars
   org-special-ctrl-k t                 ; kill lines depending on org context
   org-return-follows-link t            ; follow links with RET
   org-catch-invisible-edits 'smart     ; smart editing of hidden things
   org-todo-keywords '((sequence "☐" "☒"))
   org-modules '(org-docview org-info org-gnus org-inlinetask)
   org-export-backends '(ascii html odt taskjuggler)
   org-startup-folded t                 ; start buffers with folded headers
   org-src-fontify-natively t           ; syntax highlight code in org buffer
   org-list-allow-alphabetical t)       ; allow single-char alphabetical lists

  (delight #'org-indent-mode)
  (set-face-attribute 'org-table nil :family (my-font 'mono))
  (set-face-attribute 'org-todo nil
                      :height 'unspecified
                      :inherit 'variable-pitch)
  (set-face-attribute 'org-done nil :inherit 'org-todo)

  (bind-keys :map org-mode-map
             ("C-1" . org-clock-in)
             ("C-2" . org-clock-out)
             ("S-<return>" . org-insert-heading-after-current))

  (evil-bind-keys 'normal org-mode-map
                  ("RET" . org-insert-heading)
                  ("\t" . org-back-to-heading))

  (use-package org-clock
    :ensure nil
    :config
    (setq org-clock-persist t           ; save clock and clock history on exit
          org-clock-in-resume t))       ; resume if clocking in with open clock

  (use-package ox-odt
    :ensure nil
    :config (setq org-odt-preferred-output-format 'doc))

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
        `(,(substring project 0 -2)     ; header excluding "}\n"
          ;; properties to add
          "timeformat \"%H-%d\""
          "timingresolution 15 min")
        "\n  ")
       "\n}\n"))
    (advice-add #'org-taskjuggler--build-project
                :filter-return #'my-org-tj-add-project-options))

  (use-package evil-org                 ; Evil org-mode bindings
    :delight evil-org-mode
    :init (add-hook 'org-mode-hook #'evil-org-mode))

  (defun org-dblock-write:git-log-view (params)
    "Display a git commit log according to PARAMS."
    (let ((repo (expand-file-name
                 ".git" (or (plist-get params :repo)
                            (file-name-directory (buffer-file-name)))))
          (format (or (format "--format='%s'" (plist-get params :format)) ""))
          (trunc-p (plist-get params :trunc))
          (args (or (string-join (plist-get params :args) " ") "")))
      (let ((output
             (shell-command-to-string
              (format "git --git-dir='%s' log %s %s" repo format args))))
        (when trunc-p
          (setq output (replace-regexp-in-string "\\.\\." "" output)))
        (insert output)))))


(use-package generic-x                  ; Collection of generic modes
  :ensure nil :defer t
  :config
  (my-append 'generic-extras-enable-list generic-mswindows-modes)
  (use-package conkyrc-mode :disabled t   ; System monitor setup language
    :load-path "~/.emacs.d/elisp/" :ensure nil))


(use-package elisp-mode
  :defer t :ensure nil
  :init
  (defun my-imenu-decls ()
    "Add custom declarations to `imenu-generic-expression'."
    (dolist (expr
             `(("Packages" "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)
               ("Headings" ,(concat "^\\(" outline-regexp "\\) \\(.+\\)$") 2)))
      (add-to-list 'imenu-generic-expression expr)))
  (add-hook 'emacs-lisp-mode-hook #'my-imenu-decls)

  (use-package eldoc                      ; Documentation in echo area
    :defer t
    :delight eldoc-mode
    :init (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
    :config (setq eldoc-idle-delay 0.3))

  (use-package highlight-quoted  ; Faces for lisp quotes and quoted symbols
    :defer t
    :init (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))

  (use-package elisp-slime-nav            ; Navigate elisp documentation
    :defer t
    :delight elisp-slime-nav-mode
    :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
    :config
    (evil-bind-keys 'normal elisp-slime-nav-mode-map
      ("K" . elisp-slime-nav-describe-elisp-thing-at-point)
      ("gd" . elisp-slime-nav-find-elisp-thing-at-point)))
  :config
  (bind-key "C-c C-c" #'eval-defun emacs-lisp-mode-map))


(use-package python
  :defer t
  :config
  (setq python-shell-interpreter "python3") ; use Python 3

  (use-package elpy
    :defer t
    :init (elpy-enable)))


(use-package sh-script
  :defer t
  ;; two-space indentation
  :config (setq sh-indentation 2 sh-basic-offset 2))


(use-package vbnet-mode
  :defer t :ensure nil
  ;; use sensible faces instead of package-defined ones
  :config (setq vbnet-funcall-face 'font-lock-function-name-face
              vbnet-namespace-face 'font-lock-preprocessor-face))


(use-package js2-mode :disabled t
  :defer t
  :mode "\\.js$"
  :init (defalias 'javascript-generic-mode #'js2-mode)
  :config (setq-default js-indent-level 2))


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
  :init (add-hook 'web-mode-hook #'rainbow-mode)
  :config
  (setq
         web-mode-enable-block-face t
         web-mode-enable-part-face t
         web-mode-enable-comment-keywords t
         web-mode-enable-current-element-highlight t
         web-mode-enable-current-column-highlight t
         web-mode-css-indent-offset 2
         web-mode-code-indent-offset 2
         web-mode-markup-indent-offset 2))

;;; init.el ends here
