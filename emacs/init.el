;;; init.el --- Kelly Stewart's init file
;;; Commentary:
;; TODO figure out wtf is opening network connections when adding use-package
;;; Code:
;;;; helper functions

;; Adapted from http://stackoverflow.com/a/24357106/3912814
(defun my-add-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR if not already in list.

Return new value of LIST-VAR. If LIST-VAR is not defined, then
define it as ELEMENTS."
  (let ((l (if (listp elements) elements (list elements))))
    (if (and (boundp list-var) (symbol-value list-var))
        (dolist (item l) (add-to-list list-var item))
      (set list-var l)))
  (symbol-value list-var))

;; Would normally use `bind-key*' for this, but that would also override
;; `evil-insert-state', and that shouldn't be touched.
(defmacro my-add-binds (map)            ; TODO
  "Add essential bindings to MAP with `bind-key'."
  `(bind-keys :map ,map
              ("SPC" . execute-extended-command)
              ("q" . kill-buffer-and-window)
              (",b" . ibuffer)))

(defmacro my-bind-over-evil (&rest args)
  "Pass ARGS to `bind-keys' with `:map' set to evil states and `global-map'."
  `(bind-keys :map (global-map
                    evil-insert-state-map
                    evil-motion-state-map
                    evil-replace-state-map
                    evil-visual-state-map)
              ,@args))

(defun my-cleanup ()
  "Clean up current buffer whitespace."
  (interactive)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max))
  (when (fboundp 'whitespace-cleanup) (whitespace-cleanup)))

;; TODO this
(defun my-english-count-lines ()
  "Count screen lines in a region with handwriting font activated."
  (interactive)
  ;; (message "hi")
  (face-remap-add-relative 'default :family "Kelly Normal" :height 220)
  ;; (let ((handwriting '(:family "Kelly Normal" :height 220)))
  ;;   ;; (unless (member `(default ,handwriting default) face-remapping-alist)
  ;;   ;;   (face-remap-add-relative 'default handwriting))
  ;;   (face-remap-add-relative 'default handwriting)
  ;;   ;; (message "%s" (count-screen-lines (region-beginning) (region-end)))
  ;;   ;; (face-remap-remove-relative
  ;;   ;;  (face-remap-add-relative 'default handwriting))
  ;;   )
  )

;;;; setup
(defconst my-dir (expand-file-name  ".user/" user-emacs-directory))
(defconst my-dir-elisp (expand-file-name "my-elisp" user-emacs-directory))
(defconst my-dir-packages (expand-file-name "elisp/" user-emacs-directory))

(add-to-list 'load-path my-dir-packages) ; non-MELPA packages
(add-to-list 'load-path my-dir-elisp)    ; personal elisp files

;; load custom file
(setq custom-file (expand-file-name "custom.el" my-dir))
(load custom-file 'no-error 'no-message)

;;;;; package
(require 'package)
(setq package-enable-at-startup nil     ; we will manually initialize
      load-prefer-newer t)              ; don't load outdated byte code
(my-add-list 'package-archives
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
(use-package bind-key)

;;;; basic settings

;;;;; server
(use-package server
  :bind (("<f4>" . delete-frame)
         ("<C-f4>" . server-edit))
  :init (server-start))

;;;;; general

(setq
 disabled-command-function nil          ; no disabld
 comment-auto-fill-only-comments t
 smooth-scroll-margin 3                 ; fewer lines visible at buffer ends
 frame-title-format "%b " ; buffer name as frame title
 window-combination-resize t            ; use proportional window resize
 echo-keystrokes 0.1                    ; echo unfinished commands faster
 x-underline-at-descent-line t          ; draw underline lower
 ring-bell-function 'ignore             ; disable alarms
 initial-major-mode 'fundamental-mode   ; scratch in fundamental-mode
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

(setq-default indent-tabs-mode nil      ; turn tabs to spaces
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

;; never kill *scratch* buffer
(add-to-list
 'kill-buffer-query-functions
 (lambda ()
   (if (equal (buffer-name) "*scratch*") (progn (bury-buffer) nil) t)))

;; auto close compilation buffer on success
(add-to-list
 'compilation-finish-functions
 (lambda (buf result)
   (let ((win (get-buffer-window buf 'visible)))
     (when (and win (not (string-match ".*exited abnormally.*" result)))
       (delete-window win)))))

;; Adapted from http://www.reddit.com/r/emacs/comments/25v0eo/you_emacs_tips_and_tricks/chldur
(defun my-last-buf (&rest _args)
  "Switch to other window and display previous buffer."
  (other-window 1 nil)
  (switch-to-next-buffer))
(dolist (func '(evil-window-split
                evil-window-vsplit
                split-window-horizontally
                split-window-vertically))
  (advice-add func :after-while #'my-last-buf))

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

;; Add mode hooks
(dolist (hook '((prog-mode-hook
                 prettify-symbols-mode  ; Replace words with symbols
                 auto-fill-mode         ; Automatically fill text past fill-col
                 goto-address-prog-mode ; Buttonize URLs in comments and string
                 column-number-mode     ; Col number in modeline
                 line-number-mode)      ; Line number in modeline
                (text-mode-hook
                 goto-address-mode      ; Buttonize URLs
                 visual-line-mode)))    ; Wrap by word
  (dolist (func (cdr hook)) (add-hook (car hook) func)))

;;;; bindings

;;;;; binding helpers
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
  "Like `comment-dwim' but first toggle line comment if no active region."
  (interactive "*P")
  (if (and (not (region-active-p))
           (not (or (eq (point) (line-end-position))
                    ;; compensate for evil-mode cursor being one point ahead
                    (and (eq (point) (- (line-end-position) 1))
                         (bound-and-true-p evil-normal-state-p)))))
      (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;;;;; binds

;; I accidentally call this too much when calling C-x and trying to ESC out
(global-unset-key (kbd "C-x ESC ESC"))

(bind-keys
 ("M-SPC" . cycle-spacing)
 ("RET" . newline-and-indent)           ; indent new lines automatically
 ("C-x C-M-x" . revert-buffer)
 ("C-x n" . my-narrow-or-widen-dwim)
 ("M-u" . scroll-other-window-down)
 ("M-d" . scroll-other-window)
 ("M-/" . grep)
 ("C-M-/" . find-grep-dired)
 ([remap comment-dwim] . my-comment-dwim)
 ([remap switch-to-buffer] . ibuffer))

(my-add-binds package-menu-mode-map)

(bind-keys :map (minibuffer-local-map
                 minibuffer-local-ns-map
                 minibuffer-local-completion-map
                 minibuffer-local-must-match-map
                 minibuffer-local-isearch-map)
           ("<escape>" . minibuffer-keyboard-quit))

(bind-keys
 :prefix-map my-window-map
 :prefix "C-w"
 :prefix-docstring "My window related commands."
 ("C-w" . other-window)
 ("w" . other-window)
 ("v" . split-window-vertically)
 ("h" . split-window-horizontally)
 ("=" . balance-windows)
 ("f" . delete-frame)
 ("d" . delete-window)
 ("D" . delete-other-windows))

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
  :commands evil-define-command evil-bind-key
  :init (evil-mode t)
  :config
  (setq evil-want-C-w-in-emacs-state t   ; prefix window commands with C-w
        evil-want-fine-undo nil          ; undo insertions in single steps
        evil-want-change-word-to-end nil ; don't let cw behave like ce
        evil-echo-state nil              ; state is in the modeline anyway
        evil-ex-substitute-global t)     ; global substitutions by default
  (my-add-list 'evil-emacs-state-modes '(shell-mode term-mode multi-term-mode))
  (my-add-list 'evil-insert-state-modes '(git-commit-mode))

  (defmacro evil-bind-key (&rest args)
    "Bind BINDS to evil STATE and MAP.

STATE can be either a symbol specifying an evil state or a list
of symbols.

The rest of the arguments are conses of keybinding string, which
will be passed to `read-kbd-macro' if necessary, and an unquoted
function symbol."
    (declare (indent 1))
    (let* ((state-shorts '((n normal)
                           (m motion)
                           (v visual)
                           (r replace)
                           (o operator)
                           (i insert)
                           (nm normal motion)
                           (nv normal visual)
                           (nmv normal motion visual)
                           (a normal motion visual replace insert)))
           (map 'global-map)
           (states '(normal))
           (binds (cdr (cdr args))))
      (dolist (arg (list (car args) (nth 1 args)))
        (let ((shortened (ignore-errors (cdr (assoc (nth 1 arg) state-shorts)))))
          (cond
           ((stringp arg) (setq binds `(,arg ,@binds)))
           (shortened (setq states shortened))
           ((listp (cdr arg)) (setq map (car (cdr arg)))))))
      (setq maps (if (listp map) map (list map)))
      (macroexp-progn
       (mapcar
        (lambda (item)
          `(evil-define-key ',(car item) ,(cdr item)
             ,@(mapcar
                (lambda (item)
                  (if (stringp item)
                      (read-kbd-macro item)
                    item))
                binds)))
        ;; build (state . map) for each pairing
        (apply
         #'nconc
         (mapcar
          (lambda (s) (mapcar (lambda (m) (cons s m)) maps)) states))))))

  (evil-define-command my-evil-yank-to-eol ()
    "Call `evil-yank' with point to end of line."
    (evil-yank (point) (point-at-eol)))

  ;; remove bindings for access to own `my-window-map'
  (bind-keys
   :map (evil-motion-state-map evil-insert-state-map evil-emacs-state-map)
   ("C-w" . nil))

  (bind-keys
   :map (evil-normal-state-map evil-motion-state-map evil-visual-state-map)
   :prefix-map my-evil-leader-map
   :prefix ","
   :prefix-docstring "My evil leader key."
   ("f" . find-file)
   ("b" . list-buffers)
   ("w" . save-buffer)
   ("SPC" . evil-ex))

  (evil-bind-key 'm "y" #'evil-yank) ; Add yanking to motion map
  (evil-bind-key 'nmv
    "SPC" #'execute-extended-command
    "ESC" #'keyboard-quit
    ":" #'comment-dwim
    "Y" #'my-evil-yank-to-eol        ; more consistent
    "q" #'kill-buffer-and-window     ; consistency with other Emacs buffers
    "Q" #'evil-record-macro          ; Q replaces old q action
    ;; a-s is more memnonic but s-a follows keyboard order
    "s" #'evil-last-non-blank
    "a" #'evil-first-non-blank
    ;; movement
    "j" #'evil-next-visual-line
    "k" #'evil-previous-visual-line
    "\"" #'evil-jump-item)

  (use-package evil-escape              ; Escape from everything with two keys
    ;; trying this out instead of jk
    :defer t
    :init
    ;; make dummy keymaps to silence errors
    (dolist (map '(evil-lisp-state-map
                   evil-evilified-state-map
                   evil-iedit-insert-state-map
                   evil-iedit-state-map))
      (setq map (make-sparse-keymap)))
    (evil-escape-mode t))

  (use-package evil-surround            ; Operators for surrounding elements
    :defer t
    :init (global-evil-surround-mode t))

  (use-package evil-commentary          ; Operator for comments
    ;; TODO see if I can get this working with smartparen compliance
    :defer t
    :init (evil-commentary-mode t)
    :config (evil-bind-key 'evil-commentary-mode-map ":" #'evil-commentary))

  (use-package evil-matchit             ; Manipulate tags
    :init (global-evil-matchit-mode t)
    :config
    (setq evilmi-may-jump-percentage nil) ; allow count usage
    (evil-bind-key 'evil-matchit-mode-map "\"" #'evilmi-jump-items)))

(use-package ido :disabled t            ; As a backup for when Helm breaks
  :defer t
  :init
  (use-package smex                     ; Mini buffer command completion
    :bind ([remap execute-extended-command] . smex))
  (defalias #'ibuffer #'ido-switch-buffer)
  (ido-mode t))

(use-package helm                       ; Fuzzy minibuffer completion
  :demand t
  :init (helm-mode t)
  :functions my-helm-imenu-transformer
  :config
  (setq
   helm-quick-update t
   helm-move-to-line-cycle-in-source t     ; cycle on buffer end
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

  (helm-autoresize-mode t)

  ;; load packages manually to avoid byte-compiler warnings
  (use-package helm-files :ensure nil :defer t)
  (use-package helm-mode :ensure nil :defer t)
  (use-package helm-semantic :ensure nil :defer t)
  (use-package helm-buffers :ensure nil :defer t)
  (use-package helm-command :ensure nil :defer t)
  (use-package helm-elisp :ensure nil :defer t)
  (use-package helm-imenu :ensure nil :defer t)

  (defun my-helm-imenu-transformer (candidates)
    "Custom imenu transformer with added headings and faces."
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

  ;; less ugly colors for helm-buffer items
  (set-face-attribute 'helm-buffer-directory nil :inherit 'dired-directory
                      :foreground nil :background nil)
  (set-face-attribute 'helm-buffer-saved-out nil
                      :inherit 'font-lock-warning-face
                      :foreground nil :background nil :inverse-video nil)

  (defalias #'ibuffer #'helm-mini)

  (bind-keys*
   ([remap execute-extended-command] . helm-M-x)
   ([remap occur] . helm-occur)
   ([remap find-file] . helm-find-files)
   ([remap switch-to-buffer] . helm-mini)
   ([remap list-buffers] . helm-mini)
   ([remap ibuffer] . helm-mini)
   ([remap grep] . helm-do-grep)
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
   ([remap apropos-documentation] . helm-apropos))

  (evil-bind-key 'a "C-p" #'helm-show-kill-ring)
  (bind-key "hr" #'helm-resume my-evil-leader-map)
  (bind-key "C-/" #'helm-semantic-or-imenu emacs-lisp-mode-map)

  (bind-keys
   :map helm-buffer-map
   ("C-d" . helm-buffer-run-kill-buffers)
   ("<C-return>" . helm-buffer-switch-other-window))

  (bind-keys
   :map helm-map
   ("M-u" . helm-scroll-other-window)
   ("M-d". helm-scroll-other-window-down)
   ("<tab>" . helm-execute-persistent-action) ; execute action without closing
   ("C-z" . helm-select-action))              ; list actions

  (use-package helm-dash                ; TODO Language documentation viewer
    :config
    (setq
     helm-dash-browser-func 'eww
     helm-dash-docsets-path "~/.emacs.d/.user/docset")
    ;; (add-hook 'python-mode-hook
    ;;           (lambda()
    ;;             (setq-local helm-dash-docsets '("Python"))))
    )

  (use-package helm-descbinds           ; Replacement for `describe-bindings'
    :init (helm-descbinds-mode t)
    :config (setq helm-descbinds-window-style 'split-window))

  (use-package helm-swoop               ; Fast searching and navigation
    :bind ([remap grep] . helm-swoop)
    :init (bind-key "M-/" #'helm-swoop-all-from-isearch isearch-mode-map)
    :config
    (bind-key "M-/" #'helm-multi-swoop-all-from-helm-swoop helm-swoop-map)))

;;;; help

(use-package discover-my-major          ; List current major mode bindings
  :bind ("<help> m" . discover-my-major))

(use-package guide-key                  ; Delayed completion for possible keys
  :init (guide-key-mode t)
  :config (setq guide-key/recursive-key-sequence-flag t
                guide-key/guide-key-sequence '("C-x" "C-c" "C-w" ",")))

(use-package help-mode
  :ensure nil :defer t
  :config
  (use-package help+)
  (use-package help-fns+ :bind ("<help> o" . describe-option))
  (bind-keys :map help-mode-map
             ("H" . help-go-back)
             ("L" . help-go-forward))

  (bind-keys :map help-map
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

;;;; appearance

;;;;; fonts
(defun my-font (type)
  "Return the first valid font of TYPE and it's associated size, if given."
  (let* (valid-cand
         (handle (lambda (cand)
                   "If valid, then set `valid-cand' and exit loop."
                   (when (find-font (font-spec :name cand))
                     (setq valid-cand cand)
                     (throw 'found family))))
         (fonts '((proportional "Fira Sans"
                                "Input Sans Condensed" "Input Sans"
                                "DejaVu Sans" "Calibri" "Arial"
                                "Sans Serif")
                  (mono "Input Mono Condensed" "Input Mono" "Envy Code R"
                        "DejaVu Sans Mono" "Consolas" "Courier" "Monospace")
                  (header "Fantasque Sans Mono" proportional)
                  (cursive "Kelly Normal")))
         (sizes '(("Fira Sans" . 10)
                  ("Input Sans" . 10)
                  ("Input Sans Condensed" . 10)
                  ("Envy Code R" . 10)
                  ("Fantasque Sans Mono" . 1.15)
                  ("Input Mono Condensed" . 10)
                  ("Input Mono" . 8)
                  ("Kelly Normal" . 22)))
         (valid-family
          ;; get a valid family from `fonts'
          (catch 'found
            (dolist (family
                     ;; get font list, following symbols if they exist
                     (apply
                      #'nconc
                      (mapcar
                       (lambda (cand)
                         (if (symbolp cand)
                             (cdr (assoc cand fonts))
                           (list cand)))
                       (cdr (assoc type fonts)))))
              (funcall handle family)
              ;; check unspaced name for odd font naming
              (funcall handle (replace-regexp-in-string " " "" family)))))
         (size (cdr (assoc valid-family sizes)))
         (cand-size (if (and size (not (floatp size))) (* size 10) size)))
    ;; return candidate and associated size in 1/10ths of a pt
    (list :family valid-cand :height cand-size)))

(defun my-font-use-proportional ()
  "Use proportional font for current buffer."
  (interactive)
  (setq buffer-face-mode-face (my-font 'proportional))
  (buffer-face-mode t))

(add-hook 'text-mode-hook #'my-font-use-proportional)

;;;;; highlight fic

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

;;;;; color theme

(use-package my-theme
  :ensure nil :load-path "~/.emacs.d/my-elisp/my-theme/"
  :config
  (setq my-theme-bold-p nil)
  (load-theme 'my t))

(use-package material-theme :disabled t
  :config (load-theme 'material))

(use-package monokai-theme :disabled t
  :config (load-theme 'monokai))

(use-package spacemacs-theme :disabled t
  :config (load-theme 'spacemacs-dark))

(use-package solarized-theme :disabled t
  :demand t
  :config
  (setq solarized-scale-org-headlines nil
        solarized-height-plus-1 1.1
        solarized-height-plus-2 1.1
        solarized-height-plus-3 1.1
        solarized-height-plus-4 1.1)
  (load-theme 'solarized-dark))

(use-package color-theme-sanityinc-tomorrow :disabled t
  :demand t
  :config (load-theme 'sanityinc-tomorrow-night))

;;;;; modeline packages

(use-package delight
  :demand t
  :config (delight #'emacs-lisp-mode "Elisp" :major))

(use-package smart-mode-line            ; TODO Better modeline
  :demand t
  :init
  (use-package powerline :config (setq powerline-default-separator 'contour))

  (defun rm-add (mode &optional rep face regexpp)
    "Add MODE-STR to `rm-whitelist', and REP and FACE to `rm-text-properties'.

If REGEXPP is true then don't modify MODE before adding to
`rm-text-properties' and don't add the form to `rm-whitelist'."
    (let ((faces (if (listp face)
                     (append face 'mode-line-emphasis)
                   `(,face mode-line-emphasis)))
          (mode-regexp (if regexpp mode (concat "^ " mode "$"))))
      ;; add mode lighter to `rm-whitelist'
      (unless regexpp
        (setq rm-whitelist
              (cond
               ((or (not (bound-and-true-p rm-whitelist)) (string= rm-whitelist ""))
                (concat " " mode))
               ((not (string-match mode rm-whitelist))
                (concat rm-whitelist "\\| " mode)))))
      ;; add properties to `rm-text-properties'
      (when (and (or rep face) (not (assoc mode-regexp rm-text-properties)))
        (add-to-list
         'rm-text-properties
         `(,mode-regexp
           ,@(if rep
                 `('display (propertize ,(concat " " rep) 'face ',faces))
               `('face ',faces)))))))

  :config
  (setq
   sml/theme 'respectful
   sml/battery-format "%b%p[%t]"
   sml/position-percentage-format ""    ; no percentage, we have nyan cat.
   sml/full-mode-string " ⋯"            ; append this to modeline when full
   sml/shorten-mode-string ""           ; no indication for all modes displayed
   sml/mule-info nil                    ; don't show buffer encoding
   ;; replacers for shortening file name
   sml/replacer-regexp-list '(("^/media/user/" ":θ:")
                              (":θ:dev/dotfiles/" ":.:")
                              (":\\.:emacs/" ":.ε:")
                              (":θ:Documents/work/" ":⌢:")
                              (":θ/Doc:work/" ":⌢:")
                              (":⌢:bst/" ":⌢:b:")
                              (":⌢:mth/" ":⌢:m:")
                              (":⌢:eng/" ":⌢:e:")
                              (":⌢:sdd/" ":⌢:s:"))
   sml/use-projectile-p 'before-prefixes ; prioritize projectile prefixes
   sml/projectile-replacement-format ":%s:")

  ;; add rich-minority properties
  (dolist (props '(("Ovwrt" "▄" error)        ; overwrite
                   ("Fly" "〰")               ; flyspell
                   ("SP/s" "⒮")               ; smartparens-strict
                   ("Wg" "⦚")                 ; writegood
                   ("RAS" "ᚪ")))              ; real-auto-save
    (apply #'rm-add props))

  (use-package smart-mode-line-my-theme
    :ensure nil :load-path "~/.emacs.d/my-elisp/my-theme/"
    :config (setq sml/no-confirm-load-theme t sml/theme 'my))

  (sml/setup)

  ;; show nothing if no remote
  (setq-default
   mode-line-remote
   '((sml/show-remote
      (:propertize
       (:eval (replace-regexp-in-string "^-$" " " (format-mode-line "%1@")))
       face sml/remote)))))

(use-package nyan-mode                  ; Nyan cat scroll bar
  :defer t
  ;; TODO nyan music ☹
  :commands nyan-mode
  :init (nyan-mode t)
  :config
  (setq-default nyan-wavy-trail t)      ; TODO wavy nyan trail all the time ❥

  (defun nyan-nyan ()
    "Start or stop nyaning."
    (interactive)
    (if (and nyan-animate-nyancat
             nyan-animation-timer)
        (progn (nyan-stop-animation)
               (nyan-stop-music))
      (nyan-start-animation)
      (nyan-start-music)))

  (defun nyan-mode-off () (nyan-mode -1)))

(use-package which-func                 ; Modeline definition name
  :defer t
  :init (which-function-mode t)
  :config
  (defun which-func-current ()
    "Return a formatted which-func string if possible."
    (let ((current (gethash (selected-window) which-func-table)))
      (when current
        (truncate-string-to-width
          (replace-regexp-in-string
           "%" "%%"
           (if (eq major-mode 'emacs-lisp-mode)
               ;; remove inline comments
               (replace-regexp-in-string "\\([[:space:]]\\)*;.*$" "" current)
             current))
          21 nil nil " ⋯"))))

  (setq which-func-format
        `((:propertize "  ➤ " face (font-lock-builtin-face . which-func))
          (:propertize (:eval (which-func-current)) face which-func)
          (:propertize "  " face which-func))))

(use-package wc-goal-mode               ; WC and goal in modeline
  :defer t
  :init (add-hook 'text-mode-hook #'wc-goal-mode)
  :config

  (defvar wc-format-strings '("%tww" "%tll")
    "`wc-goal-modeline-format' strings to cycle through.")

  (defun wc-format-cycle ()
    "Cycle `wc-goal-modeline-format' through `wc-format-strings'."
    (interactive)
    (let* ((forms (or (member wc-goal-modeline-format wc-format-strings)
                      wc-format-strings))
           (form (cond
                  ((nth 1 forms))
                  ((equal (car forms) wc-goal-modeline-format)
                   (car wc-format-strings))
                  ((car forms)))))
      (setq wc-goal-modeline-format form))))


;;;;; face packages

(use-package hl-line                    ; Highlight current line
  :defer t
  :init (add-hook 'prog-mode-hook #'hl-line-mode)
  :config (setq hl-line-face 'highlight))

(use-package vline-mode                 ; Highlight current column
  :ensure nil :defer t
  :config (setq vline-face 'highlight
                vline-visual-face 'highlight))

(use-package hl-sentence                ; Highlight current sentence
  :defer t
  :init (add-hook 'text-mode-hook #'hl-sentence-mode)
  :config (setq hl-sentence-face 'highlight))

(use-package highlight-numbers          ; Highlight numbers
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package page-break-lines           ; Horizontal lines instead of ^L
  :defer t
  :init (global-page-break-lines-mode t))

(use-package paren-face                 ; Faces for parens
  :defer t
  :init (add-hook 'emacs-lisp-mode-hook #'paren-face-mode))

(use-package rainbow-mode               ; Highlight color codes
  :defer t
  :init (dolist (hook '(web-mode-hook css-mode-hook))
          (add-hook hook #'rainbow-mode)))

(use-package whitespace                 ; Faces for whitespace characters
  :defer t
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

;;;;; misc face changes
;; applied after all others to ensure precedence
(let ((font (my-font 'mono)))
  (set-frame-font (concat (nth 1 font) "-"
                          (number-to-string (/ (or (nth 3 font) 10) 10)))))

(apply #'set-face-attribute 'variable-pitch nil (my-font 'header))

;;;; interface

(use-package linum                      ; Line numbers
  :defer t
  :init (add-hook 'prog-mode-hook #'linum-mode))

(use-package golden-ratio :disabled t   ; TODO Resize windows to golden ratio
  :defer t
  ;; TODO get this to play nice with Helm
  :init (golden-ratio-mode t)
  :config
  (setq
   golden-ratio-exclude-modes '(helm-mode
                                magit-log-mode
                                magit-reflog-mode
                                magit-status-mode)
   golden-ratio-auto-scale t))

(use-package hideshow                   ; Code folding
  :init
  (use-package hideshowvis :disabled t
    :defer t
    :init
    (hideshowvis-symbols)
    (add-hook 'hs-minor-mode-hook #'hideshowvis-minor-mode))
  (add-hook 'prog-mode-hook #'hs-minor-mode)

  (evil-bind-key 'hs-minor-mode-map "TAB" #'hs-toggle-hiding)

  :config

  ;; add folding for other modes with curly bracket delimiters
  (dolist (mode '(css-mode web-mode))
    (add-to-list 'hs-special-modes-alist `(,mode "{" "}" "/[*/]" nil nil)))

  ;; TODO expand collapsed blocks when jumping to them
  ;; (defun my-hs-expand-advice (&rest _args)
  ;;   (save-excursion (if (and (boundp 'outline-minor-mode)
  ;;                            outline-minor-mode
  ;;                            (boundp 'outline-cycle))
  ;;                       (outline-toggle-children)
  ;;                     (hs-show-block))))
  ;; (advice-add #'imenu :after #'my-hs-expand-advice)
  ;; (advice-add #'goto-line :after #'my-hs-expand-advice)

  ;; fallback indentation-based hiding
  (defun toggle-hiding (column)
    "Toggle a block based on indentation level or defined markers."
    (interactive "P")
    (if hs-minor-mode
        (when (condition-case nil (hs-toggle-hiding) (error t))
          (hs-show-all))
      (set-selective-display
       (or column (unless selective-display (1+ (current-column))))))))

(use-package popwin                     ; Popup window for minor buffers
  :defer t
  :commands popwin-mode
  :init (popwin-mode t)
  :config
  (setq popwin:popup-window-position 'right)
  (my-add-list 'popwin:special-display-config
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
  :init (bind-key "<C-f11>" #'writeroom-mode text-mode-map)
  :config
  (setq writeroom-width 100)
  (defun my-writeroom-effect (arg)
    "Toggle additional functions for distraction-free writing."
    (let ((off (if arg -1 t))
          (on (if arg t -1)))
      (display-time-mode on)
      (display-battery-mode on)
      (which-function-mode off)
      (nyan-mode off)))
  (add-to-list 'writeroom-global-effects #'my-writeroom-effect))

(use-package visual-fill-column
  :defer t
  :init (add-hook 'text-mode-hook #'visual-fill-column-mode)
  :config (setq-default visual-fill-column-width 100))

;;;; navigation

(use-package avy                        ; Jump to specific points
  :init (my-bind-over-evil
         ("C-e" . avy-goto-word-1)
         ("C-S-e" . avy-goto-line))
  :config (setq avy-background t))

(use-package ace-window
  :bind ([remap other-window] . ace-window)
  :config (setq aw-keys (or avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

(use-package ag                         ; Fast search and navigation
  :defer t
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
    :config
    (setq helm-ag-insert-at-point 'symbol  ; symbol at point as default pattern
          ;; helm-ag-source-type 'file-line
          helm-ag-fuzzy-match t)))

(use-package desktop                    ; Save buffers, windows and frames
  :defer t
  :init (desktop-save-mode t)
  :config
  (setq desktop-auto-save-timeout 60
        desktop-save t                  ; always save
        desktop-dirname (expand-file-name "desktop" my-dir))
  (add-to-list 'desktop-path desktop-dirname)
  (my-add-list 'desktop-modes-not-to-save '(magit-mode git-commit-mode)))

(use-package expand-region              ; Expand functions block at a time
  :init (evil-bind-key 'nmv "zz" #'er/expand-region)
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
  :init
  (projectile-global-mode t)
  (bind-keys
   :map my-evil-leader-map
   ("p" . projectile-find-file-dwim)
   ("P" . projectile-switch-project))
  :config
  (setq
   projectile-globally-ignored-files '("TAGS" "*.odt" "*.docx" "*.doc")
   projectile-indexing-method 'alien    ; use faster OS methods
   ;; don't clutter my .emacs.d please
   projectile-cache-file (expand-file-name "projectile.cache" my-dir)
   projectile-known-projects-file (expand-file-name "projectile.eld" my-dir))

  (use-package helm-projectile
    :defer t
    :init (helm-projectile-on)
    :config
    (setq projectile-completion-system 'helm
          projectile-switch-project-action #'helm-projectile
          helm-projectile-fuzzy-match t)

    (bind-keys
     :map my-evil-leader-map
     ([remap projectile-find-file-dwim] . helm-projectile)
     ([remap projectile-switch-project] . helm-projectile-switch-project))))

;;;; editing

(use-package multiple-cursors
  :defer t
  :init (multiple-cursors-mode t))

(use-package drag-stuff                 ; TODO Transpose things
  ;; TODO fix dragging when newline is included
  ;; TODO? have dragging comply with smartparen-strict-mode
  :init (drag-stuff-global-mode t))

(use-package flycheck                   ; On-the-fly syntax checking
  :defer t
  :init
  (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (defun flycheck-mode-off () (flycheck-mode -1))
  (add-hook 'prog-mode-hook #'flycheck-mode)
  (add-hook 'sh-mode-hook #'flycheck-off)

  (bind-keys
   :map my-evil-leader-map
   ("cj" . flycheck-next-error)
   ("ck" . flycheck-previous-error))

  :config
  (defun my-flycheck-status (&optional status)
    (let ((warn-internal "W")
          (err-internal "E")
          (success-str "σ")
          (err-str "ε")
          (warn-str "ς")
          errs warns count)
      (case (or status flycheck-last-status-change)
        ('no-checker warn-internal)
        ('running "")
        ('interrupted warn-internal)
        ('suspicious err-internal)
        ('errored err-internal)
        ('finished
         (when flycheck-current-errors
           (setq count (flycheck-count-errors flycheck-current-errors)
                 errs (number-to-string (cdr (assoc 'error count)))
                 warns (number-to-string (cdr (assoc 'warning count))))
           (concat
            (unless (or errs warnings) success-str)
            (when errs (concat err-str errs))
            (when warns (concat warn-str warns))))))))
  (setq flycheck-mode-line '(" Φ" (:eval (my-flycheck-status))))
  (let ((base (s-trim (car flycheck-mode-line))))
    (dolist (args `((,base)                            ; base str
                    (,(concat base "σ") ,base success) ; no errors
                    (,(concat base "E") ,base error)   ; internal errors
                    (,(concat base "W") ,base warning) ; internal warnings
                    ("ε[0-9][0-9]?" nil error t)
                    ("ς[0-9][0-9]?" nil warning t)
                    ))
      (apply #'rm-add args)))

  (setq flycheck-indication-mode 'right-fringe)

  (use-package helm-flycheck
    :init
    (bind-key "C-c ! h" #'helm-flycheck flycheck-mode-map)
    (bind-key "cc" #'helm-flycheck my-evil-leader-map))

  (use-package flycheck-tip             ; display errors by popup
    :config (flycheck-tip-use-timer 'verbose)))

(use-package lorem-ipsum                ; Insert filler text
  :defer t
  :config
  ;; overwrite default `sgml-mode' entries
  (add-hook 'sgml-mode-hook
            (lambda () (setq lorem-ipsum-paragraph-separator "\n<p>"
                        lorem-ipsum-sentence-separator " ")))
  ;; double spaces are still dumb
  (unless sentence-end-double-space (setq lorem-ipsum-sentence-separator " ")))

(use-package writegood-mode             ; Highlight poor forms in writing
  :defer t
  :init
  (add-hook 'text-mode-hook #'writegood-mode)
  (add-hook 'writegood-mode-hook #'writegood-passive-voice-turn-off)
  :config (my-add-list 'writegood-weasel-words
                       '("thing" "different" "probably" "really")))

(use-package abbrev                     ; Auto-correct words after typing
  :ensure nil
  :init (abbrev-mode t)
  :config (setq save-abbrevs 'silently  ; save abbrevs when saving file
                abbrev-all-caps t       ; expand in all-caps if written in caps
                abbrev-file-name (expand-file-name "abbrevs.el" my-dir)))

(use-package auto-indent-mode           ; Automatic indentation
  ;; TODO get this working with indenting pasted code
  ;;      probably has something to do with Evil command hijacking
  :defer t
  :commands auto-indent-global-mode
  :init (auto-indent-global-mode t))

(use-package company                    ; Autocompletion in code
  :defer t
  :init (add-hook 'prog-mode-hook #'company-mode)
  :config
  (setq company-idle-delay 0            ; attempt completion immediately
        company-show-numbers t          ; allow M-num selection
        company-statistics-file (expand-file-name "company-stats.el" my-dir)
        company-tooltip-align-annonations t
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
    :init (company-statistics-mode t)))

(use-package ispell                     ; Spell-checking
  :defer t
  :config
  (setq
   ispell-dictionary "british-ize"
   ispell-extra-args '("--sug-mode=ultra" "--lang=en_GB")
   ;; no messages please
   ispell-silently-savep t
   ispell-quietly t)
  (my-add-list 'ispell-skip-region-alist
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

  (use-package flyspell                 ; On-the-fly spell checking
    :defer t
    :init

    (use-package helm-flyspell          ; Helm completion for spellcheck
      :defer t
      :init (evil-bind-key 'flyspell-mode-map "z=" #'helm-flyspell-correct))

    (use-package flyspell-lazy          ; Lazier checking for words
      :defer t
      :init
      (add-hook 'flyspell-mode #'flyspell-lazy-mode)
      (add-hook 'flyspell-prog-mode #'flyspell-lazy-mode))

    (add-hook 'text-mode-hook #'flyspell-mode)
    (add-hook 'prog-mode-hook #'flyspell-prog-mode)

    :config
    ;; no messages
    (setq flyspell-mode-line-string " 〰"
          flyspell-issue-welcome-flag nil
          flyspell-issue-message-flag nil)
    (advice-add #'flyspell-auto-correct-word
                :around #'my-ispell-run-together)))

(use-package outline                    ; TODO Hierarchical outlining support
  ;; TODO modify outline-promote so if already max level then demote subtrees
  :ensure nil
  :init
  ;; fontify outline headers
  (defun my-outline-fontify-headlines ()
    "Add font-lock-keywords for each outline level face."
    (font-lock-add-keywords
     nil
     (mapcar
      (lambda (face)
        `(,(replace-regexp-in-string "<num>"
                                     (substring (symbol-name face) -1)
                                     ";;;\\{<num>\\} \\(.+\\|###autoload\\)$")
          1 (quote ,face) t))
      '(outline-1 outline-2 outline-3 outline-4
                  outline-5 outline-6 outline-7 outline-8))))
  (add-hook 'outline-minor-mode-hook #'my-outline-fontify-headlines)

  (use-package outline-magic
    :defer t
    :init (evil-bind-key 'outline-minor-mode-map "TAB" #'outline-cycle))

  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)

  :config
  (bind-key "C-c o" #'outline-insert-heading outline-minor-mode-map)
  (evil-bind-key 'outline-minor-mode-map
    "gh" #'outline-up-heading
    "gj" #'outline-next-heading
    "gk" #'outline-previous-heading
    "gl" #'outline-forward-same-level
    "<" #'outline-promote
    ">" #'outline-demote))

(use-package smartparens-config      ; FIXME Balanced paren management
  ;; REVIEW autopairing quotes and backticks
  :ensure smartparens
  :init
  ;; disable builtin to avoid doubling
  (add-hook 'smartparens-mode-hook (lambda() (electric-pair-mode -1)))

  (evil-bind-key 'nm
    'emacs-lisp-mode-map
    "+" #'evil-indent
    "=" #'sp-indent-defun)

  (use-package evil-smartparens         ; Evil smartparen bindings
    :init
    (add-hook 'prog-mode-hook #'smartparens-strict-mode)
    (add-hook 'smartparens-strict-mode-hook #'evil-smartparens-mode)

    :config
    (evil-define-operator my-evil-sp-delete-line ()
      "Call `sp-kill-hybrid-sexp'."
      :motion nil
      (interactive)
      (sp-kill-hybrid-sexp 1))

    (evil-define-operator my-evil-sp-change-line ()
      "Call `sp-kill-hybrid-sexp' and enter `evil-insert-state'."
      :motion nil
      (interactive)
      (sp-kill-hybrid-sexp 1)
      (evil-insert 1))

    (evil-bind-key 'evil-smartparens-mode-map
      ;; faster than evil-smartparens variants
      [remap evil-sp-change-line] #'my-evil-sp-change-line
      [remap evil-sp-delete-line] #'my-evil-sp-delete-line))

  (show-smartparens-global-mode t)
  :config
  (setq sp-show-pair-from-inside t)     ; highlight pair when point on bracket
  (sp-local-pair 'html-mode "<" ">"))

(use-package typo                       ; Insert typographical characters
  :defer t
  :init (add-hook 'text-mode-hook #'typo-mode))

(use-package undo-tree                  ; Branching undo tree
  :init (global-undo-tree-mode t)
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        ;; undo-tree-auto-save-history t   ; would use but corrupts history
        undo-tree-history-directory-alist
        `(("." . ,(expand-file-name "undo-history" my-dir))))

  (defun undo-tree-mode-off () (undo-tree-mode -1))

  (unbind-key "C-/" undo-tree-map))

(use-package yasnippet                  ; Snippet insertion
  :defer t
  :config (setq yas-snippet-dirs
                `(,(expand-file-name "snippets/" my-dir)
                  yas-installed-snippets-dir)))

(use-package autorevert                 ; Auto revert to external modifications
  :defer t
  :init (global-auto-revert-mode t)
  :config (setq global-auto-revert-non-file-buffers t))

(use-package pandoc-mode                ; Markup conversion tool
  :bind ("C-c C-p" . pandoc-mode)
  :config
  (setq pandoc-data-dir (expand-file-name "pandoc" my-dir))
  (bind-key "C-c C-p" #'pandoc-run-pandoc pandoc-mode-map))

(use-package real-auto-save             ; Auto save buffers
  :commands real-auto-save-mode
  :init (add-hook #'after-save-hook #'real-auto-save-mode)
  :config (setq real-auto-save-interval 60))

(use-package recentf                    ; List recent files
  :defer t
  :init (recentf-mode t)
  :config
  (setq recentf-max-saved-items 300     ; increase history size
        recentf-auto-cleanup 600        ; cleanup files after 10 minutes
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
  :defer t
  :config
  (use-package async)
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
    :defer t
    :init (add-hook 'dired-mode-hook #'dired-omit-mode)  ; omit uninteresting files
    :config
    (setq diredp-dwim-any-frame-flag t  ; allow dwim target other frame
          dired-omit-verbose nil))

  (bind-keys :map dired-mode-map
             ("q" . kill-buffer-and-window)
             ("C-M-u" . dired-up-directory)
             ("C-w" . wdired-change-to-wdired-mode)))

(use-package doc-view                   ; In-buffer document viewer
  :ensure nil :defer t
  :init (add-hook 'doc-view-minor-mode-hook #'undo-tree-mode-off)
  :config
  (setq doc-view-continuous t)
  (my-add-binds doc-view-mode-map)
  (bind-keys :map doc-view-mode-map
             ("g" . nil)
             ("gg" . doc-view-first-page)
             ("gp" . doc-view-goto-page)
             ("G" . doc-view-last-page)
             ("/" . doc-view-search)
             ("?" . doc-view-search-backward)
             ("n" . doc-view-search-next-match)
             ("j" . doc-view-next-line-or-next-page)
             ("k" . doc-view-previous-line-or-previous-page)
             ("C-d" . doc-view-next-page)
             ("C-u" . doc-view-previous-page)))

(use-package ediff                      ; Emacs diff utility
  :defer t
  :config
  (setq ediff-diff-options "-w")        ; ignore whitespace
  (evil-bind-key 'ediff-mode
    "j" #'ediff-next-difference
    "k" #'ediff-previous-difference))

(use-package garak :disabled t          ; ELIM messenger front-end
  :enabled nil :load-path (concat my-dir-packages "elim"))

(use-package comint                     ; Emacs terminal emulator
  :ensure nil :defer t
  :config
  (setq comint-completion-addsuffix t   ; add space/slash after file completion
        comint-input-ignoredups t       ; ignore duplicates in command history
        comint-scroll-to-bottom-on-input t
        comint-completion-autolist t)
  (defun my-comint-evil-insert ()
    "Enter insert state after the process mark."
    (interactive)
    (comint-goto-process-mark)
    (evil-append 1))

  (evil-bind-key 'comint-mode-map
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

(use-package eshell :disabled t         ; TODO Emacs shell
  :bind ("<f12>" . eshell)
  :config
  (setq
   ;; eshell-prompt-regexp "^[^#$\n]* [#$] "
   eshell-buffer-shorthand t            ; buffer shorthand: echo foo > #'buffer
   eshell-highlight-prompt nil
   ;; eshell-prompt-function #'my-eshell-prompt
   eshell-directory-name (expand-file-name my-dir "eshell/")
   eshell-cmpl-ignore-case t
   eshell-banner-message (propertize (shell-command-to-string "fortune")
                                     'face '(:foreground "#b58900")))

  (defmacro my-with-face (str face &rest props)
    "Propertize STR with FACE and other PROPS."
    `(propertize ,str 'face ',face ,@props))

  (defmacro my-with-col (str face &rest props)
    "Propertize STR with the foreground color of FACE and other PROPS."
    `(propertize ,str 'face '(:foreground ,(face-attribute face :foreground))
                 ,@props))

  (defun my-esh-prompt-git ()
    (let* ((branch (shell-command-to-string "git symbolic-ref HEAD --short"))
           (detachedp (string-match "detached" branch))
           (status (shell-command-to-string "git status --porcelain"))
           (untrackedp (string-match "^\\?\\?" status))
           (stashedp (not (string= (shell-command-to-string
                                    "git stash list -n 1") "")))
           (remote (shell-command-to-string (concat
                                             "git config --get
                                             branch" branch)))
           (dirtyp
            (string-match
             "dirty" (shell-command-to-string
                      "git diff-index --quiet HEAD -- || echo -n 'dirty'")))
           (dirty-char "δ")
           ())
      ))

  (defun my-eshell-prompt ()
    (concat
     ;; abbreviated current working directory
     (if (fboundp 'sml/replacer) (sml/replacer (eshell/pwd)) (eshell/pwd))
     ;; git info
     (when (and (eshell-searchpath "git")
                (locate-dominating-file (eshell/pwd) ".git"))
       (format
        "( %s%s )"

        )
       ;; dirty status
       (and
        )
       ;; branch
       (let ((name ))
         (if (string-match "detached" name) "H" (substring name 0 -1)))
       ;; untracked
       (and (not (string=)))
       ")"
       )))

  (use-package esh-module
    :defer t :ensure nil
    ;; REVIEW this should fix errors with `eshell-modules-list' undefined
    :config (add-to-list 'eshell-modules-list 'eshell-smart))
  (use-package em-prompt :ensure nil :defer t)
  (use-package em-cmpl :ensure nil :defer t)
  (use-package em-banner :ensure nil :defer t)

  (use-package helm-eshell
    :ensure nil
    :init (bind-key "<C-return>" #'helm-eshell-history eshell-mode-map))

  (use-package eshell-prompt-extras     ; TODO this. all of this.
    :init
    (use-package virtualenvwrapper      ; Show Python venv info in prompt
      :config (venv-initialize-interactive-shells))
    :config
    (setq
     eshell-highlight-prompt nil
     epe-git-dirty-char "δ"
     epe-git-untracked-char "θ"
     epe-git-detached-HEAD-char "Η"
     eshell-prompt-function #'epe-theme-geoffgarside)))

(use-package malyon                     ; Z-machine text-based adventure reader
  :ensure nil :defer t)

(use-package vc-git                     ; Git Version Control
  :defer t :ensure nil
  :init

  (use-package magit                    ; Git version control management
    :bind ("<f10>" . magit-status)
    :init (setq magit-last-seen-setup-instructions "1.4.0")
    :config
    (setq magit-diff-options '("-b")    ; ignore whitespace in diffs
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

    (my-add-binds magit-status-mode-map)

    (bind-keys :map magit-status-mode-map
               ("<C-tab>" . magit-section-cycle)
               ("j" . next-line)
               ("k" . previous-line)
               ("<down>" . magit-goto-next-sibling-section)
               ("<up>" . magit-goto-previous-sibling-section)
               ("C" . magit-commit)
               ("d" . magit-discard-item)
               ("C-=" . magit-diff-working-tree)))

  (use-package git-gutter               ; Show diff with HEAD in fringe
    :bind ("<C-f10>" . git-gutter:toggle))

  (use-package git-timemachine          ; Travel through commit history
    :bind ("<M-f10>" . git-timemachine-toggle))

  (use-package git-commit-mode          ; Git commit messages
    :defer t
    :config
    (evil-bind-key 'git-commit-mode-map
      [remap save-buffer] #'git-commit-commit
      [remap kill-buffer-and-window] #'git-commit-abort)))

;;;; languages

;; various unconfigured language packages
(use-package bbcode-mode :defer t)
(use-package lua-mode :defer t)
(use-package gitignore-mode :defer t)
(use-package markdown-mode :defer t)
(use-package vimrc-mode :mode "[._]?pentadactylrc$" "\\.penta$" :defer t)

(use-package bat                        ; Windows batch script
  :defer t :ensure nil
  :init
  ;; capitalized comment keyword
  (add-hook 'bat-mode-hook (lambda () (setq comment-start "REM "))))

(use-package org                        ; TODO
  ;; TODO modify org-promote so if already max level then demote all subtrees
  :init
  (add-to-list
   'auto-insert-alist
   '(org-mode                           ; TODO TITLE isn't getting inserted
     "#+TITLE:     " (file-name-base buffer-file-name)
     "\n#+AUTHOR:    " user-full-name
     "\n#+EMAIL:     " user-mail-address
     "\n#+SETUP FILE: \"../export.org\""
     "\n\n* "))

  :config
  (defun org-insert-block (&optional name)
    "Insert an org block of NAME, using 'quote' as a default."
    (interactive "sblock: ")
    (let ((name (upcase (if (or (string= name "") (not name))
                            "quote"
                          name))))
      (insert "\n#+BEGIN_" name ":\n\n#+END_" name "\n"))
    (forward-line -3))

  (setq
   org-edit-src-content-indentation 0   ; no initial indent for source code
   org-pretty-entities t                ; render UTF chars and sub/superscript
   org-src-preserve-indentation t       ; preserve source block indentation
   org-imenu-depth 3                    ; larger imenu depth
   org-special-ctrl-a/e t               ; begin/end of line skips tags & stars
   org-special-ctrl-k t                 ; kill lines depending on org context
   org-return-follows-link t            ; follow links with RET
   org-catch-invisible-edits 'smart     ; smart editing of hidden things
   org-todo-keywords '((sequence "☐" "☑" "☒"))
   org-modules '(org-docview org-info org-gnus org-inlinetask)
   org-export-backends '(ascii html odt taskjuggler)
   org-startup-folded t                 ; start buffers with folded headers
   org-src-fontify-natively t           ; syntax highlight code in org buffer
   org-list-allow-alphabetical t)       ; allow single-char alphabetical lists

  ;; set some faces to use mono font
  (dolist (face '(org-block
                  org-code
                  org-table
                  org-special-keyword))
    (apply #'set-face-attribute face nil (my-font 'mono)))

  (bind-keys
   :map org-mode-map
   ("C-c o" . org-clock-in)
   ("C-c O" . org-clock-out)
   ("<S-return>" . org-insert-heading-after-current)
   ("C-c t" . org-todo))

  (use-package org-clock
    :ensure nil :defer t
    :config
    (setq org-clock-persist t           ; save clock and clock history on exit
          org-clock-in-resume t))       ; resume if clocking in with open clock

  (use-package ox-odt
    :ensure nil :defer t
    :config (setq org-odt-preferred-output-format 'doc))

  (use-package org-plus-contrib
    :defer t
    :config

    (use-package ox-taskjuggler
      :ensure nil :defer t
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

  (use-package evil-org                 ; TODO Evil org-mode bindings
    :init (add-hook 'org-mode-hook #'evil-org-mode)
    :config
    ;; TODO get these to work
    (evil-bind-key #'org-mode-map
      "RET" #'org-insert-heading-after-current
      "TAB" #'outline-cycle))

  (defun org-export-to-odt-and-open ()
    "Export the current buffer and open in external application."
    (interactive)
    (add-to-list 'org-file-apps '("\\.odt\\'" . "xdg-open %s"))
    (org-open-file (org-odt-export-to-odt)))

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
  (my-add-list 'generic-extras-enable-list generic-mswindows-modes)

  (define-generic-mode pseudo-mode
    '("#" "'")
    (let ((keywords '("if" "then" "else" "endif"
                      "for" "do" "next"
                      "while" "until" "repeat"
                      "to" "in" "each" "end"
                      "and" "or" "not"
                      "sub" "return")))
      (nconc keywords (mapcar #'upcase keywords)))
    '(("\\([A-Za-z_]+?\\)(.*)" 1 font-lock-function-name-face)
      ("[+\-=]" . font-lock-constant-face)
      ("-?[0-9]" . font-lock-constant-face))
    nil
    '(highlight-numbers-mode)
    "Mode for highlighting pseudocode for SDD work.")

  (use-package conkyrc-mode :disabled t ; System monitor setup language
    :load-path "~/.emacs.d/elisp/" :ensure nil))

(use-package lisp-mode
  :defer t :ensure nil
  :init

  (defun my-imenu-decls ()
    "Add custom declarations to `imenu-generic-expression'."
    (dolist (expr
             `(("Packages" "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)
               ("Headings" "^;;;+ \\(.+\\)$" 1)))
      (add-to-list 'imenu-generic-expression expr)))
  (add-hook 'emacs-lisp-mode-hook #'my-imenu-decls)

  (use-package eldoc                    ; Documentation in echo area
    :defer t
    :init (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
    :config (setq eldoc-idle-delay 0.3))

  (use-package highlight-quoted      ; Faces for lisp quotes and quoted symbols
    :defer t
    :init (add-hook 'emacs-lisp-mode-hook #'highlight-quoted-mode))

  (use-package elisp-slime-nav          ; Navigate elisp documentation
    :defer t
    :init (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
    :config
    (evil-bind-key '(elisp-slime-nav-mode-map help-mode-map)
      "K" #'elisp-slime-nav-describe-elisp-thing-at-point
      "gd" #'elisp-slime-nav-find-elisp-thing-at-point))

  :config (bind-key "C-c C-c" #'eval-defun emacs-lisp-mode-map))

(use-package python
  :defer t
  :config
  (setq python-shell-interpreter "python3") ; use Python 3
  (setq-default
   python-indent-guess-indent-offset nil
   python-indent-offset 4)     ; something is setting this to 2

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
  (defcustom my-sass-output-dir "../"
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
  :config
  (setq web-mode-enable-block-face t
        web-mode-enable-part-face t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        web-mode-enable-current-column-highlight t
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2))

;;; init.el ends here
