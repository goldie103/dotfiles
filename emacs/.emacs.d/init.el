;; Kelly Stewart
;; * setup
;; ** package
(require 'package)
(setq package-enable-at-startup nil         ; we will manually initialize
      load-prefer-newer t)                  ; don't load outdated byte code
(dolist (archive '(("melpa" . "http://melpa.milkbox.net/packages/")
                   ("org" . "http://orgmode.org/elpa/")
                   ("elpy" . "http://jorgenschaefer.github.io/packages/")))
  (add-to-list 'package-archives archive))
(package-initialize)

;; non-melpa packages
(defconst my/dir-elisp (concat user-emacs-directory "elisp/"))
(add-to-list 'load-path my/dir-elisp)

;; ** use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t             ; log message after loading packages
      use-package-always-ensure t)      ; install all packages if necessary
(eval-when-compile (require 'use-package))
;; ** bind-key
(use-package bind-key)

;; ** delight
(use-package delight
  :config
  (delight '((emacs-lisp-mode "Elisp" :major)))
  (delight visual-line-mode))

;; ** auto-compile
(use-package auto-compile
  :config
  (auto-compile-on-load-mode t)
  (auto-compile-on-save-mode t))

;; ** user info
(defconst my/dir (concat user-emacs-directory ".user/"))
(defconst my/dir-autosave (concat my/dir "autosaves/"))
(defconst my/dir-my-elisp (concat my/dir "elisp/"))
(load (concat my/dir-my-elisp "private.el") t) ; passwords and sensitive info
(setq user-full-name "Kelly Stewart")

;; * custom commands
;; ** my/cleanup
(defun my/cleanup ()
  "Perform a lot of stuff on whitespace."
  (interactive)
  (untabify (point-min) (point-max))       ; tabs are evil
  (indent-region (point-min) (point-max))  ; indent properly
  (whitespace-cleanup))                    ; whitespace stuff

;; ** my/source-class
(defvar my/source-class-base-vars
  '((tab-width . 8)
    (eval . (whitespace-mode -1))
    (eval . (linum-mode -1)))
  "Base local variables to apply to directories when `my/apply-source-class' is
called.")

;; Actual class variables
(dir-locals-set-class-variables
 'readonly
 `((nil . ,(append '((buffer-read-only . t)) my/source-class-base-vars))))

(dir-locals-set-class-variables
 'base
 `((nil . ,my/source-class-base-vars)))

;; Need this to allow easily turning read-only off so packages can be installed
;; and updated from package.el
(defun my/source-class-apply (class)
  "Interactively ask for a class to apply to `package-user-dir'. Currently can
be either `base', which turns off whitespace and linum mode, or
`readonly', which does the same as `source' with the addition of setting
the buffer to be read-only."
  (interactive "S")
  (dir-locals-set-directory-class package-user-dir class))

(my/source-class-apply 'readonly)       ; View packages in read-only mode

;; ** esc quits
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

;; ** sudo-save
;; http://www.emacswiki.org/emacs/SudoSave
(defun my/sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; ** text-mode
;; *** my/prose-based-hook
(defun my/prose-based-hook ()
  "Enable and disable modes for prose-based editing."
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
(add-hook 'org-mode-hook #'my/prose-based-hook)

;; *** my/writeroom-effect
(defun my/writeroom-effect ()
  "Effects to apply when using distraction-free writeroom mode. Can be used
outside writeroom mode."
  (interactive)
  (display-time-mode t)
  (display-battery-mode t)
  (setq mode-line-format
        '("%e"
          mode-line-front-space
          mode-line-frame-identification
          mode-line-buffer-identification
          sml/pos-id-separator
          mode-line-position
          sml/pre-modes-separator
          evil-mode-line-tag
          mode-line-modes
          mode-line-misc-info
          mode-line-end-spaces)))

;; * basic settings
;; ** settings
(setq
 read-file-name-completion-ignore-case t ; ignore case in completions
 sentence-end-double-space nil          ; double space is dumb
 tab-always-indent nil                  ; tab inserts a character
 smooth-scroll-margin 3                 ; fewer lines visible at buffer ends
 windmove-wrap-around t                 ; window movements wrap around the frame
 apropos-compact-layout t               ; single line per binding
 woman-imenu t                          ; add contents menu to menubar
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
(set-frame-font "Input Mono Condensed-9") ; main font
(set-face-attribute 'variable-pitch nil :family "Input Serif") ; headline fonts

;; Hide the stupidly specific echo area message
(defun display-startup-echo-area-message () nil)

;; ** modes
(mouse-wheel-mode t)                    ; Mouse wheel enabled
(goto-address-mode t)                   ; Highlight and buttonize URLs
(file-name-shadow-mode t)               ; Dim expanded filename parts
(show-paren-mode t)                     ; Highlight matching parens
(electric-indent-mode t)                ; Auto indent
(electric-pair-mode t)                  ; Auto add parens
(global-prettify-symbols-mode t)        ; Pretty symbols
(add-hook 'prog-mode-hook #'semantic-mode) ; Language-aware manipulations

;; we don't need no stinkin GUI
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
;; ** simple
(use-package simple :ensure nil         ; Misc builtin simple Emacs commands
  :bind ("RET" . newline-and-indent)    ; auto-indent new lines
  :config
  (setq save-interprogram-paste-before-kill t  ; save clipboard to kill-ring
        kill-do-not-save-duplicates t          ; no duplicates in kill-ring
        line-move-visual t)                    ; visual line movement
  (line-number-mode t)                  ; modeline line num
  (column-number-mode t))               ; modeline col num

;; * evil
;; Vim keybindings and modal editing
(use-package evil
  :demand t
  :commands (evil-yank
             evil-set-command-properties
             evil-insert
             evil-echo
             evil-change-to-previous-state
             evil-normal-state-p
             evil-repeat-stop
             evil-delay)
  :functions maybe-exit
  :config
  ;; ** evil/packages
  ;; *** evil-surround
  (use-package evil-surround            ; Manipulate surrounding elements
    :config (global-evil-surround-mode t))

  ;; *** evil-commentary
  (use-package evil-commentary          ; Manipulate comments
    :delight evil-commentary-mode
    :config (evil-commentary-mode t))

  ;; *** evil-args
  (use-package evil-args                ; Manipulate function arguments
    ;; cia - change inner argument
    ;; daa - delete an argument
    :config
    (bind-keys :map (evil-normal-state-map evil-motion-state-map)
               ("L" . evil-forward-arg)
               ("H" . evil-backward-arg)
               ("ga" . evil-jump-out-args))

    (bind-key "a" #'evil-inner-arg evil-inner-text-objects-map)
    (bind-key "a" #'evil-outer-arg evil-outer-text-objects-map))

  ;; *** evil-matchit
  ;; Manipulate tags
  (use-package evil-matchit
    :defines evilmi-may-jump-percentage
    :config
    (setq evilmi-may-jump-percentage nil) ; num% jumps num times
    (global-evil-matchit-mode t))

  ;; *** evil-org
  (use-package evil-org                 ; Evil org-mode bindings
    :delight evil-org-mode
    :config (bind-key "\t" 'org-back-to-heading evil-normal-state-map))

  ;; ** evil/helper functions
  ;; *** jk to exit insert mode
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

  ;; *** insert a single character
  ;; Consider this if dot repetition is needed. Otherwise \ is acceptable.
  ;; (evil-define-command evil-execute-in-insert-state (&optional arg)
  ;;   "Execute the next command in Insert state."
  ;;   (cond
  ;;    ((called-interactively-p 'any)
  ;;     (add-hook 'post-command-hook #'evil-execute-in-insert-state t)
  ;;     (evil-insert 1)
  ;;     (evil-echo "Switched to Insert state for the next command ..."))
  ;;    ((not (eq this-command #'evil-execute-in-insert-state))
  ;;     (remove-hook 'post-command-hook 'evil-execute-in-insert-state)
  ;;     (evil-change-to-previous-state)
  ;;     ;; ensure the command is recorded if we return to normal state
  ;;     (when (evil-normal-state-p)
  ;;       (evil-repeat-stop)))))

  ;; ** evil/bindings
  ;; *** evil/bindings/regular state mappings
  (bind-key "j" #'maybe-exit evil-insert-state-map) ; jk exits insert state

  (bind-keys
   :map global-map
   ;; window movement
   ("C-l" . evil-window-right)
   ("C-k" . evil-window-up)
   ("C-h" . evil-window-left)
   ("C-j" . evil-window-down))

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
   (",x" . eval-region)
   (",X" . eval-buffer)
   ([escape] . keyboard-quit))

  ;; *** evil/bindings/misc
  ;; ESC quits the minibuffer
  (bind-keys :map (minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-isearch-map)
             ("<escape>" . minibuffer-keyboard-quit))

  ;; ** evil/settings
  (setq evil-want-fine-undo t            ; undo tracks delete as well
        evil-want-change-word-to-end nil ; don't let cw behave like ce
        evil-echo-state nil              ; we have state in the modeline anyway
        evil-ex-substitute-global t)     ; global substitutions by default

  ;; set up specific initial modes
  (add-to-list 'evil-motion-state-modes 'package-menu-mode)

  ;; set `evil-shift-width' to major mode indent levels
  (dolist (mode '((shell-script-mode-hook . sh-indentation)
                  (emacs-lisp-mode-hook . lisp-body-indent)))
    (add-hook (cdr mode) (lambda() (setq evil-shift-width (car mode)))))

  (evil-mode t))

;; * faces
;; ** fic-ext
(use-package fic-ext-mode               ; Highlight annotations in comments
  :ensure nil :load-path my/dir-elisp
  :diminish fic-ext-mode
  :functions fic-ext-mode
  :config
  (add-to-list 'fic-highlighted-words "FIXME?")
  (add-hook 'prog-mode-hook #'fic-ext-mode))

;; ** golden-ratio
(use-package golden-ratio               ; Resize windows to golden ratio
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
  :config (add-hook 'prog-mode-hook #'highlight-numbers--turn-on))

;; ** page-break-lines
(use-package page-break-lines          ; Horizontal lines instead of ^L
  :delight page-break-lines-mode
  :functions global-page-break-lines-mode
  :config (global-page-break-lines-mode t))

;; ** nyan-mode
(use-package nyan-mode :disabled t      ; Nyan cat scroll bar
  ;; FIXME doesn't play nice when trying to disable
  :demand t
  :functions my/turn-off-nyan-mode
  :config
  (defun my/turn-off-nyan-mode () (nyan-mode -1))
  (setq-default nyan-wavy-trail t)
  (nyan-mode t))

;; ** rainbow-mode
(use-package rainbow-mode               ; Highlight color codes
  :delight rainbow-mode
  :config (add-hook 'prog-mode-hook #'rainbow-turn-on))

;; ** whitespace
(use-package whitespace :ensure nil     ; Faces for whitespace characters
  :delight whitespace-mode " ς"
  :config
  (setq
   whitespace-action '(auto-cleanup)      ; clean bogus whitespace on write
   whitespace-line-column nil           ; use fill-column value
   ;; only visualise trailing whitespace and tails of long lines
   whitespace-style '(face trailing lines-tail))
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (whitespace-mode t))

;; ** writegood-mode
(use-package writegood-mode             ; Highlight poor forms in writing
  :delight writegood-mode
  :bind (("C-c C-g" . writegood-grade-level)
         ("C-c C-S-g" . writegood-reading-ease)))

;; ** color theme
(use-package solarized-theme :config (load-theme 'solarized-dark))
(use-package zenburn-theme :disabled t :config (load-theme 'hc-zenburn))

;; * interface
;; ** -anzu
(use-package anzu :disabled t)          ; Search matche count in modeline

;; ** -git-messenger
(use-package git-messenger :disabled t) ; Show commit message

;; ** helm
(use-package helm              ; Fuzzy minibuffer completion
  :delight helm-mode
  :demand t
  :commands helm-autoresize-mode
  ;; *** helm/bindings
  :bind (;; Helm replacements
         ("M-x" . helm-M-x)
         ("M-s o" . helm-occur)
         ("C-y" . show-kill-ring)
         ("C-x r i" . helm-register)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("<help> C-a" . helm-apropos)
         ("<help> C-l" . helm-locate-library)
         ;; Additional Helm functions
         ("<help> C-r" . helm-info-at-point)
         ("<help> C-w" . helm-man-woman))
  ;; *** helm/config
  :config
  ;; **** helm/builtins
  ;; ***** helm-adaptive
  (use-package helm-adaptive :ensure nil ; Adaptive search
    :config
    (setq helm-adaptive-history-file (concat my/dir "helm-adaptive-history"))
    (helm-adaptive-mode t))

  ;; ***** helm-buffers
  (use-package helm-buffers :ensure nil
    :config (setq helm-buffers-fuzzy-matching t)) ; fuzzy matching

  ;; ***** helm-commands
  (use-package helm-command :ensure nil
    :config (setq helm-M-x-fuzzy-match t           ; fuzzy matching
                  helm-M-x-always-save-history t)) ; save history even on fail

  ;; ***** helm-config
  (use-package helm-config :ensure nil
    :config (setq helm-command-prefix-key "C-c h"))

  ;; ***** helm-elisp
  (use-package helm-elisp :ensure nil   ; Elisp
    :config (setq helm-apropos-fuzzy-match t
                  helm-lisp-fuzzy-completion))

  ;; ***** helm-files
  (use-package helm-files :ensure nil
    :functions helm-read-file-name my/helm-sudo-save
    :config
    (defalias #'my/sudo-save #'my/helm-sudo-save)
    (defun my/helm-sudo-save ()
      "Saves a file interactively with Helm."
      (interactive)
      (if (not buffer-file-name)
          (write-file (concat "/sudo:root@localhost:"
                              (helm-read-file-name "File:")))
        (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

    (setq helm-ff-auto-update-initial-value t ; auto update when only one match
          helm-ff-file-name-history-use-recentf t ; use recentf
          helm-ff-search-library-in-sexp t    ; get library from functions
          helm-ff-skip-boring-files t         ; skip irrelevant files
          helm-file-cache-fuzzy-match t       ; fuzzy match in file cache
          helm-findutils-search-full-path t   ; search in full path with shell
          helm-findutils-skip-boring-files t  ; skip irrelevant files in shell
          helm-recentf-fuzzy-match t))        ; fuzzy recentf matching

  ;; ***** helm-imenu
  (use-package helm-imenu :ensure nil :config (setq helm-imenu-fuzzy-match t))

  ;; ***** helm-info
  (use-package helm-info :ensure nil :bind ("<help> r" . helm-info-emacs))

  ;; ***** helm-locate
  (use-package helm-locate :ensure nil :config (setq helm-locate-fuzzy-match t))

  ;; ***** helm-mode
  (use-package helm-mode :ensure nil
    :config
    (setq helm-completion-in-region-fuzzy-match t) ; fuzzy matching
    (helm-mode t))

  ;; ***** helm-semantic
  (use-package helm-semantic :ensure nil :config (setq helm-semantic-fuzzy-match t))

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

  ;; **** helm/evil
  (bind-keys :map (evil-normal-state-map
                   evil-motion-state-map)
             ("SPC" . helm-M-x)
             (",b" . helm-mini)
             (",f" . helm-find-files)
             (",,hl" . helm-locate)
             (",,ho" . helm-occur)
             (",,hi" . helm-semantic-or-imenu))

  (bind-keys :map (evil-normal-state-map
                   evil-motion-state-map
                   evil-insert-state-map)
             ("C-y" . helm-show-kill-ring)
             ("C-/" . helm-semantic-or-imenu))

  ;; **** helm/settings and cleanup
  ;; TODO remap `C-j' to something easier to use

  (setq helm-move-to-line-cycle-in-source t     ; cycle on buffer end
        helm-display-header-line nil            ; no header line
        helm-split-window-in-side-p t)          ; split inside current window
  (helm-autoresize-mode t))

;; ** linum
(use-package linum :ensure nil          ; Line numbers
  :config
  (use-package linum-relative :disabled t  ; Relative line numbers
    ;; FIXME causes too many issues, find alternative
    :demand t
    :commands linum-on
    :config
    (set-face-attribute 'linum-relative-current-face nil
                        :inherit 'linum
                        :weight 'bold
                        :background 'unspecified
                        :foreground 'unspecified)
    (setq linum-relative-current-symbol "") ; real line number at cursor line
    (linum-on))
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
  :defines sml/use-projectile-p sml/projectile-replacement-format
  :config
  (setq
   sml/theme 'respectful                      ; modeline respects theme faces
   sml/battery-format "%b%p[%t]"              ; battery format
   sml/mule-info nil                          ; don't show buffer encoding
   sml/projectile-replacement-format "[π:%s]" ; format for projectile prefixes
   sml/use-projectile-p 'after-prefix) ; projectile file prefix if necessary

  (add-to-list 'sml/replacer-regexp-list
               '("^~/dotfiles/emacs/\\.emacs\\.d/" ":ε:"))

  (use-package smart-mode-line-powerline-theme :disabled t
    ;; TODO tell powerline-theme to use better colors good
    :config (setq sml/theme 'powerline))
  (sml/setup))

;; ** wc-goal-mode
(use-package wc-goal-mode               ; Word counts and goal in modeline
  :functions my/wc-goal-format-nochar
  :config
  ;; TODO god fix this mess
  (defun my/wc-goal-format-goal ()
    (interactive) (setq wc-goal-modeline-format "ψ%tw:%w/%gw"))
  (defun my/wc-goal-format-all ()
    (interactive) (setq wc-goal-modeline-format "ψ%tl.%tw.%tc"))
  (defun my/wc-goal-format-nochar ()
    (interactive) (setq wc-goal-modeline-format "ψ%tl.%tw"))

  (my/wc-goal-format-nochar))

;; ** which-func
(use-package which-func :ensure nil    ; Modeline definition name
  :demand t
  :init (which-function-mode t)
  :config (setq which-func-unknown "?"))

;; ** winner
(use-package winner                     ; Window configuration undo
  ;; FIXME? may have to get super bindings working on MS-WINDOWS
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
;; ** -ace-jump-mode
;; Consider as possible alternative to relative line numbers with evil-mode
(use-package ace-jump-mode  :disabled t) ; Jump to specific lines with letters

;; ** -desktop
(use-package desktop :disabled t        ; Save and restore Emacs sessions state
  :config
  (desktop-save-mode t)
  (setq desktop-path `(,(concat my/dir "desktops"))))


;; ** discover-my-major
(use-package discover-my-major          ; List current major mode bindings
  :bind ("<help> M-m" . discover-my-major))

;; ** -guide-key
(use-package guide-key :disabled t      ; Delayed completion for possible keys
  :config (guide-key-mode t))

;; ** help+
;; Enhancements to builtin help
(use-package help+)
(use-package help-fns+)

;; ** ibuffer
(use-package ibuffer
  :bind ("C-x b" . ibuffer)
  :config

  (setq ibuffer-default-shrink-to-minimum-size t ; minimize window size
        ibuffer-old-time 5           ; hours before a buffer is old
        ibuffer-read-only-char ?R    ; char for read-only
        ibuffer-use-other-window t)) ; display ibuffer in another window

;; ** savehist
(use-package savehist                   ; Save command history
  :config
  (setq savehist-file (concat my/dir "savehist")
        history-delete-duplicates t
        savehist-save-minibuffer-history t)
  (savehist-mode t))

;; ** saveplace
(use-package saveplace                  ; Save and restore cursor place in file
  :config (setq-default save-place-file (concat my/dir "places")
                        save-place t))

;; ** projectile
(use-package projectile                    ; Project-based navigation
  :demand t
  :functions (projectile-project-p
              projectile-project-root
              projectile-current-project-dirs)
;; *** projectile/init
  :init
  (setq
   projectile-enable-caching t        ; cache projectile indexes
   projectile-indexing-method 'alien  ; use faster OS methods
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
  :config
  (setq abbrev-file-name (concat my/dir-my-elisp "abbrevs.el")
        save-abbrevs 'silently           ; save abbrevs when saving file
        abbrev-all-caps t)               ; expand in all-caps if written in caps
  (abbrev-mode t))

;; ** auto-indent-mode
(use-package auto-indent-mode           ; Automatic indentation
  ;; TODO get this working with indenting pasted code
  :delight auto-indent-mode
  :demand t
  :commands (auto-indent-global-mode
             auto-indent-remove-advice-p
             auto-indent-is-bs-key-p)
  :config (auto-indent-global-mode t))

;; ** company
(use-package company                    ; Autocompletion in code
  ;; FIXME won't work in elisp buffers
  :demand t
  :config
  (setq company-idle-delay nil            ; attempt completion immediately
        company-show-numbers t            ; show quick-access nums
        company-auto-complete t           ; auto complete on special char insert
        company-lighter-base "Aψ"
        company-selection-wrap-around t)  ; wrap back around when selecting
  (add-hook 'prog-mode-hook #'company-mode))

;; ** -edit-list
(use-package edit-list :disabled t)     ; Makes it easier to edit elisp lists

;; ** -expand-region
;; Seems useful, need to get used to
(use-package expand-region :disabled t  ; Expand functions block at a time
  :bind ("C-=" . er/expand-region))

;; ** flyspell
(use-package flyspell                   ; On-the-fly spell checking
  :delight flyspell-mode
  :config
  ;; *** flyspell-lazy
  (use-package flyspell-lazy   ; Lazier flyspell
    :config
    (setq flyspell-lazy-window-idle-seconds 10) ; idle time before checking
    (flyspell-lazy-mode t))

  ;; *** settings
  (setq flyspell-issue-welcome-flag nil  ; no start message
        flyspell-issue-message-flag nil) ; no checking message

  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;; ** outline
(use-package outline :ensure nil        ; Hierarchical outlining support
  :delight outline-minor-mode " o"
  :config
  ;; *** outline/outshine
  (use-package outshine        ; Org-mode style with outline-mode
    :demand t
    :commands outshine-hook-function
    :config
    (setq outshine-fontify-whole-heading-line t
          outshine-org-style-global-cycling-at-bob-p t
          outshine-preserve-delimiter-whitespace t)
    (add-hook 'outline-minor-mode-hook #'outshine-hook-function))

  ;; *** outline/evil
  ;; Mostly adapted from evil-org
  (bind-keys :map (evil-normal-state-map
                   evil-motion-state-map)
             ("gh" . outline-up-heading)
             ("gj" . outline-forward-same-level)
             ("gk" . outline-backward-same-level)
             ("gl" . outline-next-visible-heading)
             ([tab] . outline-cycle)    ; "\t" doesn't work for some reason?
             ("za" . outline-toggle-children)
             ("C-c v" . outline-mark-subtree))

  (bind-keys :map (evil-normal-state-map
                   evil-motion-state-map
                   evil-insert-state-map)
             ("C-c o" . outline-insert-heading)
             ("C-c h" . outline-promote)
             ("C-c l" . outline-demote))

  ;; *** outline/hooks
  (add-hook 'prog-mode-hook #'outline-minor-mode)
  ;; I never want to call outline-mode as a major mode, so alias it to
  ;; always call the minor mode function.
  (defalias #'outline-mode #'outline-minor-mode))


;; ** smartparens
(use-package smartparens                ; Balanced parens
  :delight smartparens-mode
  :demand t
  :commands sp-insert-pair
  :config
  ;; TODO add evil bindings for sp commands
  (use-package smartparens-config :ensure nil)
  (smartparens-global-mode t)
  (show-smartparens-mode t))

;; ** typo
(use-package typo :delight typo-mode)  ; Insert typographical characters

;; ** undo-tree
(use-package undo-tree         ; Branching undo tree
  :delight undo-tree-mode
  :config
  (bind-key "C-x u" 'undo-tree-visualize undo-tree-map)
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,(expand-file-name
                                                     "undo-history" my/dir)))))

;; ** yasnippet
(use-package yasnippet
  :defer t
  :config (setq yas-snippet-dirs
                `(,(concat my/dir "snippets/") yas-installed-snippets-dir)))

;; * files
;; ** -auto-package-update
(use-package auto-package-update :disabled t) ; Seems useful, need to look up

;; ** autorevert
(use-package autorevert :ensure nil      ; Auto revert to external modifications
  :delight autorevert-mode
  :config
  (setq global-auto-revert-non-file-buffers t) ; auto refresh buffer
  (global-auto-revert-mode t))


;; ** dired
(use-package dired :ensure nil          ; Emacs file browser
  :config
  (use-package dired+                   ; Dired extensions and syntax highlight
    :config (setq diredp-dwim-any-frame-flag t)) ; allow dwim target other frame

  (add-to-list 'evil-emacs-state-modes 'dired-mode) ; Evil initial state

  (setq
   dired-auto-revert-buffer t           ; auto revert dired buffer on visit
   dired-backup-overwrite 'always       ; always make backup before overwrite
   dired-dwim-target t                  ; target other dired directory if exists
   dired-isearch-filenames 'dwim        ; isearch filenames if point on filename
   dired-no-confirm '(copy move symlink) ; don't confirm these operations
   dired-recursive-copies 'always       ; recursive copy by default
   dired-recursive-deletes 'top         ; confirm recursive delete
   dired-listing-switches "-lha")       ; human-readable listing
  (add-hook 'dired-mode-hook #'auto-revert-mode))

;; ** files
(use-package files :ensure nil          ; File-related actions
  :config
  (setq
   require-final-newline t              ; newline at end of file
   large-file-warning-threshhold 20000000 ; only warn at 20MB files
   find-file-visit-truename t           ; silently follow symlinks
   view-read-only t                     ; view read-only files in view-mode
   ;; *** autosave
   auto-save-list-file-prefix my/dir-autosave ; autosave location
   auto-save-file-name-transforms `((".*" ,my/dir-autosave t))
   ;; *** backups
   backup-by-copying t                  ; copy file into backup dir
   backup-directory-alist `(("." . ,(expand-file-name "backups" my/dir)))
   version-control t                    ; add version numbers
   delete-old-versions t                ; delete old backups silently
   kept-old-versions 5                  ; old versions to keep
   kept-new-versions 8)                 ; new versions to keep

  ;; *** working directory location
  (cd "~"))                             ; start in home dir

;; ** image
(use-package image :ensure nil          ; Emacs image viewing
  ;; Don't normally open images in Emacs, but when I do I don't want them to
  ;; open in an external process.
  :config
  ;; *** image-dired
  (use-package image-dired :ensure nil  ; Image manipulation in dired
    :config
    (setq
     ;; Save everything inside `my/dir'
     image-dired-temp-rotate-image-file (concat image-dired-dir ".tmp-rotate")
     image-dired-temp-image-file (concat image-dired-dir ".tmp")
     image-dired-gallery-dir (concat image-dired-dir ".gallery")
     image-dired-db-file (concat image-dired-dir ".db")
     image-dired-dir (concat my/dir "image-dired/")))
  ;; *** image settings
  (setq image-animate-loop t)                ; loop animated images
  (auto-image-file-mode t))                  ; view images in Emacs
;; ** -pandoc
(use-package pandoc-mode :disabled t    ; Markup conversion tool
  :config (add-hook 'org-mode-hook 'pandoc-mode))

;; ** real-auto-save
(use-package real-auto-save    ; Auto save buffers
  :delight real-auto-save-mode " α"
  :demand t
  :commands real-auto-save-mode
  :config (real-auto-save-mode t))


;; ** recentf
(use-package recentf                    ; List recent files
  :config
  (setq recentf-save-file (concat my/dir "recentf")
        recentf-menu-filter 'recentf-sort-ascending
        recentf-max-saved-items 50)
  (recentf-mode t))

;; ** uniquify
(use-package uniquify :ensure nil           ; Distinguish buffers with same name
  :config (setq uniquify-buffer-name-style 'forward
                uniquify-trailing-separator-p t)) ; add separator to dired names

;; * tools
;; ** cus-edit
(use-package cus-edit :ensure nil       ; Customize user variables
  :defer t
  :defines my/custom-file
  :init
  (add-to-list 'evil-motion-state-modes 'Custom-mode) ; Start in motion state
  ;; define and load custom file
  (defconst my/custom-file (concat my/dir-my-elisp "custom.el"))
  (load my/custom-file 'no-error 'no-message)
  :config (setq custom-file my/custom-file         ; where to save customization
                custom-buffer-done-kill t          ; kill buffer when closing
                custom-raised-buttons nil          ; use brackets for buttons
                ;; show real variable names
                custom-unlispify-tag-names nil
                custom-unlispify-menu-entries nil))

;; ** flycheck
(use-package flycheck          ; On-the-fly syntax checking
  :demand t
  :functions my/flycheck-mode-line-status flycheck-mode-line-status-text
  :commands flycheck-count-errors
  :config
  ;; *** flycheck modeline
  (defalias #'flycheck-mode-line-status-text #'my/flycheck-mode-line-status)
  (defun my/flycheck-mode-line-status (&optional status)
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

  ;; *** flycheck-tip
  (use-package flycheck-tip    ; display errors by popup
    ;; FIXME may or may not clobber company popups
    :commands flycheck-tip-use-timer
    :config (flycheck-tip-use-timer 'verbose))

  ;; *** flycheck evil bindings
  (bind-keys :map evil-normal-state-map
             (",,F" . helm-flycheck)
             (",,fj" . flycheck-next-error)
             (",,fl" . flycheck-previous-error))

  ;; *** flycheck settings
  (add-hook 'prog-mode-hook #'flycheck-mode))


;; ** lorem-ipsum
(use-package lorem-ipsum)              ; Insert filler text

;; ** magit
(use-package magit             ; Git version control management
  :bind (("C-x m" . magit-status))
  :init (setq magit-last-seen-setup-instructions "1.4.0") ; hide startup msg
  :config (setq magit-save-some-buffers 'dontask ; don't ask before saving
                magit-diff-options '("-b")       ; ignore whitespace
                magit-auto-revert-mode nil))     ; don't revert

;; ** org
(use-package org
  :commands (org-bookmark-jump-unhide
             org-babel-where-is-src-block-head
             org-edit-src-code
             org-edit-src-exit)
  :init

  (use-package org-indent :ensure nil
    :delight org-indent-mode)

  (set-face-attribute 'org-done nil :strike-through t)
  (set-face-attribute 'org-headline-done nil :strike-through t)

  (setq
   org-startup-folded t
   org-src-fontify-natively t           ; syntax highlight code in org buffer
   org-fontify-done-headline t          ; fontify whole done headline
   org-list-allow-alphabetical t))      ; allow single-char alphabetical lists

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
                ispell-personal-dictionary "~/.local/share/aspell/aspell.pws"))

;; * languages
;; ** BBCode
(use-package bbcode-mode )

;; ** generic-x
(use-package generic-x :ensure nil      ; Collection of generic modes
  :config (setq generic-define-mswindows-modes t))   ; define MS-Windows modes

;; ** gitignore
(use-package gitignore-mode)

;; ** lisp
;; *** eldoc-mode
(use-package eldoc                      ; Documentation in echo area
  :config
  (setq eldoc-idle-delay 0.3)
  (add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode t))

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
  :config
  (use-package elpy
    :config
    (nbutlast elpy-modules)            ; disable yasnippet
    (elpy-enable))
  (setq
   python-shell-interpreter "python3"  ; use Python 3
   python-indent-offset 4))            ; this is being overwritten by something?

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

;; Local Variables:
;; evil-args-delimiters: (" ")
;; no-byte-compile: t
;; outline-regexp: " *;; [*]\\{1,8\\} "
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
