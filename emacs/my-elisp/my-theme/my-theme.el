;;; my-theme.el --- my Darcula-inspired color theme
;;; Commentary:
;; Mostly sourced from https://github.com/fommil/darcula-theme-emacs
;;; Code:
(deftheme my)

;;; fonts
(defvar my-fonts
  '((proportional . ("Fira Sans" "Input Sans Condensed" "Input Sans"
                     "DejaVu Sans" "Calibri" "Arial" "Sans Serif"))
    (mono . ("Envy Code R" "Input Mono Condensed" "Input Mono"
             "DejaVu Sans Mono" "Consolas" "Courier" "Monospace" "Fixed")))
  "Alist of font types paired with an ordered list of preferences.
Used with `my-font' to get the first valid entry of each font pairing.")

(add-to-list 'my-fonts
             `(header . ,(append '("Fantasque Sans Mono")
                                 (cdr (assoc 'proportional my-fonts)))))

(defun my-font (font)
  "Return the first valid entry for FONT in `my-fonts'."
  (let (found)
    (dolist (family (cdr (assoc font my-fonts)))
      (dolist (candidate `(,family
                           ;; try unspaced name to allow for odd font naming
                           ,(replace-regexp-in-string " " "" family)))
        (when (and (find-font (font-spec :name candidate)) (not found))
          (setq found candidate))))
    found))

(defun my-font-use-proportional ()
  "Use proportional font for current buffer."
  (interactive)
  (face-remap-add-relative 'default :family (my-font 'proportional)))
(add-hook 'text-mode-hook #'my-font-use-proportional)

;;; colors
(let* ((bg "#2b2b2b")
       (bg0 "#202020")
       (bg1 "#313335")
       (bg2 "#404040")
       (fg "#a9b7c6")
       (fg0 "#808080")
       (fg1 "#465a61")
       (fg2 "#dcdcdc")
       (yellow "#fec66c")
       (orange "#cc7832")
       (orange0 "#a57705")
       (red "#bd3612")
       (magenta "#c61b6e")
       (violet "#ae529f")
       (violet0 "#8e8cd3")
       (violet1  "#9876aa")
       (blue "#6897bb")
       (blue0 "#5859b7")
       (blue1 "#99daf9")
       (cyan  "#259185")
       (cyan0 "#4e807d")
       (green "#a6c25c")
       (green0 "#629755"))
;;; faces
  (custom-theme-set-faces
   'my
;;;; basic colouring
   `(default ((t (:background ,bg :foreground ,fg
                              :family ,(my-font 'mono) :height 100))))
   `(shadow ((t (:foreground ,fg0))))
   `(match ((t (:background ,fg :foreground ,fg0 :weight bold))))
   `(cursor ((t (:background ,fg0))))
   `(region ((t (:foreground ,fg1 :background ,bg2))))
   `(secondary-selection ((t (:background ,bg2))))
   `(escape-glyph ((t (:foreground ,orange0))))
   `(fringe ((t (:background nil :foreground ,fg1))))
   `(header-line ((t (:foreground ,fg1 :background ,bg2 :inherit mode-line))))
   `(highlight ((t (:background ,bg1))))
   `(link ((t (:underline t :foreground ,blue1))))
   `(link-visited ((t (:underline t :foreground ,blue1))))
   `(success ((t (:foreground ,green))))
   `(warning ((t (:underline (:style wave :color ,yellow)))))
   `(error ((t (:foreground ,red))))
   `(lazy-highlight ((t (:foreground ,yellow :background ,bg1))))
   `(widget-field ((t (:background ,fg))))
   `(button ((t (:inherit link))))
   `(variable-pitch ((t (:height 1.1 :family ,(my-font 'header)))))
;;;; font lock
   `(font-lock-builtin-face ((t (:foreground ,violet))))
   `(font-lock-comment-delimiter-face ((t (:slant normal :inherit font-lock-comment-face))))
   `(font-lock-comment-face ((t (:foreground ,fg0 :slant italic))))
   `(font-lock-constant-face ((t (:foreground ,blue :weight normal))))
   `(font-lock-doc-face ((t (:foreground ,green0))))
   `(font-lock-function-name-face ((t (:weight bold :foreground nil))))
   `(font-lock-keyword-face ((t (:foreground ,orange :weight bold))))
   `(font-lock-negation-char-face ((t (:underline t))))
   `(font-lock-preprocessor-face ((t (:foreground ,yellow))))
   `(font-lock-string-face ((t (:foreground ,green))))
   `(font-lock-type-face ((t (:foreground ,violet0))))
   `(font-lock-variable-name-face ((t (:foreground ,cyan))))
   `(font-lock-warning-face ((t (:underline (:style wave :color ,red)))))
;;;; compilation
   `(compilation-info ((t (:foreground ,green :underline nil))))
   `(compilation-mode-line-fail ((t (:inherit error :weight bold))))
   `(compilation-error ((t (:inherit error))))
;;;; isearch
   `(isearch ((t (:foreground ,red :background ,bg1 :inverse-video t))))
   `(isearch-fail ((t (:inherit isearch))))
;;;; modeline
   `(mode-line ((t (:foreground nil :background ,bg0))))
   `(mode-line-inactive ((t (:foreground ,fg0 :background ,bg1))))
   `(mode-line-emphasis ((t (:weight bold))))
;;;; outline
   `(outline-1 ((t (:inherit variable-pitch :foreground ,red))))
   `(outline-2 ((t (:inherit variable-pitch :foreground ,orange))))
   `(outline-3 ((t (:inherit variable-pitch :foreground ,yellow))))
   `(outline-4 ((t (:inherit variable-pitch :foreground ,green))))
   `(outline-6 ((t (:inherit variable-pitch :foreground ,blue1))))
   `(outline-5 ((t (:inherit variable-pitch :foreground ,cyan))))
   `(outline-6 ((t (:inherit variable-pitch :foreground ,violet))))
   `(outline-7 ((t (:inherit variable-pitch :foreground ,magenta))))
;;;; show-paren
   `(show-paren-match ((t (:foreground ,magenta :weight bold))))
   `(show-paren-mismatch ((t (:foreground ,magenta :inverse-video t))))
;;;; misc
   `(minibuffer-prompt ((t (:weight bold :foreground ,cyan))))
   `(hl-line ((t (:inherit highlight))))
   `(trailing-whitespace ((t (:inherit warning))))
   `(whitespace-trailing ((t (:inherit warning))))
;;;; ace-jump-mode
   `(ace-jump-face-foreground ((t (:foreground ,magenta))))
;;;; auto-complete
   `(ac-candidate-face ((t (:inherit mode-line))))
   `(ac-selection-face ((t (:inherit highlight))))
;;;; company
   `(company-echo-common ((t (:foreground ,red))))
   `(company-preview ((t (:inherit company-echo-common))))
   `(company-preview-common ((t (:inherit company-preview))))
   `(company-preview-search ((t (:inherit company-preview))))
   `(company-scrollbar-bg ((t (:inherit mode-line-inactive))))
   `(company-scrollbar-fg ((t (:inherit tooltip))))
   `(company-template-field ((t (:foreground ,yellow :background ,bg1
                                             :inverse-video t))))
   `(company-tooltip ((t (:inherit mode-line))))
   `(company-tooltip-selection ((t (:inherit highlight))))
   `(company-tooltip-common ((t (:inherit mode-line))))
   `(company-tooltip-common-completion ((t (:inherit mode-line))))
   `(company-tooltip-common-selection ((t (:foreground ,red))))
   `(company-tooltip-annotation ((t (:inherit mode-line))))
;;;; diredp
   `(diredp-file-name ((t (:foreground ,violet0))))
   `(diredp-file-suffix ((t (:foreground ,blue))))
   `(diredp-symlink ((t (:foreground ,green))))
   `(diredp-executable-tag ((t (:foreground ,red))))
   `(diredp-mode-line-flagged ((t (:foreground ,red))))
   `(diredp-autofile-name ((t (:inherit diredp-file-name :inverse-video t))))
   `(diredp-compressed-file-suffix ((t (:foreground ,blue))))
   `(diredp-deletion-file-name ((t (:foreground ,magenta))))
   `(diredp-deletion ((t (:inverse-video t :inherit diredp-deletion-file-name))))
   `(diredp-dir-heading ((t (:inherit font-lock-keyword-face :weight bold))))
;;;; flymake
   `(flymake-errline ((t (:inherit error))))
   `(flymake-warnline ((t (:inherit warning))))
;;;; git-gutter
   `(git-gutter:modified ((t (:foreground ,violet))))
   `(git-gutter:added ((t (:foreground ,green0))))
   `(git-gutter:deleted ((t (:foreground ,orange))))
;;;; guide-key
   `(guide-key/key-face ((t (:foreground ,green))))
   `(guide-key/prefix-command-face ((t (:foreground ,green0 :slant italic))))
   `(guide-key/highlight-command-face ((t (:foreground ,cyan0))))
;;;; helm
   `(helm-header ((t (:inherit font-lock-function-name-face
                               :inverse-video t :weight bold))))
   `(helm-source-header ((t (:inherit font-lock-keyword-face
                                      :inverse-video t :weight bold))))
   `(helm-M-x-key ((t (:foreground ,orange :underline t))))
   `(helm-match ((t (:background ,bg0 :foreground nil))))
   `(helm-selection ((t (:background ,bg2))))
   `(helm-visible-mark ((t (:inherit dired-mark))))
   `(helm-candidate-number ((t (:foreground ,fg2))))
   `(helm-separator ((t (:foreground ,red))))
   `(helm-time-zone-current ((t (:foreground ,green0))))
   `(helm-time-zone-home ((t (:foreground ,red))))
   `(helm-bookmark-addressbook ((t (:foreground ,orange))))
   `(helm-bookmark-directory ((t (:foreground nil :background nil :inherit helm-ff-directory))))
   `(helm-bookmark-file ((t (:foreground nil :background nil :inherit helm-ff-file))))
   `(helm-bookmark-gnus ((t (:foreground ,magenta))))
   `(helm-bookmark-info ((t (:foreground ,green))))
   `(helm-bookmark-man ((t (:foreground ,orange0))))
   `(helm-bookmark-w3m ((t (:foreground ,green0))))
   `(helm-buffer-not-saved ((t (:foreground ,red))))
   `(helm-buffer-process ((t (:foreground ,cyan))))
   `(helm-buffer-saved-out ((t (:foreground nil :background nil :inherit warning))))
   `(helm-buffer-size ((t (:foreground ,fg0))))
   `(helm-ff-file ((t (:foreground ,blue))))
   `(helm-ff-directory ((t (:foreground nil :background nil :inherit dired-directory))))
   `(helm-ff-executable ((t (:foreground ,green0))))
   `(helm-ff-invalid-symlink ((t (:inherit error))))
   `(helm-ff-symlink ((t (:foreground ,green))))
   `(helm-ff-prefix ((t (:foreground ,violet0))))
   `(helm-grep-cmd-line ((t (:foreground ,cyan))))
   `(helm-grep-file ((t (:inherit helm-ff-file))))
   `(helm-grep-finish ((t (:foreground ,green))))
   `(helm-grep-lineno ((t (:foreground ,fg0))))
   `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
   `(helm-grep-running ((t (:foreground ,red))))
   `(helm-moccur-buffer ((t (:foreground ,cyan0))))
   `(helm-lisp-completion-info ((t (:foreground ,red))))
   `(helm-lisp-show-completion ((t (:inverse-video t))))
;;;; TODO magit
   `(magit-tag ((t (:foreground ,orange0 :inverse-video t))))
   `(magit-branch ((t (:foreground ,blue))))
   `(magit-log-sha1 ((t (:foreground ,orange))))
   `(magit-section-title ((t (:foreground ,yellow :inverse-video t))))
   `(magit-process-ng ((t (:foreground ,red))))
   `(magit-process-ok ((t (:foreground ,green))))

;;;; popup
   `(popup-menu-face ((t (:inherit mode-line))))
   `(popup-menu-mouse-face ((t (:inherit mode-line))))
   `(popup-menu-selection-face ((t (:inherit highlight))))
   `(popup-face ((t (:inherit mode-line))))
   `(popup-menu-summary-face ((t (:inherit mode-line :weight bold))))
   `(popup-summary-face ((t (:inherit mode-line :weight bold))))
   `(popup-tip-face ((t (:foreground ,green :background ,bg :inverse-video t))))
;;;; smart-mode-line
   `(sml/modified ((t (:foreground ,red))))
   `(sml/discharging ((t (:foreground ,violet))))
;;;; smartparens
   `(sp-show-pair-match-face ((t (:inherit show-paren-match))))
   `(sp-show-pair-mismatch-face ((t (:inherit show-paren-mismatch))))
;;;; tooltip
   `(tooltip ((((class color)) (:foreground ,yellow :background ,bg
                                            :inverse-video t))))
;;;; whitespace
   `(whitespace-empty ((t (:background ,cyan0))))
   `(whitespace-indentation ((t (:inherit whitespace-empty))))
   `(whitespace-space-after-tab ((t (:inherit whitespace-empty))))
   `(whitespace-space-before-tab ((t (:background ,cyan))))
   `(whitespace-line ((t (:underline (:style wave :color ,magenta)))))
;;; Cleanup
   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'my)

