;;; smart-mode-line-my-theme.el --- smart-mode-line theme -*- lexical-binding: t -*-

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/smart-mode-line
;; Package-Version: 20150426.910
;; Version: 0.1a
;; Package-Requires: ((emacs "24.3") (powerline "2.2") (smart-mode-line "2.5"))
;; Keywords: mode-line faces themes
;; Separator: -

;;; Code:

;;;; setup
(deftheme smart-mode-line-my
  "My smart-mode-line theme.")

(require 'powerline)

;;;; helpers
(setq powerline-default-separator 'utf-8)
(setq powerline-utf-8-separator-left #x25d7
      powerline-utf-8-separator-right #x25d6)

(defcustom my-theme-sml-evil
  '(:eval (evil-state-property evil-state :tag t))
  "Evil mode-line tag."
  :group 'face)

(defalias #'evil-generate-mode-line-tag (lambda (&optional _arg) (symbol-value 'my-theme-sml-evil))
  "Use `my-theme-sml-evil-tag' as the mode-line evil tag.")

(defun my-theme-sml-sep (dir before after)
  `((:propertize " " face ,before)
    (:eval
     (propertize
      " " 'display
      (funcall
       #',(intern (format "powerline-%s-%s"
                          powerline-default-separator
                        (symbol-name dir)))
       ',before ',after)))
    (:propertize " " face ,after)))

(let* ((face (if (eq (frame-selected-window) (selected-window))
                 'mode-line 'mode-line-inactive))
       (bg-0 (face-attribute 'mode-line-emphasis :background))
       (fg-0 (face-attribute 'mode-line-emphasis :foreground))
       (fg-1 (face-attribute face :foreground))
       (bg-1 (face-attribute face :background)))
  (custom-theme-set-faces
   'smart-mode-line-my
;;;; faces
;;;;; base
   `(mode-line ((t :foreground ,fg-1 :background ,bg-1)))
   `(sml/global ((t :inherit ,face)))
;;;;; front space, mule info, client, modified, remote, frame identification
   `(sml/line-number ((t :inherit sml/global)))
   `(sml/col-number ((t :inherit sml/line-number)))
   `(sml/remote ((t :inherit sml/line-number)))
   `(sml/numbers-separator ((t :inherit sml/line-number :background nil)))
   `(sml/client ((t :inherit sml/line-number)))
   `(sml/mule-info ((t :inherit sml/line-number)))
   `(sml/not-modified ((t :inherit sml/line-number)))
   `(sml/modified ((t :inherit (sml/not-modified warning))))
   `(sml/read-only ((t :inherit sml/line-number)))
;;;;; buffer identification
   `(sml/prefix ((t :inherit (sml/global font-lock-preprocessor-face) :background ,bg-0 :foreground ,fg-1)))
   `(sml/projectile ((t :inherit sml/prefix)))
   `(sml/folder ((t :inherit sml/global :background ,bg-0 :foreground ,fg-0)))
   `(sml/filename ((t :inherit (sml/global dired-directory) :background ,bg-0)))
   `(sml/sudo ((t :inherit sml/outside-modified :background ,bg-0)))
;;;;; position, evil tag, vc
   `(sml/name-filling ((t :inherit sml/global :weight normal)))
   `(sml/position-percentage ((t :inherit sml/global :weight normal)))
   `(sml/process ((t :inherit sml/global)))
   `(sml/vc ((t :inherit font-lock-type-face)))
   `(sml/vc-edited ((t :inherit font-lock-doc-face)))
   `(sml/git ((t :inherit sml/vc)))
;;;;; major mode
   `(sml/modes ((t :inherit (sml/global font-lock-variable-name-face)
                   :background ,bg-0
                   :inverse-video t)))
;;;;; minor-modes
   `(sml/minor-modes ((t :inherit sml/folder)))
;;;;; misc, end spaces
   `(sml/discharging ((t :background ,fg-0 :inherit (sml/global warning))))
   `(sml/time ((t :background ,fg-0 :inherit sml/global))))
;;; vars
  (custom-theme-set-variables
   'smart-mode-line-my
   '(sml/pre-id-separator '(:propertize "  " face mode-line-emphasis))
   '(sml/pos-id-separator '(:propertize "  " face mode-line-emphasis))
   '(sml/pre-modes-separator '(:propertize "  " face sml/modes))
   '(sml/pre-minor-modes-separator '(:propertize "  " face sml/modes))
   '(sml/pos-minor-modes-separator '(:propertize "  " face mode-line-emphasis))
   '(my-theme-sml-evil '((:propertize "  " face mode-line-emphasis)
                         (:propertize (:eval (substring (evil-state-property evil-state :tag t) 2 -2))
                                      face mode-line-emphasis)
                         (:propertize "  " face mode-line-emphasis)))
   ;; `(my-theme-sml-evil
   ;;   `(,@(my-theme-sml-sep 'left 'mode-line 'sml/minor-modes)
   ;;     (:eval (substring (evil-state-property evil-state :tag t) 2 -2))
   ;;     ,@(my-theme-sml-sep 'right 'sml/minor-modes 'mode-line )))
   ;; `(sml/pre-id-separator ',(my-theme-sml-sep 'right face 'sml/prefix))
   ;; `(sml/pos-id-separator ',(my-theme-sml-sep 'left 'sml/prefix face))
   ;; `(sml/pre-modes-separator ',(my-theme-sml-sep 'right face 'sml/modes))
   ;; `(sml/pre-minor-modes-separator ',(my-theme-sml-sep 'left 'sml/modes 'sml/minor-modes))
   ;; `(sml/pos-minor-modes-separator ',(my-theme-sml-sep 'left 'sml/minor-modes face))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'smart-mode-line-my)
;;; smart-mode-line-my-theme.el ends here.

;; Local Variables:
;; eval: (rainbow-mode t)
;; eval: (whitespace-mode -1)
;; no-byte-compile: t
;; End:
