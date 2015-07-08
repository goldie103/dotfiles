;;; my-wc-mode.el --- show wc-like information in status bar

;;; Commentary:
;; A simple minor-mode to display the length of the buffer in the status bar.
;; Mostly adapted from wc-mode.el

;;; Code:

(defgroup my/wc-mode nil
  "Custom options for my/wc-mode"
  :group 'emacs)

(defcustom wc-mode-string " %d,%d,%d"
  "Format to use for displaying information in modeline.
This only takes effect if used in wc-mode-format-region."
  :group 'my/wc-mode)

(defcustom wc-mode-format-with-buffer
  (format wc-mode-string
          (point-max)
          (count-words-region (point-min) (point-max))
          (line-number-at-pos (point-max)))
  "Function to use to display wc modeline text.
If you edit this, you may also want to edit `wc-mode-format-with-region'."
  :group 'my/wc-mode)

(defcustom wc-mode-format-with-region
  (format wc-mode-string
          (abs (- (point) (mark)))
          (count-words-region (region-beginning) (region-end))
          (abs (- (line-number-at-pos (point))
                  (line-number-at-pos (mark)))))
  "Function to use to display wc modeline text if a region is active.
If you edit this, you may also want to edit `wc-mode-format-with-buffer'"
  :group 'my/wc-mode)

;; add length display to mode-line construct
(setq mode-line-position (assq-delete-all 'my/wc-mode mode-line-position))

(setq
 mode-line-position
 (append
  mode-line-position
  '((my/wc-mode
     (6 (:eval (if (use-region-p)
                   (wc-mode-format-with-region) (wc-mode-format-with-buffer))))
     nil))))


(define-minor-mode my/wc-mode
  "Toggle word-count mode.
With no argument, this command toggles the mode.
A non-null prefix argument turns the mode on.
A null prefix argument turns it off.

When enabled, the total number of characters, words, and lines is
displayed in the mode-line.")

(provide 'my/wc-mode)
;;; my-wc-mode.el ends here
