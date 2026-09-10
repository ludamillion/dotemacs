;;; prose-mode.el --- Center-column prose editing -*- lexical-binding: t; -*-

;;; Commentary:

;; A buffer-local minor mode for prose-oriented editing.  Centers the
;; visible text around `esprit-emacs/prose-fill-column' columns by
;; widening the window margins, similar to `olivetti-mode'.  Buffers
;; with `prose-mode' enabled are exempt from the default 2-column left
;; margin set by `esprit-emacs/set-default-window-margins' in init.el.

;;; Code:

(eval-when-compile
  (declare-function esprit-emacs/set-default-window-margins nil))

(defvar esprit-emacs/prose-fill-column 90
  "Desired text width, in columns, for buffers with `prose-mode' enabled.")

(defvar-local esprit-emacs/center-document-mode nil
  "Non-nil when the buffer's text should be centered instead of using the
default window margins.  Set automatically by `prose-mode'.")

(define-minor-mode prose-mode
  "Toggle centered-text prose editing in the current buffer."
  :lighter " Prose"
  (setq esprit-emacs/center-document-mode prose-mode)
  (esprit-emacs/set-default-window-margins))

(provide 'prose-mode)

;;; prose-mode.el ends here
