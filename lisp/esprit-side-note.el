;;; esprit-side-note.el --- Show designated buffers in a pinned side window -*- lexical-binding: t; -*-

;;; Commentary:

;; Buffer-local minor mode + predicate list.  Any buffer for which a
;; predicate in `esprit-side-note-predicates' returns non-nil is shown
;; in a pinned side window instead of taking over the current window.

;;; Code:

(defgroup esprit-side-note nil
  "Show designated buffers in a pinned side window."
  :group 'convenience)

(defcustom esprit-side-note-predicates nil
  "List of functions, each called with no args in the buffer being visited.
If any returns non-nil, that buffer is shown in the side window."
  :type '(repeat function))

(defcustom esprit-side-note-side 'right
  "Side of the frame the side-note window is placed on."
  :type 'symbol)

(defcustom esprit-side-note-slot 1
  "Slot of the side-note window."
  :type 'integer)

(defcustom esprit-side-note-width 80
  "Width of the side-note window."
  :type 'integer)

(define-minor-mode esprit-side-note-mode
  "Marker mode for buffers that should live in the side window."
  :lighter " ⌗"
  :global nil)

(defun esprit-side-note--maybe-enable ()
  "Enable `esprit-side-note-mode' if any predicate matches this buffer."
  (when (seq-some #'funcall esprit-side-note-predicates)
    (esprit-side-note-mode 1)))

(add-hook 'find-file-hook #'esprit-side-note--maybe-enable)

(add-to-list 'display-buffer-alist
             `((lambda (buf _act) (buffer-local-value 'esprit-side-note-mode (get-buffer buf)))
               (display-buffer-in-side-window)
               (side . ,esprit-side-note-side)
               (slot . ,esprit-side-note-slot)
               (window-width . ,esprit-side-note-width)
               (window-parameters
                (no-delete-other-windows . t))))

(provide 'esprit-side-note)
;;; esprit-side-note.el ends here
