;;; flymake-follow-mode.el --- Have the Flymake diagnostic get to the point.

;; Author: Luke Inglis <ld.inglis@gmail.com>
;; Version: 1.0
;; Keywords: flymake
;; URL: https://github.com/aaronbieber/octopress.el

;;; Commentary:

;; This code has been copied whole-cloth from a blog post by James Dyer
;; which can be found here https://www.emacs.dyerdwelling.family/emacs/20260409061315-emacs--wiring-flymake-diagnostics-into-a-follow-mode/
;;
;; It is a small minor mode which causes Flymake to highlight
;; diagnostics in the diagnostics buffer when the point is nearby.
;;
;; Is also adds a convenience function to toggle the diagnostic
;; buffer visibility.

;;; Code:

(defun flymake-follow--diag-buffer ()
  "Return the visible Flymake diagnostics buffer, or nil."
  (seq-some (lambda (b)
              (and (with-current-buffer b
                     (derived-mode-p 'flymake-diagnostics-buffer-mode))
                   (get-buffer-window b)
                   b))
            (buffer-list)))

(defun flymake-follow-toggle-diagnostics ()
  "Toggle the Flymake diagnostics buffer."
  (interactive)
  (let ((buf (flymake-follow--diag-buffer)))
    (if buf
        (quit-window nil (get-buffer-window buf))
      (flymake-show-buffer-diagnostics)
      (flymake-follow-sync-diagnostics))))

(defvar flymake-follow--sync-overlay nil
  "Overlay used to highlight the current entry in the diagnostics buffer.")

(defun flymake-follow-sync-diagnostics ()
  "Highlight the diagnostics buffer entry matching the error at point."
  (when-let* ((buf (flymake-follow--diag-buffer))
              (win (get-buffer-window buf))
              (diag (or (car (flymake-diagnostics (point)))
                        (car (flymake-diagnostics (line-beginning-position)
                                                  (line-end-position))))))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (let ((found nil))
          (while (and (not found) (not (eobp)))
            (let ((id (tabulated-list-get-id)))
              (if (and (listp id) (eq (plist-get id :diagnostic) diag))
                  (setq found (point))
                (forward-line 1))))
          (when found
            (unless (overlayp flymake-follow--sync-overlay)
              (setq flymake-follow--sync-overlay (make-overlay 1 1))
              (overlay-put flymake-follow--sync-overlay 'face 'highlight)
              (overlay-put flymake-follow--sync-overlay 'priority 100))
            (move-overlay flymake-follow--sync-overlay
                          found
                          (min (point-max) (1+ (line-end-position)))
                          buf)
            (set-window-point win found)))))))

(define-minor-mode flymake-follow-mode
  "Sync the diagnostics buffer to the error at point."
  :global t
  :lighter nil
  (if flymake-follow-mode
      (add-hook 'post-command-hook #'flymake-follow-sync-diagnostics)
    (remove-hook 'post-command-hook #'flymake-follow-sync-diagnostics)))

(provide 'flymake-follow-mode)
;;; flymake-follow-mode.el ends here
