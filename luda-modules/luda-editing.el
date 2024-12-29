;;; luda-editing --- Text Editing and Manipulation Module -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defun avy-action-embark (pt)
	"Invoke embark at PT."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(defun avy-action-copy-whole-line (pt)
	"Copy entire line starting at PT."
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-yank-whole-line (pt)
	"Yank line starting at PT."
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(defun avy-action-kill-whole-line (pt)
	"Kill line starting at PT."
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-teleport-whole-line (pt)
	"Teleport whole line starting at PT."
  (avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)

(use-package avy
  :config
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark
        (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)
  :bind ("M-j" . avy-goto-char-timer))

(use-package surround
  :bind-keymap ("M-'" . surround-keymap))

(use-package misc
  :straight (misc :type built-in)
  :bind
  ("C-z" . #'zap-up-to-char))

(use-package elec-pair
  :init
  (electric-pair-mode))

(use-package accent
	:bind ("C-x '" . #'accent-menu))

(use-package puni)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(keymap-global-set "C-j" #'join-line)

(provide 'luda-editing)
;;; luda-editing.el ends here
