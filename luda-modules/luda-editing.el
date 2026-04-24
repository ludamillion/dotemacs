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

(use-package misc
  :straight (misc :type built-in)
  :bind
  ("C-z" . #'zap-up-to-char))

(use-package elec-pair
  :init
  (electric-pair-mode))

(use-package accent
  :bind ("C-x '" . #'accent-menu))

(use-package simple
  :straight (:type built-in)
  :hook (text-mode . visual-line-mode))

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(keymap-global-set "C-j" #'join-line)

(use-package evil
  :ensure t
  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-keybinding nil)
  :custom
  (evil-undo-system 'undo-fu)
  :config
  (evil-mode)
  ;; If you use Magit, start editing in insert state
  (add-hook 'git-commit-setup-hook 'evil-insert-state)
  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'eat-mode 'emacs)
  (evil-set-initial-state 'vterm-mode 'emacs))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(provide 'luda-editing)
;;; luda-editing.el ends here
