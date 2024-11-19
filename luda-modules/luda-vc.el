;;; luda-vc --- Version Control Module -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package magit
  :straight t
  :bind
  ("C-M-;" . magit-status))

(defvar-keymap sensible-vc-branch-map
  :doc "Sensible prefix map for version control branch actions."
  "b" #'magit-checkout
  "c" #'magit-branch-create)

(defvar-keymap sensible-vc-pull-map
  :doc "Sensible prefix map for version control pull/fetch actions."
  "p" #'magit-pull-from-pushremote
  "u" #'magit-pull-from-upstream
  "e" #'magit-pull-branch)

(defvar-keymap sensible-vc-file-map
  :doc "Sensible prefix map for version control file actions."
  "r" #'magit-file-rename)

(defvar-keymap sensible-vc-map
  :doc "Sensible prefix key maps version control operations ."
  "b" sensible-vc-branch-map
  "F" sensible-vc-pull-map
  "f" sensible-vc-file-map)

(keymap-set global-map "C-c g" sensible-vc-map)

(defvar luda/ediff-original-windows nil)

(defun luda/store-pre-ediff-winconfig ()
  "Store the window arrangement before opening Ediff."
  (setq luda/ediff-original-windows (current-window-configuration)))

(defun luda/restore-pre-ediff-winconfig ()
  "Reset original window arrangement."
  (set-window-configuration luda/ediff-original-windows))

(use-package ediff
  :straight (ediff :type built-in)
  :hook ((ediff-before-setup . 'luda/store-pre-ediff-winconfig)
         (ediff-quit . 'luda/restore-pre-ediff-winconfig))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))


(provide 'luda-vc)
;;; luda-vc.el ends here
