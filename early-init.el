;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;; Author: Luke Inglis
;; URL: https://github.com/ludamillion/esprit-emacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.1.0
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary
;;
;; Early init configuration. Largely copied from the Emacs Solo project.
;; 

;;; Code

;; Load customizations as early as possible so user settings
;; (e.g. esprit-avoid-flash-options) take effect before they are used.
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(defcustom esprit-emacs-avoid-flash-options
  '((enabled          . t)
    (background       . "#222222")
    (foreground       . "#f6f6f6")
    (reset-background . "#222222")
    (reset-foreground . "#f6f6f6"))
  "Options to avoid flash of light on Emacs startup.
- `enabled`: Whether to apply the workaround.
- `background`, `foreground`: Initial colors to use.
- `reset-background`, `reset-foreground`: Optional explicit colors to restore after startup.

NOTE: The default values here presented are set for the default
`emacs-solo' custom theme.  If you'd like to turn this ON with another
theme, change the background/foreground variables.

If reset values are nil, nothing is reset."
  :type '(alist :key-type symbol :value-type (choice (const nil) string))
  :group 'esprit-emacs)


;;; -------------------- PERFORMANCE & HACKS
;; HACK: increase startup speed

;; Delay garbage collection while Emacs is booting
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Schedule garbage collection sensible defaults for after booting
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024)
                  gc-cons-percentage 0.1)))

;; Single VC backend increases booting speed
(setq vc-handled-backends '(Git))

;; Do not native compile if on battery power
(setopt native-comp-async-on-battery-power nil) ; EMACS-31

;; HACK: avoid being flashbanged
(defun esprit-emacs/avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs, based esprit-emacs-avoid-flash-options`."
  (when (alist-get 'enabled esprit-emacs-avoid-flash-options)
    (setq mode-line-format nil)
    (set-face-attribute 'default nil
                        :background (alist-get 'background esprit-emacs-avoid-flash-options)
                        :foreground (alist-get 'foreground esprit-emacs-avoid-flash-options))))

(defun esprit-emacs/reset-default-colors ()
  "Reset any explicitly defined reset values in `esprit-emacs-avoid-flash-options`."
  (when (alist-get 'enabled esprit-emacs-avoid-flash-options)
    (let ((bg (alist-get 'reset-background esprit-emacs-avoid-flash-options))
          (fg (alist-get 'reset-foreground esprit-emacs-avoid-flash-options)))
      (when bg
        (set-face-attribute 'default nil :background bg))
      (when fg
        (set-face-attribute 'default nil :foreground fg)))))

(esprit-emacs/avoid-initial-flash-of-light)
(add-hook 'after-init-hook #'esprit-emacs/reset-default-colors)

;; Better Window Management handling
(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format
      '(:eval
        (let ((project (project-current)))
          (if project
              (concat "Emacs - [p] " (project-name project))
              (concat "Emacs - " (buffer-name))))))

(when (eq system-type 'darwin)
  (setq ns-use-proxy-icon nil))

(setq inhibit-compacting-font-caches t)

;; Disables unused UI Elements
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
(if (fboundp 'fringe-mode) (fringe-mode -1))

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

(let ((default-directory  (expand-file-name "lisp" user-emacs-directory)))
  (setq load-path
        (append
         (let ((load-path  (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; early-init.el ends here
