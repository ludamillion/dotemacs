(defun luda/add-to-list (list element)
  "Add to symbol of LIST the given ELEMENT.
  Simplified version of `add-to-list' halfway between `push' and `add-to-list'."
  (set list (cons element (symbol-value list))))

(mapc
 (lambda (var)
   (luda/add-to-list var '(width . (text-pixels . 1000)))
   (luda/add-to-list var '(height . (text-pixels . 800)))
   (luda/add-to-list var '(scroll-bar-width  . 10)))
 '(default-frame-alist initial-frame-alist))

(setopt frame-resize-pixelwise t
	frame-inhibit-implied-resize t
	frame-title-format '("%b"))

(scroll-bar-mode -1)
(tool-bar-mode -1)

(setopt inhibit-splash-screen t
	inhibit-x-resources t
	inhibit-startup-echo-area-message user-login-name
	inhibit-startup-buffer-menu t)

(setopt inhibit-startup-screen t
				initial-major-mode 'lisp-interaction-mode)

(setopt initial-scratch-message
	(format ";; This is `%s'.  Use `%s' to evaluate and print results\n\n"
		'lisp-interaction-mode
		(propertize
		 (substitute-command-keys "\\<lisp-interaction-mode-map>\\[eval-print-last-sexp]")
		 'face 'help-key-binding)))

(setopt ring-bell-function 'ignore
				use-dialog-box t
				use-file-dialog nil
				use-short-answers t)

(setf site-run-file nil
      inhibit-default-init t
      package-enable-at-startup nil
      use-package-ensure-function 'ignore
      package-archives nil)

(require 'xdg)
(startup-redirect-eln-cache
 (expand-file-name "emacs/eln-cache/" (xdg-cache-home)))

(defvar luda/file-name-handler-alist file-name-handler-alist)
(defvar luda/vc-handled-backends vc-handled-backends)

(defvar luda/gc-threshold gc-cons-threshold (* 1000 1000 8))
(defvar luda/gc-cons-percentage 0.1)

(setopt file-name-handler-alist nil
				vc-handled-backends nil)

(setopt gc-cons-threshold most-positive-fixnum
				gc-cons-percentage 0.5)

(defun luda/reset-startup-values ()
  (setopt gc-cons-threshold luda/gc-threshold
					gc-cons-percentage luda/gc-cons-percentage
					file-name-handler-alist luda/file-name-handler-alist
					vc-handled-backends luda/vc-handled-backends))

(add-hook 'emacs-startup-hook 'luda/reset-startup-values)

(provide 'early-init)
