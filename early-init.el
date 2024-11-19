;;; dotemacs --- A literate Emacs configuration -*- lexical-binding: t -*-

;;; Copyright (C) 2024 Luke D. Inglis

;;; This file is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your option)
;;; any later version.

;;; This file is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; For a full copy of the GNU General Public License
;;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defun luda/add-to-list (list element)
  "Add to symbol of LIST the given ELEMENT.
Simplified version of `add-to-list'."
  (set list (cons element (symbol-value list))))

(mapc
 (lambda (var)
   (luda/add-to-list var '(width . (text-pixels . 1000)))
   (luda/add-to-list var '(height . (text-pixels . 800)))
   (luda/add-to-list var '(scroll-bar-width  . 10)))
 '(default-frame-alist initial-frame-alist))

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      frame-title-format '("%b")
      ring-bell-function 'ignore
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      use-short-answers t
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Often turned off along with the other two above when going for the
;; minimalist UI. However I'm currently always on a Mac where the
;; menu-bar is displayed separately from the main application frame on
;; the desktop level.
;; (menu-bar-mode -1) 

(setq initial-buffer-choice t)
(setq initial-major-mode 'lisp-interaction-mode)
(setq initial-scratch-message
      (format ";; This is `%s'.  Use `%s' to evaluate and print results\n\n"
              'lisp-interaction-mode
              (propertize
               (substitute-command-keys "\\<lisp-interaction-mode-map>\\[eval-print-last-sexp]")
               'face 'help-key-binding)))

(setq site-run-file nil
			inhibit-default-init t
			package-enable-at-startup nil
			use-package-ensure-function 'ignore
			package-archives nil)

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.  The
;; `most-positive-fixnum' is DANGEROUS AS A PERMANENT VALUE.  See the
;; `emacs-startup-hook' a few lines below for what I actually use.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Same idea as above for the `file-name-handler-alist' and the
;; `vc-handled-backends' with regard to startup speed optimisation.
;; Here I am storing the default value with the intent of restoring it
;; via the `emacs-startup-hook'.
(defvar luda/file-name-handler-alist file-name-handler-alist)
(defvar luda/vc-handled-backends vc-handled-backends)

(setq file-name-handler-alist nil
      vc-handled-backends nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 1000 1000 8)
                  gc-cons-percentage 0.1
                  file-name-handler-alist luda/file-name-handler-alist
                  vc-handled-backends luda/vc-handled-backends)))

(require 'xdg)
(startup-redirect-eln-cache
 (expand-file-name "emacs/eln-cache/" (xdg-cache-home)))

;; Reset garbage collector limit after init process has ended (8Mo)
(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold (* 8 1024 1024))))

(setq debug-on-error t)

(provide 'early-init)
;;; early-init.el ends here
