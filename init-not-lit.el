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

;;;; Use straight.el for more deterministic package management and
;;;; make this config the source of truth on packages.

(unless (featurep 'straight)
  ;; Bootstrap straight.el
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;;;; use-package
(require 'use-package)
(require 'straight)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setopt straight-use-package-by-default t
        use-package-always-defer t)

;;;; Startup and configuration related code

;;;; Bring in PATH env variables and make their values available to Emacs

(use-package exec-path-from-shell
  :demand t
  :init
  (exec-path-from-shell-initialize))

(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / Sensible edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

(defun reload-init-file ()
  "Reload the init.el file in the Emacs directory."

  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(keymap-global-set "<f5>" 'reload-init-file)

(use-package on
  :demand t
  :straight (on :type git :host gitlab :repo "ajgrf/on.el"))

(use-package use-package-xdg
  :demand t
  :straight (use-package-xdg :type git
                             :host codeberg
                             :repo "rossabaker/use-package-xdg"))

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

;;;; Backups, history, etc.

;;; Put the bookmarks file in the proper XDG location
(use-package bookmark
  :commands (bookmark-set)
  :xdg-state
  (bookmark-default-file "bookmarks.eld"))

(use-package auto-save
  :straight nil
  :no-require
  :xdg-state
  (auto-save-list-prefix "saves/"))

;; Adjust limits for and activate recentf mode
(use-package recentf
  :hook
  (on-first-buffer . recentf-mode)
  :custom
  (recentf-max-menu-items 10)
  (recentf-max-saved-items 100))

;; SAVE ALL THE HISTORIES!
(setq savehist-watchlist
      '(kill-ring
        command-history
        set-variable-value-history
        custom-variable-history
        query-replace-history
        read-expression-history
        minibuffer-history
        read-char-history
        face-name-history
        bookmark-history
        file-name-history))

(use-package savehist
  :hook
  (on-first-buffer . savehist-mode)
  :xdg-state
  (savehist-file "history")
  :custom
  (kill-ring-max 50)
  (history-length 50)
  (history-delete-duplicates t)
  (savehist-additional-variables savehist-watchlist))

;;;; Savehist items above are saved when Emacs quits
;;;; When that happens strip text properties from the
;;;; kill-ring entries. This makes them much faster to
;;;; load when savehist restores them.

(defun unpropertize-kill-ring ()
  "Strip all text properties from text save to the `kill-ring'."
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

;;;; Make Emacs aware of the proper paths for the asdf version
;;;; manager. https://asdf-vm.com/

(use-package asdf
  :straight (:type git :host github :repo "tabfugnic/asdf.el")
  :config
  (asdf-enable))

;;;; Make keys a little more ergonomic in macOS and tell Dired not to
;;;; use the underlying ls

(when (string-equal system-type "darwin")
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq dired-use-ls-dired nil))

;; Function and key binds to create new frames by either cloning the
;; current buffer or jumping to the scratch buffer.

(defun esprit/make-scratch-frame ()
  "Create a new frame and switch to *scratch* buffer."

  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))

(defun esprit/make-vterm-frame ()
  "Create a new frame and create a vterm buffer."

  (interactive)
  (select-frame (make-frame))
  (vterm))

(defvar-keymap sensible-frame-map
  :doc "Liminal prefix map for frame operations."
  "m" #'make-frame
  "n" #'esprit/make-scratch-frame
  "v" #'esprit/make-vterm-frame)

(keymap-global-set "M-n" sensible-frame-map)

;; Sensible Configuation
;;   - Set up my own little bundle of packages to tailor the Emacs experience

(use-package nerd-icons
  :demand t)

(use-package sensible-settings
  :straight (:type git :local-repo "~/code/emacs-lisp/sensible-settings")
  :demand t
  :init
  (setf sensible-font-size 18
        sensible-manage-cursor t
        sensible-manage-fonts t
        sensible-manage-ui t
        sensible-manage-ux t)
  (sensible-mode))

;;; Provide my custom themes with an approach inspired by Nicolas Rougier's
;;; NANO-Emacs projects and colors taken from Andrew Howell's Reasonable Colors
;;; project.

;; (use-package sensible-themes
;;   :straight (:type git :local-repo "~/code/emacs-lisp/sensible-themes")
;;   :demand t
;;   :requires (sensible-settings))

(use-package sensible-modeline
  :straight (:type git :local-repo "~/code/emacs-lisp/sensible-modeline")
  :demand t
  :requires (nerd-icons)
  :init (sensible-modeline-mode))

(use-package logos-themes
  :straight (:type git :local-repo "~/code/emacs-lisp/logos-themes")
  :demand t)

;;; Choose light or dark theme based on the time of day at my location

(use-package circadian
  :demand t
  :custom
  (calendar-latitude 42.4)
  (calendar-longitude -71.0)
  (circadian-themes '((:sunrise . modus-operandi)
                      (:sunset  . modus-vivendi)))
  :config
  (circadian-setup))

(use-package display-line-numbers
  :custom (display-line-numbers-widen t)
  :hook
  ((prog-mode conf-mode) . display-line-numbers-mode))

(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("esprit-lisp" "esprit-modules"))

(require 'esprit-completion)
(require 'esprit-lsp)
(require 'esprit-analysis)
(require 'esprit-editing)
(require 'esprit-interface)
(require 'esprit-vc)
(require 'esprit-prog-modes)

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  :custom
  (tempel-trigger-prefix "<")
  :bind (("M-=" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)))

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)

(defvar-keymap sensible-toggles-map
  :doc "Sensible prefix key maps | minor mode toggling."
  "v" #'global-visual-line-mode
  "f" #'toggle-frame-fullscreen
  "w" #'whitespace-mode)

(keymap-global-set "C-c t" sensible-toggles-map)

(use-package bind-key
  :straight (bind-key :type built-in))

(use-package which-key
  :straight (which-key :type built-in)
  :custom
  (which-key-idle-delay 1)
  :hook
  (on-first-input . which-key-mode))

(use-package emacs
  :bind (("C-x C-m" . execute-extended-command)
         :map isearch-mode-map ("C-o" . isearch-occur)))

;; Clean and straightforward undo/redo
(use-package undo-fu
  :config
  (setopt undo-fu-allow-undo-in-region t)
  :bind
  ("C-/" . undo-fu-only-undo)
  ("C-M-/" . undo-fu-only-redo))

;; Persist undo history across sessions
(use-package undo-fu-session
  :custom
  (undo-fu-session-incompatible-files
	 '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))

(use-package vterm
  :init
  (setq vterm-max-scrollback 10000)
  :bind
  ("C-x !" . projectile-run-vterm))

(use-package helpful
  :bind
  ("C-h f"    . #'helpful-callable)
  ("C-c F"    . #'helpful-function)
  ("C-h v"    . #'helpful-variable)
  ("C-h k"    . #'helpful-key)
  ("C-h x"    . #'helpful-command)
  ("C-c C-d"  . #'helpful-at-point))

(use-package nxml-mode
  :straight (:type built-in)
  :mode (rx (| ".xml" ".csproj")))

(use-package markdown-mode
  :mode (rx ".md"))

(use-package yaml-ts-mode
  :straight (:type built-in)
  :mode (rx (| ".yml" ".yaml")))

(defvar esprit/local-root "~/"
  "The explicit root directory value.")

(defvar esprit/default-org-directory
  (expand-file-name "org" esprit/local-root))

(setq esprit/sync-org-directory (expand-file-name "Dropbox/org" esprit/local-root))
(setq esprit/beorg-directory
      (expand-file-name "Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org" esprit/local-root))

(setq esprit/org-dir
      (if (file-directory-p esprit/default-org-directory)
          esprit/default-org-directory
        esprit/sync-org-directory))

(setq esprit/journal-file (expand-file-name "journal.org" esprit/org-dir))
(setq esprit/projects-file (expand-file-name "projects.org" esprit/org-dir))
(setq esprit/org-id-locations-file (expand-file-name ".org-id-locations" esprit/org-dir))

(setq esprit/todo-keywords
      `((sequence
         "TODO(t!)" "ACTIVE(a!)" "WAITING(w!)" "MAYBE(m!)" "|" "DONE(d!)"
	 "OBSOLETE(o!)" "CANCELED(-!)")))

(use-package org
  :straight (:type built-in)
  :init
  (setq org-export-backends '(ascii md html icalendar latex))
  :config
  (setq org-default-notes-file (expand-file-name "todo.org" esprit/org-dir)) ;; Should maybe be inbox
  (setq org-log-done 'time)
  (setq org-log-reschedule 'time)
  (setq org-log-into-drawer t)
  (setq org-startup-truncated nil)
  (setq org-todo-keywords esprit/todo-keywords)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (setq org-id-locations-file esprit/org-id-locations-file)
  (setq org-id-locations-file-relative t)
  (setq org-fontify-whole-heading-line t)
  (setq org-agenda-files `(,esprit/org-dir ,esprit/beorg-directory))
  (setq org-latex-pdf-process
        '("tectonic %f"))

  (setq org-capture-templates
        (doct `(("Projects"
                 :keys "p" :file ,esprit/projects-file
                 :template ("* %{todo-state} %^{Description}\n:PROPERTIES:\n:Created: %U\n:END:\n\n\n%i\n%a")
                 :children (("Todo"
                             :keys "t"
                             :headline "Tasks"
                             :todo-state "TODO")
                            ("Note"
                             :keys "n"
                             :headline "Notes"
                             :todo-state ""))
                 )
                ("Journal"
                 :keys "j"
                 :type plain
                 :file ,esprit/journal-file
                 :datetree t
                 :template ":PROPERTIES\n:Created: %U\n:END:\n\n%?\n%i\n%a"
                 :empty-lines 1))))

  ;; ;; One of my big uses for Org is my literate config so having elisp as a template is a must
  (add-to-list 'org-structure-template-alist '("sl" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("s#" . "src csharp"))

  (org-babel-do-load-languages
	 'org-babel-load-languages
	 '((emacs-lisp . t)
		 (shell . t)
		 (csharp . t)))

  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link))

(use-package doct
  :straight t
  ;;recommended: defer until calling doct
  :commands (doct))

(use-package emacs-lock
  :config
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill)))

(use-package command-log-mode
  :straight (:type git :local-repo "~/code/emacs-lisp/command-log-mode"))

;;;; Rainbow mode for color previewing (rainbow-mode.el)
(use-package rainbow-mode
  :init
  (setq rainbow-ansi-colors nil)
  (setq rainbow-x-colors nil)

  (defun prot/rainbow-mode-in-themes ()
    (when-let ((file (buffer-file-name))
               ((derived-mode-p 'emacs-lisp-mode))
               ((string-match-p "-theme" file)))
      (rainbow-mode 1)))
  :bind (:map ctl-x-x-map
        ("c" . rainbow-mode)) ; C-x x c
  :hook (emacs-lisp-mode . prot/rainbow-mode-in-themes))

(add-hook 'after-init-hook (lambda () (set-frame-name "home")))

(provide 'init)
