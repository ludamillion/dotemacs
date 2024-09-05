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

(setq straight-check-for-modifications nil)

;;;; use-package

;; Package `use-package' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features.
(straight-use-package 'use-package)

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)

;;;; List the packages to install/use which are available through one of straight's
;;;; default list of package archives.
(setq package-list
      '(
        accent               ; Easier access to accented characters
        ace-window           ; Easier moving between windows
        avy                  ; Jump to things (but really much, much more...)
        cape                 ; Completion At Point Extensions
        circadian            ; Change my theme in rhythm with nature
        command-log-mode     ; More insight into what commands are being run/looks fancy when showing off
        consult              ; Consulting completing-read
        consult-lsp          ; LSP extras for consult to, well, consult
        corfu                ; Completion Overlay Region FUnction
        denote
        doct                 ; (D)eclarative (O)rg (C)apture (T)emplates
        embark
        embark-consult
        exec-path-from-shell ; Get environment variables such as $PATH from the shell
        flycheck             ; Enhanced syntax checking, more flexible than flymake
        flycheck-eglot       ; Allow Flycheck to understand Eglot as a checker
        haml-mode            ; Rails templates not covered by treesitter or web-mode
        helpful              ; A better help buffer
        magit                ; A Git porcelain inside Emacs.
        marginalia           ; Enrich existing commands with completion annotations
        markdown-mode        ; Major mode for Markdown-formatted text
        orderless            ; Completion style for matching regexps in any order
        pdf-tools
        projectile           ; Project scoped stuffness
        rg                   ; Ripgrep for speed and profit(?)
        surround
        tempel
        tempel-collection
        treesit-auto
        undo-fu              ; Work around Emacs' clunky undo interface
        undo-fu-session      ; Persistant undo across sessions
        vertico              ; VERTical Interactive COmpletion
        vertico-posframe
        visual-fill-column   ; Nicer wrapping mostly for text modes
        vterm                ; A real terminal emulator running in Emacs
        web-mode             ; Uber mode for web templating languages
        ))

;; Install packages that are not yet installed
(dolist (package package-list)
  (straight-use-package package))

;;;; Install packages directly that are not yet published to one of the archives
(straight-use-package
 '(jtsx :type git :host github :repo "llemaitre19/jtsx"))

(straight-use-package
 '(asdf :type git :host github :repo "tabfugnic/asdf.el"))

(straight-use-package
 '(eglot-ltex :type git :host github :repo "emacs-languagetool/eglot-ltex"))

(straight-use-package
 '(eglot-booster :type git :host github :repo "jdtsmith/eglot-booster"))

(straight-use-package
 '(use-package-xdg :type git :host codeberg :repo "rossabaker/use-package-xdg"))

(straight-use-package
 '(on :type git :host gitlab :repo "ajgrf/on.el"))

(straight-use-package
 '(sensible-settings :type git :local-repo "~/code/sensible-settings"))

(straight-use-package
 '(sensible-themes :type git :local-repo "~/code/sensible-themes"))

(straight-use-package
 '(sensible-modeline :type git :local-repo "~/code/sensible-modeline"))

(straight-use-package '(org :type built-in))

;;;; Startup and configuration related code

(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / Sensible edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

(defun reload-init-file ()
  "Reload the init.el file in the emacs directory."

  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(keymap-global-set "<f5>" 'reload-init-file)

(require 'use-package-xdg)
(require 'asdf)

;;;; Backups, history, etc.

;;; Put the bookarks file in the proper XDG location
(use-package bookmark
  :xdg-state
  (bookmark-default-file "bookmarks.eld"))

;;;; Adjust limits for and activate recentf mode

(use-package recentf
  :custom
  (recentf-max-menu-items 10)
  (recentf-max-saved-items 100)
  :config
  (recentf-mode t))

;;;; SAVE ALL THE HISTORIES!

(use-package savehist
  :custom
  (kill-ring-max 50)
  (history-length 50)
  (history-delete-duplicates t)
  (savehist-additional-variables
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
     file-name-history)))

;;;; Savehist items above are saved when Emacs quits
;;;; When that happens strip text properties from the
;;;; kill-ring entries. This makes them much faster to
;;;; load when savehist restores them.

(defun unpropertize-kill-ring ()
  "Strip all text properties from text save to the kill-ring."

  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

;;;; Bring in PATH env variables and make their values available to Emacs

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;;;; Make Emacs aware of the proper paths for the asdf version manager. https://asdf-vm.com/

(use-package asdf
  :config
  (asdf-enable))

;;;; Make keys a little more ergonomic in macOS and tell Dired not to use the underlying ls

(when (string-equal system-type "darwin")
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq dired-use-ls-dired nil))

;;;; Function and key binds to create new frames by either cloning the current buffer
;;;; or jumping to the scratch buffer.

(defun luda/make-scratch-frame ()
  "Create a new frame and switch to *scratch* buffer."

  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))

(defvar-keymap sensible-frame-map
  :doc "Liminal prefix map for frame operations."
  "m" #'make-frame
  "n" #'luda/make-scratch-frame)

(keymap-global-set "M-n" sensible-frame-map)

;;;; Sensible Configuation
;;;;   - Set up my own little bundle of packages to tailor the Emacs experience

;;; Settings does things like turn off extra UI elements, use my perfer font
;;; and cursor settings etc.

(use-package sensible-settings
	:demand t
  :init
  (setopt sensible-font-size 16
          sensible-manage-cursor t
          sensible-manage-fonts t
          sensible-manage-ui t
          sensible-manage-ux t)
  (sensible-mode))

;;; Provide my custom themes with an approach inspired by Nicolas Rougier's
;;; NANO-Emacs projects and colors taken from Andrew Howell's Reasonable
;;; Colors project.

(use-package sensible-themes
  :demand t
  :after 'sensible-settings
  :load-path "~/code/sensible-themes")

;;; Choose light or dark theme based on the time of day at my location
;;; Ideally I'll use my own themes designed above but I'm iterating a
;;; bit on them at the moment and they're a bit borked. So, use Prot's
;;; excellent Modus themes for now.

(use-package circadian
	:demand t
  :custom
  (calendar-latitude 42.4)
  (calendar-longitude -71.0)
  (circadian-themes '((:sunrise . modus-operandi-tinted)
                      (:sunset  . modus-vivendi)))
  :config
  (circadian-setup))

;;;; Miscellaneous visual niceties

(setq-default line-spacing 1)

(use-package display-line-numbers
  :custom
  (display-line-numbers-widen t)
  :hook
  ((prog-mode conf-mode) . display-line-numbers-mode))

;;;; PDF Tools

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

(add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1)))

;;;; Iceberg Stack (aka Minad is a wizard)

(use-package vertico
  :bind (:map vertico-map
              ("C-<backspace>" . vertico-directory-up))
  :custom
  (vertico-resize t)
  (vertico-multiform-categories ; Choose a multiform
   '((file reverse)
     (consult-location)
     (imenu buffer)
     (library reverse indexed)
     (t reverse)
     ))
  (vertico-multiform-commands
   '(("flyspell-correct-*" grid reverse)
     (org-refile grid reverse indexed)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (consult-lsp-diagnostics)
     ))
  :init
  (vertico-mode))

(defun wrapper/consult-ripgrep (&optional dir given-initial)
  "Pass the region to consult-ripgrep if available.

  DIR and GIVEN-INITIAL match the method signature of `consult-wrapper'."
  (interactive "P")
  (let ((initial
         (or given-initial
             (when (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))))))
    (consult-ripgrep dir initial)))

(use-package consult
  :config
  (recentf-mode)
  :bind (
         ("M-s d" . consult-fd) ;; Requires having fd installed otherwise use consult-find
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . wrapper/consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s <SPC>"   . consult-buffer)
         ("M-y"   . consult-yank-pop)
         ("C-x M-k"   . consult-kmacro)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-g o" . consult-outline)
         ("C-x b" . consult-bookmark)))

(use-package orderless
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(defun corfu-x-eshell-hook ()
  "Set up Corfu behaviors in a shell friendly way."
  (setq-local corfu-auto nil)
  (corfu-mode))

(use-package corfu
  :init
  (setq corfu-cycle t
        corfu-auto t
        corfu-on-exact-match nil)
  (corfu-history-mode)
  (corfu-echo-mode)
  (global-corfu-mode)
  :hook
  (eshell-mode . corfu-x-eshell-hook))

(defun luda/cape-capf-setup-eglot ()
	(let ((result))
    (dolist (element `(,(cape-capf-buster #'eglot-completion-at-point)
											 cape-file
											 cape-dabbrev) result)
      (add-to-list 'completion-at-point-functions element))))

(defun luda/cape-capf-setup-org ()
	(let ((result))
    (dolist (element '(cape-dict cape-dabbrev) result)
      (add-to-list 'completion-at-point-functions element))))

(defun luda/cape-capf-setup-git-commit ()
  (let ((result))
    (dolist (element '(cape-symbol cape-dabbrev) result)
      (add-to-list 'completion-at-point-functions element))))

(use-package cape
  :config
  (setq completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless))))
  :hook
  ((eglot-managed-mode . luda/cape-capf-setup-eglot)
   (org-mode . luda/cape-capf-setup-org)
   (git-commit-mode . luda/cape-capf-setup-git-commit)))

(use-package marginalia
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  :custom
  (tempel-trigger-prefix "<")
  :bind (("M-=" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)))

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)

(use-package ace-window
  :bind
  ("M-o" . 'ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?j ?k ?l))
  (aw-dispatch-always t)
  (aw-dispatch-alist
   '((?x aw-delete-window "Ace - Delete Window")
     (?c aw-swap-window "Ace - Swap Window")
     (?n aw-flip-window)
     (?v aw-split-window-vert "Ace - Split Vert Window")
     (?h aw-split-window-horz "Ace - Split Horz Window")
     (?m delete-other-windows "Ace - Maximize Window")
     (?b balance-windows)
     (?u (lambda ()
           (progn
             (winner-undo)
             (setq this-command 'winner-undo))))
     (?r winner-redo))))

(keymap-global-set "C-M-o" 'mode-line-other-buffer)

(defvar-keymap sensible-modes-toggle-map
  :doc "Sensible prefix key maps | minor mode toggling."
  "w" #'whitespace-mode)

(keymap-set global-map "C-c m" sensible-modes-toggle-map)

(use-package which-key
  :custom
  (which-key-idle-delay 0.75)
  :config
  (which-key-mode))

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-x l r" . eglot-rename)
              ("M-k" . eglot-code-actions))
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (add-to-list 'eglot-server-programs
               '(ruby-mode . ("solargraph" "stdio")))
	(add-to-list 'eglot-server-programs
               '(csharp-mode . ("omnisharp" "-lsp"))))

(use-package eglot-booster
  :after eglot
  :init
  (eglot-booster-mode))

(use-package eglot-ltex
  :hook
  (text-mode . (lambda ()
                 (require 'eglot-ltex)
                 (eglot-ensure)))
  :init
  (setq eglot-ltex-server-path "/usr/local/bin/ltex-ls"))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package flycheck-eglot
  :hook
  (eglot-managed-mode . flycheck-eglot-mode)
  :after (flycheck eglot))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(setq treesit-language-source-alist
      '((css "https://github.com/tree-sitter/tree-sitter-css")
        (lua "https://github.com/MunifTanjim/tree-sitter-lua")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (scss "https://github.com/serenadeai/tree-sitter-scss")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(dolist (ts-pair treesit-language-source-alist)
  (let ((language (car ts-pair)) (repo (cadr ts-pair)))
    (unless (treesit-language-available-p language)
      (message "Installing parser for %s from %s" language repo)
      (treesit-install-language-grammar language))))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(defun avy-action-copy-whole-line (pt)
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
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-teleport-whole-line (pt)
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

(use-package emacs
  :bind (:map isearch-mode-map ("C-o" . isearch-occur)))

(use-package surround
  :bind-keymap ("M-'" . surround-keymap))

(global-set-key (kbd "C-z") 'zap-up-to-char)

(use-package elec-pair
  :init
  (electric-pair-mode))

(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Clean and straightforward undo/redo
(use-package undo-fu
  :config
  (setopt undo-fu-allow-undo-in-region t)
  :bind
  ("C-/" . undo-fu-only-undo)
  ("C-M-/" . undo-fu-only-redo))

;; Persist undo history across sessions
(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))

(setq visible-bell nil
      ring-bell-function #'ignore)

(setq switch-to-buffer-obey-display-actions t)

(use-package vterm
  :init
  (setq vterm-max-scrollback 10000)
  :bind
  ("C-x !" . projectile-run-vterm))

(use-package helpful
  :bind
  ("C-h f" . #'helpful-callable)
  ("C-c F" . #'helpful-function)
  ("C-h v" . #'helpful-variable)
  ("C-h k" . #'helpful-key)
  ("C-h x" . #'helpful-command)
  ("C-c C-d" . #'helpful-at-point))

(add-to-list 'display-buffer-alist
             '("\\*Help\\*\\|\\*helpful.*\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 80)
               (window-parameters
                (no-delete-other-windows . t))))

(defun luda/quit-dwim (&optional arg)
  "If current frame is the last frame kill emacs, else delete it."
  (interactive "P")

  (if (> (length (frame-list)) 1)
      (delete-frame arg)
    (if (y-or-n-p (format "Are you sure you want to close the last frame?"))
        (save-buffers-kill-terminal arg)
      (message "Great, back to what you were doing then."))))

(global-set-key (kbd "C-x C-c") 'luda/quit-dwim)

(keymap-set global-map "C-x k" 'kill-current-buffer)
(keymap-set global-map "C-x C-k" 'kill-buffer)

(use-package css-mode
  :custom
  (tab-width 2)
  (css-indent-offset 2))

(use-package web-mode
  :mode
  (("\\.erb\\'" . web-mode)
   ("\\.html?\\'" . web-mode)))

(use-package haml-mode
  :defer t)

(use-package jtsx
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :custom
  (js-indent-level 2)
  (typescript-ts-mode-indent-offset 2)
  (jtsx-switch-indent-offset 0)
  (jtsx-indent-statement-block-regarding-standalone-parent nil)
  (jtsx-jsx-element-move-allow-step-out t)
  (jtsx-enable-jsx-electric-closing-element t)
	:config
	(eglot-ensure))

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :mode "\\.pryrc\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
	:config
	(eglot-ensure))

;; (use-package csharp-mode
;; 	:straight (:type built-in)
;; 	:config
;; 	(eglot-ensure))

(use-package yaml-ts-mode
  :mode "\\.y[a]?ml")

(defvar luda/local-root "~/" "The explicit root directory value")

(defvar luda/default-org-directory
	(expand-file-name "org" luda/local-root))

(setq luda/sync-org-directory (expand-file-name "Dropbox/org" luda/local-root))
(setq luda/beorg-directory (expand-file-name "Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org" luda/local-root))

(setq luda/org-dir
      (if (file-directory-p luda/default-org-directory)
          luda/default-org-directory
        luda/sync-org-directory))

(setq luda/journal-file (expand-file-name "journal.org" luda/org-dir))
(setq luda/projects-file (expand-file-name "projects.org" luda/org-dir))
(setq luda/org-id-locations-file (expand-file-name ".org-id-locations"
																									 luda/org-dir))

(setq luda/todo-keywords
      `((sequence
         "TODO(t!)" "ACTIVE(a!)" "WAITING(w!)" "MAYBE(m!)" "|" "DONE(d!)"
				 "OBSOLETE(o!)" "CANCELED(-!)")))

(use-package doct
  ;;recommended: defer until calling doct
  :commands (doct))

(use-package org
  :init
  (setq org-export-backends '(ascii md html icalendar latex))
  :config
  (setq org-default-notes-file (expand-file-name "todo.org" luda/org-dir)) ;; Should maybe be inbox
  (setq org-log-done 'time)
  (setq org-log-reschedule 'time)
  (setq org-log-into-drawer t)
  (setq org-startup-truncated nil)
  (setq org-todo-keywords luda/todo-keywords)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (setq org-id-locations-file luda/org-id-locations-file)
  (setq org-id-locations-file-relative t)
  (setq org-fontify-whole-heading-line t)
  (setq org-agenda-files `(,luda/org-dir ,luda/beorg-directory))
  (setq org-latex-pdf-process
        '("tectonic %f"))

  (setq org-capture-templates
        (doct `(("Projects"
                 :keys "p" :file ,luda/projects-file
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
                 :file ,luda/journal-file
                 :datetree t
                 :template ":PROPERTIES\n:Created: %U\n:END:\n\n%?\n%i\n%a"
                 :empty-lines 1))))

  ;; ;; One of my big uses for Org is my literate config so having elisp as a template is a must
  (add-to-list 'org-structure-template-alist '("sl" . "src emacs-lisp"))

  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link))

(use-package magit
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
  "Stores the window arrangement before opening ediff."
  (setq luda/ediff-original-windows (current-window-configuration)))

(defun luda/restore-pre-ediff-winconfig ()
  "Resets original window arrangement"
  (set-window-configuration luda/ediff-original-windows))

(use-package ediff
  :hook ((ediff-before-setup . 'luda/store-pre-ediff-winconfig)
         (ediff-quit . 'luda/restore-pre-ediff-winconfig))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))


