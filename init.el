;;; dotemacs --- A literate Emacs configuration -*- lexical-binding: t -*-
;;; This file has been generated from dotemacs.org file. DO NOT EDIT.

;;; Copyright (C) 2023 Luke D. Inglis

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

;;; Code

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

;; Use straight.el for use-package expressions
(setq straight-check-for-modifications nil)

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

(setq package-list
      '(
        cape                 ; Completion At Point Extensions
        circadian            ; Change my theme in rhythm with nature
        coffee-mode          ; Sadly still have to deal with coffeescript sometimes
        consult              ; Consulting completing-read
        consult-lsp          ; LSP extras for consult to, well, consult
        corfu                ; Completion Overlay Region FUnction
        css-mode             ; If everything is !important than nothing is !important
        deft                 ; Enhanced note taking with Org
        docker
        dockerfile-mode
        exec-path-from-shell ; Get environment variables such as $PATH from the shell
        f                    ; Modern API for working with files and directories
        flycheck             ; Enhanced syntax checking, more flexible than flymake
        flycheck-eglot       ; Allow Flycheck to understand Eglot as a checker
        haml-mode            ; Rails templates not covered by treesitter or web-mode
        helpful              ; A better help buffer
        imenu-list           ; Show imenu entries in a separate
        magit                ; A Git porcelain inside Emacs.
        marginalia           ; Enrich existing commands with completion annotations
        markdown-mode        ; Major mode for Markdown-formatted text
        multi-vterm
        multiple-cursors     ; Sometimes many cursors are better than one
        no-littering         ; Keep our things clean and tidy
        objed                ; Navigate and manipulate text objects
        orderless            ; Completion style for matching regexps in any order
        org-auto-tangle
        projectile           ; Project scoped stuffness
        rainbow-mode         ; Sometime you just need to see the colors
        rainbow-delimiters   ; Light up matching delimiters for shiny ease of reading
        rg                   ; Ripgrep for speed and profit(?)
        slim-mode
        smartparens          ; Like parens but, you know, ...smarter
        transpose-frame
        treesit-auto
        undo-fu              ; Work around Emacs' clunky undo interface
        undo-fu-session      ; Persistant undo across sessions
        vertico              ; VERTical Interactive COmpletion
        vertico-posframe
        visual-fill-column   ; Nicer wrapping mostly for text modes
        vterm                ; A real terminal emulator running in Emacs
        web-mode             ; Uber mode for web templating languages
        which-key            ; Discovery method for key bindings
        ))

;; Install packages that are not yet installed
(dolist (package package-list)
  (straight-use-package package))

;; Install a selection of the N Λ N O suite of packages install straight from GitHub

;; Modeline (eventually to be replace with my own)
(straight-use-package
 '(nano-modeline :type git :host github :repo "rougier/nano-modeline"))

;; A cleaner, more minimal Org agenda
(straight-use-package
 '(nano-agenda :type git :host github :repo "rougier/nano-agenda"))

(straight-use-package
 '(jtsx :type git :host github :repo "llemaitre19/jtsx"))

(straight-use-package
 '(liminal-theme :type nil :local-repo "~/code/liminal-theme"))

(straight-use-package '(org :type built-in))

(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / Liminal edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

(defun reload-init-file ()
  "Reload the file referenced by `user-init-file`."

  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "<f5>") 'reload-init-file)

(add-hook 'org-mode-hook 'org-auto-tangle-mode)

(require 'no-littering)

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(setq luda/local-root "~/")

(when (string-equal system-type "darwin")
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq dired-use-ls-dired nil))

(defun luda/make-scratch-frame ()
  "Create a new frame and switch to *scratch* buffer."

  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))

(defun luda/make-vterm-frame ()
  "Create a new frame and switch to *scratch* buffer."

  (interactive)
  (select-frame (make-frame))
  (vterm))

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-M-o") 'other-frame)

(use-package liminal-theme
  :init
  (setq liminal-font-size 16)
  :config
  (liminal-mode))

(use-package nano-modeline
  :init
  (setopt mode-line-format nil)
  :hook
  (prog-mode            . nano-modeline-prog-mode)
  (text-mode            . nano-modeline-text-mode)
  (org-mode             . nano-modeline-org-mode)
  (term-mode            . nano-modeline-term-mode)
  (messages-buffer-mode . nano-modeline-message-mode)
  (org-capture-mode     . nano-modeline-org-capture-mode)
  (org-agenda-mode      . nano-modeline-org-agenda-mode))

(use-package circadian
  :custom
   (calendar-latitude 42.4)
   (calendar-longitude -71.0)
   (circadian-themes '((:sunrise . liminal-light)
                       (:sunset  . liminal-dark)))
   :config
   (circadian-setup))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(setq-default line-spacing 1)

(use-package vertico
  :bind (:map vertico-map
              ("C-<backspace>" . vertico-directory-up))
  :custom
  (vertico-scroll-margin 0 "Remove the top/bottom margins of the completion window")
  (vertico-resize t "Let the completion window grow and shrink")
  (vertico-cycle t "Let the curson loop back to the begining on reaching the end")
  :init
  (vertico-mode))

(use-package consult
  :bind (
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-x"   . consult-buffer)
         ("M-y"   . consult-nk-pop)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-g o" . consult-outline)
         ("C-x b" . consult-bookmark)))

(use-package orderless
  :config
   (setq completion-styles '(orderless partial-completion basic))
   (setq completion-category-defaults nil)
   (setq completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :config
  (defun corfu-x-eshell-hook ()
    (setq-local corfu-auto nil)
    (corfu-mode))
  (add-hook 'eshell-mode-hook 'corfu-x-eshell-hook)
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 2)
  (setq corfu-auto-delay 0.25)
  (setq corfu-popupinfo-delay '(0.5 . 0.2))
  (setq corfu-preview-current 'insert)
  (setq corfu-preselect 'prompt)
  (setq corfu-on-exact-match nil)

  :bind
  (:map corfu-map
        ("M-SPC" . corfu-insert-separator)
        ("C-n"   . corfu-next)
        ("C-p"   . corfu-previous)
        ("TAB"   . corfu-insert)
        ("RET"   . nil))

  :init
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  (global-corfu-mode))

(use-package cape
  :bind
  (("C-c f" .  cape-file))

  :config
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (defalias 'dabbrev-after-2 (cape-capf-prefix-length #'cape-dabbrev 2))
  (add-to-list 'completion-at-point-functions 'dabbrev-after-2 t)

  (cl-pushnew #'cape-file completion-at-point-functions)

  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.

  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-dict)

  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package marginalia
  :config
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

(use-package which-key
  :config
  (setq which-key-idle-delay 0.75)
  (which-key-mode))

(use-package eglot
  :bind
  ("M-k" . eglot-code-actions)
  :hook ((eglot-managed-mode . eglot-inlay-hints-mode)
         (typescript-ts-base-mode . eglot-ensure)
         (js-base-mode . eglot-ensure)
         (ruby-ts-base-mode . eglot-ensure)
         (scss-ts-base-mode . eglot-ensure))
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package flycheck-eglot
  :config
  (global-flycheck-eglot-mode)
  :after (flycheck eglot))

(defun luda/save-word ()
  "Mark a Flyspell reported error as acceptable."

  (interactive)

  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(use-package flyspell
  :bind
   ("C-x $" . 'luda/save-word)
   ("C-'" . 'flyspell-auto-correct-previous-word)
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

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

;; (use-package treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

(use-package projectile
  :config
  (projectile-global-mode)
  :bind
  ("s-p" . projectile-command-map)
  ("C-c p" . projectile-command-map))

(use-package surround
  :bind-keymap
  ("M-'" . surround-keymap))

(use-package objed
  :config
  (setq objed-modeline-hint nil)
  (objed-mode)
  :bind
  ("M-SPC" . objed-activate-object))

;; Clean and straightforward undo/redo
(use-package undo-fu
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

(use-package multi-vterm
  :bind
  ("M-v" . multi-vterm-dedicated-toggle)
  :init
  (setopt multi-vterm-dedicated-window-height-percent 30))

(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(defun luda/switch-theme (theme)
  "Load THEME after unloading previously loaded themes N.B. this will not remove any customization done outside of themes."

  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defun luda/kill-frame ()
  "Delete frame or kill Emacs if there is only one frame."
  (interactive)
  (condition-case nil
      (delete-frame)
    (error (save-buffers-kill-terminal))))

(global-set-key (kbd "C-x C-c") 'luda/kill-frame)

(use-package transpose-frame
  :bind
  ("C-x |" . transpose-frame))

(use-package coffee-mode
  :config
  (setq coffee-tab-width 2))

(use-package web-mode
  :mode "\\.erb\\'")

(use-package haml-mode
  :defer t)

(use-package jtsx
  :mode (("\\.jsx?\\'" . jsx-mode)
         ("\\.tsx?\\'" . tsx-mode))
  :config
  (setq js-indent-level 2)
  (setq typescript-ts-mode-indent-offset 2)
  (setq jtsx-switch-indent-offset 0)
  (setq jtsx-indent-statement-block-regarding-standalone-parent nil)
  (setq jtsx-jsx-element-move-allow-step-out t)
  (setq jtsx-enable-jsx-electric-closing-element t))

(use-package docker
  :bind ("C-c d" . docker))

(use-package dockerfile-mode)

(setq luda/default-org-directory (expand-file-name "org" luda/local-root))
(setq luda/sync-org-directory (expand-file-name "Dropbox/org" luda/local-root))

(setq luda/org-dir
      (if (file-directory-p luda/default-org-directory)
          luda/default-org-directory
        luda/sync-org-directory))

(setq luda/current-journal (expand-file-name "journal.org" luda/org-dir))
(setq luda/org-id-locations-file (expand-file-name ".org-id-locations" luda/org-dir))

(setq luda/todo-keywords
      `((sequence
         "CALENDAR(c!)" "SHORT(s!)" "LONG(l!)" "|" "DONE(d!)" "NOPE(-!)")))

(use-package org
  :config
  (setq org-log-done 'time)
  (setq org-log-reschedule 'time)
  (setq org-log-into-drawer t)
  (setq org-startup-indented t)
  (setq org-startup-truncated nil)
  (setq org-todo-keywords luda/todo-keywords)
  (setq org-id-track-globally t)
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (setq org-id-locations-file luda/org-id-locations-file)
  (setq org-id-locations-file-relative t)
  (setq org-fontify-whole-heading-line t)
  ;; One of my big uses for Org is my literate config so having elisp as a template is a must
  (add-to-list 'org-structure-template-alist '("sl" . "src emacs-lisp"))
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link))



(use-package deft
  :config
  (setq deft-extensions '("org"))
  (setq deft-directory luda/org-dir)
  (setq deft-recursive-ignore-dir-regexp "\\(?:\\.\\|\\.\\.\\|roam\\|brain\\)")
  (setq deft-ignore-file-regexp "\\(?:~\\|py\\)$")
  (setq deft-recursive t))

(use-package magit
  :bind
   ("C-M-;" . magit-status))

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
