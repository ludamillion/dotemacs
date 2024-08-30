;;; dotemacs --- A literate Emacs configuration -*- lexical-binding: t -*-
;;; This file has been generated from dotemacs.org file. DO NOT EDIT.

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

(setq straight-check-for-modifications nil)

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

(setq package-list
      '(
        accent               ; Easier access to accented characters
        ace-window           ; Easier moving between windows
        apheleia             ; Code formatting without the pain and blocking
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
        no-littering         ; Keep our things clean and tidy
        orderless            ; Completion style for matching regexps in any order
				pdf-tools
        projectile           ; Project scoped stuffness
        rg                   ; Ripgrep for speed and profit(?)
        save-visited-files   ; Simplest form of session persistance
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
        which-key            ; Discovery method for key bindings
        ))

;; Install packages that are not yet installed
(dolist (package package-list)
  (straight-use-package package))

(straight-use-package
 '(jtsx :type git :host github :repo "llemaitre19/jtsx"))

(straight-use-package
 '(asdf :type git :host github :repo "tabfugnic/asdf.el"))

(straight-use-package
 '(eglot-ltex :type git :host github :repo "emacs-languagetool/eglot-ltex"))

(straight-use-package
 '(eglot-booster :type git :host github :repo "jdtsmith/eglot-booster"))

(straight-use-package '(org :type built-in))

(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / Sensible edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

(defun reload-init-file ()
  "Reload the file referenced by `user-init-file`."

  (interactive)
	(load-file (expand-file-name "init.el" user-emacs-directory)))

(keymap-global-set "<f5>" 'reload-init-file)

(use-package no-littering
  :init
  (setq no-littering-etc-directory "~/.cache/emacs/etc/"
        no-littering-var-directory "~/.cache/emacs/var/")
  (when (fboundp 'startup-redirect-eln-cache)
    (startup-redirect-eln-cache
     (convert-standard-filename
      (expand-file-name  "eln-cache/" no-littering-var-directory))))
  (setq vc-make-backup-files nil     ; No backup of files under version contr
        backup-by-copying t          ; Don't clobber symlinks
        version-control t            ; Version numbers for backup files
        delete-old-versions t        ; Delete excess backup files silently
        kept-old-versions 3          ; Number of old versions to keep
        kept-new-versions 6          ; Number of new versions to keep
        delete-by-moving-to-trash t  ; Delete files to trash
        create-lockfiles nil)        ; More trouble than worth
  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/")))
        auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setq bookmark-default-file (expand-file-name "bookmark" user-emacs-directory))

(use-package recentf
  :custom
  (recentf-max-menu-items 10)
  (recentf-max-saved-items 100)
  :init
  (recentf-mode t))

(use-package saveplace
  :custom
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-forget-unreadable-files t))

(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

(use-package savehist
  :init
  (savehist-mode t)
  :custom
  (kill-ring-max 50)
  (history-length 50)
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
     file-name-history))
  :config
  (put 'minibuffer-history         'history-length 50)
  (put 'file-name-history          'history-length 50)
  (put 'set-variable-value-history 'history-length 25)
  (put 'custom-variable-history    'history-length 25)
  (put 'query-replace-history      'history-length 25)
  (put 'read-expression-history    'history-length 25)
  (put 'read-char-history          'history-length 25)
  (put 'face-name-history          'history-length 25)
  (put 'bookmark-history           'history-length 25))

(setq history-delete-duplicates t)

(let (message-log-max)
  (savehist-mode))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(use-package asdf
  :config
  (asdf-enable))

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

(defvar-keymap liminal-frame-map
  :doc "Liminal prefix map for frame operations."
  "m" #'make-frame
  "n" #'luda/make-scratch-frame)

(keymap-set global-map "M-n" liminal-frame-map)

(use-package sensible-settings
  :load-path "~/code/sensible-settings"
  :init
  (setopt sensible-font-size 16
          sensible-manage-cursor t
          sensible-manage-fonts t
          sensible-manage-ui t
          sensible-manage-ux t)
  :config
  (sensible-mode))

(use-package sensible-themes
	:after 'sensible-settings
  :load-path "~/code/sensible-themes")

(use-package sensible-modeline
  :after 'sensible-themes
  :load-path "~/code/sensible-modeline"
  :init
  (setopt mode-line-format nil)
  :hook
  (prog-mode            . sensible-modeline-prog-mode)
  (text-mode            . sensible-modeline-text-mode)
  (org-mode             . sensible-modeline-org-mode)
  (term-mode            . sensible-modeline-term-mode)
  (vterm-mode           . sensible-modeline-term-mode)
  (messages-buffer-mode . sensible-modeline-message-mode)
  (org-capture-mode     . sensible-modeline-org-capture-mode)
  (org-agenda-mode      . sensible-modeline-org-agenda-mode))

(use-package circadian
  :custom
  (calendar-latitude 42.4)
  (calendar-longitude -71.0)
  (circadian-themes '((:sunrise . sensible-amber-light)
                      (:sunset  . sensible-azure-dark)))
  :config
  (circadian-setup))

(setq-default line-spacing 1)
(global-visual-line-mode)

(use-package display-line-numbers
  :custom
  (display-line-numbers-widen t)
  :hook
  ((prog-mode conf-mode) . display-line-numbers-mode))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))

(add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1)))

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
  :config
  (setq completion-styles '(orderless partial-completion basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))

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

(defun luda/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'tempel-expand
                     #'cape-file))))

(defun luda/cape-capf-setup-lsp ()
  "Replace the default `lsp-completion-at-point' with its
`cape-capf-buster' version."
  (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
        (cape-capf-buster #'eglot-completion-at-point))
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

(defun luda/cape-capf-setup-org ()
  (add-to-list 'completion-at-point-functions (cape-capf-super #'cape-dict #'cape-dabbrev)))

(defun luda/cape-capf-setup-git-commit ()
  (let ((result))
    (dolist (element '(cape-dabbrev cape-symbol) result)
      (add-to-list 'completion-at-point-functions element))))

(use-package cape
  :config
  (setq completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless))))
  :hook
  ((eglot-managed-mode . luda/eglot-capf)
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

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

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
         ("M-*" . tempel-insert))
  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)

(use-package ace-window
  :bind
  ("M-o" . 'ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
        aw-dispatch-always t
        aw-dispatch-alist
        '((?x aw-delete-window "Ace - Delete Window")
          (?c aw-swap-window "Ace - Swap Window")
          (?n aw-flip-window)
          (?v aw-split-window-vert "Ace - Split Vert Window")
          (?h aw-split-window-horz "Ace - Split Horz Window")
          (?m delete-other-windows "Ace - Maximize Window")
          (?g delete-other-windows)
          (?b balance-windows)
          (?u (lambda ()
                (progn
                  (winner-undo)
                  (setq this-command 'winner-undo))))
          (?r winner-redo))))

(keymap-global-set "C-M-o" 'mode-line-other-buffer)

(use-package save-visited-files
  :init
  (save-visited-files-mode t)
  :custom
  (save-visited-files-location (expand-file-name "save-visited-files" user-emacs-directory))
  (save-visited-files-ignore-tramp-files t)
  (save-visited-files-ignore-directories nil)
  (save-visited-files-auto-restore nil))

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

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

(defvar-keymap liminal-modes-toggle-map
  :doc "Liminal prefix key maps | mode toggling."
  "w" #'whitespace-mode)

(keymap-set global-map "C-x m" liminal-modes-toggle-map)

(use-package which-key
  :config
  (setq which-key-idle-delay 0.75)
  (which-key-mode))

(use-package projectile
	:config
	(projectile-mode)
	:custom
	(projectile-project-search-path `(,(concat luda/local-root "code")))
	:bind (:map projectile-mode-map
							("s-," . projectile-command-map)))

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-x l r" . eglot-rename)
              ("M-k" . eglot-code-actions))
  :hook ((prog-mode . eglot-ensure)
         (css-mode . eglot-ensure))
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (add-to-list 'eglot-server-programs
               '(ruby-ts-mode . ("solargraph" "stdio"))))

(use-package eglot-booster
  :after eglot
  :config
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

(use-package apheleia
  :custom
  ((alist-get 'prettier apheleia-formatters)
   '(npx "prettier" "--print-width" "100" file))
  :config
  (add-to-list 'apheleia-mode-alist '(jsx-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(ruby-ts-mode . rubocop)))

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

(use-package occur
  :bind (:map isearch-mode-map ("C-o" . isearch-occur)))

(use-package re-builder
  :bind (("M-s %" . #'re-builder)
         :map reb-mode-map ("RET" . #'reb-replace-regexp)
         :map reb-lisp-mode-map ("RET" . #'reb-replace-regexp))
  :custom
  (reb-re-syntax 'string)
  :config
  (defvar my/re-builder-positions nil
    "Store point and region bounds before calling re-builder")

  (advice-add 're-builder
              :before
              (defun my/re-builder-save-state (&rest _)
                "Save into `my/re-builder-positions' the point and region
positions before calling `re-builder'."
                (setq my/re-builder-positions
                      (cons (point)
                            (when (region-active-p)
                              (list (region-beginning)
                                    (region-end)))))
                (message "Set positions to: %s" my/re-builder-positions)))

  (defun reb-replace-regexp (&optional delimited)
    "Run `query-replace-regexp' with the contents of re-builder. With
non-nil optional argument DELIMITED, only replace matches
surrounded by word boundaries."
    (interactive "P")
    (reb-update-regexp)
    (let* ((re (reb-target-value 'reb-regexp))
           (replacement (query-replace-read-to
                         re
                         (concat "Query replace"
                                 (if current-prefix-arg
                                     (if (eq current-prefix-arg '-) " backward" " word")
                                   "")
                                 " regexp"
                                 (if (with-selected-window reb-target-window
                                       (region-active-p)) " in region" ""))
                         t))
           (pnt (car my/re-builder-positions))
           (beg (cadr my/re-builder-positions))
           (end (caddr my/re-builder-positions)))
      (with-selected-window reb-target-window
        (goto-char pnt) ; replace with (goto-char (match-beginning 0)) if you want
                                        ; to control where in the buffer the replacement starts
                                        ; with re-builder
        (setq my/re-builder-positions nil)
        (reb-quit)
        (query-replace-regexp re replacement delimited beg end)))))

(use-package accent
  :bind
  ("C-x e" . 'accent-menu))

(use-package surround
  :bind-keymap ("M-'" . surround-keymap))

(defun current-line-empty-p ()
  "Return true is the point is in an empty line, false otherwise."

  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:blank:]]*$")))

(defun delete-blank-space-dwim ()
  "Delete surrounding whitespace in do-what-I-mean manner.

When point is in a blank line invoke (delete-blank-lines).
When point is in whitespace between non-whitespace invoke (delete-horizontal-space)."

  (interactive)

  (if (current-line-empty-p)
      (delete-blank-lines)
    (delete-horizontal-space)))

(global-set-key (kbd "M-\\") 'delete-blank-space-dwim)

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(super up)]  'move-line-up)
(global-set-key [(super down)]  'move-line-down)

(global-set-key (kbd "C-z") 'zap-up-to-char)

(global-set-key [remap downcase-word] 'downcase-dwim)
(global-set-key [remap upcase-word] 'upcase-dwim)
(global-set-key [remap capitalize-word] 'capitalize-dwim)

(use-package elec-pair
  :config
  (electric-pair-mode))

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(keymap-set global-map "C-c '" 'narrow-to-region-indirect)

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
  (apheleia-mode t))

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :mode "\\.pryrc\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'"
  :config
  (apheleia-mode t))

(use-package yaml-ts-mode
  :mode "\\.y[a]?ml")

(setq luda/default-org-directory (expand-file-name "org" luda/local-root))
(setq luda/sync-org-directory (expand-file-name "Dropbox/org" luda/local-root))
(setq luda/beorg-directory (expand-file-name "Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org" luda/local-root))

(setq luda/org-dir
      (if (file-directory-p luda/default-org-directory)
          luda/default-org-directory
        luda/sync-org-directory))

(setq luda/journal-file (expand-file-name "journal.org" luda/org-dir))
(setq luda/projects-file (expand-file-name "projects.org" luda/org-dir))
(setq luda/org-id-locations-file (expand-file-name ".org-id-locations" luda/org-dir))

(setq luda/todo-keywords
      `((sequence
         "TODO(t!)" "ACTIVE(a!)" "WAITING(w!)" "MAYBE(m!)" "|" "DONE(d!)" "OBSOLETE(o!)" "CANCELED(-!)")))

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

(defvar-keymap liminal-vc-branch-map
  :doc "Liminal prefix map for version control branch actions."
  "b" #'magit-checkout
  "c" #'magit-branch-create)

(defvar-keymap liminal-vc-pull-map
  :doc "Liminal prefix map for version control pull/fetch actions."
  "p" #'magit-pull-from-pushremote
  "u" #'magit-pull-from-upstream
  "e" #'magit-pull-branch)

(defvar-keymap liminal-vc-file-map
  :doc "Liminal prefix map for version control file actions."
  "r" #'magit-file-rename)

(defvar-keymap liminal-vc-map
  :doc "Liminal prefix key maps version control operations ."
  "b" liminal-vc-branch-map
  "F" liminal-vc-pull-map
  "f" liminal-vc-file-map)

(keymap-set global-map "C-x g" liminal-vc-map)

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
