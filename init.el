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
        avy                  ; Jump to things (but really much, much more...)
        cape                 ; Completion At Point Extensions
        circadian            ; Change my theme in rhythm with nature
        consult              ; Consulting completing-read
        consult-lsp          ; LSP extras for consult to, well, consult
        corfu                ; Completion Overlay Region FUnction
        deft                 ; Enhanced note taking with Org
        embark
        embark-consult
        exec-path-from-shell ; Get environment variables such as $PATH from the shell
        f                    ; Modern API for working with files and directories
        flycheck             ; Enhanced syntax checking, more flexible than flymake
        flycheck-eglot       ; Allow Flycheck to understand Eglot as a checker
        flyspell
        haml-mode            ; Rails templates not covered by treesitter or web-mode
        helpful              ; A better help buffer
        imenu-list           ; Show imenu entries in a separate
        language-id
        lua-mode
        magit                ; A Git porcelain inside Emacs.
        marginalia           ; Enrich existing commands with completion annotations
        markdown-mode        ; Major mode for Markdown-formatted text
        no-littering         ; Keep our things clean and tidy
        orderless            ; Completion style for matching regexps in any order
        org-auto-tangle
        projectile           ; Project scoped stuffness
        rainbow-mode         ; Sometime you just need to see the colors
        rg                   ; Ripgrep for speed and profit(?)
        slim-mode
        smartscan            ; A little package to quick hop to 
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
        zetteldeft           ; Put your deft notes in little slip boxen
        ))

;; Install packages that are not yet installed
(dolist (package package-list)
  (straight-use-package package))

;; Install a selection of the N Λ N O suite of packages install straight from GitHub

;; A cleaner, more minimal Org agenda
(straight-use-package
 '(nano-agenda :type git :host github :repo "rougier/nano-agenda"))

(straight-use-package
 '(jtsx :type git :host github :repo "llemaitre19/jtsx"))

;; (straight-use-package
;;  '(liminal-theme :local-repo "~/code/liminal-theme"))

;; ;; Modeline (eventually to be replace with my own)
;; (straight-use-package
;;  '(liminal-modeline :local-repo "~/code/liminal-modeline"))

(straight-use-package
 '(asdf :type git :host github :repo "tabfugnic/asdf.el"))

(straight-use-package
 '(eglot-ltex :type git :host github :repo "emacs-languagetool/eglot-ltex"))

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

(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

(require 'savehist)

(setq kill-ring-max 50
      history-length 50)

(setq savehist-additional-variables
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

 (put 'minibuffer-history         'history-length 50)
 (put 'file-name-history          'history-length 50)
 (put 'set-variable-value-history 'history-length 25)
 (put 'custom-variable-history    'history-length 25)
 (put 'query-replace-history      'history-length 25)
 (put 'read-expression-history    'history-length 25)
 (put 'read-char-history          'history-length 25)
 (put 'face-name-history          'history-length 25)
 (put 'bookmark-history           'history-length 25)

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

(require 'server)

(unless (server-running-p)
  (start-server))

(defun luda/make-scratch-frame ()
  "Create a new frame and switch to *scratch* buffer."

  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))

(defvar-keymap liminal-new-frame-map
  :doc "Liminal prefix map for creating frames."
  "c" #'make-frame
  "s" #'luda/make-scratch-frame)

(defvar-keymap liminal-frame-map
  :doc "Liminal prefix key maps for frames."
  "n" liminal-new-frame-map)

(keymap-set global-map "M-n" liminal-frame-map)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "s-o") 'other-frame)

(use-package liminal-theme
  :load-path "~/code/liminal-theme"
  :init
  (setopt liminal-manage-cursor t
          liminal-manage-fonts t
          liminal-manage-ui t
          liminal-manage-ux t
          liminal-font-size 16)
  :config
  (liminal-mode))

(use-package liminal-modeline
  :after liminal-theme
  :load-path "~/code/liminal-modeline"
  :init
  (setopt mode-line-format nil)
  :hook
  (prog-mode            . liminal-modeline-prog-mode)
  (text-mode            . liminal-modeline-text-mode)
  (org-mode             . liminal-modeline-org-mode)
  (term-mode            . liminal-modeline-term-mode)
  (vterm-mode           . liminal-modeline-term-mode)
  (messages-buffer-mode . liminal-modeline-message-mode)
  (org-capture-mode     . liminal-modeline-org-capture-mode)
  (org-agenda-mode      . liminal-modeline-org-agenda-mode))

(use-package circadian
  :custom
   (calendar-latitude 42.4)
   (calendar-longitude -71.0)
   (circadian-themes '((:sunrise . liminal-light)
                       (:sunset  . liminal-dark)))
   :config
   (circadian-setup))

(setq-default line-spacing 1)
(global-visual-line-mode)

(use-package display-line-numbers
  :custom
  (display-line-numbers-widen t)
  :hook
  ((prog-mode conf-mode) . display-line-numbers-mode))

(use-package vertico
  :bind (:map vertico-map
              ("C-<backspace>" . vertico-directory-up))
  :custom
  (vertico-scroll-margin 0 "Remove the top/bottom margins of the completion window")
  (vertico-resize t "Let the completion window grow and shrink")
  (vertico-multiform-categories ; Choose a multiform
   '((file reverse)
     (consult-location)
     (imenu buffer)
     (library reverse indexed)
     (org-roam-node reverse indexed)
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
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . wrapper/consult-ripgrep)
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
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-prefix 2
        corfu-auto-delay 0.25
        corfu-popupinfo-delay '(0.5 . 0.2)
        corfu-preview-current 'insert
        corfu-preselect 'prompt
        corfu-on-exact-match nil)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("TAB" . corfu-insert)
        ("RET" . nil))
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

  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
  ;; Bust the Corfu completion cache when using Eglot to ensure fresh completions
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

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

(defun luda/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-file))))

(use-package eglot
  :bind (:map eglot-mode-map
              ("C-x l r" . eglot-rename))
  ;; ("M-k" . eglot-code-actions)
  :hook ((eglot-managed-mode . luda/eglot-capf)
         (ruby-ts-mode . eglot-ensure)
         (jsx-mode . eglot-ensure)
         (css-mode . eglot-ensure)
         (org-mode . eglot-ensure))
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (add-to-list 'eglot-server-programs
               '(ruby-base-mode . ("solargraph" "stdio")))
  (add-to-list 'eglot-server-programs
               '(org-mode . ("efm-langserver"))))

(use-package eglot-ltex
  :hook (text-mode . (lambda ()
                       (require 'eglot-ltex)
                       (eglot-ensure)))
  :init
  (setq eglot-ltex-server-path "/usr/local/bin/ltex-ls"
        eglot-ltex-communication-channel 'stdio))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package flycheck-eglot
  :config
  (global-flycheck-eglot-mode)
  :after (flycheck eglot))

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

(use-package projectile
  :config
  (projectile-global-mode)
  :bind
  ("s-p" . projectile-command-map)
  ("C-c p" . projectile-command-map))

(use-package avy
  :bind ("M-j" . avy-goto-char-timer))

(use-package occur
  :bind (:map isearch-mode-map ("C-o" . isearch-occur)))

(use-package re-builder
  :bind (("M-s %" . #'re-builder)
         :map reb-mode-map ("RET" . #'reb-replace-regexp)
         :map reb-lisp-mode-map ("RET" . #'reb-replace-regexp))
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

(global-set-key (kbd "C-z") 'zap-up-to-char)
(global-set-key (kbd "C-M-z") 'zap-to-char)

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

(global-set-key (kbd "C-x C-m") 'execute-extended-command)

(defun luda/switch-theme (theme)
  "Load THEME after unloading previously loaded themes.

Unloading themes in this manned does not remove any
customization done outside of themes."

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
    (error
     (if (y-or-n-p (format "Are you sure you want to close the last frame?"))
         (save-buffers-kill-terminal)
       (message "Great, back to what you were doing then.")))))

(global-set-key (kbd "C-x C-c") 'luda/kill-frame)

(use-package transpose-frame
  :bind
  ("C-x |" . transpose-frame))

(keymap-set global-map "C-x k" 'kill-this-buffer)
(keymap-set global-map "C-x C-k" 'kill-buffer)

(defun luda/hop-buffer ()
  (interactive)
  (if (= (length (window-list)) 1)
      (switch-to-buffer nil)
    (other-window 1)))

(global-set-key (kbd "M-o") 'luda/hop-buffer)

(use-package css-mode
  :custom
  (css-indent-offset 2))

(use-package rainbow-mode
  :custom
  (rainbow-html-colors nil)
  :hook (css-mode . rainbow-mode))

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

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :mode "\\.pryrc\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'")

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
         "CALENDAR(c!)" "SHORT(s!)" "LONG(l!)" "WAIT(!w)" "|" "DONE(d!)" "NOPE(-!)")))

(use-package org
  :init
  (setq org-export-backends
        '(ascii md html icalendar latex odt))
  :config
  (setq org-default-notes-file (expand-file-name "todo.org" luda/org-dir))
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
  (setq org-agenda-files `(,luda/org-dir ,luda/beorg-directory))
  (setq org-latex-pdf-process
        '("tectonic %f"))

  (setq org-capture-templates
        `(("t" "Todo" entry (file+headline ,luda/projects-file "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("pn" "Project Note" entry (file+headline ,luda/projects-file "Notes")
           "* Bench Note %?\n  %i\n  %a")
          ("j" "Journal" entry (file+olp+datetree ,luda/journal-file)
           "* %?\nEntered on %U\n  %i\n  %a")))

  ;; One of my big uses for Org is my literate config so having elisp as a template is a must
  (add-to-list 'org-structure-template-alist '("sl" . "src emacs-lisp"))

  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link))



(require 'f)

(defun deft-sidebar ()
  (interactive)
  (let ((sidebar-buf (generate-new-buffer deft-buffer)))
    (with-current-buffer sidebar-buf
      (deft-mode))
    (display-buffer-in-side-window sidebar-buf
                                   '((slot . 1)
                                     (dedicated . t)
                                     (window-height . 0.35)))))

(use-package deft
  :commands deft
  :init
  (setq deft-directory (f-expand "notes/" luda/org-dir)
        deft-default-extension "org"
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        deft-auto-save-interval -1.0
        deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))
  :config
  (add-to-list 'deft-extensions "tex"))

(use-package zetteldeft
  :init (zetteldeft-set-classic-keybindings))

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
