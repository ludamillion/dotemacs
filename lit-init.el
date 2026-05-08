;;; Esprit Emacs --- A literate configuration -*- lexical-binding: t -*-
;;; Copyright (C) 2026 Luke D. Inglis

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

(setopt straight-check-for-modifications nil)
(setopt straight-use-package-by-default t)

(use-package on
  :demand t
  :straight (:type git :host gitlab :repo "axgfn/on.el"))

(use-package use-package-xdg
  :demand t
  :straight (use-package-xdg :type git
                             :host codeberg
                             :repo "rossabaker/use-package-xdg"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / Sensible edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

(defun reload-init-file ()
  "Reload the file referenced by `user-init-file`."

  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(keymap-global-set "<f5>" 'reload-init-file)

(setq vc-make-backup-files nil     ; No backup of files under version contr
      backup-by-copying t          ; Don't clobber symlinks
      version-control t            ; Version numbers for backup files
      delete-old-versions t        ; Delete excess backup files silently
      kept-old-versions 3          ; Number of old versions to keep
      kept-new-versions 6          ; Number of new versions to keep
      delete-by-moving-to-trash t  ; Delete files to trash
      create-lockfiles nil)        ; More trouble than worth

(use-package recentf
  :hook
  (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 100)
  :xdg-state
  (recentf-save-file "recentf"))

(use-package saveplace
  :xdg-state
  (save-place-file "saveplace")
  :custom
  (save-place-forget-unreadable-files t))

(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))

(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

(setq savehist-watchlist
      '(bookmark-history
        command-history
        custom-variable-history
        face-name-history
        file-name-history
        minibuffer-history
        query-replace-history
        read-char-history
        read-expression-history
        set-variable-value-history
        kill-ring))

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

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-variables
   '(
     "PATH"
     "MANPATH"
     "XDG_CONFIG_DIRS"
     "XDG_DATA_DIRS"
     ))
  :config
  (exec-path-from-shell-initialize))

(use-package asdf
  :config
  (asdf-enable))

(defvar esprit/local-root "~/"
  "Convenience pointer to my local root directory.")

(when (string-equal system-type "darwin")
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq dired-use-ls-dired nil))

(defun esprit/make-scratch-frame ()
  "Create a new frame and switch to *scratch* buffer."
  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))

(defun esprit/make-eat-frame ()
  "Create a new frame and create an Eat buffer."
  (interactive)
  (select-frame (make-frame))
  (eat-project))

(defvar-keymap esprit-frame-map
  :doc "Prefix map for frame operations."
  "m" #'make-frame
  "n" #'esprit/make-scratch-frame
  "v" #'esprit/make-eat-frame)

(keymap-global-set "M-n" esprit-frame-map)

(use-package esprit-themes
  ;; :straight (:type git :host github :repo "ludamillion/esprit-themes")
  :straight nil
  :load-path "~/code/esprit-themes"
  :demand t)

(use-package esprit-line
  :demand t
  :straight nil
  :load-path "~/code/esprit-line"
  :custom
  (esprit-line-glyph-alist esprit-line-glyphs-unicode)
  :config (esprit-line-mode))

(use-package circadian
  :custom
  (calendar-latitude 42.4)
  (calendar-longitude -71.0)
  (circadian-themes '((:sunrise . esprit-amber-light)
                      (:sunset  . esprit-azure-dark)))
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

(defun esprit/eglot-capf ()
  (setq-local completion-at-point-functions
              (list ((cape-capf-buster #'eglot-completion-at-point)
		     #'cape-file))))

(defun esprit/cape-capf-setup-lsp ()
  "Replace the default `lsp-completion-at-point' with its
`cape-capf-buster' version."
  (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
        (cape-capf-buster #'eglot-completion-at-point))
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

(defun esprit/cape-capf-setup-org ()
  (add-to-list 'completion-at-point-functions (cape-capf-super #'cape-dict #'cape-dabbrev)))

(defun esprit/cape-capf-setup-git-commit ()
  (let ((result))
    (dolist (element '(cape-dabbrev cape-symbol) result)
      (add-to-list 'completion-at-point-functions element))))

(use-package cape
  :config
  (setq completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless))))
  :hook
  ((eglot-managed-mode . esprit/eglot-capf)
   (org-mode . esprit/cape-capf-setup-org)
   (git-commit-mode . esprit/cape-capf-setup-git-commit)))

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
  :xdg-state
  (save-visited-files-location "save-visited-files")
  :custom
  (save-visited-files-ignore-tramp-files t)
  (save-visited-files-ignore-directories nil)
  (save-visited-files-auto-restore nil))

;; Add prompt indicator to `completing-read-multiple'.
;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
(defun crm-indicator (args)
  (unless 
      (cons (format "[crm: %s] %s"
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

(defvar-keymap esprit-modes-toggles-map
  :name "esprit-toggles"
  :doc "Esprit prefix key maps | minor mode toggling."
  "v" #'global-visual-line-mode
  "f" #'toggle-frame-fullscreen
  "w" #'whitespace-mode
  "c" #'command-log-mode)

(keymap-set global-map "C-x m" esprit-modes-toggle-map)

(use-package which-key
  :config
  (setq which-key-idle-delay 0.75)
  (which-key-mode))

(use-package projectile
  :config
  (projectile-mode)
  :custom
  (projectile-project-search-path `(,(concat esprit/local-root "code")))
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

(use-package undo-fu
  :custom
  (undo-fu-allow-undo-in-region t)
  (undo-limit 67108864) ; 64mb.
  (undo-strong-limit 100663296) ; 96mb.
  (undo-outer-limit 1006632960) ; 960mb.
  :bind
  ("C-/" . undo-fu-only-undo)
  ("C-?" . undo-fu-only-redo))

;; Persist undo history across sessions
(use-package undo-fu-session
  :hook
  (after-init . undo-fu-session-global-mode)
  :xdg-state
  (undo-fu-session-directory "undo-fu-session")
  :custom
  (undo-fu-session-compression 'nil)
  (undo-fu-session-incompatible-files
   '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(setq visible-bell nil
      ring-bell-function #'ignore)

(setq switch-to-buffer-obey-display-actions t)

(use-package vterm
  :init
  (setq vterm-max-scrollback 10000)
  :bind
  ("C-x !" . projectile-run-vterm))

(defun esprit/helpful-switch-to-buffer (buffer-or-name)
  "Switch to helpful BUFFER-OR-NAME.

If we're already in a helpful buffer than reuse its window;
otherwise create a new window."

  (if (eq major-mode 'heplful-mode)
      switch-to-buffer buffer-or-name)
  (pop-to-buffer buffer-or-name))

(use-package helpful
  :custom
  (helpful-switch-to-buffer #'esprit/helpful-switch-to-buffer)
  :bind
  ("C-h o"    . #'helpful-symbol)
  ("C-h f"    . #'helpful-callable)
  ("C-c F"    . #'helpful-function)
  ("C-h v"    . #'helpful-variable)
  ("C-h k"    . #'helpful-key)
  ("C-h x"    . #'helpful-command)
  ("C-c C-d"  . #'helpful-at-point))

(add-to-list 'display-buffer-alist
             '((derived-mode special-mode)
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 80)
               (window-parameters
                (no-delete-other-windows . t))))

(defun esprit/quit-dwim (&optional arg)
  "If current frame is the last frame kill emacs, else delete it."
  (interactive "P")

  (if (> (length (frame-list)) 1)
      (delete-frame arg)
    (if (y-or-n-p (format "Are you sure you want to close the last frame?"))
	(save-buffers-kill-terminal arg)
      (message "Great, back to what you were doing then."))))

(global-set-key (kbd "C-x C-c") 'esprit/quit-dwim)

(keymap-set global-map "C-x k" 'kill-current-buffer)
(keymap-set global-map "C-x C-k" 'kill-buffer)

(setq esprit/default-org-directory (expand-file-name "org" esprit/local-root))
(setq esprit/sync-org-directory (expand-file-name "Dropbox/org" esprit/local-root))
(setq esprit/beorg-directory (expand-file-name "Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org" esprit/local-root))

(setq esprit/org-dir
      (if (file-directory-p esprit/default-org-directory)
          esprit/default-org-directory
        esprit/sync-org-directory))

(setq esprit/journal-file (expand-file-name "journal.org" esprit/org-dir))
(setq esprit/projects-file (expand-file-name "projects.org" esprit/org-dir))
(setq esprit/org-id-locations-file (expand-file-name ".org-id-locations" esprit/org-dir))

(setq esprit/todo-keywords
      `((sequence
         "TODO(t!)" "ACTIVE(a!)" "WAITING(w!)" "MAYBE(m!)" "|" "DONE(d!)" "OBSOLETE(o!)" "CANCELED(-!)")))

(use-package org
  :init
  (setq org-export-backends '(ascii md html icalendar latex))
  :custom
  (setq org-default-notes-file (expand-file-name "todo.org" esprit/org-dir)) ;; Should maybe be inbox
  (org-log-done 'time)
  (org-log-reschedule 'time)
  (org-log-into-drawer t)
  (org-startup-truncated nil)
  (org-todo-keywords esprit/todo-keywords)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-id-locations-file esprit/org-id-locations-file)
  (org-id-locations-file-relative t)
  (org-fontify-whole-heading-line t)
  (org-agenda-files `(,esprit/org-dir ,esprit/beorg-directory))
  (org-latex-pdf-process '("tectonic %f"))
  (org-capture-templates
   '(("f" "Fleeting note" item
      (file+headline org-default-notes-file "Notes")
      "- %?")
     ("p" "Permanent note" plain
      (file denote-last-path)
      #'denote-org-capture
      :no-save t
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t)
     ("t" "New task" entry
      (file+headline org-default-notes-file "Tasks")
      "* TODO %i%?")))
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell . t)))

(use-package denote
  :init
  (denote-rename-buffer-mode 1)
  :custom
  (denote-directory esprit/notes-directory)
  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic)))))

;; Denote extensions
(use-package consult-notes
  :bind
  ("M-s n" . #'consult-notes)
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :custom
  (consult-notes-file-dir-sources
   `(("Denote" ?d ,esprit/notes-directory))))

(use-package emacs-lock
  :config
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill)))

(use-package wgrep
  :straight t
  :custom
  (wgrep-auto-save-buffer t))

(use-package combobulate
  :custom
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (combobulate-key-prefix "C-c o")
  :hook
  ((prog-mode . combobulate-mode)))

(use-package esprit-movement
  :straight nil
  :bind ("C-a" . #'esprit/beginning-of-line))

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

(defvar esprit/ediff-original-windows nil)

(defun esprit/store-pre-ediff-winconfig ()
  "Stores the window arrangement before opening ediff."
  (setq esprit/ediff-original-windows (current-window-configuration)))

(defun esprit/restore-pre-ediff-winconfig ()
  "Resets original window arrangement"
  (set-window-configuration esprit/ediff-original-windows))

(use-package ediff
  :hook ((ediff-before-setup . 'esprit/store-pre-ediff-winconfig)
         (ediff-quit . 'esprit/restore-pre-ediff-winconfig))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

(provide 'init)
