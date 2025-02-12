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

(require 'use-package)
(require 'straight)

(setopt straight-use-package-by-default t
				use-package-always-defer t)

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(use-package exec-path-from-shell
  :demand t
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-variables '("PATH" "MANPATH" "XDG_CONFIG_DIRS" "XDG_DATA_DIRS"))
  :config
  (exec-path-from-shell-initialize))

(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / Logos Edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

(defun luda/reload-init-file ()
  "Reload the init.el file in the Emacs directory."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(keymap-global-set "<f5>" 'luda/reload-init-file)

(use-package on
  :demand t
  :straight (:type git :host gitlab :repo "ajgrf/on.el"))

(use-package use-package-xdg
  :demand t
  :straight (use-package-xdg :type git
                             :host codeberg
                             :repo "rossabaker/use-package-xdg"))

(set-fontset-font
 t
 'symbol
 (cond
  ((member "Apple Symbols" (font-family-list)) "Apple Symbols")
  ((member "Symbola" (font-family-list)) "Symbola")))

(set-fontset-font
 t
 'emoji
 (cond
  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
  ((member "Symbola" (font-family-list)) "Symbola")))

(use-package mini-ontop
  :straight (:type git :host github :repo "hkjels/mini-ontop.el")
  :hook
  (on-first-input . mini-ontop-mode))

(use-package auto-save
  :no-require
  :straight nil
  :xdg-state
  (auto-save-list-prefix "saves/"))

(use-package filelock
  :straight (:type built-in)
  :custom
  (create-lockfiles nil))

(use-package autorevert
  :straight (:type built-in)
  :custom
  (global-auto-revert-mode t))

(use-package recentf
  :hook
  (on-first-input . recentf-mode)
  :custom
  (recentf-max-saved-items 100)
  :xdg-state
  (recentf-save-file "recentf"))

(setopt inhibit-eol-conversion t)
(setopt indent-tabs-mode nil)

(defun no-junk-please-were-unixish ()
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (set-buffer-file-coding-system 'unix))))

;; (add-hook 'find-file-hook 'no-junk-please-were-unixish)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package dired
  :straight (:type built-in)
  :custom
  (dired-dwim-target t))

(use-package bookmark
  :commands (bookmark-set)
  :xdg-state
  (bookmark-default-file "bookmarks.eld"))

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

(defun luda/make-scratch-frame ()
  "Create a new frame and switch to *scratch* buffer."

  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))

(defun luda/make-eat-frame ()
  "Create a new frame and create a vterm buffer."

  (interactive)
  (select-frame (make-frame))
  (eat-project))

(defvar-keymap sensible-frame-map
  :doc "Liminal prefix map for frame operations."
  "m" #'make-frame
  "n" #'luda/make-scratch-frame
  "v" #'luda/make-eat-frame)

(keymap-global-set "M-n" sensible-frame-map)

;; Sensible Configuation
;;   - Set up my own little bundle of packages to tailor the Emacs experience

(use-package nerd-icons
  :demand t)

(use-package sensible-settings
  :straight (:type git :local-repo "~/code/emacs-lisp/sensible-settings")
  :demand t
  :init
  (setf sensible-manage-cursor t
        sensible-manage-fonts t
        sensible-manage-ui t
        sensible-manage-ux t)
  :config
  (sensible-mode))

(use-package sensible-modeline
  :straight (:type git :local-repo "~/code/emacs-lisp/sensible-modeline")
  :demand t
  :requires 'nerd-icons
  :hook
  (on-init-ui . sensible-modeline-mode))

(use-package logos-themes
  :straight (:type git :local-repo "~/code/emacs-lisp/logos-themes")
  :demand t)

;;; Choose light or dark theme based on the time of day at my location

(use-package circadian
  :demand t
  :custom
  (calendar-latitude 42.4)
  (calendar-longitude -71.0)
  (circadian-themes '((:sunrise . logos-light)
                      (:sunset  . logos-dark)))
  :config
  (circadian-setup))

;;;; PDF Tools

(use-package pdf-tools
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights")
  :hook
  (pdf-view-mode-hook . (lambda() (display-line-numbers-mode -1)))
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("luda-lisp" "luda-modules"))

(require 'luda-completion)
(require 'luda-lsp)
(require 'luda-analysis)
(require 'luda-editing)
(require 'luda-formatting)
(require 'luda-interface)
(require 'luda-vc)
(require 'luda-term)
(require 'luda-prog-modes)

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

(use-package whitespace-mode
  :straight (:type built-in)
  :custom
  (whitespace-style
   '(face tabs spaces trailing lines-tail space-before-tab newline indentation
          empty space-after-tab space-mark tab-mark newline-mark missing-newline-at-eof)))

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

(use-package yaml-pro
  :after yaml-ts-mode
  :hook (yaml-ts-mode . yaml-pro-ts-mode))

(defvar luda/local-root "~/"
  "The explicit root directory value.")

(defvar luda/default-org-directory
  (expand-file-name "org" luda/local-root))

(setq luda/sync-org-directory (expand-file-name "Dropbox/org" luda/local-root))
(setq luda/beorg-directory
      (expand-file-name "Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org" luda/local-root))

(setq luda/org-dir
      (if (file-directory-p luda/default-org-directory)
          luda/default-org-directory
        luda/sync-org-directory))

(setq luda/journal-file (expand-file-name "journal.org" luda/org-dir))
(setq luda/projects-file (expand-file-name "projects.org" luda/org-dir))
(setq luda/notes-directory (expand-file-name "notes" luda/org-dir))
(setq luda/org-id-locations-file (expand-file-name ".org-id-locations" luda/org-dir))

(use-package org
  :straight (:type built-in)
  :init
  (setq org-export-backends '(ascii md html icalendar latex))
  :custom
  (org-default-notes-file (expand-file-name "inbox.org" luda/org-dir)) ;; Should maybe be inbox
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-capture-bookmark nil)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-id-locations-file luda/org-id-locations-file)
  (org-id-locations-file-relative t)
  (org-fontify-whole-heading-line t)
  (org-agenda-files `(,luda/org-dir ,luda/beorg-directory))
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

  ;; ;; ;; One of my big uses for Org is my literate config so having elisp as a template is a must
  (add-to-list 'org-structure-template-alist '("sl" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("s#" . "src csharp"))

  (org-babel-do-load-languages
	 'org-babel-load-languages
	 '((emacs-lisp . t)
		 (shell . t)
		 (csharp . t)))

  :bind
  ("M-<return>" . org-insert-heading-after-current)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link))

(use-package doct
  :straight t
  ;;recommended: defer until calling doct
  :commands (doct))

(use-package denote
  :init
  (require 'denote-org-extras)
  (denote-rename-buffer-mode 1)
  :custom
  (denote-directory luda/notes-directory)
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
   `(("Denote" ?d ,luda/notes-directory))))

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

(use-package wgrep
  :straight t
  :custom
  (wgrep-auto-save-buffer t))

(use-package plz
  :straight t)

(use-package combobulate
  :custom
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (combobulate-key-prefix "C-c o")
  :hook
  ((prog-mode . combobulate-mode)))

(provide 'init)
