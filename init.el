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

(straight-use-package 'use-package)

;; (straight-use-package
;;  '(asdf :type git :host github :repo "tabfugnic/asdf.el"))

;; (straight-use-package
;;  '(eglot-ltex :type git :host github :repo "emacs-languagetool/eglot-ltex"))

;; (straight-use-package
;;  '(use-package-xdg :type git :host codeberg :repo "rossabaker/use-package-xdg"))

;; (straight-use-package
;;  '(:type git :host gitlab :repo "axgfn/on.el"))

;; (straight-use-package '(org :type built-in))

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(use-package use-package-xdg
  :straight (:type git :host codeberg :repo "rossabaker/use-package-xdg")
  :demand t)

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-variables '("PATH" "MANPATH" "XDG_CONFIG_DIRS" "XDG_DATA_DIRS"))
  :config
  (exec-path-from-shell-initialize))

(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / Esprit Edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

(defun esprit/reload-init-file ()
  "Reload the init.el file in the Emacs directory."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(keymap-global-set "<f5>" 'esprit/reload-init-file)

(use-package on
  :straight (:host gitlab :repo "axgfn/on.el")
  :demand t)

(set-fontset-font t 'symbol
                  (cond
                   ((member "Apple Symbols" (font-family-list)) "Apple Symbols")
                   ((member "Symbols Nerd Font" (font-family-list)) "Symbols Nerd Font")
                   ((member "Symbola" (font-family-list)) "Symbola")))

(set-fontset-font t 'emoji
                  (cond
                   ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
                   ((member "Symbols Nerd Font" (font-family-list)) "Symbols Nerd Font")
                   ((member "Symbola" (font-family-list)) "Symbola")))

(set-fontset-font "fontset-default" nil "Symbola")

(use-package diminish
  :straight t
  :demand t)

(defvar esprit-prose-modes
  '(
    bibtex-mode
    context-mode
    git-commit-elisp-text-mode
    latex-mode
    markdown-mode
    org-mode
    rst-mode
    text-mode
    )
  "List of which modes esprit considers prose.")

(use-package mini-ontop
  :straight (:host github :repo "hkjels/mini-ontop.el")
  :hook
  (on-first-input . mini-ontop-mode))

(use-package emacs
  :xdg-state
  (auto-save-list-prefix "saves/"))

(use-package autorevert
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

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package dired
  :straight (:type built-in)
  :custom
  (dired-dwim-target t))

(use-package bookmark
  :straight (:type built-in)
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
  :straight (:type built-in)
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

(defun esprit/make-eat-frame ()
  "Create a new frame and create a vterm buffer."
  (interactive)
  (select-frame (make-frame))
  (eat-project))

(defvar-keymap esprit-frame-map
  :doc "Prefix map for frame operations."
  "m" #'make-frame
  "n" #'esprit/make-scratch-frame
  "v" #'esprit/make-eat-frame)

(keymap-global-set "M-n" esprit-frame-map)

;; Esprit Configuation
;;   - Set up my own little bundle of packages to tailor the Emacs experience

(use-package esprit-themes
  :straight nil
  :load-path "~/code/esprit-themes")

;;; Choose light or dark theme based on the time of day at my location

(use-package circadian
  :straight t
  :demand t
  :custom
  (calendar-latitude 42.4)
  (calendar-longitude -71.0)
  (circadian-themes '((:sunrise . esprit-cerulean-light)
                      (:sunset  . esprit-cerulean-dark)))
  :config
  (circadian-setup))

(use-package nerd-icons
  :straight t
  :demand t)

;; (require 'esprit-line)
;; (setq esprit-line-glyph-alist esprit-line-glyphs-unicode)
;; (esprit-line-mode)

(use-package esprit-line
  :straight nil
  :load-path "~/code/esprit-line"
  :custom
  (esprit-line-glyph-alist esprit-line-glyphs-unicode)
  :config (esprit-line-mode))

(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("esprit-modules"))

(use-package flymake
  :bind  (:map ctl-x-x-map
               ("m" . flymake-mode) ; C-x x m
               :map flymake-mode-map
               ("C-c ! s" . flymake-start)
               ("C-c ! d" . flymake-show-buffer-diagnostics) ; Emacs28
               ("C-c ! D" . flymake-show-project-diagnostics) ; Emacs28
               ("C-c ! n" . flymake-goto-next-error)
               ("C-c ! p" . flymake-goto-prev-error))
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-no-changes-timeout nil)
  (flymake-start-on-flymake-mode t)
  (flymake-start-on-save-buffer t)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-wrap-around nil)
  (flymake-mode-line-format
   '("" flymake-mode-line-exception flymake-mode-line-counters))
  (flymake-mode-line-counter-format
   '("" flymake-mode-line-error-counter
     flymake-mode-line-warning-counter
     flymake-mode-line-note-counter ""))
  (flymake-show-diagnostics-at-end-of-line nil))

(use-package jinx
  :straight t
  :hook (after-init . global-jinx-mode)
  :custom (jinx-languages "en_US")
  :bind
  (("C-;" . jinx-correct-nearest)
   ("C-x j a" . jinx-correct-all)
   ("C-x j n" . jinx-next)
   ("C-x j p" . jinx-previous)))

(defun require-and-ensure-eglot-ltex ()
  "Require the eglot-ltex package and run `eglot-ensure'."
  (require 'eglot-ltex)
  (eglot-ensure))

(use-package eglot-ltex
  :straight (:host github :repo "emacs-languagetool/eglot-ltex")
  :hook
  (esprit/prose-mode-list . require-and-ensure-eglot-ltex)
  :init
  (setq eglot-ltex-server-path "~/tools/ltex-ls-plus/bin/ltex-ls-plus"
	eglot-ltex-communication-channel 'stdio))

(use-package vertico
  :straight t
  :bind (:map vertico-map
              ("C-<backspace>" . vertico-directory-up))
  :hook
  (on-first-input . vertico-mode)
  (on-first-input . vertico-multiform-mode)
  :custom
  (vertico-resize t))

(defun wrapper/consult-ripgrep (&optional dir given-initial)
  "Pass the region to `consult-ripgrep' if available.

  DIR and GIVEN-INITIAL match the method signature of `consult-wrapper'."
  (interactive "P")
  (let ((initial
         (or given-initial
             (when (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))))))
    (consult-ripgrep dir initial)))

(use-package consult
  :straight t
  :hook (on-init-ui . consult-mode)
  :bind (("M-s d"     . consult-fd) ;; Requires having fd installed otherwise use consult-find
         ("M-s G"     . consult-git-grep)
         ("M-s r"     . wrapper/consult-ripgrep)
         ("M-s l"     . consult-line)
         ("M-s L"     . consult-line-multi)
         ("M-s k"     . consult-keep-lines)
         ("M-s u"     . consult-focus-lines)
         ("M-s <SPC>" . consult-buffer)
         ("M-y"       . consult-yank-pop)
         ("C-x M-k"   . consult-kmacro)
         ("M-g g"     . consult-goto-line)
         ("M-g i"     . consult-imenu)
         ("M-g o"     . consult-outline)
         ("C-x b"     . consult-bookmark)))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(defun corfu-x-eshell-hook ()
  "Set up Corfu behaviors in a shell friendly way."
  (setq-local corfu-auto nil)
  (corfu-mode))

(defun esprit/corfu-modes ()
  "Activate the desired corfu modes."
  (corfu-history-mode)
  (corfu-echo-mode)
  (global-corfu-mode))

(use-package corfu
  :straight t
  :init
  (setq corfu-cycle t
        corfu-auto t
        corfu-on-exact-match nil)
  :hook
  (on-first-buffer . esprit/corfu-modes)
  (eshell-mode . corfu-x-eshell-hook))

(defun esprit/cape-capf-setup-eglot ()
  "Configure cape completion at point functions for Eglot managed modes."
  (let ((result))
    (dolist (element `(,(cape-capf-buster #'eglot-completion-at-point)
		       cape-file
		       cape-dabbrev) result)
      (add-to-list 'completion-at-point-functions element))))

(defun esprit/cape-capf-setup-org ()
  "Configure cape completion at point functions for org mode."
  (let ((result))
    (dolist (element '(cape-dict cape-dabbrev) result)
      (add-to-list 'completion-at-point-functions element))))

(defun esprit/cape-capf-setup-git-commit ()
  "Configure cape completion at point functions for git-commit mode."
  (let ((result))
    (dolist (element '(cape-dict cape-dabbrev) result)
      (add-to-list 'completion-at-point-functions element))))

(use-package cape
  :straight t
  :config
  (setq completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless))))
  :hook
  ((eglot-managed-mode . esprit/cape-capf-setup-eglot)
   (org-mode . esprit/cape-capf-setup-org)
   (git-commit-mode . esprit/cape-capf-setup-git-commit)))

(use-package marginalia
  :straight t
  :hook
  (on-first-buffer . marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package embark
  :straight t
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
  :straight t
  :after (consult embark)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(defvar eglot-enabled-modes
  '(js-ts-mode
    typescript-ts-mode
    ruby-mode
    go-ts-mode
    astro-mode)
  "Opt in list of modes which Eglot should manage.")

(use-package eglot
  :straight (:type built-in)
  :demand t
  :hook (eglot-enabled-modes . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-x l r" . eglot-rename)
              ("M-k" . eglot-code-actions))
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eglot-events-buffer-config '(:size 2000000 :format lisp))
  :config
  (setq-default eglot-workspace-configuration
                '(:ltex-ls (:language "en-US"
                                      :disabledRules ["MORFOLOGIK_RULE_EN_US"]))))

(defun avy-action-embark (pt)
  "Invoke embark at PT."
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(defun avy-action-copy-whole-line (pt)
  "Copy entire line starting at PT."
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
  "Yank line starting at PT."
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(defun avy-action-kill-whole-line (pt)
  "Kill line starting at PT."
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun avy-action-teleport-whole-line (pt)
  "Teleport whole line starting at PT."
  (avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)

(use-package avy
  :straight t
  :bind ("M-j" . avy-goto-char-timer)
  :custom
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark
        (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line))

(use-package emacs
  :straight (misc :type built-in)
  :bind
  ("C-z" . #'zap-up-to-char))

(use-package elec-pair
  :straight (misc :type built-in)
  :hook (on-init-ui . electric-pair-mode))

(use-package accent
  :straight t
  :bind ("C-x '" . #'accent-menu))

(use-package emacs
  :straight (:type built-in)
  :hook ((esprit-prose-modes . visual-line-mode)
         (esprit-prose-modes . variable-pitch-mode)))

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(keymap-global-set "C-j" #'join-line)

(use-package multiple-cursors
  :hook (on-init-ui . multiple-cursors-mode)
  :straight t
  :bind
  ("C->" . #'mc/mark-next-like-this)
  ("C-<" . #'mc/mark-previous-like-this)
  ("C-c C->" . #'mc/mark-all-like-this)
  ("C-S-c C-S-c" . #'mc/edit-lines))

(use-package ace-window
  :straight t
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

(setq visible-bell nil
      ring-bell-function #'ignore)

(setq switch-to-buffer-obey-display-actions t)

(defun esprit-close-dwim ()
  "Quit a frame the same way no matter what kind of frame you are on."
  (interactive)
  (let ((frames (visible-frame-list)))
    (if (eq (car frames) (selected-frame))
        ;; For parent/master frame...
        (if (cdr frames)
            ;; Close a parent with children present.
            (progn
              (delete-frame (selected-frame))
              (when (and (eq (cadr frames) terminal-frame)
                         (null (cddr frames)))
                ;; No frames left on this daemon: shut it down.
                (save-buffers-kill-emacs)))
          ;; Close a parent with no children present.
          (save-buffers-kill-emacs))
      ;; Close a child frame.
      (delete-frame (selected-frame)))))

;; (use-package swiss-move
;;   :bind (("s-n" . swiss-move-line-down)
;; 	 ("s-p" . swiss-move-line-up)))

(global-set-key (kbd "C-x C-c") 'esprit-close-dwim)

(keymap-set global-map "C-x k" 'kill-current-buffer)
(keymap-set global-map "C-x C-k" 'kill-buffer)

(use-package fontaine
  :straight t
  :demand t
  :custom
  (fontaine-latest-state-file
   (locate-user-emacs-file "fontaine-latest-state.eld"))
  (fontaine-presets
   '((small
      :default-family "Geist Mono"
      :default-height 80
      :variable-pitch-family "Geist")
     (regular) ; like this it uses all the fallback values and is named `regular'
     (medium
      :default-height 140
      :bold-weight regular)
     (large
      :inherit medium
      :default-height 180)
     (t
      :default-family "Aporetic Sans Mono"
      :variable-pitch-family "Aporetic Serif"
      :fixed-pitch-height 1.0
      :fixed-pitch-serif-height 1.0
      :variable-pitch-height 1.0)))
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  ;; Persist the latest font preset when closing/starting Emacs and
  ;; while switching between themes.
  (fontaine-mode 1)
  :bind
  ("C-c f" . #'fontaine-set-preset))

(use-package magit
  :straight t
  :bind
  ("C-M-;" . magit-status))

(defvar-keymap esprit-vc-branch-map
  :doc "Esprit prefix map for version control branch actions."
  "b" #'magit-checkout
  "c" #'magit-branch-create)

(defvar-keymap esprit-vc-pull-map
  :doc "Esprit prefix map for version control pull/fetch actions."
  "p" #'magit-pull-from-pushremote
  "u" #'magit-pull-from-upstream
  "e" #'magit-pull-branch)

(defvar-keymap esprit-vc-file-map
  :doc "Esprit prefix map for version control file actions."
  "r" #'magit-file-rename)

(defvar-keymap esprit-vc-map
  :doc "Esprit prefix key maps version control operations ."
  "b" esprit-vc-branch-map
  "F" esprit-vc-pull-map
  "f" esprit-vc-file-map)

(keymap-set global-map "C-c g" esprit-vc-map)

(defvar esprit/ediff-original-windows nil)

(defun esprit/store-pre-ediff-winconfig ()
  "Store the window arrangement before opening Ediff."
  (setq esprit/ediff-original-windows (current-window-configuration)))

(defun esprit/restore-pre-ediff-winconfig ()
  "Reset original window arrangement."
  (set-window-configuration esprit/ediff-original-windows))

(use-package ediff
  :straight (ediff :type built-in)
  :hook ((ediff-before-setup . 'esprit/store-pre-ediff-winconfig)
         (ediff-quit . 'esprit/restore-pre-ediff-winconfig))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally))

(use-package eat
  :straight (:type git :host codeberg :repo "akib/emacs-eat"
	           :files ("*.el" ("term" "term/*.el") "*.texi"
		           "*.ti" ("terminfo/e" "terminfo/e/*")
		           ("terminfo/65" "terminfo/65/*")
		           ("integration" "integration/*")
		           (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package treesit-env
  :straight (:host github :repo "cottontailia/treesit-env")
  :custom
  (treesit-env-default-revision-auto t)
  (treesit-env-abi-max 14)
  :config
  (treesit-env typescript javascript go ruby lua css)
  ;; Use the provided minimal sample recipes (Optional)
  (require 'treesit-env-recipe-placeholder)
  (treesit-env-source treesit-env-recipe-placeholder))

(use-package css-mode
  :custom
  (css-indent-offset 2))

(use-package js
  :custom
  (js-indent-level 4))

(use-package go-ts-mode
  :custom
  (go-ts-mode-indent-offset 4)
  :mode (rx ".go"))

(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

(use-package typescript-ts-mode
  :mode (rx ".ts")
  :custom
  (typescript-indent-level 2)
  :config
  (unbind-key "M-." typescript-ts-base-mode-map))

(use-package web-mode
  :straight t)

(use-package lua-ts-mode
  :mode (rx ".lua"))

(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '((".*\\.astro\\'" . astro-mode))
              auto-mode-alist))

(use-package tempel
  ;; Require trigger prefix before template name when completing.
  :custom
  (tempel-trigger-prefix "<")
  :bind (("M-=" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)))

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection
  :straight t)

(defvar-keymap esprit-toggles-map
  :name "esprit-toggles"
  :doc "Esprit prefix key maps | minor mode toggling."
  "v" #'global-visual-line-mode
  "f" #'toggle-frame-fullscreen
  "w" #'whitespace-mode
  "c" #'command-log-mode)

(use-package whitespace
  :custom
  (whitespace-style
   '(face tabs spaces trailing lines-tail space-before-tab newline indentation
          empty space-after-tab space-mark tab-mark newline-mark missing-newline-at-eof)))

(keymap-global-set "C-x m" esprit-toggles-map)

(use-package outline
  :straight (:type built-in)
  :diminish "¶")

(use-package which-key
  :straight (:type built-in)
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 1)
  :hook
  (on-first-input . which-key-mode))

(use-package emacs
  :custom (create-lockfiles nil)
  :bind (("C-x C-m" . execute-extended-command)
         :map isearch-mode-map ("C-o" . isearch-occur)))

;; Clean and straightforward undo/redo
(use-package undo-fu
  :straight t
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
  :straight t
  :hook
  (after-init . undo-fu-session-global-mode)
  :xdg-state
  (undo-fu-session-directory "undo-fu-session")
  :custom
  (undo-fu-session-compression 'nil)
  (undo-fu-session-incompatible-files
   '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vterm
  :straight t
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
  :straight t
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

;; Special mode is "A special major mode is intended to view specially formatted data
;; rather than files.". Most practically this means help(ful) buffers and the like.
;; I prefer this kind of informational display to be in a side window.
(add-to-list 'display-buffer-alist
             '((derived-mode . special-mode)
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 80)
               (window-parameters
                (no-delete-other-windows . t))))

(use-package nxml-mode
  :straight (:type built-in)
  :mode (rx (| ".xml" ".svg")))

(use-package markdown-mode
  :straight t
  :mode ((rx ".md") . gfm-mode)
  :commands (markdown-mode gfm-mode)
  :bind ("C-c C-c C-p" . 'esprit/markdown-preview)
  :config
  (setq markdown-command "pandoc -t html5"))

(use-package simple-httpd
  :straight t
  :custom
  (httpd-port 7070)
  (httpd-host (system-name)))

(use-package impatient-mode
  :straight t
  :commands impatient-mode)

(defun esprit/markdown-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

(defun esprit/markdown-preview ()
  "Open a live, styled markdown preview."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'esprit/markdown-filter)
  (imp-visit-buffer))

(use-package md-mermaid
  :straight (:host github :repo "ahmetus/md-mermaid")
  :commands (md-mermaid-render-current
             md-mermaid-preview-last-svg
             md-mermaid-transient))

(use-package yaml-ts-mode
  :straight (:type built-in)
  :mode (rx (| ".yml" ".yaml")))

(use-package command-log-mode
  :straight (:host github :repo "ludamillion/command-log-mode"))

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
(setq esprit/notes-directory (expand-file-name "notes" esprit/org-dir))
(setq esprit/org-id-locations-file (expand-file-name ".org-id-locations" esprit/org-dir))

(use-package org
  :straight (:type built-in)
  :init
  (setq org-export-backends '(ascii md html icalendar latex))
  :custom
  (org-default-notes-file (expand-file-name "inbox.org" esprit/org-dir)) ;; Should maybe be inbox
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-capture-bookmark nil)
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

  ;; ;; ;; One of my big uses for Org is my literate config so having elisp as a template is a must
  (add-to-list 'org-structure-template-alist '("sl" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("s#" . "src csharp"))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))

  :bind
  ("M-<return>" . org-insert-heading-after-current)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link))

(use-package denote
  :straight t
  :init
  (denote-rename-buffer-mode 1)
  :custom
  (denote-directory esprit/notes-directory)
  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic)))))

(use-package consult-notes
  :straight t
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
  :straight t
  :custom
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (combobulate-key-prefix "C-c o")
  :hook
  ((prog-mode . combobulate-mode)))

(use-package esprit-movement
  :bind ("C-a" . #'esprit/beginning-of-line))

(setq esprit-emacs--success t)

(provide 'init)

