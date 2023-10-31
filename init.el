; Foundations
; Give Emacs some breathing room
;
; max-specpdl-size sets the upper limit for how many variable bindings and unwind-protect Emacs allows. max-lisp-eval-depth says how deep we can get into a recursive function call.

; I got the RAM so let’s go past the respective defaults of 1600 and 800.

(setq max-specpdl-size 3200)
(setq max-lisp-eval-depth 3200)

; And of course I’m sure to screw something up so let’s make sure the debugger is enabled for when I do.

(setq debug-on-error t)
;
; Enable local lisp

(let ((default-directory  (expand-file-name "lisp" user-emacs-directory)))
  (setq load-path
        (append
         (let ((load-path  (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

; Simplify reloading my config

; I putter with this config marginally less than I did initially - progress! - but enough that restarting Emacs for every config tweak gets tedious.

; One of the ideas I grabbed from Vianney Lebouteiller’s Emacs config.
;
(defun reload-init-file ()
  "Reloads Emacs' configuration file"
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "<f5>") 'reload-init-file)

; Use straight.el to install packages

; straight.el is my new friend.
; early-init.el

; But if I’m using straight.el I better disable package.el during the early init stage.

; Bootstrap straight.el
; Boilerplate from the straight.el documentation.

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
   (load bootstrap-file nil 'nomessage))

; Integrate with use-package

; I tried to avoid use-package here for a more “minimal” setup. That did not work. Since straight.el plays nice with use-package, let’s let it do that.

(straight-use-package 'use-package)

(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))


; General Usability
; General guidelines for text handling

; Where to put the fill column marker for line wraps, how many pixels to put between lines, stuff like that.

(setq fill-column 100)
(setq-default line-spacing 1)

; invoke M-x without Alt

; I read Steve Yegge’s effective-emacs a long time ago — back when it was an internal Amazon blog. Applied his suggestion to invoke X-m with C-x C-m and that’s been part of my Emacs muscle memory ever since.

(global-set-key (kbd "C-x C-m") 'execute-extended-command)

; Make a few adjustments for running on macOS

; Make sure the macOS Emacs GUI app picks up environment variables.

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

; macOS doesn’t use GNU Coreutils and of course its ls isn’t what dired expects. Adjust for that.

(if (string-equal system-type "darwin")
    (setq mac-option-modifier 'super)
    (setq dired-use-ls-dired nil))

; visual-fill-column for a nice soft wrap

(visual-line-mode t)

(use-package visual-fill-column
  :commands visual-fill-column-mode

  :custom
  (fill-column-enable-sensible-window-split t)

  :bind
  (("C-x p" . 'visual-fill-column-mode)))

; More pleasent (slightly evil) undo experience

(use-package undo-fu
  :config
  (global-unset-key (kbd "C-z"))
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

; Aesthetics
; Fonts

; The Roboto Mono font that NANO wants is not part of any *roboto* package I found in Pop! OS repositories. Ended up going to Font Library for a direct download.

(setq ldi/face-height-default
      (if (eq system-type 'darwin)
          160
        140))

(setq nano-font-size (/ ldi/face-height-default 10))

; Configure nano
; Install nano and its dependencies

; Installing via straight.el.

(straight-use-package
 '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

;; the Nano layout

(require 'nano-layout)

; Set up font faces
; I feel comfortable loading nano-faces for font rules now that I’ve defined my colors. Will need to fuss a bit more in a second though.

(require 'nano-faces)
(nano-faces)

; Let nano theme everything

; nano-theme maps those custom faces to pretty much everything everywhere. Pretty nice.

(use-package nano-theme
  :straight (:type git :host github :repo "rougier/nano-theme")
  :config
  (setq nano-fonts-use t) ; Use theme font stack
  (nano-light)            ; Use theme light version
  (nano-mode)             ; Recommended settings
  )

; Once I have my base established, I should be able to load the nano theme.
; Load nano defaults

(require 'nano-defaults)

; Enable nano session handling

(require 'nano-session)

; Enable the nano modeline

; One of my favorite bits really.

(use-package nano-modeline
  :ensure t
  :config
  (add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
  (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
  (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
  (add-hook 'term-mode-hook            #'nano-modeline-term-mode)
  (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
  (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
  (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode))

; Enable nano key bindings

; C-x k
;     ; kill current buffer without asking
; ; M-n
;     ; open a new frame
; ; M-`
;     ; switch to other frame
; ; C-x C-c
;     ; delete the current frame; exit if no frames remain
; ; C-c r
;     ; interactive select from recent files
; ; <M-return>
;     ; toggle maximization of current frame

; ; not sure if I like this one; it confuses org muscle memory, and if I want “maximized” I usually toggle tiling in the window manager

(require 'nano-bindings)

(use-package amx)

(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

(require 'nano-splash)

; Life management with Org

; Okay here we go. Building up my org-roam experience while keeping Deft handy for the longer, more intentional notes.
; File locations

; I work this out piecemeal, as some of the files and folders build on what’s been defined before.
;
; First: what’s the top level of everything? That depends on whether I’m in a UNIX-like system or playing with the native Windows version of Emacs.

(setq ldi/local-root "~/")

; Trying an experiment where first we look for a local ~org/ folder and use that if found, otherwise going with my actual default of ~/Dropbox/org. Trying to shift over to git-synchronized Org files instead of Dropbox-synchronized, but that change will take a bit to percolate through all my systems.

(setq ldi/default-org-directory (expand-file-name "org" ldi/local-root))
(setq ldi/sync-org-directory (expand-file-name "Dropbox/org" ldi/local-root))

(setq ldi/org-dir
      (if (file-directory-p ldi/default-org-directory)
          ldi/default-org-directory
        ldi/sync-org-directory))

; That’s enough to define most of the files I need.

(setq
 ldi/current-journal (expand-file-name "journal.org" ldi/org-dir)
 ldi/org-id-locations-file (expand-file-name
                            ".org-id-locations" ldi/org-dir))

; Custom keywords
;
; A process vagiuely similar to GTD but my brain insists on its own task classifications.
;
; LATER
;     I need to do it, but it can wait (or it’s waiting on something)
; NOW
;     I got everything I need to do this
; MAYBE
;     An idea, suggestion, or action that I may or may not want to to
; PROJECT
;     A multi-part task with notable dependencies
; DONE
;     I did it!
; NOPE
;     Never mind

(setq ldi/todo-keywords
      `((sequence
         "LATER(l!)" "NOW(n!)" "MAYBE(m!)" "PROJECT(p!)"
         "|"
         "DONE(d!)" "NOPE(-!)")))

(use-package org
  :straight (:type built-in)
  :ensure org-plus-contrib
  :custom
  (org-log-done 'time)
  (org-log-reschedule 'time)
  (org-log-into-drawer t)
  (org-startup-indented t)
  (org-startup-truncated nil)
  (org-todo-keywords ldi/todo-keywords)
  (org-id-track-globally t)
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (org-id-locations-file ldi/org-id-locations-file)
  (org-id-locations-file-relative t)

  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)

  :config
  (setq-local org-fontify-whole-heading-line t))

(require 'nano-writer)

; Additional Org tools
; Deft
;
; The perfect solution for knowledge management varies by context. But the core thing really needed: someplace to drop my notes where I can find them when I need them.
;
; Deft provides exactly that. And since Org mode is the main reason I load Emacs, my ~/org folder is where Deft will look for notes.

(use-package deft
  :custom (deft-extensions '("org")) (deft-directory ldi/org-dir)
  (deft-recursive-ignore-dir-regexp "\\(?:\\.\\|\\.\\.\\|roam\\|brain\\)")
  (deft-ignore-file-regexp "\\(?:~\\|py\\)$")
  (deft-recursive t))

; Helpful hint when enabling deft-recursive: =../ is one of the entries in your directory listing, and Deft will do its darndest to follow it if you forget to include it in deft-recursive-ignore-dir-regexp (set to "\\(?:\\.\\|\\.\\.\\)" by default).
;
; This can lead to all sorts of recursive headaches, so don’t forget!
;
; Of course I’ll end up tweaking it. But to get me started?

; Project management with Projectile and friends

; Projectile plus a .dir-locals.el file seems like the right way to handle development projects without bumping into everything else.

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map)
        ("C-c p" . projectile-command-map)))

; lsp-mode and related for an IDE experience

; lsp-mode adds support for Microsoft’s Language Server Protocol. Hypothetically that means easier setup of commonly desired features like linting and autocompletion.

; lsp-mode uses YASnippet for abbreviation and expansion.

(use-package yasnippet)

; nano-modeline and lsp-mode’s breadcrumb trail wrestle with each other for space on that top line. Maybe someday I can figure out how to stack them. Until then, I like the modeline and its placement more than I like the breadcrumb.

(use-package orderless
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package lsp-mode
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  (defun luda/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook ((ruby-ts-mode
          css-ts-mode
          js-ts-mode
          tsx-ts-mode
          web-mode
          ) . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-completion-mode . luda/lsp-mode-setup-completion)
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-eldoc-hook nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.5)
  :commands lsp)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\vendor\\'"))

(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-border (face-foreground 'default))
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-delay 0.05))

; Which Key?
; which-key adds a completion panel for commands. That helps me learn the many Emacs key maps.

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(defun ldi/save-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)    
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(global-set-key (kbd "C-x $") 'ldi/save-word)
(global-set-key (kbd "C-'") 'flyspell-auto-correct-previous-word)

(use-package js2-mode
  :ensure t
  :init
  (setq js-indent-level 2)
  (setq-default js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "jQuery" "$"))

  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

(use-package js2-refactor
   :ensure t
   :init   (add-hook 'js2-mode-hook 'js2-refactor-mode)
   :config (js2r-add-keybindings-with-prefix "C-c ."))

(use-package coffee-mode
   :ensure t
   :init
   (setq-default coffee-tab-width 2))

(use-package web-mode
  :ensure t
  :init
  (setq font-lock-maximum-decoration '((web-mode . t) (t . nil)))
  :config
  (add-to-list 'auto-mode-alist '("\\.haml$" . web-mode)))

(use-package haml-mode
  :ensure t)

(use-package lua-mode
  :ensure t)

(use-package css-mode
  :ensure t
  :config
  (setq css-indent-offset 2))

(use-package yaml-mode
  :ensure t)

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package magit
  :commands magit-status
  :bind
  (("C-x g" . 'magit-status)))

;; Code Completion
(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.25)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; Do not preview current candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . nil))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                              corfu-quit-no-match t
                              corfu-auto nil)
              (corfu-mode))))

(use-package cape
  :defer 10
  :bind ("C-c f" . cape-file)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (defalias 'dabbrev-after-2 (cape-capf-prefix-length #'cape-dabbrev 2))
  (add-to-list 'completion-at-point-functions 'dabbrev-after-2 t)
  (cl-pushnew #'cape-file completion-at-point-functions)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-dict)
    
  :config
  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;; Enable vertico
(use-package vertico
  :custom
  (setq vertico-scroll-margin 0)
  (setq vertico-resize t)
  (setq vertico-cycle t)
  :config
  (vertico-mode))

(use-package consult
  :ensure consult-lsp
  :bind
  (("M-x" . 'consult-recent-file)
   ("C-x b" . 'consult-buffer)
   ("C-x C-b" . 'consult-bookmark)
   ("M-y" . 'consult-yank-pop)))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
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
  (setq enable-recursive-minibuffers t))

(use-package rg
  :ensure t)

(use-package avy
  :bind
  ("C-;" . avy-goto-char-2))

(use-package vterm
  :ensure t
  :bind
  ("C-x !" . projectile-run-vterm))

; ;; Workaround for issue with typescript lsp server found here https://github.com/typescript-language-server/typescript-language-server/issues/559#issuecomment-1259470791
; ;; Shouldn't be necessary after upgrading to Emacs v29
; (advice-add 'json-parse-string :around
;             (lambda (orig string &rest rest)
;               (apply orig (s-replace "\\u0000" "" string)
;                      rest)))
;
; ;; minor changes: saves excursion and uses search-forward instead of re-search-forward
; (advice-add 'json-parse-buffer :around
;             (lambda (oldfn &rest args)
; 	      (save-excursion 
;                 (while (search-forward "\\u0000" nil t)
;                   (replace-match "" nil t)))
; 		(apply oldfn args)))

(use-package objed
  :ensure t
  :bind
  ("M-o" . objed-activate-object)
  :config
  (add-hook 'prog-mode-hook #'objed-local-mode))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

;; From: https://github.com/rougier/nano-emacs/issues/128
(defun luda/kill-emacs ()
  "Delete frame or kill Emacs if there is only one frame."

  (interactive)
  (condition-case nil
      (delete-frame)
    (error (save-buffers-kill-terminal))))

(global-set-key (kbd "C-x C-c") 'luda/kill-emacs)

(setq visible-bell nil
      ring-bell-function #'ignore)

;; Hack in f-shortdoc.el that removes the error:
(when (version<= "28.1" emacs-version)
  (when t ;;(< emacs-major-version 29)
    (require 'shortdoc)))

(setf mode-line-format nil)
