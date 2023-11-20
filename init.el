;; On the road to a literate configuration. For the moment my dotemacs.org file will
;; output the lit-init.el file so that I can move things from this init file to the
;; literate version piecemeal.
(load (expand-file-name "lit-init.el" user-emacs-directory))

; Integrate with use-package

; I tried to avoid use-package here for a more “minimal” setup. That did not work. Since straight.el plays nice with use-package, let’s let it do that.

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
;; (setq auto-save-file-name-transforms
      ;; `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))


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
  :init (exec-path-from-shell-initialize))

; macOS doesn’t use GNU Coreutils and of course its ls isn’t what dired expects. Adjust for that.

(when (string-equal system-type "darwin")
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq dired-use-ls-dired nil))

; visual-fill-column for a nice soft wrap



(use-package visual-fill-column
  :commands visual-fill-column-mode

  :custom
  (fill-column-enable-sensible-window-split t)
  :config
  (visual-line-mode t)
  :bind
  (("C-x p" . 'visual-fill-column-mode)))

; More pleasent (slightly evil) undo experience

(use-package undo-fu
  :bind
  ("C-/" . undo-fu-only-undo)
  ("C-M-/" . undo-fu-only-redo))

(use-package undo-fu-session
  :init
  (undo-fu-session-global-mode)
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

; Aesthetics
; Fonts

; Configure nano
; Install nano and its dependencies

; Installing via straight.el.

(straight-use-package
 '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

; nano-theme maps those custom faces to pretty much everything everywhere. Pretty nice.

(use-package liminal-theme
  :straight (:type nil :local-repo "~/code/liminal-theme")
  :config
  (liminal-mode))

; Enable nano session handling

(require 'nano-session)

; Enable the nano modeline

; One of my favorite bits really.

;; (use-package liminal-modeline
;;   :straight (:type nil :local-repo "~/code/liminal-modeline")
;;   :hook
;;   (prog-mode            . liminal-modeline-prog-mode)
;;   (text-mode            . liminal-modeline-text-mode)
;;   (org-mode             . liminal-modeline-org-mode)
;;   (term-mode            . liminal-modeline-term-mode)
;;   (messages-buffer-mode . liminal-modeline-message-mode)
;;   (org-capture-mode     . liminal-modeline-org-capture-mode)
;;   (org-agenda-mode      . liminal-modeline-org-agenda-mode)
;;   )

(use-package nano-modeline
  :straight (:type git :host github :repo "rougier/nano-modeline")
  :config
  (setq-default mode-line-format nil)
  :hook
  (prog-mode            . nano-modeline-prog-mode)
  (text-mode            . nano-modeline-text-mode)
  (org-mode             . nano-modeline-org-mode)
  (term-mode            . nano-modeline-term-mode)
  (messages-buffer-mode . nano-modeline-message-mode)
  (org-capture-mode     . nano-modeline-org-capture-mode)
  (org-agenda-mode      . nano-modeline-org-agenda-mode)
  )

(use-package surround
  :straight (:type git :host github :repo "mkleehammer/surround")
  :bind-keymap ("M-'" . surround-keymap))

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
  :init
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
        ("s-p" . projectile-command-map)
        ("C-c p" . projectile-command-map)))

(use-package orderless
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package eglot
  :bind
  ("M-k" . eglot-code-actions)
  :hook
  (eglot-managed-mode . eglot-inlay-hints-mode)
  (typescript-ts-base-mode . eglot-ensure)
  (js-base-mode . eglot-ensure)
  (ruby-ts-base-mode . eglot-ensure)
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package flycheck
  :init (global-flycheck-mode))
  
(use-package flycheck-eglot
  :after (flycheck eglot)
  :init
  (global-flycheck-eglot-mode 1))

; Which Key?
; which-key adds a completion panel for commands. That helps me learn the many Emacs key maps.

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(defun ldi/save-word ()
  "Mark a Flyspell reported error as acceptable."
  
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)    
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(global-set-key (kbd "C-x $") 'ldi/save-word)
(global-set-key (kbd "C-'") 'flyspell-auto-correct-previous-word)

(use-package flyspell
  :bind
  (("C-x $" . ldi/save-word)
   ("C-'" . flyspell-auto-correct-previous-word))
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))
  
(use-package coffee-mode
  :init
  (setq coffee-tab-width 2))

(use-package web-mode
  :init
  (setq font-lock-maximum-decoration '((web-mode . t) (t . nil)))
  :config
  (add-to-list 'auto-mode-alist '("\\.haml$" . web-mode)))

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package jtsx
  :straight (:type git :host github :repo "llemaitre19/jtsx")
  :mode (("\\.jsx?\\'" . jsx-mode)
         ("\\.tsx?\\'" . tsx-mode))
  :commands jtsx-install-treesit-language
  :hook ((jsx-mode . hs-minor-mode)
         (tsx-mode . hs-minor-mode))
  :custom
  (js-indent-level 2)
  (typescript-ts-mode-indent-offset 2)
  (jtsx-switch-indent-offset 0)
  (jtsx-indent-statement-block-regarding-standalone-parent nil)
  (jtsx-jsx-element-move-allow-step-out t)
  (jtsx-enable-jsx-electric-closing-element t)
  :config
  (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
    (define-key mode-map (kbd "C-c j o") 'jtsx-jump-jsx-opening-tag)
    (define-key mode-map (kbd "C-c j c") 'jtsx-jump-jsx-closing-tag)
    (define-key mode-map (kbd "C-c j r") 'jtsx-rename-jsx-element)
    (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
    (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
    (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
    (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
    (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
    (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
    (define-key mode-map (kbd "C-c j w") 'jtsx-wrap-in-jsx-element))
  
  (defun jtsx-bind-keys-to-jsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jsx-mode-map))

  (defun jtsx-bind-keys-to-tsx-mode-map ()
    (jtsx-bind-keys-to-mode-map tsx-mode-map))

  (add-hook 'jsx-mode-hook 'jtsx-bind-keys-to-jsx-mode-map)
  (add-hook 'tsx-mode-hook 'jtsx-bind-keys-to-tsx-mode-map))

(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(use-package magit
  :commands magit-status
  :bind
  (("C-M-;" . magit-status)))

;; Code Completion
(use-package corfu
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
  :init
  (vertico-mode)
  :bind
  (:map vertico-map
        ("C-<backspace>" . vertico-directory-up))
  :custom
  (setq vertico-scroll-margin 0)
  (setq vertico-resize t)
  (setq vertico-cycle t))

(use-package consult
  :bind
  (("M-x"     . 'consult-buffer)
   ("M-y"     . 'consult-yank-pop)
   ("C-x b"   . 'consult-bookmark))
  )

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

(use-package vterm
  :config
  (setq vterm-max-scrollback 10000)
  :bind
  ("C-x !" . projectile-run-vterm))

; ;; Workaround for issue with typescript lsp server found here https://github.com/typescript-language-server/typescript-language-server/issues/559#issuecomment-1259470791
;; Shouldn't be necessary after upgrading to Emacs v29
;; (advice-add 'json-parse-string :around
;;             (lambda (orig string &rest rest)
;;               (apply orig (s-replace "\\u0000" "" string)
;;                      rest)))

;; ;; minor changes: saves excursion and uses search-forward instead of re-search-forward
;; (advice-add 'json-parse-buffer :around
;;             (lambda (oldfn &rest args)
;; 	      (save-excursion 
;;                 (while (search-forward "\\u0000" nil t)
;;                   (replace-match "" nil t)))
;; 		(apply oldfn args)))

(use-package objed
  :ensure t
  :init
  (objed-mode)
  :config
  (setq objed-modeline-hint nil)
  :bind
  ("M-SPC" . objed-activate-object))

(use-package rainbow-mode
  :ensure t
  :init (rainbow-mode t))

(defun toggle-window-split ()
  "Toggle dual window split between vertical to horizontal orientation."
  
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

(global-set-key (kbd "M-o") 'other-window)

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

(setf mode-line-format nil)

(use-package multiple-cursors
  :ensure   t
  :bind (("C-M-SPC" . set-rectangular-region-anchor)
         ("C-M->" . mc/mark-next-like-this)
         ("C-M-<" . mc/mark-previous-like-this)
         ("C-c C->" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/edit-lines)
         ))

(defun reload-init-file ()
  "Reload the file referenced by `user-init-file`."
  
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "<f5>") 'reload-init-file)

(use-package circadian
  :config
  (setq calendar-latitude 42.4)
  (setq calendar-longitude -71.0)
  (setq circadian-themes '((:sunrise . liminal-light)
                           (:sunset  . liminal-dark)))
  (circadian-setup))

;; Cribbed from https://panadestein.github.io/emacsd/
(use-package ediff
  :straight nil
  :preface
  (defvar my-ediff-original-windows nil)
  (defun my-store-pre-ediff-winconfig ()
    "Stores the window arrangement before opening ediff."
    (setq my-ediff-original-windows (current-window-configuration)))
  (defun my-restore-pre-ediff-winconfig ()
    "Resets original window arrangement"
    (set-window-configuration my-ediff-original-windows))
  :hook
  ((ediff-before-setup . my-store-pre-ediff-winconfig)
   (ediff-quit . my-restore-pre-ediff-winconfig))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

(defun switch-theme (theme)
  "Load THEME after unloading previously loaded themes N.B. this will not remove any customization done outside of themes."
  
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme theme t))
