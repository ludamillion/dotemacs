; Foundations
; Give Emacs some breathing room
; 
; max-specpdl-size sets the upper limit for how many variable bindings and unwind-protect Emacs allows. max-lisp-eval-depth says how deep we can get into a recursive function call.

; I got the RAM so let’s go past the respective defaults of 1600 and 800.

(setq max-specpdl-size 3200)
(setq max-lisp-eval-depth 3200)

; And of course I’m sure to screw something up so let’s make sure the debugger is enabled for when I do.

(setq debug-on-error t)

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

(defun reload-init-file ()
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
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
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

; General Usability
; General guidelines for text handling

; Where to put the fill column marker for line wraps, how many pixels to put between lines, stuff like that.

(setq fill-column 80)
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

; With that note out of the way - I still lean towards Fantasque Sans Mono.

(setq ldi/face-height-default
      (if (eq system-type 'darwin)
          160
        140))

;(set-face-attribute 'default t
;                    :family "Iosevka"
;                    :width 'expanded
;                    :height ldi/face-height-default)

;(setq nano-font-family-monospaced "Iosevka")
(setq nano-font-size (/ ldi/face-height-default 10))

; Configure nano
; Install nano and its dependencies

; Installing via straight.el.

(straight-use-package
 '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

; Load the Nano layout

(require 'nano-layout)

; Define my colors

; Because I’m the kind of person I am: setting the nano theme colors to match my own tacky tastes. Maybe not tacky but certainly not as refined as the author of nano.

; This particular set of colors comes from the Spaceduck theme.

(defun nano-theme-set-liminal-light ()
  (setq frame-background-mode 'light)
  (setq nano-color-foreground "#353c3f")
  (setq nano-color-background "#fafafa")
  (setq nano-color-highlight  "#f9f9f9")
  (setq nano-color-critical   "#b4637a")
  (setq nano-color-salient    "#286983")
  (setq nano-color-strong     "#353c3f")
  (setq nano-color-popout     "#d7827e")
  (setq nano-color-subtle     "#b4bcbf")
  (setq nano-color-faded      "#6f7e84"))

(nano-theme-set-liminal-light)

; Set up font faces
; I feel comfortable loading nano-faces for font rules now that I’ve defined my colors. Will need to fuss a bit more in a second though.

(require 'nano-faces)
(nano-faces)

; Let nano theme everything

; nano-theme maps those custom faces to pretty much everything everywhere. Pretty nice.

(require 'nano-theme)
(nano-theme)

; Once I have my base established, I should be able to load the nano theme.
; Load nano defaults

(require 'nano-defaults)

; Enable nano session handling

(require 'nano-session)

; Enable the nano modeline

; One of my favorite bits really.

(require 'nano-modeline)

; Enable nano key bindings

; C-x k
    ; kill current buffer without asking
; M-n
    ; open a new frame
; M-`
    ; switch to other frame
; C-x C-c
    ; delete the current frame; exit if no frames remain
; C-c r
    ; interactive select from recent files
; <M-return>
    ; toggle maximization of current frame

; not sure if I like this one; it confuses org muscle memory, and if I want “maximized” I usually toggle tiling in the window manager

(require 'nano-bindings)

; nano Counsel integration

; nano-counsel.el is small. I’ll just map its logic directly to some use-package magic.

(use-package counsel
  :bind
  (("M-x" . 'counsel-recentf)
   ("C-x b" . 'counsel-bookmark)
   ("C-c r" . 'counsel-recentf)
   ("C-x C-b" . 'counsel-switch-buffer)
   ("C-c c" . 'counsel-org-capture)))

(use-package smex)
(use-package ivy
  :custom
  (ivy-height 4)
  (ivy-count-format "")
  (ivy-initial-inputs-alist: '((counsel-minor . "^+")
                               (counsel-package . "^+")
                               (counsel-org-capture . "^")
                               (counsel-M-x . "^")
                               (counsel-refile . "")
                               (org-agenda-refile . "")
                               (org-capture-refile . "")
                               (Man-completion-table . "^")
                               (woman . "^")))
  (ivy-use-virtual-buffers t)

  :init
  (setq enable-recursive-minibuffers t)

  :config
  (ivy-mode 1))

; I need to give myself a little context here.
; Ivy, Counsel, and Swiper
;
;     flexible, simple tools for minibuffer completion in Emacs
;
; These are technically separate packages developed together in the swiper repo.
;
; Ivy
;     an alternative completion framework for Emacs
; Counsel
;     Ivy-enhanced alternatives to common Emacs commands
; Swiper
;     Ivy-enhanced alternative to Isearch
;
; Loading nano-counsel failed with complaints about missing smex. Smex provides enhancements to M-x behavior, such as an interface to recent and commonly used commands. Since I want my foundation to be a clean Nano experience, I install smex as well.

; nano splash

(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

(require 'nano-splash)

(require 'nano-help)

; Life management with Org

; Okay here we go. Building up my org-roam experience while keeping Deft handy for the longer, more intentional notes.
; File locations
;
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
                            ".org-id-locations" ldi/org-dir)
 ldi/org-roam-directory (expand-file-name "roam" ldi/org-dir))

; Oh, one more thing. I want to include org-roam files in my Org agenda. I found helpful instructions, but I’m not adding that code to my config until I understand it. Maybe if I follow the link to the beginning of the post series and start there.

; What a novel idea.

; But today? With my small collection of org-roam notes, I can get away with directly including them in my agenda searches.

(setq ldi/org-agenda-files (list ldi/org-dir
                                 ldi/org-roam-directory
                                 (expand-file-name
                                  "daily" ldi/org-roam-directory)))

; ; Tumblelogging with ox-hugo

; ; I started an experiment with using Org to drive a tumblelog at Random Geekery Life. Tumblelog is an older term for a blog that mainly consists of dumping whatever thoughts, links, or just whatever. Sort of like Twitter or Tumblr, but on my own site and less constrained than a tweet-length microblog. Someday I may put more words elsewhere and replace this all this explanatory text with a link.

; ; For now I build the visible tumblelog as a static site with Hugo. It’s quick and it’s familiar.

; ; Org enters the scene with a single file within my Hugo site.

; ; (setq
; ;  ldi/tumble-log (expand-file-name
; ;                  "~/Sites/rgb-life/content-org/posts.org"))

; ; That file contains all the content for the tumblelog, but the important part here is a function to dynamically generate a filename for ox-hugo based on the current time.
; ;
; ; (defun ldi/build-tumble-template ()
; ;   (format-spec
; ;    "* NOW %%U %%?\n:properties:\n:export_file_name: %s.md\n:end:\n"
; ;    (list (cons ?s (format-time-string "%s")))))
;
; ; Hugo configuration determines permalinks from post dates, which ox-hugo derives from task completion timestamps. If I want to keep all the times for an entry in sync I manually edit, but it’s not a big deal yet. C-t d muscle memory so far is quicker than figuring out how to automate that process.
;
; ; The actual filename gets ignored, but the epoch timestamp keeps each output file unique at my normal human rate of adding entries.
;
; ; This also squeaks me past the fact that I came up with this version of the template after I already had a few entries.
;
; ; Then when I’m building org-capture-templates I create a datetree entry for tumblelogging.
;
; ; ("t" "Tumblelog" entry
; ;  (file+olp+datetree ldi/tumble-log "Posts")
; ;  (function ldi/build-tumble-template))

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

; CUSTOM_ID generation via writequit

; Grabbing directly from this post.

; More to keep my org-roam-ui graph in order than for publishing, but hopefully it’ll come in handy there too.

(defun eos/org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.
     If POM is nil, refer to the entry at point. If the entry does
     not have an CUSTOM_ID, the function returns nil. However, when
     CREATE is non nil, create a CUSTOM_ID if none is present
     already. PREFIX will be passed through to `org-id-new'. In any
     case, the CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new (concat prefix "h")))
        (org-entry-put pom "CUSTOM_ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))

(defun eos/org-add-ids-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the
     current file which do not already have one."
  (interactive)
  (org-map-entries (lambda () (eos/org-custom-id-get (point) 'create))))

; Putting it all together

(use-package org
  :ensure org-plus-contrib
  :custom
  (org-agenda-files ldi/org-agenda-files)
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
  (setq org-id-link-to-org-use-id t)
  (setq org-capture-templates
        '(("j" "Jot" entry
           (file+olp+datetree ldi/current-journal)
           "* %U %? \n%i\n %a")
          ("t" "Tumblelog" entry
           (file+olp+datetree ldi/tumble-log "Posts")
           (function ldi/build-tumble-template))
          ))
  (setq-local org-fontify-whole-heading-line t))

(require 'nano-writer)

; Additional Org tools
; Deft
;
; The perfect solution for knowledge management varies by context. But the core thing really needed: someplace to drop my notes where I can find them when I need them.
;
; Deft provides exactly that. And since Org mode is the main reason I load Emacs, my ~/org folder is where Deft will look for notes.
;
; I don’t want org-roam notes obscuring the more persistent notes in my Org folder. Better ignore them. Also the org-brain stuff until I have a good handle on that.

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
;
; “Ask deft about my notes” is more than sufficient.
; org-roam
;

; Project management with Projectile and friends
;
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
;
; lsp-mode adds support for Microsoft’s Language Server Protocol. Hypothetically that means easier setup of commonly desired features like linting and autocompletion.
;
; lsp-mode uses YASnippet for abbreviation and expansion.

(use-package yasnippet)

; nano-modeline and lsp-mode’s breadcrumb trail wrestle with each other for space on that top line. Maybe someday I can figure out how to stack them. Until then, I like the modeline and its placement more than I like the breadcrumb.

(use-package lsp-mode
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  (add-hook 'lsp-completion-mode-hook
            (lambda ()
              (setf (alist-get 'lsp-capf completion-category-defaults)
                    '((styles . (flex))))))
  :hook ((ruby-mode
          css-mode
          js2-mode
          web-mode
          ) . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :config
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting nil)
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

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

; Which Key?
;
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

(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)

(require 'tree-sitter)
(require 'tree-sitter-langs)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(setq major-mode-remap-alist
 '(
   (ruby-mode . ruby-ts-mode)
   (js2-mode . js-ts-mode)
   (lua-mode . lua-ts-mode)
   (css-mode . css-ts-mode)))

(use-package magit
  :commands magit-status
  :bind
  (("C-x g" . 'magit-status))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Code Completion
(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
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
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                              corfu-quit-no-match t
                              corfu-auto nil)
              (corfu-mode))))

(use-package rg
  :ensure t)

(use-package avy
  :bind
  ("C-;" . avy-goto-char-2))

(defun projectile-run-vterm ()
  (interactive)
  (let* ((project (projectile-ensure-project (projectile-project-root)))
        (buffer "vterm"))
    (require 'vterm)
    (if (buffer-live-p (get-buffer buffer))
        (switch-to-buffer buffer)
      (vterm))
    (vterm-send-string (concat "cd " project))
    (vterm-send-return)))

(use-package vterm
  :ensure t
  :bind
  ("C-x !" . projectile-run-vterm))

(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))

(with-eval-after-load 'ivy
  (push (cons #'swiper (cdr (assq t ivy-re-builders-alist)))
        ivy-re-builders-alist)
  (push (cons t #'ivy--regex-fuzzy) ivy-re-builders-alist))


;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Workaround for issue with typescript lsp server found here https://github.com/typescript-language-server/typescript-language-server/issues/559#issuecomment-1259470791
;; Shouldn't be necessary after upgrading to Emacs v29
(advice-add 'json-parse-string :around
            (lambda (orig string &rest rest)
              (apply orig (s-replace "\\u0000" "" string)
                     rest)))

;; minor changes: saves excursion and uses search-forward instead of re-search-forward
(advice-add 'json-parse-buffer :around
            (lambda (oldfn &rest args)
	      (save-excursion 
                (while (search-forward "\\u0000" nil t)
                  (replace-match "" nil t)))
		(apply oldfn args)))
