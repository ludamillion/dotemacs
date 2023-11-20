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
        exec-path-from-shell ; Get environment variables such as $PATH from the shell
        f                    ; Modern API for working with files and directories
        flycheck             ; Enhanced syntax checking suppposedly superior to flymake
        flycheck-eglot       ; Allow Flycheck to understand Eglot as a checker
        helpful              ; A better help buffer
        imenu-list           ; Show imenu entries in a separate
        lua-mode             ; Because it's a lovely little scripting/configuration language
        magit                ; A Git porcelain inside Emacs.
        marginalia           ; Enrich existing commands with completion annotations
        markdown-mode        ; Major mode for Markdown-formatted text
        multiple-cursors     ; Sometimes many cursors are better than one
        no-littering         ; Keep our things clean and tidy
        objed                ; Navigate and manipulate text objects
        orderless            ; Completion style for matching regexps in any order
        org-auto-tangle      ; Tangle org file when it is saved
        projectile           ; Project scoped stuffness
        rainbow-mode         ; Sometime you just need to see the colors
        rg                   ; Ripgrep for speed and profit(?)
        treesit-auto         ; Treesitter is native now but we can give it some help
        undo-fu              ; Work around Emacs' clunky undo interface
        undo-fu-session      ; Persistant undo across sessions
        use-package          ; A configuration macro for simplifying your .emacs
        vertico              ; VERTical Interactive COmpletion
        visual-fill-column   ; Nicer wrapping mostly for text modes
        vterm                ; A real terminal emulator running in Emacs
        web-mode             ; Uber mode for web templating languages
        which-key            ; Discovery method for key bindings
        yaml-mode            ; YAML mode
        ))

;; Install packages that are not yet installed
(dolist (package package-list)
  (straight-use-package package))

(defun my/org-babel-expand-body:emacs-lisp (orig-fun body params)
  "Expand BODY according to PARAMS and call original function with new body"

  (let* ((pro (or (cdr (assq :prologue params)) ""))
         (epi (or (cdr (assq :epilogue params)) ""))
         (body (concat pro body epi)))
    (apply orig-fun `(,body ,params))))

(advice-add 'org-babel-expand-body:emacs-lisp
            :around
            #'my/org-babel-expand-body:emacs-lisp)

(add-hook 'org-mode-hook 'org-auto-tangle-mode)

(require 'no-littering)
