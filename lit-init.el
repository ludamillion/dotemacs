(setq straight-check-for-modifications nil)

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

(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path
             (expand-file-name "theme" user-emacs-directory))

(setq package-list
      '(cape                ; Completion At Point Extensions
        orderless           ; Completion style for matching regexps in any order
        vertico             ; VERTical Interactive COmpletion
        marginalia          ; Enrich existing commands with completion annotations
        consult             ; Consulting completing-read
        corfu               ; Completion Overlay Region FUnction
        f                   ; Modern API for working with files and directories
        helpful             ; A better help buffer
        imenu-list          ; Show imenu entries in a separate buffer
        magit               ; A Git porcelain inside Emacs.
        markdown-mode       ; Major mode for Markdown-formatted text
        use-package         ; A configuration macro for simplifying your .emacs
        yaml-mode           ; YAML mode
        org-auto-tangle     ; Tangle org file when it is saved
        exec-path-from-shell; Get environment variables such as $PATH from the shell 
        which-key))         ; Display available keybindings in popup

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
