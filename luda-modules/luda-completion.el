;;; luda-completion --- Completion Module -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package vertico
  :bind (:map vertico-map
              ("C-<backspace>" . vertico-directory-up))
	:hook
	(on-first-input . vertico-mode)
	(on-first-input . vertico-multiform-mode)
  :custom
  (vertico-resize t))

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
  :custom
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(defun corfu-x-eshell-hook ()
  "Set up Corfu behaviors in a shell friendly way."
  (setq-local corfu-auto nil)
  (corfu-mode))

(defun ludamacs--corfu-modes ()
  "Activate the desired corfu modes."
  (corfu-history-mode)
  (corfu-echo-mode)
  (global-corfu-mode))

(use-package corfu
  :init
  (setq corfu-cycle t
        corfu-auto t
        corfu-on-exact-match nil)
  :hook
  (on-first-buffer . ludamacs--corfu-modes)
  (eshell-mode . corfu-x-eshell-hook))

(defun luda/cape-capf-setup-eglot ()
  "Configure cape completion at point functions for Eglot managed modes."
  (let ((result))
    (dolist (element `(,(cape-capf-buster #'eglot-completion-at-point)
		       cape-file
		       cape-dabbrev) result)
      (add-to-list 'completion-at-point-functions element))))

(defun luda/cape-capf-setup-org ()
  "Configure cape completion at point functions for org mode."
  (let ((result))
    (dolist (element '(cape-dict cape-dabbrev) result)
      (add-to-list 'completion-at-point-functions element))))

(defun luda/cape-capf-setup-git-commit ()
  "Configure cape completion at point functions for git-commit mode."
  (let ((result))
    (dolist (element '(cape-dict cape-dabbrev) result)
      (add-to-list 'completion-at-point-functions element))))

(use-package cape
  :config
  (setq completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless))))
  :hook
  ((eglot-managed-mode . luda/cape-capf-setup-eglot)
   (org-mode . luda/cape-capf-setup-org)
   (git-commit-mode . luda/cape-capf-setup-git-commit)))

(use-package marginalia
  :hook
  (on-first-buffer . marginalia-mode)
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

(provide 'luda-completion)
;;; luda-completion.el ends here
