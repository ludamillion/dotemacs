;;; luda-lsp --- Language Server Protocol Module -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar eglot-enabled-modes
  '(csharp-mode
    js-ts-mode
    typescript-ts-mode
    ruby-mode)
  "Opt in list of modes which Eglot should manage.")

(use-package eglot
  :straight (:type built-in)
  :demand t
  :hook (eglot-enabled-modes . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-x l r" . eglot-rename)
              ("M-k" . eglot-code-actions))
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eglot-events-buffer-config '(:size 2000000 :format lisp))
  (setq-default eglot-workspace-configuration
                '(:ltex-ls (:language "en-US"
                                      :disabledRules ["MORFOLOGIK_RULE_EN_US"])))
  (add-to-list 'eglot-server-programs
               '(ruby-mode . ("solargraph" "stdio"))))

(use-package eglot-booster
  :straight (:type git :host github :repo "jdtsmith/eglot-booster")
	:after (eglot)
  :config
  (eglot-booster-mode))

(provide 'luda-lsp)
;;; luda-lsp.el ends here
