;;; luda-lsp --- Language Server Protocol Module -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(defvar eglot-enabled-modes
  '(js-ts-mode
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
  :custom
  (eldoc-echo-area-use-multiline-p nil)
  (eglot-events-buffer-config '(:size 2000000 :format lisp))
  :config
  (setq-default eglot-workspace-configuration
                '(:ltex-ls (:language "en-US"
                                      :disabledRules ["MORFOLOGIK_RULE_EN_US"])))
  )

(provide 'luda-lsp)
;;; luda-lsp.el ends here
