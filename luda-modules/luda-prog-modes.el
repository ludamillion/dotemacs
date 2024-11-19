;;; luda-prog-modes --- Programming Modes Module -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(setq treesit-language-source-alist
      '((c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (lua "https://github.com/MunifTanjim/tree-sitter-lua")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (scss "https://github.com/serenadeai/tree-sitter-scss")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(dolist (ts-pair treesit-language-source-alist)
  (let ((language (car ts-pair)) (repo (cadr ts-pair)))
    (unless (treesit-language-available-p language)
      (message "Installing parser for %s from %s" language repo)
      (treesit-install-language-grammar language))))

(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))

(use-package css-mode
  :custom
  (tab-width 2)
  (css-indent-offset 2))

(use-package web-mode
  :mode
  (("\\.erb\\'" . web-mode)
   ("\\.html?\\'" . web-mode)))

(use-package jtsx
  :straight (jtsx :type git :host github :repo "llemaitre19/jtsx")
  :mode (("\\.jsx?\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.ts\\'" . jtsx-typescript-mode))
  :custom
  (js-indent-level 2)
  (typescript-ts-mode-indent-offset 2)
  (jtsx-switch-indent-offset 0)
  (jtsx-indent-statement-block-regarding-standalone-parent nil)
  (jtsx-jsx-element-move-allow-step-out t)
  (jtsx-enable-jsx-electric-closing-element t))

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :mode "\\.pryrc\\'"
  :mode "Rakefile\\'"
  :mode "Gemfile\\'")

(use-package csharp-mode
  :straight (:type built-in))

(use-package ob-csharp
  :straight (ob-csharp :type git :host github :repo "samwdp/ob-csharp"))

(provide 'luda-prog-modes)
;;; luda-prog-modes.el ends here
