;;; luda-analysis --- Code and Text Analysis Module -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; (add-to-list 'display-buffer-alist
;;              `(,(rx bos "*Flycheck errors*" eos)
;;                (display-buffer-reuse-window
;; 		display-buffer-in-side-window)
;;                (side            . bottom)
;;                (reusable-frames . visible)
;;                (window-height   . 0.33)))

;; (use-package flycheck
;;   :hook
;;   (on-first-buffer . global-flycheck-mode))

;; (use-package flycheck-eglot
;;   :hook
;;   (eglot-managed-mode . flycheck-eglot-mode)
;;   :after (flycheck eglot))


(use-package flymake
  :ensure nil
  :bind
  (:map ctl-x-x-map
    ("m" . flymake-mode) ; C-x x m
    :map flymake-mode-map
    ("C-c ! s" . flymake-start)
    ("C-c ! d" . flymake-show-buffer-diagnostics) ; Emacs28
    ("C-c ! D" . flymake-show-project-diagnostics) ; Emacs28
    ("C-c ! n" . flymake-goto-next-error)
    ("C-c ! p" . flymake-goto-prev-error))
  :config
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)
  (setq flymake-mode-line-format
        '("" flymake-mode-line-exception flymake-mode-line-counters))
  (setq flymake-mode-line-counter-format
        '("" flymake-mode-line-error-counter
          flymake-mode-line-warning-counter
          flymake-mode-line-note-counter ""))
  (setq flymake-show-diagnostics-at-end-of-line nil)) ; Emacs 30

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("C-;" . jinx-correct)
         ("C-M-;" . jinx-languages)))

(defvar ludamacs-prose-mode-list
  '(org-mode
    git-commit-elisp-text-mode
    bibtex-mode
    context-mode
    latex-mode
    markdown-mode
    rst-mode
    text-mode)
  "A list of what ludamacs considers prose modes.")

(defun require-and-ensure-eglot-ltex ()
  "Require the eglot-ltex package and run `eglot-ensure'."
  (require 'eglot-ltex)
  (eglot-ensure))

(use-package eglot-ltex
  :straight (eglot-ltex :type git
			:host github
			:repo "emacs-languagetool/eglot-ltex")
  :hook
  (ludamacs-prose-mode-list . require-and-ensure-eglot-ltex)
  :init
  (setq eglot-ltex-server-path "/usr/local/"
	eglot-ltex-communication-channel 'stdio))

(provide 'luda-analysis)
;;; luda-analysis.el ends here
