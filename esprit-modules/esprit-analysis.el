;;; esprit-analysis --- Code and Text Analysis Module -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

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
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-no-changes-timeout nil)
  (flymake-start-on-flymake-mode t)
  (flymake-start-on-save-buffer t)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-wrap-around nil)
  (flymake-mode-line-format
        '("" flymake-mode-line-exception flymake-mode-line-counters))
  (flymake-mode-line-counter-format
        '("" flymake-mode-line-error-counter
          flymake-mode-line-warning-counter
          flymake-mode-line-note-counter ""))
  (flymake-show-diagnostics-at-end-of-line nil)) ; Emacs 30

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :custom (jinx-languages "en_US")
  :bind
  (("C-;" . jinx-correct-nearest)
   ("C-x j a" . jinx-correct-all)
   ("C-x j n" . jinx-next)
   ("C-x j p" . jinx-previous)))

(defun require-and-ensure-eglot-ltex ()
  "Require the eglot-ltex package and run `eglot-ensure'."
  (require 'eglot-ltex)
  (eglot-ensure))

(use-package eglot-ltex
  :straight (:type git
		   :host github
		   :repo "emacs-languagetool/eglot-ltex")
  :hook
  (esprit/prose-mode-list . require-and-ensure-eglot-ltex)
  :init
  (setq eglot-ltex-server-path "~/tools/ltex-ls-plus/bin/ltex-ls-plus"
	eglot-ltex-communication-channel 'stdio))

(provide 'esprit-analysis)
;;; esprit-analysis.el ends here
