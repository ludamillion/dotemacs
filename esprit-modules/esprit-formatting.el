;;; esprit-formatting --- Code and Text Formatting Module -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package apheleia
  :straight (:host github :repo "raxod502/apheleia")
	:custom
  (add-to-list 'apheleia-mode-alist '(js-ts-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier)))

(provide 'esprit-formatting)
;;; esprit-formatting.el ends here
