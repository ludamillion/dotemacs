;;; esprit-term --- Terminal Emulation and related functionality -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package eat
  :straight (:type git
		   :host codeberg
		   :repo "akib/emacs-eat"
		   :files ("*.el" ("term" "term/*.el") "*.texi"
			   "*.ti" ("terminfo/e" "terminfo/e/*")
			   ("terminfo/65" "terminfo/65/*")
			   ("integration" "integration/*")
			   (:exclude ".dir-locals.el" "*-tests.el"))))

(provide 'esprit-term)
;;; esprit-term.el ends here

