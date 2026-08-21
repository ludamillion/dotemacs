(define-minor-mode prose-mode
  "Toggles prose-mode."
  nil
  :global nil
  :group 'prose
  :lighter " prose"

  (if dotcrafter-mode
      (add-hook 'org-mode-hook #'dotcrafter--org-mode-hook)
    (remove-hook 'org-mode-hook #'dotcrafter--org-mode-hook)))
