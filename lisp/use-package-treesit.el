;;; use-package-treesit.el --- Support for the :treesit keyword  -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Luke D. Inglis

;; Author: Luke D. Inglis <ld.inglis@gmail.com>
;; Created: 01 Nov 2024

;; URL: 

;; Package-Requires: ((emacs "29.1") (dash "2.19.1"))

;;; Commentary:

;; Provides support for the :tree keyword within the `use-package` macro.
;; This keyword allows for specifiying

;;; Code:

(require 'dash)
(require 'use-package)
(require 'xdg)

(defun use-package-xdg--normalize (_name-symbol keyword arg)
  (let ((error-msg
         (format "%s: wants a (<var> <filename>) or list of these, where var is a
  symbol and filename is a string."
                 keyword)))
    (unless (listp arg)
      (use-package-error error-msg))
    (cl-dolist (def arg arg)
      (unless (listp def)
        (use-package-error error-msg))
      (let ((var (nth 0 def))
            (file (nth 1 def)))
        (when (or (not (symbolp var))
                  (not (stringp file))
                  (not (= (length def) 2)))
          (use-package-error error-msg))))))

(defalias 'use-package-normalize/:xdg-state #'use-package-xdg--normalize)
(defalias 'use-package-normalize/:xdg-cache #'use-package-xdg--normalize)

(defun use-package-xdg--handler (base name _keyword args rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (list
      `(let ((dir (expand-file-name ,(symbol-name name)
                   (expand-file-name "emacs"
                    ,base))))
        (eval-after-load (quote ,name)
         (make-directory dir t))
        ,@(mapcar #'(lambda (def)
                      `(setopt ,(car def) (expand-file-name ,(cadr def) dir)))
           args)))
     body)))

(defun use-package-handler/:xdg-state (name _keyword args rest state)
  (use-package-xdg--handler '(xdg-state-home) name _keyword args rest state))
(defun use-package-handler/:xdg-cache (name _keyword args rest state)
  (use-package-xdg--handler '(xdg-cache-home) name _keyword args rest state))

(defun use-package-xdg--add-to-use-package-keywords (keyword)
  (unless (member keyword use-package-keywords)
    (setopt use-package-keywords
            (--splice-list (equal it :custom) `(:custom ,keyword) use-package-keywords))))
(use-package-xdg--add-to-use-package-keywords :xdg-state)
(use-package-xdg--add-to-use-package-keywords :xdg-cache)

(provide 'use-package-xdg)

;;; use-package-xdg.el ends here
