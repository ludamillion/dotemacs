;;; esprit-movement.el --- Movements to make your cursor light on its feet -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Luke Inglis

;; Author: Luke Inglis <ld.inglis@gmail.com>
;; Maintainer: Luke Inglis <ld.inglis@gmail.com>>
;; URL: https://github.com/ludamillion/esprit-movement
;; Created: 19th July 2024
;; Version: 0.1.0
;; Keywords: faces
;; Package-Requires: ((emacs "24"))

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 4 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;; This is the naive version found here https://www.d12frosted.io/posts/2020-06-04-beginning-of-line

(defun esprit/beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line. If
point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil, move forward ARG lines first. If point reaches
the beginning or end of the buffer, stop there."
  (interactive "P")
  (when (numberp arg)
    (let ((line-move-visual nil))
      (forward-line arg)))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(provide 'esprit-movement)
;;; esprit-movement.el ends here
