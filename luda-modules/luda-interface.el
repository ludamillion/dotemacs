;;; luda-interface --- Interface Module -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package ace-window
  :bind
  ("M-o" . 'ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?j ?k ?l))
  (aw-dispatch-always t)
  (aw-dispatch-alist
   '((?x aw-delete-window "Ace - Delete Window")
     (?c aw-swap-window "Ace - Swap Window")
     (?n aw-flip-window)
     (?v aw-split-window-vert "Ace - Split Vert Window")
     (?h aw-split-window-horz "Ace - Split Horz Window")
     (?m delete-other-windows "Ace - Maximize Window")
     (?b balance-windows)
     (?u (lambda ()
           (progn
             (winner-undo)
             (setq this-command 'winner-undo))))
     (?r winner-redo))))

(keymap-global-set "C-M-o" 'mode-line-other-buffer)

(setq visible-bell nil
      ring-bell-function #'ignore)

(setq switch-to-buffer-obey-display-actions t)

(add-to-list 'display-buffer-alist
             '("\\*Help\\*\\|\\*helpful.*\\*"
               (display-buffer-in-side-window)
               (side . right)
               (slot . 0)
               (window-width . 80)
               (window-parameters
                (no-delete-other-windows . t))))

(defun luda-close-dwim ()
  "Quit a frame the same way no matter what kind of frame you are on."
  (interactive)
  (let ((frames (visible-frame-list)))
    (if (eq (car frames) (selected-frame))
        ;; For parent/master frame...
        (if (cdr frames)
            ;; Close a parent with children present.
            (progn
              (delete-frame (selected-frame))
              (when (and (eq (cadr frames) terminal-frame)
                         (null (cddr frames)))
                ;; No frames left on this daemon: shut it down.
                (save-buffers-kill-emacs)))
          ;; Close a parent with no children present.
          (save-buffers-kill-emacs))
      ;; Close a child frame.
      (delete-frame (selected-frame)))))

;; (use-package swiss-move
;;   :bind (("s-n" . swiss-move-line-down)
;; 	 ("s-p" . swiss-move-line-up)))

(global-set-key (kbd "C-x C-c") 'luda-close-dwim)

(keymap-set global-map "C-x k" 'kill-current-buffer)
(keymap-set global-map "C-x C-k" 'kill-buffer)

(use-package fontaine
  :demand t
  :custom
  (fontaine-latest-state-file
   (locate-user-emacs-file "fontaine-latest-state.eld"))
  (fontaine-presets
      '((small
         :default-family "Aporetic Sans Mono"
         :default-height 80
         :variable-pitch-family "Aporetic Serif")
        (regular) ; like this it uses all the fallback values and is named `regular'
        (medium
         :default-height 160
         :bold-weight regular)
        (large
         :inherit medium
         :default-height 180)
        (t
         :default-family "Aporetic Sans Mono"
         :variable-pitch-family "Aporetic Serif"

         :fixed-pitch-height 1.0
         :fixed-pitch-serif-height 1.0
         :variable-pitch-height 1.0)))
  :config
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  ;; Persist the latest font preset when closing/starting Emacs and
  ;; while switching between themes.
  (fontaine-mode 1)
  :bind
  ("C-c f" . #'fontaine-set-preset))

(provide 'luda-interface)
;;; luda-interface.el ends here
