;;; early-init.el --- Early Init -*- lexical-binding: t; -*-

;; Author: Luke Inglis
;; URL: https://github.com/ludamillion/esprit-emacs
;; Package-Requires: ((emacs "29.1"))
;; Keywords: maint
;; Version: 0.1.0
;; SPDX-License-Identifier: GPL-3.0-or-later

(setopt debug-on-error t)

(defvar esprit-emacs--backup-gc-cons-threshold gc-cons-threshold)
(defvar esprit-emacs--backup-gc-cons-percentage gc-cons-percentage)

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

(defvar esprit-emacs-user-directory "~/code/dotemacs/"
  "Pointer to me Emacs home directory.")

(defvar esprit-emacs-ui-features '()
  "List of user interface features to enable in minimal Emacs setup.
  This variable holds a list of Emacs UI features that can be enabled:
  - context-menu (Enables the context menu in graphical environments.)
  - tool-bar (Enables the tool bar in graphical environments.)
  - menu-bar (Enables the menu bar in graphical environments.)
  - dialogs (Enables both file dialogs and dialog boxes.)
  - tooltips (Enables tooltips.)")

(defvar esprit-emacs-frame-title-format "%b – Emacs"
  "Template for displaying the title bar of visible and iconified frame.")

(defvar esprit-emacs-debug (bound-and-true-p init-file-debug)
  "Non-nil to enable debug.")

(defvar esprit-emacs-optimize-startup-gc t
  "If non-nil, increase `gc-cons-threshold' during startup to reduce pauses.
After Emacs finishes loading, `gc-cons-threshold' is restored to the value
stored in `esprit-emacs-gc-cons-threshold'.")

(defvar esprit-emacs-gc-cons-threshold-restore-delay nil
  "Number of seconds to wait before restoring `gc-cons-threshold'.")

(defvar esprit-emacs-gc-cons-threshold (* 32 1024 1024)
  "Value to which `gc-cons-threshold' is set after Emacs startup.
Ignored if `esprit-emacs-optimize-startup-gc' is nil.")

(defvar esprit-emacs-gc-cons-percentage gc-cons-percentage
  "Value to which `gc-cons-percentage' is set after Emacs startup.
Ignored if `esprit-emacs-optimize-startup-gc' is nil.")

(defvar esprit-emacs-optimize-file-name-handler-alist t
  "Enable optimization of `file-name-handler-alist'.
When non-nil, this variable activates optimizations to reduce file name handler
lookups during Emacs startup.")

(defvar esprit-emacs-disable-mode-line-during-startup t
  "Disable the mode line during startup.
This reduces visual clutter and slightly enhances startup performance. The
tradeoff is that the mode line is hidden during the startup phase.")

(defvar esprit-emacs-package-initialize-and-refresh nil
  "Whether to automatically initialize and refresh packages.
When set to non-nil, Emacs will automatically call `package-initialize' and
`package-refresh-contents' to set up and update the package system.")

(defvar esprit-emacs-inhibit-redisplay-during-startup nil
  "Suppress redisplay during startup to improve performance.
This prevents visual updates while Emacs initializes. The tradeoff is that you
won't see the progress or activities during the startup process.")

(defvar esprit-emacs-inhibit-message-during-startup nil
  "Suppress startup messages for a cleaner experience.
This slightly enhances performance. The tradeoff is that you won't be informed
of the progress or any relevant activities during startup.")

(defvar esprit-emacs--success nil)
(defun esprit-emacs--check-success ()
  "Verify that the Emacs configuration has loaded successfully."
  (unless esprit-emacs--success
    (cond
     ((or (file-exists-p (expand-file-name "~/.emacs.el"))
          (file-exists-p (expand-file-name "~/.emacs")))
      (error "Emacs ignored loading 'init.el'. Please ensure that files such as ~/.emacs or ~/.emacs.el do not exist, as they may be preventing Emacs from loading the 'init.el' file"))

     (t
      (error "Configuration error. Debug by starting Emacs with: --debug-init")))))

(unless noninteractive
  (add-hook 'emacs-startup-hook #'esprit-emacs--check-success 102))

(setq custom-theme-directory
      (expand-file-name "themes/" esprit-emacs-user-directory))

(setq custom-file (expand-file-name "custom.el" esprit-emacs-user-directory))

(defun esprit-emacs--restore-gc-values ()
  "Restore garbage collection values to esprit-emacs.d values."
  (setq gc-cons-threshold esprit-emacs-gc-cons-threshold)
  (setq gc-cons-percentage esprit-emacs-gc-cons-percentage))

(defun esprit-emacs--restore-gc ()
  "Restore garbage collection settings."
  (if (and (bound-and-true-p esprit-emacs-gc-cons-threshold-restore-delay)
           ;; In noninteractive mode, the event loop does not run
           (not noninteractive))
      ;; Defer garbage collection during initialization to avoid 2 collections.
      (run-with-timer esprit-emacs-gc-cons-threshold-restore-delay nil
                      #'esprit-emacs--restore-gc-values)
    (esprit-emacs--restore-gc-values)))

(if esprit-emacs-optimize-startup-gc
    ;; `gc-cons-threshold' is managed by esprit-emacs.d
    (add-hook 'emacs-startup-hook #'esprit-emacs--restore-gc 105)
  ;; gc-cons-threshold is not managed by esprit-emacs.d.
  (when (= gc-cons-threshold most-positive-fixnum)
    (setq gc-cons-threshold esprit-emacs--backup-gc-cons-threshold)
    (setq gc-cons-percentage esprit-emacs--backup-gc-cons-percentage)))

(unless (and (featurep 'native-compile)
             (fboundp 'native-comp-available-p)
             (native-comp-available-p))
  ;; Deactivate the `native-compile' feature if it is not available
  (setq features (delq 'native-compile features)))

(setq native-comp-warning-on-missing-source esprit-emacs-debug
      native-comp-async-report-warnings-errors (or esprit-emacs-debug 'silent))

(setq jka-compr-verbose esprit-emacs-debug)
(setq byte-compile-warnings esprit-emacs-debug
      byte-compile-verbose esprit-emacs-debug)

(set-language-environment "UTF-8")

(setq read-process-output-max (* 2 1024 1024))  ; 1024kb
(setq process-adaptive-read-buffering nil)

(setq max-specpdl-size 3200)
(setq max-lisp-eval-depth 3200)

(setq warning-minimum-level (if esprit-emacs-debug :warning :error))
(setq warning-suppress-types '((lexical-binding)))

(when esprit-emacs-debug
  (setq message-log-max 16384))

(setq ffap-machine-p-known 'reject)

(setq ad-redefinition-action 'accept)

(setq inhibit-compacting-font-caches t)

(when (not noninteractive)
  ;; Resizing the Emacs frame can be costly when changing the font. Disable this
  ;; to improve startup times with fonts larger than the system default.
  (setq frame-resize-pixelwise t)

  ;; Without this, Emacs will try to resize itself to a specific column size
  (setq frame-inhibit-implied-resize t)

  ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  ;; No second pass of case-insensitive search over auto-mode-alist.
  (setq auto-mode-case-fold nil)

  ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
  ;; dashboard) is more than enough, and faster to display.
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name)
  (setq initial-buffer-choice nil
        inhibit-startup-buffer-menu t
        inhibit-x-resources t)

  ;; y/n rather than yes/no
  (setq-default use-short-answers t)

  ;; Disable bidirectional text scanning for a modest performance boost.
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; Give up some bidirectional functionality for slightly faster re-display.
  (setq bidi-inhibit-bpa t)

  ;; Remove "For information about GNU Emacs..." message at startup
  (advice-add 'display-startup-echo-area-message :override #'ignore)

  ;; Suppress the vanilla startup screen completely. We've disabled it with
  ;; `inhibit-startup-screen', but it would still initialize anyway.
  (advice-add 'display-startup-screen :override #'ignore)

  (unless esprit-emacs-debug
    ;; Unset command line options irrelevant to the current OS. These options
    ;; are still processed by `command-line-1` but have no effect.
    (unless (eq system-type 'darwin)
      (setq command-line-ns-option-alist nil))
    (unless (memq initial-window-system '(x pgtk))
      (setq command-line-x-option-alist nil))))

;;; Performance: File-name-handler-alist

(defvar esprit-emacs--old-file-name-handler-alist (default-toplevel-value
                                                   'file-name-handler-alist))

(defun esprit-emacs--respect-file-handlers (fn args-left)
  "Respect file handlers.
FN is the function and ARGS-LEFT is the same argument as `command-line-1'.
Emacs processes command-line files very early in startup. These files may
include special paths like TRAMP paths, so restore `file-name-handler-alist' for
this stage of initialization."
  (let ((file-name-handler-alist (if args-left
                                     esprit-emacs--old-file-name-handler-alist
                                   file-name-handler-alist)))
    (funcall fn args-left)))

(defun esprit-emacs--restore-file-name-handler-alist ()
  "Restore `file-name-handler-alist'."
  (set-default-toplevel-value
   'file-name-handler-alist
   ;; Merge instead of overwrite to preserve any changes made since startup.
   (delete-dups (append file-name-handler-alist
                        esprit-emacs--old-file-name-handler-alist))))

(when (and esprit-emacs-optimize-file-name-handler-alist
           (not esprit-emacs-debug))
  ;; Determine the state of bundled libraries using calc-loaddefs.el. If
  ;; compressed, retain the gzip handler in `file-name-handler-alist`. If
  ;; compiled or neither, omit the gzip handler during startup for improved
  ;; startup and package load time.
  (set-default-toplevel-value
   'file-name-handler-alist
   (if (locate-file-internal "calc-loaddefs.el" load-path)
       nil
     (list (rassq 'jka-compr-handler
                  esprit-emacs--old-file-name-handler-alist))))

  ;; Ensure the new value persists through any current let-binding.
  (put 'file-name-handler-alist 'initial-value
       esprit-emacs--old-file-name-handler-alist)

  ;; Emacs processes command-line files very early in startup. These files may
  ;; include special paths TRAMP. Restore `file-name-handler-alist'.
  (advice-add 'command-line-1 :around #'esprit-emacs--respect-file-handlers)

  (add-hook 'emacs-startup-hook #'esprit-emacs--restore-file-name-handler-alist
            101))

;;; Performance: Inhibit redisplay

(defun esprit-emacs--reset-inhibit-redisplay ()
  "Reset inhibit redisplay."
  (setq-default inhibit-redisplay nil)
  (remove-hook 'post-command-hook #'esprit-emacs--reset-inhibit-redisplay))

(when (and esprit-emacs-inhibit-redisplay-during-startup
           (not noninteractive)
           (not esprit-emacs-debug))
  ;; Suppress redisplay and redraw during startup to avoid delays and
  ;; prevent flashing an unstyled Emacs frame.
  (setq-default inhibit-redisplay t)
  (add-hook 'post-command-hook #'esprit-emacs--reset-inhibit-redisplay -100))

;;; Performance: Inhibit message

(defun esprit-emacs--reset-inhibit-message ()
  "Reset inhibit message."
  (setq-default inhibit-message nil)
  (remove-hook 'post-command-hook #'esprit-emacs--reset-inhibit-message))

(when (and esprit-emacs-inhibit-message-during-startup
           (not noninteractive)
           (not esprit-emacs-debug))
  (setq-default inhibit-message t)
  (add-hook 'post-command-hook #'esprit-emacs--reset-inhibit-message -100))

;;; Performance: Disable mode-line during startup

(defvar-local esprit-emacs--hidden-mode-line nil
  "Store the buffer-local value of `mode-line-format' during startup.")

(when (and esprit-emacs-disable-mode-line-during-startup
           (not noninteractive)
           (not esprit-emacs-debug))
  (put 'mode-line-format
       'initial-value (default-toplevel-value 'mode-line-format))
  (setq-default mode-line-format nil)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (local-variable-p 'mode-line-format)
        (setq esprit-emacs--hidden-mode-line mode-line-format)
        (setq mode-line-format nil)))))

;;; Restore values

(defun esprit-emacs--startup-load-user-init-file (fn &rest args)
  "Advice to reset `mode-line-format'. FN and ARGS are the function and args."
  (unwind-protect
      ;; Start up as normal
      (apply fn args)
    ;; If we don't undo inhibit-{message, redisplay} and there's an error, we'll
    ;; see nothing but a blank Emacs frame.
    (when esprit-emacs-inhibit-message-during-startup
      (setq-default inhibit-message nil))
    (when esprit-emacs-inhibit-redisplay-during-startup
      (setq-default inhibit-redisplay nil))
    ;; Restore the mode-line
    (when esprit-emacs-disable-mode-line-during-startup
      (unless (default-toplevel-value 'mode-line-format)
        (setq-default mode-line-format (get 'mode-line-format
                                            'initial-value)))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (local-variable-p 'esprit-emacs--hidden-mode-line)
            (setq mode-line-format esprit-emacs--hidden-mode-line)
            (kill-local-variable 'esprit-emacs--hidden-mode-line)))))))

(advice-add 'startup--load-user-init-file :around
            #'esprit-emacs--startup-load-user-init-file)

;;; UI elements

(setq frame-title-format esprit-emacs-frame-title-format
      icon-title-format esprit-emacs-frame-title-format)

;; Disable startup screens and messages
(setq inhibit-splash-screen t)

;; I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;; `scroll-bar-mode' because manipulating frame parameters can trigger or queue
;; a superfluous and potentially expensive frame redraw at startup, depending
;; on the window system. The variables must also be set to `nil' so users don't
;; have to call the functions twice to re-enable them.
(unless (memq 'menu-bar esprit-emacs-ui-features)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (unless (memq window-system '(mac ns))
    (setq menu-bar-mode nil)))

(defun esprit-emacs--setup-toolbar (&rest _)
  "Setup the toolbar."
  (when (fboundp 'tool-bar-setup)
    (advice-remove 'tool-bar-setup #'ignore)
    (when (bound-and-true-p tool-bar-mode)
      (funcall 'tool-bar-setup))))

(unless noninteractive
  (when (fboundp 'tool-bar-setup)
    ;; Temporarily override the tool-bar-setup function to prevent it from
    ;; running during the initial stages of startup
    (advice-add 'tool-bar-setup :override #'ignore)

    (advice-add 'startup--load-user-init-file :after
                #'esprit-emacs--setup-toolbar)))

(unless (memq 'tool-bar esprit-emacs-ui-features)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (setq tool-bar-mode nil))

(setq default-frame-scroll-bars 'right)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)

(unless (memq 'tooltips esprit-emacs-ui-features)
  (when (bound-and-true-p tooltip-mode)
    (tooltip-mode -1)))

;; Disable GUIs because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(unless (memq 'dialogs esprit-emacs-ui-features)
  (setq use-file-dialog nil)
  (setq use-dialog-box nil))

;;; Security
(setq gnutls-verify-error t)  ; Prompts user if there are certificate issues
(setq tls-checktrust t)  ; Ensure SSL/TLS connections undergo trust verification
(setq gnutls-min-prime-bits 3072)  ; Stronger GnuTLS encryption

;; This results in a more compact output that emphasizes performance
(setq use-package-expand-minimally t)

(setq use-package-minimum-reported-time (if esprit-emacs-debug 0 0.1))
(setq use-package-verbose esprit-emacs-debug)
(setq use-package-enable-imenu-support t)

;; package.el
(setq package-enable-at-startup nil)  ; Let the init.el file handle this
(setq package-archives '(("melpa"        . "https://melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("nongnu"       . "https://elpa.nongnu.org/nongnu/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(setq package-archive-priorities '(("gnu"    . 99)
                                   ("nongnu" . 80)
                                   ("melpa"  . 70)
                                   ("melpa-stable" . 50)))

(let ((default-directory  (expand-file-name "lisp" user-emacs-directory)))
  (setq load-path
        (append
         (let ((load-path  (copy-sequence load-path))) ;; Shadow
           (append
            (copy-sequence (normal-top-level-add-to-load-path '(".")))
            (normal-top-level-add-subdirs-to-load-path)))
         load-path)))

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; early-init.el ends here
