(setq site-run-file nil)                        ; No site-wide run-time initializations.
(setopt inhibit-default-init t)                 ; No site-wide default library
(setopt gc-cons-threshold most-positive-fixnum) ; Very large threshold for garbage collector during init
(setopt package-enable-at-startup nil)          ; We'll use straight.el
(setopt use-package-ensure-function 'ignore)    ; Don't let use-package load package.el on 'ensure'
(setopt package-archives nil)                   ; Don't even reference the idea of the archives

(require 'xdg)
(startup-redirect-eln-cache
 (expand-file-name "emacs/eln-cache/" (xdg-cache-home)))

;; Reset garbage collector limit after init process has ended (8Mo)
(add-hook 'after-init-hook
          #'(lambda () (setq gc-cons-threshold (* 8 1024 1024))))
