# Emacs Config Review

Veteran minimalist perspective. Bugs first, then contradictions, redundancies, and anti-patterns.

No open issues. All findings from prior passes have been fixed and verified:

- `helpful-switch-to-buffer` typo/broken `if`
- `avy` dispatch alist misplaced in `:custom`
- malformed nested `ghostel` block
- `org` `:custom`/`:config` misplacement
- cross-file duplicate settings (early-init.el vs init.el)
- redundant `recentf-mode`/`savehist-mode`/`global-auto-revert-mode` activations
- leftover `straight.el` bootstrap and `:straight` keywords (project moved to `package.el`/`:vc`)
- `asdf` + `mise` both loaded
- `web-mode` declared twice
- `cape` capf setup accumulation bug
- `save-place-mode` state file bypassing XDG (`places.eld` now routed through `:xdg-state`,
  untracked from git, and added to `.gitignore`)
- dead `declare-function` for `esprit-emacs/disable-global-scrolling-in-ansi-term`
