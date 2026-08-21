# Emacs Config Review

Veteran minimalist perspective. Bugs first, then contradictions, redundancies, and anti-patterns.

---

## Bugs

### `esprit/helpful-switch-to-buffer` (init.el:1308)
Typo `'heplful-mode` + broken `if` form — the `then` branch is `switch-to-buffer buffer-or-name)` (evaluates the symbol, not a function call), so `pop-to-buffer` always fires.

```elisp
;; Fix:
(defun esprit/helpful-switch-to-buffer (buffer-or-name)
  (if (eq major-mode 'helpful-mode)
      (switch-to-buffer buffer-or-name)
    (pop-to-buffer buffer-or-name)))
```

### `avy` dispatch alist in `:custom` (init.el:972)
`:custom` expects `(VARIABLE VALUE)` pairs — a bare `setf` expression is not one. `use-package` silently treats `setf` as the variable name. The dispatch alist is never actually configured. Move the `setf` block to `:config`.

### Malformed `ghostel` block (init.el:1512)
A second `(use-package ghostel ...)` is nested inside the outer one's body (not under a keyword). The outer block has only `:straight`; all actual config is in the orphaned inner form. Flatten into one `use-package`.

### `add-to-list` and `org-babel-do-load-languages` in `org` `:custom` (init.el:1446)
`:custom` calls `customize-set-variable` on each pair. `add-to-list` and `org-babel-do-load-languages` get treated as variable names — silently wrong. Move to `:config`.

---

## Contradictory Settings

### `global-visual-line-mode` vs `truncate-lines t` (init.el:192, 471)
`global-visual-line-mode` sets `truncate-lines` to nil buffer-locally. But `:custom` sets the default to `t`. They fight per-buffer. Decide: soft-wrap everywhere → remove `(truncate-lines t)`; hard-wrap → disable `global-visual-line-mode`.

### `recentf-max-saved-items`: `300` (init.el:160) vs `100` (init.el:543)
The `recentf` use-package block wins. Remove the entry from `emacs` `:custom`.

### `history-length`: `300` (init.el:143) vs `50` (init.el:587)
The `savehist` use-package block wins. Remove from `emacs` `:custom`.

### `read-process-output-max`: `2MB` (early-init.el:135) vs `4MB` (init.el:158)
Comment on line 135 also says "1024kb" which is wrong for 2MB. Remove from `early-init.el` — this variable affects LSP throughput, not startup. Set it once in `init.el`.

### `visible-bell` / `ring-bell-function` (init.el:153, 196 vs 1031–1032)
Set in `emacs` `:custom` and again via bare `setq`. Remove lines 1031–1032.

### `switch-to-buffer-obey-display-actions` (init.el:186 vs 1034)
Same pattern. Remove line 1034.

### `create-lockfiles nil` (init.el:149 vs 1274)
Set in two separate `use-package emacs` blocks. Remove one.

### `savehist-additional-variables` (init.el:176–180 vs 589)
Set in `emacs` `:custom` and overridden by the `savehist` block via `savehist-watchlist`. The `emacs` `:custom` entry is dead code.

---

## Redundant Mode Activations

- `recentf-mode`: line 464 (`emacs` `:init`) + hooked at line 541 (`recentf` use-package)
- `savehist-mode`: line 466 + hooked at line 582

Remove both from `emacs` `:init`. Let each package's block own its mode activation.

---

## Cross-file Redundancies (early-init.el vs init.el)

`init.el` values win in all cases — the `early-init.el` entries are noise (or vice versa). Pick one home per setting.

| Setting | early-init.el | init.el |
|---|---|---|
| `ffap-machine-p-known` | line 147 | line 137 |
| `ad-redefinition-action` | line 149 | line 125 |
| `frame-resize-pixelwise` | line 156 | line 139 |
| `use-short-answers` | line 174 | line 195 |
| `use-dialog-box` / `use-file-dialog` | lines 368–369 | lines 193–194 |
| `inhibit-startup-screen` | line 167 + `inhibit-splash-screen` line 324 | `inhibit-startup-message` line 144 (all three are aliases for the same var) |

Rule: startup/frame/performance settings → `early-init.el`. Everything else → `init.el`.

---

## Incorrect `use-package` Patterns

### `(use-package emacs :straight (misc :type built-in))` (init.el:982)
`emacs` is a pseudo-package — `:straight` is meaningless here. The intent is to bind `zap-up-to-char`, which lives in `misc.el`. Should be:
```elisp
(use-package misc :straight (:type built-in)
  :bind ("C-z" . zap-up-to-char))
```
Also note: `zap-up-to-char` is already bound to `M-Z` at line 111. Two bindings for the same command.

### `(use-package elec-pair :straight (misc :type built-in))` (init.el:988)
`elec-pair` is not in `misc.el`. Drop `:straight` entirely — it's a built-in feature.

### Modes called in `:custom` (init.el:135, 140, 152)
`global-goto-address-mode`, `pixel-scroll-precision-mode`, and `delete-selection-mode` are minor mode toggles, not `defcustom` variables. Move to `:init` or `:config`.

---

## Four `use-package emacs` Blocks (init.el:93, 531, 982, 1273)

`use-package` merges them, but splitting config across four separate blocks makes the file hard to reason about. Consolidate into one (or two at most: general config + xdg-state).

---

## Other Issues

### `debug-on-error t` unconditional (early-init.el:10)
Not guarded by `esprit-emacs-debug`. Fires for all sessions.
```elisp
;; Fix:
(when esprit-emacs-debug
  (setopt debug-on-error t))
```

### `straight.el` bootstrap URL (init.el:23)
Points to `raxod502/straight.el` — the project moved to `radian-software/straight.el`. The old URL may currently redirect but is unreliable long-term.

### `asdf` + `mise` both loaded (init.el:605–608, 1610–1612)
Both are version managers. `mise` supersedes `asdf` and handles the same toolchains. Running both is redundant. If `mise` is the current tool, remove `asdf`.

### `tempel` missing `:straight t` (init.el:1219)
`tempel-collection` at line 1229 has `:straight t` but `tempel` itself doesn't. Either both should have it, or neither — check your `straight-use-package-by-default` setting.

### `web-mode` declared twice (init.el:1205, 1606)
Second declaration only has a `:config` block. Merge into the first.

### `cape` capf setup functions (init.el:808–826)
`let ((result)) ... (dolist ... result)` returns `nil` — the accumulation never collects. `add-to-list` also doesn't guarantee capf order. Use `setq-local` instead:
```elisp
(defun esprit/cape-capf-setup-eglot ()
  (setq-local completion-at-point-functions
              (list (cape-capf-buster #'eglot-completion-at-point)
                    #'cape-file #'cape-dabbrev)))
```
