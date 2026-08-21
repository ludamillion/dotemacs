# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Personal Emacs configuration ("Esprit Emacs") for Emacs 29.1+. Uses `straight.el` for package management and `use-package` for declarative configuration.

## Commands

**Run ERT tests for esprit-line:**
```sh
emacs --batch -l lisp/esprit-line/esprit-line.el \
  -l lisp/esprit-line/test/esprit-line-test.el \
  -f ert-run-tests-batch-and-exit
```

**Byte-compile a lisp file (catches syntax errors):**
```sh
emacs --batch -f batch-byte-compile lisp/esprit-line/esprit-line.el
```

**Tangle the literate config** (generates `lit-init.el` from `README.org`):
```
M-x org-babel-tangle (inside Emacs, with README.org open)
```
Note: `init.el` is the active config — `lit-init.el` is generated/reference only.

## Architecture

```
early-init.el     Startup optimization, UI suppression, load-path, package archives
init.el           All package config (straight.el bootstrap → use-package declarations)
README.org        Literate documentation source (not the active config)
lisp/             Custom local packages (on load-path via early-init.el)
themes/           Custom theme files
```

### lisp/ local packages

| Package | Purpose |
|---|---|
| `esprit-themes/` | 6 themes (cerulean/azure/blue × light/dark) built on Rougier's face hierarchy |
| `esprit-line/` | Lightweight modular mode line; segments in `esprit-line-segment-*.el` |
| `esprit-movement.el` | Smart home key: toggle between indent and true line start |
| `swiss-move.el` | Accelerated `s-n`/`s-p` line movement |
| `flymake-follow-mode.el` | Syncs diagnostics buffer highlight to point |

### init.el structure (sections in order)

1. `straight.el` bootstrap
2. `use-package emacs` — global keybindings, ibuffer groups, TRAMP optimizations
3. Completion stack: `vertico` → `consult` → `orderless` → `corfu` → `cape` → `marginalia`
4. Navigation: `avy`, `embark`, `ace-window`, `multiple-cursors`
5. LSP: `eglot` (JS/TS/Ruby/Go/Astro), `eldoc-box`, `flymake`
6. Language modes: all use tree-sitter variants (`*-ts-mode`)
7. Themes: `circadian` (auto light/dark) + `esprit-themes` + `fontaine`
8. Notes: `org`, `denote`, `consult-notes`, `obsidian`
9. Misc: `magit`, `eat` terminal, `tempel`, `undo-fu`

### Key patterns

**Adding a package:**
```elisp
(use-package package-name
  :straight t
  :bind (...)
  :custom (...)
  :config (...))
```

**TRAMP optimization**: `project-current`, `magit-toplevel`, and `vc-git-root` are memoized per-connection in `init.el`. Don't add remote path calls outside those memo caches.

**XDG compliance**: State/cache files go through `use-package-xdg` — use `:xdg-state` and `:xdg-cache` keywords, not hardcoded `~/.emacs.d/` paths.

**Theme face hierarchy** (Rougier-style, used in `esprit-themes`):
`default` > `strong` > `salient` > `popout` > `faded` > `subtle` > `critical`

**esprit-line segments**: Each segment is a separate file following the pattern in `esprit-line-segment-vc.el`. Add to `esprit-line-left-segments` / `esprit-line-right-segments` lists.
