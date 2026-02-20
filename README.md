# w.el

Thin workspace manager on top of `tab-bar-mode`.

A workspace is a name + project root + reset function. The reset function receives the project root and sets up
windows/buffers in a fresh tab. No buffer tracking, window-state persistence, autosave, or hidden hooks.

## Commands

- `w-new` — register a workspace (does not create a tab)
- `w-go` — switch to a workspace; creates a new tab and calls the reset function if none exists
- `w-delete` — remove a workspace and close its tab
- `w-close` — close workspace tab; workspace stays registered
- `w-mode` — global minor mode; when enabled, closing a workspace tab prompts to kill its project buffers
- `w-edit` — modify a workspace's fields
- `w-current` — return the current workspace plist (or nil)
- `w-visit` — open a buffer/file in its project's workspace; prefills with a misplaced buffer on the current frame

## Persistence with savehist

`w-workspaces` is a plain variable holding a list of plists. Add it to `savehist-additional-variables` to persist
workspace definitions across Emacs sessions:

```elisp
(use-package savehist
  :config
  (add-to-list 'savehist-additional-variables 'w-workspaces)
  (savehist-mode 1))
```

This works because workspace plists contain only strings and symbols, which `print`/`read` round-trip cleanly.

## Installation

Requires Emacs 28.1+. Place `w.el` on your `load-path` and:

```elisp
(require 'w)
```
