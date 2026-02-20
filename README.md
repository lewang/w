# w.el

Thin workspace manager on top of `tab-bar-mode`.

A workspace is a name + project root + populate function. The populate function receives the project root and sets up
windows/buffers in a fresh tab. No buffer tracking, window-state persistence, or autosave.

## Commands

- `w-new` — register a workspace (does not create a tab)
- `w-go` — switch to a workspace; creates a new tab and calls the populate function if none exists
- `w-delete` — remove a workspace and close its tab
- `w-close` — close a workspace's tab (workspace stays registered for later `w-go`)
- `w-edit` — modify a workspace's fields
- `w-current` — return the current workspace plist (or nil)
- `w-visit` — open a buffer/file in its project's workspace; prefills with a misplaced buffer on the current frame

## w-mode

`w-mode` is a global minor mode that cleans up project buffers when a workspace tab is closed — by any means
(`w-close`, `w-delete`, `tab-bar-close-tab`, or clicking the tab-bar close button).

Without `w-mode`, closing a tab leaves the workspace's buffers open. With `w-mode`, you get a confirmation prompt to
kill them. This is implemented via `tab-bar-tab-pre-close-functions`, which is why a minor mode is needed: it gives you
a clean way to install and remove the hook.

```elisp
(w-mode 1)
```

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
(w-mode 1)
```
