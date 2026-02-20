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

`w-mode` is a global minor mode that prompts to kill project buffers when any workspace tab is closed — whether via
`w-close`, `tab-bar-close-tab`, or the tab-bar UI. Uses `tab-bar-tab-pre-close-functions` under the hood.

## Installation

Requires Emacs 28.1+.

```elisp
(use-package w
  :config
  (w-mode 1)
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'w-workspaces)))
```

`w-workspaces` contains only strings and symbols, so it round-trips cleanly through `savehist`.
