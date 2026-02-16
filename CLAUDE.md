# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`w.el` is a single-file Emacs Lisp package — a thin workspace manager built on top of `tab-bar-mode`. A workspace is
a plist with three keys: `:name`, `:project-root`, and `:reset-function`. The reset function receives the project root
and sets up windows/buffers in a fresh tab. There is no buffer tracking, window-state persistence, or autosave.

## Development

This is a standard Emacs Lisp package (no build tool, Makefile, or test suite). Requires Emacs 28.1+.

**Lint:** `emacs -Q --batch -l package-lint.el -f package-lint-batch-and-exit w.el`

**Byte-compile check:** `emacs -Q --batch --eval '(byte-compile-file "w.el")'`

**Load for manual testing:** `emacs -Q -l w.el`

## Architecture

Code lives in `w.el` (core) and `w-embark.el` (Embark integration). Key structure:

- **State** — `w-workspaces` (list of plists) is the sole data store. Each tab carries a `w-workspace` alist entry
  linking it back to a workspace name.
- **Internal helpers** (`w--` prefix) — find workspaces/tabs, read user input via completing-read, get/set the
  workspace name on a tab. `w--find-tab` searches across all frames, returning `(TAB . FRAME)`.
- **Commands** — `w-go` (switch/create, crosses frames), `w-new` (register), `w-delete` (remove), `w-edit` (modify
  fields), `w-current` (query).
- **Hooks** — `w-after-reset-hook` (new tab created), `w-after-switch-hook` (switched to existing tab). Both receive
  the workspace plist.
- **Embark** (`w-embark.el`) — `w-embark-visit` opens a buffer/file in its matching workspace. If no workspace exists,
  one is auto-created from `project-current` with default settings.

## Conventions

- Follow GNU Emacs Lisp packaging conventions and `package-lint` rules.
- Internal symbols use the `w--` prefix; public API uses `w-`.
- Interactive commands that operate on a workspace default to the current workspace when sensible (see `w-edit`).
