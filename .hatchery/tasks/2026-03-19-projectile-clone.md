# Task: projectile-clone

**Status**: complete
**Branch**: hatchery/projectile-clone
**Created**: 2026-03-19 09:36

## Objective

Add a `spc p c` ("projectile clone") command that prompts for a destination directory and a remote URL, clones the repo there, adds it to projectile, and opens it — analogous to the existing `spc p n` (`lg/create-new-project`).

## Context

The Doom config already has `lg/create-new-project` in `doom.d/packages/projectile.el`. The new command follows the same pattern: `read-directory-name` → create dir if needed → shell-command → `projectile-add-known-project` → `projectile-switch-project-by-name`.

## Summary

- **File changed**: `doom.d/packages/projectile.el`
- Added `lg/clone-project` inside the existing `use-package! projectile :config` block, after `lg/create-new-project`.
- Uses `git clone <url> .` so the user specifies the exact destination path (no repo-name guessing).
- `shell-quote-argument` on the URL prevents shell injection.
- Keybinding `spc p c` via `map!` matches the existing `spc p n` style.
- Reload with `SPC h r r`; verify with `SPC p c`.
