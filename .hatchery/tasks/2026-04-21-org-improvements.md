# Task: org-improvements

**Status**: complete
**Branch**: hatchery/org-improvements
**Created**: 2026-04-21 16:43

## Objective

Improve the org-mode setup with three changes:
- When capturing a TODO item, put it in the `weeklies` for the current week
- Track state changes on TODO items
- Add a binding of `+` in the custom todo view to add a todo item

## Context

The existing setup uses a vulpea-based org-roam project tagging system where any roam file containing active TODOs is auto-tagged `:project:` and included in the agenda. TODOs were previously captured to a central `todo` roam node under a `REFILE` heading. Weekly notes live at `~/org/roam/weekly/YYYY-WVV.org` with a structured skeleton (Goals, Log, Carry-over). No state-change logging existed. The custom "Ordered TODOs" agenda (`SPC n a`) had j/k navigation and J/K reordering but no capture shortcut.

## Summary

### Files changed

- `doom.d/packages/org-roam.el`
- `doom.d/packages/org-config.el`

### Changes made

**Capture to weekly file** (`org-roam.el`):
- Added `* TODOs` section to `lg/org-roam-weeklies--new-skeleton` (between Goals and Log).
- Added `lg/org-roam-weeklies--ensure-current-file`: returns the current week's file path, creating the file with the skeleton if it doesn't exist. Used as the file-function in the capture template.
- Simplified `lg/org-roam-weeklies-goto-today` to reuse `lg/org-roam-weeklies--ensure-current-file`.
- Updated `org-capture-templates` "t" template: target changed from `(file+headline my/find-todo-org-roam-file "REFILE")` to `(file+headline lg/org-roam-weeklies--ensure-current-file "TODOs")`.
- The vulpea project-tagging system auto-tags the weekly file as `:project:` once it has active TODOs, so items appear in the agenda automatically — no extra wiring needed.

**State change logging** (`org-config.el`):
- Added `(setq! org-log-into-drawer t)` after `org-todo-keyword-faces` — routes all log entries into a `:LOGBOOK:` drawer rather than the item body.
- Added `!` suffix to all TODO keyword shortcuts (`TODO(t!)`, `NEXT(n!)`, `PROG(p!)`, `WAIT(w!)`, `DONE(d!)`, `KILL(k!)`) — logs a timestamp on every state transition.

**`+` agenda keybinding** (`org-config.el`):
- Added `:n "+" (cmd! (org-capture nil "t"))` to `org-agenda-mode-map` in the `(after! org-agenda ...)` block. Pressing `+` in the Ordered TODOs view fires the TODO capture template.

### Gotchas

- `lg/org-roam-weeklies--ensure-current-file` uses `with-temp-file` for creation, which avoids opening a buffer for the new file — safe to call during capture without side effects.
- The `my/find-todo-org-roam-file` helper (looks up the `todo` roam node) is still defined in the config but no longer used by capture; it could be removed later if not needed elsewhere.
- State logging with `!` on all keywords means even the initial `TODO` state logs when an item is re-set to TODO (e.g. un-completing). This is intentional for full history tracking.
