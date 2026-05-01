# Task: better-org-formatting

**Status**: complete
**Branch**: hatchery/better-org-formatting
**Created**: 2026-04-14 15:15

## Objective

Improve org-mode formatting: enable mixed-pitch mode, fix company/org-tempo TAB conflict, update todo keywords, and fix todo face colors.

## Context

The user wanted a more polished org-mode experience. Original asks included markdown-style backticks and triple-tick fences, but those were deferred. The scope narrowed to mixed-pitch mode, todo keyword improvements, and fixing a TAB interception bug.

During implementation, several pre-existing bugs surfaced (todo faces all red, org-roam DB query broken, `org-entry-put` corrupting file-level drawers) which were diagnosed and either fixed on this branch or separately on master.

## Summary

### Changes (2 files vs master)

**`doom.d/packages/company.el`**
- New `lg/company-tab-or-org-tempo` function: when company popup is active in org-mode and the line matches an org-tempo trigger (`<s`, `<e`, etc.), TAB aborts company and calls `org-cycle` for template expansion instead of completing.

**`doom.d/packages/org-config.el`**
- **Mixed-pitch mode** enabled via `(org-mode . mixed-pitch-mode)` hook. Tab-bar faces (`tab-bar`, `tab-bar-tab`, `tab-bar-tab-inactive`) pinned to `mixed-pitch-fixed-pitch-faces` so workspaces stay monospace.
- **Todo faces declared**: uncommented `custom-declare-face` block for `+org-todo-active/project/onhold/cancel`, added new `+org-todo-delegated` face. Added orange color for delegated in `custom-set-faces!`.
- **Todo keywords updated** in `(after! org)` at EOF: dropped NEXT, added HOLD (`h!`) and DELG (`g!`). All keywords use `!` for state-change logging.
- **Agenda colorize** updated: `lg/agenda-colorize` maps PROG→active, WAIT/HOLD→onhold, DELG→delegated (was NEXT/PROG→active, WAIT→onhold).

### Key decisions

- **NEXT dropped** in favor of priority + ordering (user preference: "we already have priority and ordering").
- **HOLD vs WAIT**: WAIT = blocked on a specific person/action; HOLD = paused by choice or circumstance.
- **DELG**: delegated to someone else, tracked but not owned.
- **`(after! org)` at EOF**: `setq!` on `org-log-into-drawer` triggers org re-init that resets keywords to Doom defaults. Using plain `setq` inside `(after! org)` at the very end of the file avoids this.
- **Src block indentation deferred**: `org-indent-mode` intentionally skips virtual indentation for src block content. No clean fix exists without patching org-indent internals.
- **Backtick/triple-tick markdown syntax deferred**: not implemented.

### Gotchas for future agents

- **`(after! org)` keyword placement matters**: Doom's org module and `setq!` on certain org variables can reset `org-todo-keywords`. The `(after! org)` block at EOF is the single source of truth — do not add a duplicate earlier in the file.
- **mixed-pitch-mode and tab-bar**: `mixed-pitch-mode` sets the default face to variable-pitch. Any UI element inheriting from `default` (like tab-bar) will switch fonts unless explicitly pinned to `mixed-pitch-fixed-pitch-faces`.
- **org-roam tag format**: org-roam v2 changed tag storage from JSON arrays to plain strings. The `lg/vulpea-project-files` query format must match the DB schema version. If tags stop working after a Doom upgrade, check the raw DB with `(org-roam-db-query [:select * :from tags :limit 5])`.
- **`lg/vulpea-project-update-tag` safety**: this function runs on `org-mode-hook` and strips `:project:` from files where `lg/vulpea-project-p` returns nil. If `org-todo-keywords` isn't initialized when it runs, it can silently strip tags from every file during `org-roam-db-sync`. The `(bound-and-true-p org-not-done-regexp)` guard on master prevents this.
