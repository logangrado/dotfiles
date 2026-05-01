# Task: fix-org-todo

**Status**: complete
**Branch**: hatchery/fix-org-todo
**Created**: 2026-04-21 15:46

## Objective

`org-entry-put` was being called on markers from the agenda to update `:ORDER:` properties. For items in `20250519152528-todo.org`, something caused `org-entry-put` to corrupt the file-level properties drawer at the top of the file. The result was the first line changing from `:PROPERTIES:` to `:ORDER: 1:PROPERTIES:` — merging the ORDER value with the drawer opening tag on one line. This breaks org-roam's ability to read the `:ID:` property, so the file stops being indexed.

## Context

The two functions that call `org-entry-put` with ORDER:
1. `lg/org-agenda-normalize-orders` — bulk-renumbers all `:ORDER:` properties across agenda items on every `org-agenda-finalize-hook`
2. `lg/org-agenda-swap-order` — swaps ORDER between two adjacent items on J/K keypresses

`20250519152528-todo.org` is an org-roam node whose file-level `:PROPERTIES:` drawer contains `:ID:` (and had gained a stale `:ORDER: 1` from a previous corruption). When an agenda marker for an item in that file resolves to a position before the first heading — e.g. pointing into the file-level drawer — `org-entry-put` internally calls `org-back-to-heading` with `NOERROR=t`, which returns nil, causing it to fall back to file-level property handling. Its in-place replacement then writes `:ORDER: N` onto the same line as `:PROPERTIES:`, producing the corrupted `:ORDER: 1:PROPERTIES:` text.

## Summary

**Fix**: Added `lg/org-entry-put-safe` (in `doom.d/packages/org-config.el`), a thin wrapper that calls `org-before-first-heading-p` via `org-with-point-at` before delegating to `org-entry-put`. If the marker is in the file preamble (before the first heading), the write is silently skipped. Replaced all three ORDER-writing `org-entry-put` calls — one in `lg/org-agenda-normalize-orders` and two in `lg/org-agenda-swap-order` — with `lg/org-entry-put-safe`.

**Key decisions**:
- Chose a helper over inline guards to keep all three call sites clean and consistently protected.
- Used `org-before-first-heading-p` (the standard org predicate) rather than a manual position check.
- Did not add any logging or user-visible warning on skip, because agenda normalization runs silently on every finalize and noise would be disruptive.

**Data cleanup needed (out-of-repo)**: `20250519152528-todo.org` may still have a stale `:ORDER: 1` line in its file-level `:PROPERTIES:` drawer. Remove it manually — the code fix prevents future corruption but does not retroactively clean the existing stale property. After removal, verify org-roam re-indexes the file (`M-x org-roam-node-find` → "todo" should appear).

**Files changed**: `doom.d/packages/org-config.el` (+10 lines, -3 lines)
