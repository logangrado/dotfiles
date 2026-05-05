# Task: fix-merges

**Status**: complete
**Branch**: hatchery/fix-merges
**Created**: 2026-05-05 12:27

## Objective

Inspect the last 10 commits and find instances where code was accidentally reverted.

## Context

Commit `af37303` (2026-04-14, "feat(org): improve formatting — mixed-pitch, todo keywords, company TAB fix") made a set of deliberate, well-documented changes to the org/company configuration. These were inadvertently wiped by a later commit and only partially recovered.

### Reversion chain

| Commit | What happened |
|--------|---------------|
| `af37303` | Intentional: changed org-todo keywords (NEXT → HOLD/DELG), added `lg/company-tab-or-org-tempo`, added `+org-todo-delegated` face, updated agenda colorize, updated mixed-pitch config |
| `57e5740` | Accidental revert: vterm-modeline feature commit that also silently reset `company.el` and `org-config.el` to pre-af37303 state; deleted af37303's task file |
| `03b0edc` | Partial restore: re-applied the todo keyword changes while adding org-modern-indent, but missed face declarations, agenda colorize, mixed-pitch config, and never touched company.el |

## Summary

**Root cause:** `57e5740` was likely authored from a stale working tree or resolved a merge conflict incorrectly, causing it to clobber af37303's changes even though its commit message makes no mention of org/company changes.

**Fix (commit `07a5881`):** Restored all six missing pieces from af37303:

1. **`doom.d/packages/company.el`** — Added `lg/company-tab-or-org-tempo` function and changed `<tab>` binding from `company-complete-selection` to it. This fixes TAB interference between company-mode and org-tempo template expansion (`<s TAB` etc.).

2. **`doom.d/packages/org-config.el` — face declarations** — Uncommented the `with-no-warnings` `custom-declare-face` block and added `+org-todo-delegated` declaration.

3. **`doom.d/packages/org-config.el` — delegated face color** — Added `+org-todo-delegated` (orange) to the `custom-set-faces!` todo faces block.

4. **`doom.d/packages/org-config.el` — agenda colorize** — Updated `lg/agenda-colorize` state-face cond: PROG only (not NEXT+PROG) → active; WAIT+HOLD → onhold; DELG → delegated.

5. **`doom.d/packages/org-config.el` — todo keywords** — Replaced the misplaced NEXT-based `(after! org)` block at line ~350 with a comment marker; appended the correct HOLD/DELG version at EOF as the single source of truth.

6. **`doom.d/packages/org-config.el` — mixed-pitch** — Updated `use-package! mixed-pitch` to pin tab-bar faces to fixed-pitch so entering an org buffer doesn't affect tab appearance.

**Pattern to watch for:** If a future commit changes these files but its commit message doesn't mention org/company, verify the diff manually — this kind of silent clobber can be hard to spot.
