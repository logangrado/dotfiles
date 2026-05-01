# Task: vterm-modeline

**Status**: complete
**Branch**: hatchery/vterm-modeline
**Created**: 2026-05-01 09:11

## Objective

vterm buffers do not have a modeline. Can we update it so they get one? It's hard to tell if we are in insert vs copy vs other modes currently

## Summary

Doom's `:term vterm` module adds `hide-mode-line-mode` to `vterm-mode-hook`,
which hides the modeline in all vterm buffers. Since the `modeline` module
(doom-modeline) is already enabled and shows evil state indicators, buffer
names, and workspace info, the fix is a single `remove-hook` call.

**File changed:** `doom.d/packages/vterm.el` — added `(remove-hook 'vterm-mode-hook #'hide-mode-line-mode)` in the `:config` section.

**What this provides:**
- Evil state indicator (INSERT/NORMAL/VISUAL) with colored badge
- Buffer name (`*v:workspace*`)
- Major mode indicator (VTerm)
- 📋 icon next to evil state badge when vterm-copy-mode is active (uses
  `:filter-return` advice on `doom-modeline-segment--modals` to avoid
  fragile modeline layout redefinition)

**Gotchas:**
- `+vterm/toggle` (`SPC o T`) uses Doom's popup system which may independently
  hide the modeline via popup rules. The user's custom `lg/vterm-toggle-window`
  (`SPC o t`) uses manual `split-window-vertically` and is not affected.
- The existing `lg/vterm-adjust-cursor` hook still provides cursor shape changes
  (bar for insert, box for normal) as a complementary visual cue.
