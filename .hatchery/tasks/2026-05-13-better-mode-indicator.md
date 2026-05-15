# Task: better-mode-indicator

**Status**: complete
**Branch**: hatchery/better-mode-indicator
**Created**: 2026-05-13 08:15

## Objective

Knowing if I am in Normal vs Insert mode is a bit difficult. I only have the indicator at the bottom, and the cursor shape (which doesn't even change in vterm correctly).

Can we make the mode more obvious? Either:

- Different buffer background for normal vs insert
- colorize the entire modeline by normal/insert?
- other thoughts?

## Summary

Added a new self-contained file `doom.d/packages/evil-modeline.el` that
tints the active modeline's background with a muted color keyed off the
current evil state (green normal, cyan insert, magenta visual, red replace,
orange emacs). The file is auto-loaded by `config.el:171`'s
`packages/*.el` glob — no other file needed touching.

### Key decisions

- **Modeline tint, not buffer background.** Less intrusive than recoloring
  the editing area, and the modeline is already in peripheral view.
- **Muted blends, not saturated.** `(doom-blend (doom-color 'green)
  (doom-color 'bg) 0.28)` etc., so the cue is obvious but not loud, and
  auto-adapts to any doom theme.
- **Per-buffer remap via `face-remap-add-relative`.** Cookie stored in a
  buffer-local var, removed before each new remap. This lets different
  splits show different states simultaneously and never leaks across
  buffers.
- **Only `mode-line` is remapped, not `mode-line-inactive`.** So the tint
  always identifies the *active* buffer's state; unfocused windows stay
  neutral.
- **Colors recomputed on every state change.** Theme switching is free —
  the next state transition picks up the new palette.

### Gotcha worth knowing

Emacs's `header-line` face inherits from `mode-line` by default. Remapping
`mode-line` `:background` therefore leaks into anything that uses
`header-line` — `lsp-headerline-breadcrumb`, magit's log header, etc. The
fix (committed as `0ff2e05`) is to simultaneously remap `header-line`'s
background back to the theme's original `mode-line` bg, captured with
`(face-attribute 'mode-line :background nil t)` which reads the global
face spec and ignores buffer-local remaps. If a future agent adds more
faces that happen to inherit from `mode-line`, the same trick applies.

### Files changed

- **new:** `doom.d/packages/evil-modeline.el` — entire feature (~55 lines)

### Tuning knob

If any state's tint feels off, bump or trim its blend weight by 0.02–0.04
in `lg/evil-modeline--color-for-state`. The current weights (0.28 / 0.30
/ 0.32) work well on `doom-one`.

### Possible follow-ups (not in scope)

- Tint fringes (left/right gutters) with the same state colors to wrap
  the focused window on three sides. Floated and deferred — fringe remap
  is per-buffer (not per-window), but combined with the existing modeline
  tint + `auto-dim-other-buffers` it would still work.
- `mode-line-inactive` could carry a very subtle (~0.08 weight) tint if
  per-buffer state should be visible across all windows at once.
