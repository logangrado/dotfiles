# Task: fix-vterm-jk

**Status**: complete
**Branch**: hatchery/fix-vterm-jk
**Created**: 2026-05-19 11:13

## Objective

The vterm auto-copy-mode feature (commit `87102ec`) makes `j`/`k` in vterm
normal-state automatically enable `vterm-copy-mode` so motion isn't
clobbered by vterm's live cursor reset. That works well for genuine
scrolling, but breaks a common evil-escape habit:

1. User is in vterm, insert mode.
2. Presses `jk` → `evil-escape` exits to normal mode.
3. Out of habit, presses `jk` again "to confirm".
4. The second `j` enters copy-mode + moves point down; `k` moves it back
   up. The buffer is now frozen in copy-mode and stops updating.

Goal: detect rapid `jk` / `kj` and treat it as a no-op, while leaving
genuine scrolling (single `j`, `jjj`, `k`-pause-`j`, `J`/`K`, arrows)
untouched.

## Context

All changes are in `doom.d/packages/vterm.el`, in the
`lg/vterm-maybe-enter-copy-mode` pre-command-hook region added in
`87102ec`. The fix mirrors `evil-escape`'s own primitive
(`read-event` with a small timeout) so the first `j` never moves point
when `k` follows quickly — no post-hoc undo, no visible flicker.

## Summary

**Final design.** When `j` is about to trigger an auto-entry into
`vterm-copy-mode`, `lg/vterm-maybe-enter-copy-mode` calls
`read-event` with `lg/vterm-auto-copy-jk-window` (0.1 s, matching
`evil-escape-delay`) as the timeout. If `k` arrives in that window,
`this-command` is set to `ignore`, copy-mode is not entered, and
neither key moves point. Any other event is pushed back via
`unread-command-events` and copy-mode is entered as before. The peek
is one-directional — only `j` waits for `k` — because `kj` isn't an
`evil-escape` habit and adding a peek to `k` would penalise every
legitimate scroll-up. Single `j` carries a 100 ms peek delay; single
`k` is instant; subsequent presses inside copy-mode skip the peek
entirely.

**Files changed.**

- `doom.d/packages/vterm.el` — added `lg/vterm-auto-copy-jk-window`,
  `lg/vterm--peek-opposite-jk`, the peek branch inside
  `lg/vterm-maybe-enter-copy-mode`, and `evil-collection-vterm-next-line`
  to `lg/vterm-auto-copy-motion-commands`.

**Key gotchas for future agents.**

- **evil-collection rebinds `j` in vterm normal-state.** In a normal
  Emacs buffer `j` in evil-normal-state runs `evil-next-line` (or
  `evil-next-visual-line` if motion-state-map is rebound). In a vterm
  buffer with `evil-collection-vterm` loaded, `j` runs
  `evil-collection-vterm-next-line` — a wrapper that clamps motion to
  the prompt area. Anything keyed off `this-command` in vterm must list
  this symbol explicitly. `k` is NOT rebound (still
  `evil-previous-visual-line`), so the asymmetry is invisible until
  half of a `jk` test silently no-ops.
- **`pre-command-hook` traces must run BEFORE the gate condition.**
  Several debugging passes only logged when the hook's
  `(memq this-command motions)` test passed, so the missing-from-the-list
  `j` press never appeared in `*Messages*` and the bug looked like a
  timing issue. The clean diagnostic is a log line right at function
  entry, regardless of gates.
- **`evil-escape-p` returns t in normal-state when `[escape]` is
  rebound.** This config has
  `(define-key evil-normal-state-map [escape] 'keyboard-quit)`, which
  flips `evil-escape-p`'s normal-state branch on. evil-escape would in
  principle fire on `jk` in normal too, but its
  `(equal (this-command-keys) (evil-escape--first-key))` check
  fails because `this-command-keys` is a vector and `--first-key`
  returns a string — so in practice evil-escape no-ops here. Worth
  knowing if it ever does start interfering: don't assume evil-escape is
  inert in normal state.
- **vterm-copy-mode is not tied to evil state.** Neither
  `vterm-copy-mode`, doom's vterm module, nor evil-collection adds a
  hook that switches evil state when copy-mode toggles. Earlier
  debugging worried about this; it's not a thing.

**User-tunable knob.** `lg/vterm-auto-copy-jk-window` (default 0.1 s,
matching `evil-escape-delay`) controls the peek timeout. Tighten it
to make single `j` more responsive at the cost of missing slower
habitual `jk` presses; widen it to catch slower typists.
