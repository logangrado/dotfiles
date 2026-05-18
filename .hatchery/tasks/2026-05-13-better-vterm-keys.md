# Task: better-vterm-keys

**Status**: complete
**Branch**: hatchery/better-vterm-keys
**Created**: 2026-05-13 08:16

## Objective

Improve vterm UX in three ways:

- When in normal mode, if I try to navigate, automatically enter copy mode
  (otherwise the cursor keeps being reset).
- When switching back to a vterm buffer and NOT in copy mode, scroll to the
  bottom automatically.
- Fix J/K (fast move 10 lines) which behaved erratically in vterm
  visual-line state, snapping back every few keypresses.

## Context

In Doom Emacs with vterm + evil-mode, pressing motion keys (h/j/k/l/arrows) in
normal state moves Emacs point, but vterm immediately resets point to the
terminal's live cursor on the next render — navigation feels broken. vterm
provides `vterm-copy-mode` to freeze the buffer, but it's manual. The fix is to
auto-toggle copy mode on the right transitions.

## Summary

Most changes are in `doom.d/packages/vterm.el` inside the existing `:config` block
(immediately after the cursor-shape hooks). The J/K fix also touches `doom.d/config.el`.

**Four coordinated pieces:**

1. **Auto-enter `vterm-copy-mode` on basic cursor navigation**
   `pre-command-hook` → `lg/vterm-maybe-enter-copy-mode`. Fires when
   `this-command` is in `lg/vterm-auto-copy-motion-commands` (the six basic
   evil motions: `evil-next-line`, `evil-previous-line`, the visual-line
   variants, `evil-forward-char`, `evil-backward-char`) AND the buffer is
   vterm AND evil-normal-state AND copy-mode is off. Cheap to leave global —
   `derived-mode-p` short-circuits everywhere else.

2. **Auto-exit `vterm-copy-mode` on insert-state entry**
   `evil-insert-state-entry-hook` → `lg/vterm-exit-copy-mode-on-insert`.
   Required because `vterm-copy-mode` makes the buffer read-only — without
   this, pressing `i` after auto-entering copy mode would leave the user
   unable to type.

3. **Scroll to live cursor when re-focusing a vterm buffer**
   `window-selection-change-functions` AND `window-buffer-change-functions`
   both call `lg/vterm-jump-to-bottom-if-live`, which calls
   `vterm-reset-cursor-point` and `recenter -1`. Both hooks are needed: the
   first covers window-focus changes (`evil-window-left` etc.), the second
   covers buffer swaps inside a window (`switch-to-buffer`). Skipped entirely
   when `vterm-copy-mode` is active, so the user's scroll position is
   preserved while they're scrolling intentionally.

4. **Fix J/K (10-line jump) in vterm visual-line state**
   The previous bindings in `config.el` were anonymous lambdas calling
   `(previous-line 10)` / `(next-line 10)` plus `evil-scroll-line-up/down`.
   Plain (non-motion) point movement breaks evil's visual-state region
   tracking, causing a snap-back cycle every few keypresses. Reimplemented
   as proper `evil-define-motion`s (`lg/evil-up-10` / `lg/evil-down-10`)
   that use `evil-next-line`/`evil-previous-line` for the point movement.
   Also added those names to `lg/vterm-auto-copy-motion-commands` and
   broadened `lg/vterm-maybe-enter-copy-mode`'s state guard from
   `evil-normal-state-p` to also fire in visual states.

**Key decisions:**

- Motion scope limited to basic cursor keys (h/j/k/l + arrows) plus the
  10-line J/K motions. Wider motions (`gg`, `G`, `C-u`, `C-d`, `w`, `b`,
  etc.) do NOT auto-enter copy mode. To extend later, add the relevant
  evil commands to `lg/vterm-auto-copy-motion-commands`.
- `pre-command-hook` chosen over remapping each key — keeps the keybindings in
  `vterm-mode-map` untouched, single source of truth (the command list).
- Two separate `window-*-change-functions` rather than `buffer-list-update-hook`
  — the change-functions are more precise (per-window, on actual focus/swap
  events) and don't fire on every buffer-list mutation.

**Gotchas / things to know:**

- `vterm-copy-mode` is a buffer-local minor mode. All checks use
  `(bound-and-true-p vterm-copy-mode)` since the symbol may not be defined
  in non-vterm buffers.
- The 📋 modeline indicator (from existing `lg/vterm-copy-mode-modeline-indicator`
  at `packages/vterm.el:106-112`) will now appear/disappear automatically as the
  user navigates — useful visual feedback that the auto-toggle is working.
- `recenter -1` places the live prompt at the bottom of the window. If the user
  wants it positioned differently after focus return, change that single arg.

**Files changed:**

- `doom.d/packages/vterm.el` — added three blocks (~60 lines) after the
  cursor-sync hooks, before the auto-dim-other-buffers fix; extended the
  auto-copy motion list with the J/K motions and broadened the state guard.
- `doom.d/config.el` — converted the J/K lambdas to named
  `evil-define-motion`s (`lg/evil-up-10` / `lg/evil-down-10`).

Note: `doom.d/packages/magit.el:305-306` carries the same J/K lambdas for
magit-section-mode-map. It wasn't part of this scope and may want the same
treatment in a follow-up.

**Verification (manual, requires running Emacs):**

1. Reload Doom config (`SPC h r R` → `lg/reset-emacs`) or restart Emacs.
2. Open vterm (`SPC o h`), run `ls /usr` for scrollable output.
3. Press `ESC` then `k k k` — cursor moves up, 📋 appears in modeline.
4. Press `i` — copy mode exits, cursor returns to prompt, typing works.
5. With copy mode off, switch away (`SPC b b`) and back — window shows prompt
   at bottom.
6. Enter copy mode, scroll up, switch away and back — position preserved.
7. In vterm, `ESC` then `V` (visual-line state), then `K`/`J` — region
   extends 10 lines per press, no snap-back cycle.
