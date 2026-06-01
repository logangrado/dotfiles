# Task: reorder-vterm-buffers

**Status**: complete
**Branch**: hatchery/reorder-vterm-buffers
**Created**: 2026-05-28 11:23

## Objective

We did a lot of work to prevent vterm buffers from being re-ordered on us without us doing it. It is very good and stable.

However, now when I use M-[/] (swap tab left/right), it has no effect on vterm buffers (they get swapped right back).

We should do a few things in this request:
- Make swapping work. When swapping vterm buffers, re-name them so they remain in order after the swap
- Update vterm so when we delete a terminal, it re-indexes so we don't have low empty numbers (e.g. if we have 1,2,3 open, and delete 2, rename 3->2)

## Context

Earlier work made vterm tab order stable by sorting `*v:WS<N>*` buffers by
numeric suffix on every centaur-tabs rebuild (`lg/centaur-tabs-sort-vterm-after-update`
in `doom.d/packages/centaur-tabs.el`). That had two side effects:

1. `M-{` / `M-}` (`centaur-tabs-move-current-tab-to-{left,right}`) reorder
   the tabset list, but the next centaur-tabs rebuild re-sorts vterms back
   by their numeric suffix. The *persistent identity* of a vterm tab is its
   suffix, so only renaming can actually move it.
2. Killing `*v:WS<2>` from `{1,2,3}` leaves `{1,3}`. The next new vterm
   (via `lg/vterm-next-workspace-buffer-name`) fills the gap at 2, which is
   jarring.

Both behaviors share one primitive: rename a workspace's vterms to canonical
`<1..N>` order. That primitive was already inside `lg/vterm-resort-buffers-by-name`
— we just needed to extract it and call it from a swap wrapper and a kill hook.

## Summary

### Files changed

- `doom.d/packages/vterm-reindex-buffers.el` — extract the rename helper,
  add the swap wrappers, the kill hook, and a tiny predicate.
- `doom.d/packages/centaur-tabs.el` — repoint `M-{` / `M-}` at the new
  wrappers.

User confirmed the intended bindings are `M-{` / `M-}` (swap) — the
`M-[/]` shorthand in the original task was for "the swap keys." `M-[` / `M-]`
remain `centaur-tabs-backward/forward` (navigation), unchanged.

### Key design decisions

- **Rename, don't fight the sort.** Rather than disabling the auto-sort
  advice for swap operations, the swap functions rename buffers so the
  numeric order matches the desired visual order. Once names are right,
  the existing auto-sort produces the correct visual outcome — single
  source of truth.
- **Wrappers, not advice.** `lg/vterm-move-tab-{left,right}` are plain
  commands that dispatch on `(major-mode ∈ vterm)` and fall through to the
  raw `centaur-tabs-move-current-tab-to-{left,right}` for non-vterm
  buffers. Easier to reason about than `:around` advice.
- **Kill hook uses `run-at-time 0`.** `kill-buffer-hook` runs *before* the
  buffer is removed from `buffer-list`, so a synchronous renumber would
  include the dying buffer. Scheduling on the next event-loop turn lets us
  observe post-kill state directly. We capture the workspace name and the
  dying buffer at hook time via lexical closure so we don't have to detect
  the kill afterward.
- **Helper accepts `ws` arg.** `lg/vterm--rename-to-canonical` takes
  `ws` as a parameter rather than reading `(lg/vterm--workspace-name)`,
  so the deferred kill-handler renames the *right* workspace even if the
  user has switched perspectives by the time the timer fires.

### Public API added

- `lg/vterm-move-tab-left` / `lg/vterm-move-tab-right` — interactive,
  vterm-aware swap. Bound to `M-{` / `M-}` in
  `doom.d/packages/centaur-tabs.el`.

### Internal helpers added

- `lg/vterm--rename-to-canonical (ordered-bufs ws)` — two-phase
  rename; returns the list of (buf . new-name) actually renamed.
- `lg/vterm--current-is-workspace-vterm-p` — gates the wrappers and the
  kill hook on "current buffer is a workspace vterm".
- `lg/vterm--workspace-vterms-by-index (ws)` — workspace vterms sorted
  by current numeric index.
- `lg/vterm--swap-and-renumber (direction)` — swaps current with neighbor
  in `'left` or `'right` direction, then canonicalizes. No-op at the edges.
- `lg/vterm--reindex-on-kill` — `kill-buffer-hook` entry; deferred via
  `run-at-time`.

### Gotchas / notes for future agents

- **`lg/vterm-index` lives in `centaur-tabs.el`**, not
  `vterm-reindex-buffers.el`. The cross-file dependency works at runtime
  because both are loaded before any keystroke can invoke the swap
  functions. If you ever load these files in isolation (tests, etc.)
  you may need to require centaur-tabs first.
- **`kill-buffer-hook` is a global hook**; the entry function gates itself
  on `lg/vterm--current-is-workspace-vterm-p` so non-vterm kills are
  cheap (just one predicate call).
- **The existing `SPC o r` (`lg/vterm-reindex-buffers`) was *not*
  refactored** — it does visual-order reindex, which is a different
  operation from "canonical by current index." Only
  `lg/vterm-resort-buffers-by-name` was refactored to use the helper.
- **`lg/vterm--swap-and-renumber` uses a vector** for the in-place swap
  rather than `setf`/`nth` so we don't mutate the list returned by the
  sorter (defensive).
- **Why `(or (lg/vterm-index x) 0)` in sorts:** for safety. `lg/vterm-index`
  returns nil for non-vterm buffers, but `lg/vterm--workspace-vterms-by-index`
  has already filtered to workspace vterms only — the `or` is belt-and-
  braces.
