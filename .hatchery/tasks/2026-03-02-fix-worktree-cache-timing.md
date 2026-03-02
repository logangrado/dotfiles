# Fix: lg/magit--worktree-ref-labels cache timing bug

**Status**: complete
**Branch**: master
**Created**: 2026-03-02

## Objective

Fix worktree branch coloring in `magit-log` not appearing on first buffer open.

## Context

`magit-refresh-buffer-hook` (which called `lg/worktree--refresh-cache`) fires **after** the
first render in `magit-refresh-buffer`. On first open, `lg/worktree--branch-cache` has no
entry for the repo → `(gethash ...)` returns nil → `when-let*` short-circuits → advice
returns `result` unchanged. After pressing `g`, the hook had fired and populated the cache,
so subsequent renders worked correctly.

Root cause traced to `magit-mode.el:1070–1097`: `funcall refresh` at line 1082–1093 runs
before the hook dispatch at line 1097.

## Summary

Replaced the `when-let*`-gated cache lookup in `lg/magit--worktree-ref-labels` with an
imperative lazy-init pattern using a sentinel value `'lg/miss`.

**Key change** (`doom.d/packages/magit.el`, lines 44–73):

- `(gethash top-abs cache 'lg/miss)` distinguishes "key never stored" (sentinel) from "key
  stored but value is `()` " (single-worktree repo where we correctly return `result`
  unchanged).
- On first render: key absent → sentinel detected → `lg/worktree--refresh-cache` called
  inline → cache populated → coloring works immediately.
- On subsequent renders: key present → no extra git subprocess → cache hit as before.
- `magit-refresh-buffer-hook` still fires after each render and re-invokes
  `lg/worktree--refresh-cache`, keeping the cache current when worktrees change.

Single-worktree repos are unaffected: `lg/worktree--refresh-cache` stores `()` for the
current top, `(not wt-branches)` is truthy → returns `result` unchanged.
