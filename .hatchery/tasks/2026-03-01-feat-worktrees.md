# Task: feat-worktrees — Magit Worktree Awareness

**Status**: complete
**Branch**: master
**Created**: 2026-03-01

## Objective

Update `doom.d/packages/magit.el` so that:

- `magit-status` shows a Worktrees section (when >1 worktree exists) listing each worktree's path and checked-out branch.
- `magit-log` highlights local branches that are currently checked out in *another* worktree using a distinct face (yellow + bold + underline).

## Context

The dotfiles use Doom Emacs with a custom Magit config in `doom.d/packages/magit.el`. Magit already ships `magit-insert-worktrees` (in `magit-worktree.el`) and `magit-list-worktrees` (in `magit-git.el`) — no third-party packages needed. The log coloring required a `:filter-return` advice on `magit-format-ref-labels` since that function assembles the entire propertized decoration string with no built-in worktree hook point.

## Summary

### Changes made (`doom.d/packages/magit.el`)

**1. Worktrees status section** (line 85–89)
Hooked the built-in `magit-insert-worktrees` into `magit-status-sections-hook` after `magit-insert-status-headers`. The section is suppressed automatically by Magit when only one worktree exists — no custom guard needed.

**2. `magit-branch-worktree` face** (line 16)
Added to the existing `custom-set-faces!` block. Uses yellow + bold + underline, visually distinct from:
- `magit-branch-local`: blue, bold
- `magit-branch-current`: blue, bold, underlined
- `magit-branch-remote`: green, bold
- `magit-branch-remote-head`: green, bold, underlined

**3. Log decoration advice** (lines 24–73)
- `lg/worktree--branch-cache`: hash table keyed by absolute repo toplevel → list of short branch names checked out in non-current worktrees.
- `lg/worktree--refresh-cache`: rebuilds the cache on every `magit-refresh-buffer-hook` call. Filters out `nil` entries (detached HEAD worktrees) and skips the current worktree itself.
- `lg/magit--worktree-ref-labels`: `:filter-return` advice on `magit-format-ref-labels`. For each cached branch name, scans the propertized result string and re-applies `font-lock-face 'magit-branch-worktree` only when: (a) the existing face is `magit-branch-local`, and (b) the match is at a word boundary (start-of-string or preceded/followed by a space). This prevents false matches on remote branch labels or partial substring matches within longer branch names.

### Key decisions

- **Cache on refresh, not on every log line**: `magit-list-worktrees` shells out to git; caching per-repo avoids repeated subprocess calls on every commit row render.
- **Boundary check over `\b`**: Magit's propertized strings can contain non-word chars; an explicit space/boundary check is more reliable than Emacs `\b` in this context.
- **`when-let` guards throughout**: Handles nil `magit-toplevel` (non-git buffers), nil cache entry (first render before refresh), and nil branch in detached-HEAD worktrees.

### Verification steps

1. Open a repo with 2+ worktrees → `M-x magit-status` → Worktrees section appears after headers.
2. `M-x magit-log-all` → branches checked out in another worktree appear yellow+underline.
3. Press `g` to refresh → cache rebuilds, display updates.
4. Single-worktree repo → Worktrees section absent; log unaffected.
