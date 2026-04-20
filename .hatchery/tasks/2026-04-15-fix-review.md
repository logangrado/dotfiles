# Task: fix-review

**Status**: complete
**Branch**: hatchery/fix-review
**Created**: 2026-04-15 11:33

## Objective

We broke `review.el` back in 5c04949 I think. We get an error:

let*: Symbol's function definition is void: lg/review--read-branch


## Summary

**Root cause:** Commit 5c04949 (`feat(review): Better review system, use magit diff directly`) was a large refactor that deleted `lg/review--read-branch`, `lg/review--read-base-ref`, `lg/review--base-ref`, and the `lg/review--last-branch` variable — but left the calls to the first two in `lg/review-start` (lines 125–126) intact. Incomplete migration.

**Fix:** Restored the four missing definitions (one `defvar`, three `defun`) into the Helpers section of `doom.d/packages/review.el`, immediately before the `Start` section. These are self-contained interactive-prompt helpers with no dependency on the old diff-renderer code that was removed.

**File changed:** `doom.d/packages/review.el`

**Pattern:** When doing large refactors that remove functions, grep for call sites before committing. The definitions in `lg/review--read-branch` / `lg/review--read-base-ref` / `lg/review--base-ref` are exactly what the new worktree-based `lg/review-start` still needs for branch and base-ref selection.
