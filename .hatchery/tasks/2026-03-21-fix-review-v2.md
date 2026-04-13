# Task: fix-review-v2

**Status**: complete
**Branch**: hatchery/review-v2
**Created**: 2026-03-21 13:06

## Objective

Discards made during a review (`x` in magit to discard hunks from the working
tree) were not reflected in the published branch. The publish commit always
restored the full original branch state, erasing any discards.

## Context

`review-v2` sets up a worktree in detached HEAD at `old-sha`, soft-resets to
`base` (staging all branch changes), then unstages everything into the working
tree. On publish the old code did `git reset --hard old-sha` immediately,
restoring the original state with no record of what was discarded.

## Summary

**File changed**: `doom.d/packages/review-v2.el`, `lg/review-v2-publish`

**Fix**: Before `git reset --hard old-sha`, stage all remaining changes with
`git add -A`, then capture `git diff --cached old-sha` as a "discard patch".
After resetting, if the patch is non-empty, apply it with `git apply` and
commit as `review(discard): <branch>`. The REVIEW.md commit follows as before.

**Result**:
- No discards → single `review: <branch>` commit (unchanged behaviour)
- With discards → `review(discard): <branch>` commit followed by `review: <branch>` commit

**Pattern**: `make-temp-file` + `unwind-protect` used to write patch to disk
and guarantee cleanup even on error.

**Key insight**: `git diff --cached old-sha` generates a patch in the direction
"old-sha → kept state", so applying it to old-sha removes exactly the discarded
hunks. It handles new files, deletions, and modifications correctly.
