# Task: fix-review-again

**Status**: complete
**Branch**: hatchery/fix-review-again
**Created**: 2026-05-18 15:24

## Objective

Two fixes to the worktree-based review system in `doom.d/packages/review.el`:

1. Restore the prompt that previously appeared when starting a second review
   of a branch — "Prior review found at <sha>. Review since then?" — and
   have it default the diff base to either that SHA or the merge-base based
   on the user's answer.
2. Make file renames render correctly in the review diff (renamed-to file
   was showing up as untracked, hiding the addition).

## Context

The prompt existed in the v1 review system (commit `3d5c239`) and was
dropped in commit `5c04949` ("feat(review): Better review system, use magit
diff directly"), which rewrote review.el to use a dedicated git worktree
instead of a custom diff buffer. The rewrite removed both the storage
helpers (`.last-reviewed` file) and the `y-or-n-p` call, leaving every
review of a branch as "branch vs merge-base" regardless of history.

## Summary

### Changes

Single file modified: `doom.d/packages/review.el`.

1. **Three new helpers** (Helpers section, ~lines 112–131):
   - `lg/review--last-reviewed-file (repo-top branch)`
   - `lg/review--load-last-reviewed (repo-top branch)`
   - `lg/review--save-last-reviewed (repo-top branch sha)`

2. **`lg/review-start`**: renamed the `base` `let*` binding to `merge-base`,
   added a `last-sha` lookup, and computed an effective `base` via
   `y-or-n-p`. Guarded by three checks before prompting (stored SHA exists,
   differs from current HEAD, is still reachable) so stale markers fall
   through silently.

3. **`lg/review-publish`**: persists the branch's post-publish HEAD into the
   marker file, inserted between the `update-ref` step and the cleanup call.
   `lg/review-cancel` is deliberately untouched — only completed publishes
   advance the marker.

4. **Rename handling in `lg/review-start`**: added `--no-renames` to the
   `git diff --name-only --diff-filter=A` call that drives intent-to-add.
   With `diff.renames=true` (the modern default), a rename is reported as
   `R` and the new path is excluded from `--diff-filter=A`, so the
   intent-to-add loop never ran for renamed-to paths and they stayed
   untracked. `--no-renames` makes renames decompose into `A` + `D`, so the
   new path gets intent-to-add'd and shows up in the unstaged diff. The
   deletion side already worked (after `restore --staged .`, the base copy
   of the old name lives in the index and the working tree has nothing,
   yielding a clean unstaged deletion).

### Key decisions / gotchas

- **Marker storage** lives at
  `~/.review-worktrees/<repo>-<hash>/<branch-slug>.last-reviewed` — a
  **sibling** of the worktree directory, not inside it. The worktree is
  removed by `git worktree remove` on every publish/cancel, so a marker
  stored inside it would be wiped on the first publish and never observed
  on subsequent reviews. The sibling location reuses
  `lg/review--repo-dir` (which already keys by repo-hash) and the same slug
  function as `lg/review--worktree-path`, so it stays in lockstep.

- **`base` binding kept its name** so downstream code (`reset --soft base`,
  the `--diff-filter=A` new-file scan, and the `:base base-ref` plist) needs
  no changes — `base` is now whichever of `merge-base` or `last-sha` was
  selected.

- **Save SHA after `update-ref`, not before**: publish can advance the
  branch ref (the discard-commit path), so reading `rev-parse branch` after
  `update-ref` gives the SHA that next review will see as `old-sha`.

- **Verification could not be done in this sandbox** — no Emacs binary
  available. The user should smoke-test in Emacs:
  1. Publish a first review → confirm marker file exists with the branch SHA.
  2. Add a commit → start a second review → confirm prompt appears.
  3. Answer "y" → diff shows only the new commit.
  4. Cancel a review → marker is not updated.
  5. Stale-marker case (e.g. branch rebased): `magit-rev-verify` returns
     nil and we silently fall back to merge-base.

### Commits

- `4a1355a` feat(review): add .last-reviewed marker helpers
- `be897dc` feat(review): prompt 'review since last review?' when prior SHA exists
- `9eae362` feat(review): persist .last-reviewed SHA on publish
- `9b1f2cf` fix(review): pass --no-renames so renamed files show as A+D
