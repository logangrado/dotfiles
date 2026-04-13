# Task: improve-review

**Status**: complete
**Branch**: hatchery/improve-review
**Created**: 2026-03-30 08:59

## Objective

Our `review` is working well.

However, the spot where it doesn't work so well is for _new_ files.

They show up as "untracked" files, so we don't see a "diff" for them.

Instead, can we create a system where:

- For NEW files, we first add them to our review worktree as _empty_ files. Then, the new lines will show up as additions.

## Summary

**File changed**: `doom.d/packages/review-v2.el` — inserted 6 lines after the `git restore --staged .` block in `lg/review-v2-start`.

**Approach**: After unstaging all changes into the working tree, query the *main repo* (`repo-top`) for files added by the branch (`git diff --name-only --diff-filter=A <base> <old-sha>`), then run `git add --intent-to-add <file>` in the *worktree* for each. This registers an empty blob in the worktree's index, causing `git diff` to show all lines as `+` additions — making new files reviewable alongside modified files.

**Key details**:
- Diff is run against `repo-top` (not `worktree-path`) because the worktree HEAD is now `merge-base` — the branch history only exists in the main repo context.
- `--intent-to-add` touches only the index; the working-tree file is unchanged.
- Empty `new-files` list → loop is a no-op; branches with no new files are unaffected.
- Publish flow unchanged: `git add -A` on publish already handles intent-to-add files correctly.
