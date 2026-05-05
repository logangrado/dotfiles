# Task: better-worktrees

**Status**: complete
**Branch**: hatchery/better-worktrees
**Created**: 2026-05-04 09:06

## Objective

Now that I use hatchery a lot, I am working with worktrees alot! This leads to confusion though because:

- centaur tabs: It is showing all buffers from a project, not just the current worktree
- ibuffer: the sections don't work well with worktrees. It does have separate sections for worktrees, but they are not labeled well.
  - we should label them <project> (default tree) and <project>:<tree> when in a worktree
- Can we also put a visual indicator somewhere when editing files to know what worktree we are in? Perhaps the modeline?

## Summary

### Key decisions

- **Worktree detection via `.git` file**: In a git worktree, `.git` is a regular file containing `gitdir: <repo>/.git/worktrees/<name>`. We parse this to extract the worktree name and derive the main repo name. Results are cached per project root.

- **Display name convention**: Main tree shows as `<repo>`, worktrees as `<repo>:<worktree>`. This is used consistently across centaur-tabs groups, ibuffer sections, and the modeline.

- **Load order**: Utility file named `00-worktree.el` so it loads before other packages that depend on it (packages are globbed alphabetically from `~/.doom.d/packages/*.el`).

- **Modeline placement**: Worktree indicator is a custom doom-modeline segment placed immediately before the `vcs` segment on the right side, using `doom-modeline-warning` face.

### Files changed

- `doom.d/packages/00-worktree.el` — **new** — core worktree detection utilities (`lg/git-worktree-name`, `lg/git-main-repo-name`, `lg/project-display-name`) plus doom-modeline worktree segment
- `doom.d/packages/centaur-tabs.el` — updated `lg/centaur-tabs-buffer-groups` to use `lg/project-display-name` for both vterm and default tab groups
- `doom.d/packages/ibuffer-projectile.el` — added `lg/ibuffer-projectile-set-worktree-groups` that temporarily overrides `projectile-project-name` with worktree-aware naming during group setup

### Gotchas

- The `doom-modeline-def-modeline 'main` call redefines the entire main modeline segment list. If upstream Doom changes the default segments, this definition may need updating to match.
- `ibuffer-projectile-set-filter-groups` calls `projectile-project-name` internally, which we override via `cl-letf`. If ibuffer-projectile changes its internals, the override may need adjustment.
- The worktree cache is cleared on `projectile-after-switch-project-hook`. If buffers from multiple worktrees coexist without project switches, the cache stays valid since it's keyed by project root path.
