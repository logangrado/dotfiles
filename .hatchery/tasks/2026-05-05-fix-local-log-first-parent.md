# Task: fix-local-log-first-parent

**Status**: complete
**Branch**: hatchery/fix-local-log-first-parent
**Created**: 2026-05-05 09:01

## Objective

When doing magit log, we have a custom "local log" bound to `l l`. It shows local branches and their remotes.

However, if I try to use `first parent (=p)` with this log view, it does not work. First parent only seems to work with `l a` (log all)

## Summary

**Root cause:** `lg/magit-log-branches` and `lg/magit-log-current-and-main` called `magit-log-setup-buffer` with a hardcoded args list. They never read the live transient state, so any flags the user toggled (e.g. `--first-parent` via `=p`) were silently discarded.

**Fix (`doom.d/packages/magit.el`):**

Both functions were updated to:
1. Accept `&optional args files` parameters.
2. Use `(interactive (list (transient-args 'magit-log) nil))` to capture the transient's current argument state when invoked as a suffix.
3. Compute `merged-args` by deduplicating the union of the hardcoded defaults (`--graph --decorate --ignore-missing`) and the captured `args`, then pass `merged-args` and `files` to `magit-log-setup-buffer`.

**Why built-in commands (`l a`) worked:** `magit-log-all` and friends already used `(interactive (magit-log-read-revs …))` to read transient args. The custom functions bypassed that mechanism entirely.

**Pattern for future log commands:** Any custom function registered as a `magit-log` transient suffix must read `(transient-args 'magit-log)` in its `interactive` form and forward those args to `magit-log-setup-buffer`.
