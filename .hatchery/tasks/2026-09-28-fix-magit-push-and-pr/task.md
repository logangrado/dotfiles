# Task: fix-magit-push-and-pr

**Status**: complete
**Branch**: hatchery/fix-magit-push-and-pr
**Created**: 2026-09-28 08:42

## Objective

Keep `Push & open PR` available in Magit's push transient when no branch is
checked out, explicitly select the source branch, prompt for an unconfigured
remote, and ask for the target branch with the remote's HEAD as the default.

## Context

The command was inserted after `magit-push-current-to-pushremote`, inside a
Magit transient group guarded by `magit-get-current-branch`. Consequently the
custom suffix disappeared at detached HEAD even though its workflow can push
any local branch. The command also inferred its source branch and selected its
PR target without consistently prompting, making the detached-HEAD workflow
impossible and target selection less explicit.

## Summary

- `lg/magit-push-and-create-pr` now requires a local branch argument. Its
  interactive form prompts with `PR source branch to push`, using
  `magit-read-local-branch` so the local branch at point and then the
  checked-out branch are preferred while remaining usable at detached HEAD.
- `lg/magit--read-push-remote` first uses the selected branch's configured
  push remote (`branch.<name>.pushRemote` / `remote.pushDefault` through
  `magit-get-push-remote`), then its upstream remote. It prompts with
  `magit-read-remote` when neither is configured or the configured value is
  stale, and reports an error when the repository has no remotes. The existing
  `git push -u` records the selected remote as the branch's upstream.
- `lg/magit--read-pr-target-branch` always prompts with fully qualified
  branches from the selected remote. It reads `refs/remotes/<remote>/HEAD`
  directly using `git symbolic-ref --short`, producing a default such as
  `origin/main`; this avoids the version-dependent arity of
  `magit-main-branch`. If remote HEAD is unavailable, it falls back to the
  first existing qualified `main`, `master`, or `dev` branch. The prompt says
  `PR base branch (merge <remote>/<source> into)`, distinguishing the PR base
  from the implicit push destination `<remote>/<source>`.
- The `R` suffix is now inserted after the command `magit-push-other` in
  Magit's unconditional `Push` group. Do not anchor detached-HEAD-capable
  actions in the current-branch group, whose entire group is hidden when HEAD
  is detached. A command-symbol anchor is clearer and less brittle than layout
  coordinates.
- Existing push-error propagation, Forge PR composition, and post-submit
  browser behavior remain unchanged. Only `doom.d/packages/magit.el` needed
  functional changes; `doom.d/packages/forge-config.el` was not modified.
- Validation: `git diff --check` passed, a static scan confirmed balanced
  Lisp parentheses and strings, and the direct lookup in this repository
  resolved `refs/remotes/origin/HEAD` to the qualified candidate
  `origin/master`. The task container does not include Emacs, so runtime smoke
  testing remains: verify `P R` and prompt defaults on an attached branch,
  then detach HEAD and verify `P R` remains visible and can select/push a local
  branch.
