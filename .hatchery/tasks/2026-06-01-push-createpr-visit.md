# Task: push-createpr-visit

**Status**: complete
**Branch**: hatchery/push-createpr-visit
**Created**: 2026-06-01 09:31

## Objective

I have a common workflow in magit:

Push branch (oftentimes, push _other branch), create PR from that branch to origin/main or origin/master, and then visit the PR.

Can we make this a single workflow?

## Context

User invokes three magit/forge actions in a row roughly every time a
feature branch is ready: `magit-push` → `forge-create-pullreq` → manual
browser-visit of the new PR. Three keystrokes / transients for one
conceptual operation. This task collapses them into a single command
bound under the existing `magit-push` transient (`P R`, mnemonic "PR").

## Summary

### Command

`lg/magit-push-and-create-pr`, bound as `R` inside the `magit-push`
transient (so `P R` invokes it). Defined in
`doom.d/packages/magit.el` (~line 340).

Flow:

1. **Source branch** — `lg/magit--push-target-branch`: branch at point
   when in `magit-log-mode` / `magit-refs-mode` *and* the ref is a
   local branch (filtered via `magit-local-branch-p`), else falls
   through to `magit-get-current-branch`. Remote-tracking refs at
   point are intentionally ignored so the workflow doesn't try to push
   `origin/foo:origin/foo`. Detached HEAD → `user-error`.
2. **Remote** — `lg/magit--read-push-remote`: silent for a single
   remote, prompts via `magit-read-remote` for multiple, errors on zero.
3. **Target** — `lg/magit--default-pr-target-branch`: tries
   `(oref repo default-branch)` on `(forge-get-repository :tracked)`,
   else first existing of `<remote>/main` → `master` → `dev`, else
   prompts via `magit-read-remote-branch`. Both forge calls are
   `ignore-errors`-wrapped against API/version drift.
4. **Push** — `magit-run-git "push" "-u" <remote> <branch>:<branch>`,
   synchronous (matches `lg/magit-checkout-detached`'s style). Errors
   from the git subprocess propagate normally and abort the rest of
   the flow before forge is invoked.
5. **PR creation** — sets `lg/forge--pending-pr-browse` then calls
   `(forge-create-pullreq branch target)`, which opens the post buffer
   for title/body composition.
6. **Browse after submit** — handled by the companion hook below.

### Forge-side companion

In `doom.d/packages/forge-config.el`:

- `lg/forge--pending-pr-browse` — one-shot flag.
- `lg/forge--maybe-browse-new-pr` on `forge-post-submit-callback-hook`
  (depth 90, runs after forge's own callbacks). Reads `html_url` from
  the API response `value` (the parsed JSON body GitHub returns from
  `POST /repos/:owner/:repo/pulls`), `browse-url`s it inside
  `condition-case` (so a missing browser surfaces the URL via
  `message` instead of crashing), and clears the flag *unconditionally*
  — so a failed PR creation (e.g. 422 "PR already exists") doesn't
  leave the flag set and steal the next unrelated comment submission.
- `lg/forge--clear-pending-on-cancel` on `forge-post-mode-hook` — adds
  a buffer-local `kill-buffer-hook` that clears the flag if the post
  buffer is killed without submitting (i.e. user hit `C-c C-k`).

### Key decisions

- **Sync push, not async.** Matches the existing `lg/magit-checkout-detached`
  pattern. Async + sentinel was considered but rejected for code
  complexity: magit's `magit-run-git-async` attaches its own sentinel
  and overriding it loses magit's process-status display. Practical
  pushes are sub-second; if auth hangs, the user can `C-g`.
- **Browse via `browse-url` on `html_url`, not `forge-browse-pullreq`.**
  Forge's `forge-browse-pullreq` requires the PR to be in forge's
  local DB, which only happens after `forge--pull-topic` finishes
  asynchronously. The API response itself already contains the URL,
  so we don't have to wait.
- **One-shot flag, not advice.** `forge-post-submit-callback-hook` is
  shared across all post submissions (issues, comments, PRs). The
  flag scopes browsing to the PR-creation path specifically. Two
  cleanup paths (success-clear in the hook, cancel-clear via
  kill-buffer-hook) prevent the flag from leaking into unrelated
  submissions.
- **File split.** Magit-side helpers + command + transient suffix in
  `magit.el`; forge-side flag + hooks in `forge-config.el`. Crossing
  reference (`magit.el` sets a defvar from `forge-config.el`) is
  defended by `(require 'forge)` in the interactive command and a
  forward-declaring `(defvar lg/forge--pending-pr-browse)` in
  `magit.el` to silence byte-compile warnings.

### Files changed

- `doom.d/packages/magit.el` — added the "Push & create PR" section
  with 4 helpers and the command, plus one `transient-append-suffix`
  in the existing transient-suffix block.
- `doom.d/packages/forge-config.el` — added the flag, callback hook,
  and cancel-cleanup hook inside the existing `(use-package! forge)`.

### Non-obvious traps (discovered during live testing)

- **`source` must be qualified as `<remote>/<branch>`**, not the bare
  local branch. Forge's interactive `forge-create-pullreq--read-args`
  completes source over `magit-list-remote-branch-names`, so its
  internal call to `magit-split-branch-name` expects a remote prefix.
  An unqualified branch returns `("." . branch)` from that split, and
  at submit time forge looks up `(magit-get "remote" "." "url")` →
  nil → "Cannot determine forge repository. No url configured for
  ‘.’". `source` is computed as `(format "%s/%s" remote branch)`.
- **`magit-run-git` does not signal on git failure** — it shows the
  error in the magit-process buffer and returns the non-zero exit
  code. Without raising, a failed push silently let the workflow
  proceed to open a PR buffer for a never-pushed branch. The push is
  wrapped in `(let ((magit-process-raise-error t)) ...)`.
- **GitLab uses `web_url`, GitHub uses `html_url`.** The auto-browse
  hook reads `(or (alist-get 'html_url value) (alist-get 'web_url
  value))` so it works on both forges.

### Gotchas / future-agent notes

- **Forge API version**: `forge-get-repository` is called with the
  keyword `:tracked`. Older forge versions used a symbol arg
  (`'tracked`). Both calls are wrapped in `ignore-errors`, so a
  signature mismatch silently falls through to the static fallback
  list — but if all forge calls suddenly become no-ops, this is the
  first place to look.
- **`forge-post-submit-callback-hook` arity**: documented upstream as
  `(value &optional headers status req)`. Our function uses
  `(value &rest _)` so any future signature change won't break it.
- **`html_url` is GitHub-specific**: works for GitLab too (forge's
  GitLab backend also returns `html_url`/`web_url`-compatible keys in
  the `value` alist), but if/when a non-forge-provider host is added,
  re-check the response shape.
- **No automated tests** in this Doom config; smoke-testing is the
  established convention. Verification steps are listed in the plan
  file at `/home/hatchery/.claude/plans/the-task-file-is-wobbly-crystal.md`.
- **Doom reload**: just `M-x doom/reload` (or restart Emacs) — no
  `doom sync` needed, since no new packages were added.
