# AGENTS.md

This file captures context for AI agents working in this repository.
It is updated at the end of each task.

## Architecture Overview

The active Emacs configuration is Doom Emacs in `doom.d/`. Package declarations
live in `doom.d/packages.el`; package-specific configuration files in
`doom.d/packages/` are loaded by `doom.d/config.el`.

## Conventions

Review base selection prefers origin defaults, then local main/master/dev.
Without a conventional default, prompt for a branch rather than requiring a
remote. Verify local defaults as refs/heads/<name> to avoid tag collisions.

Declare third-party packages with `package!` and place their configuration in a
same-named file under `doom.d/packages/`. Keep machine-specific settings and
secrets in `doom.d/computer-locals.el` or `auth-source`, not the repository.
Load reusable macros from `doom.d/custom_funcs.el` before package files that
expand them. In Magit transients, place actions that work at detached HEAD in
an unconditional group; groups built around current-branch commands are hidden
when no branch is checked out.

Magit Delta coloring must use ONLY face text properties, including :extend t
on newlines for full-width backgrounds. Neither xterm-color syntax overlays
nor background overlays are acceptable: even one valid overlay per changed
line made a 50k-line review sluggish. Magit rewrites font-lock-face, so normalize
colors to the separate face property. Cache stripped/propertized text, never
live overlays or section objects (`doom.d/magit-delta-cache.el`). Ignore legacy
cached span descriptors; never recreate their overlays. Retain targeted
background-overlay cleanup before refresh to repair already-open buffers.
Test zero coloring overlays on large diffs and repeated SAME-buffer refreshes,
not just bounded accumulation on small fixtures. Batch cache misses into one
Delta call per section and decode only misses. Include file-local synthetic
context metadata and conversion settings in cache identity. Keep Magit's
synchronous refresh/section parsing. Source-context caches must use immutable
HEAD IDs and preserve blank lines (`magit-git-lines` drops them).
The shared cache defaults to 128 MiB payload /512 entries. Both HEAD sources
and rendered versions consume capacity; undersizing the working set causes
FIFO thrashing and full re-decoding. Payload bytes exclude Lisp overhead, so
retain the separate entry cap. See `PROFILING.md` for wall-clock profiling.

Keep package-local helpers distinctly named: Magit's cached worktree reader
is `lg/magit--worktree-path-branches`, separate from the tab-bar's
`lg/worktree--porcelain-list`. Duplicate function names across package files
can silently replace cached readers and reintroduce subprocess calls.
