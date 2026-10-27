# AGENTS.md

This file captures context for AI agents working in this repository.
It is updated at the end of each task.

## Architecture Overview

The active Emacs configuration is Doom Emacs in `doom.d/`. Package declarations
live in `doom.d/packages.el`; package-specific configuration files in
`doom.d/packages/` are loaded by `doom.d/config.el`.

## Conventions

Declare third-party packages with `package!` and place their configuration in a
same-named file under `doom.d/packages/`. Keep machine-specific settings and
secrets in `doom.d/computer-locals.el` or `auth-source`, not the repository.
Load reusable macros from `doom.d/custom_funcs.el` before package files that
expand them. In Magit transients, place actions that work at detached HEAD in
an unconditional group; groups built around current-branch commands are hidden
when no branch is checked out.

Magit Delta syntax colors must use face text properties, not xterm-color
overlays: dense syntax overlays caused review-buffer navigation lag. Magit
rewrites font-lock-face, so normalize colors to the separate face property.
Cache stripped/propertized file text and relative background spans, never
live overlays or section objects (`doom.d/magit-delta-cache.el`). Retain one
background-extension overlay at each changed line's newline; full-line
background overlays mask text-property word-emphasis colors. Background overlays
must evaporate on text deletion, including those restored from older cached
snapshots. Clear only our background overlays before full refreshes, skip
empty spans, and test repeated refreshes in the SAME buffer: erasing text alone
does not remove non-evaporating overlays. Batch cache
misses into one Delta call per section and decode only misses. Include
file-local synthetic-context metadata and conversion settings in cache
identity. Keep Magit's synchronous refresh/section parsing. Source-context
caches must use immutable HEAD IDs and preserve blank lines (`magit-git-lines`
drops them). See `PROFILING.md` for wall-clock profiling.

Keep package-local helpers distinctly named: Magit's cached worktree reader
is `lg/magit--worktree-path-branches`, separate from the tab-bar's
`lg/worktree--porcelain-list`. Duplicate function names across package files
can silently replace cached readers and reintroduce subprocess calls.
