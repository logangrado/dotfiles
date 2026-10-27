# Task: fix-magit-again

**Status**: complete
**Branch**: (none — no-worktree mode)
**Created**: 2026-10-27 08:49

## Objective

Diagnose slow Magit review navigation and stage/unstage refreshes; retain
Delta syntax highlighting without dense syntax overlays or repeated work.

## Summary

### Diagnosis and rationale

- Initial visual-fill-column window adjustment dominated redisplay.
  Temporarily disabling it helped; restarting Emacs made navigation usable,
  but later review sessions still lagged. No permanent visual-fill-column
  configuration change was made.
- Initial staging ELP: ~995 ms total, ~946 ms refresh, ~50 ms actual staging,
  ~429 ms Delta/context wrapper including ~179 ms context fetching.
  Parent/child timings overlap. Review-comment overlays were negligible.
- ANSI output caching, then parsed-span caching, removed repeated Delta and
  decoding work. They did not solve navigation: recreating dense syntax
  overlays still imposed native redisplay and allocation costs.
- Direct user isolation established the cause: removing only background
  overlays did NOT help; removing only xterm-color overlays made navigation
  fast/snappy. An earlier combined mode-toggle test was inconclusive, so
  prefer this direct evidence over assumptions about mode state.
- CPU profiles showed substantial redisplay/GC; CPU sampling cannot account
  for all synchronous Git waits. Separate captures varied in workload
  (39 versus 93 file parses); a temporary ~632 ms average versus the initial
  ~995 ms is not a stable end-to-end speedup claim.

### Final architecture and decisions

Preserve Magit's synchronous refresh/section parsing and Delta's syntax
coloring. Upstream batches files per staged/unstaged section; parallel or
async section rendering is unnecessary for the isolated overlay bottleneck.

- `doom.d/packages/magit.el` converts xterm ANSI directly to text properties,
  normalizing syntax colors to `face`, NOT `font-lock-face` (Magit rewrites
  the latter during washing and section painting). Normal rendering creates
  ZERO xterm-color syntax overlays. Syntax faces and hidden-marker display
  properties are copied with cached text.
- Preserve one background-extension overlay per added/removed line, including
  colors and full-window extension. Compatibility adjustment: its range is
  the newline, not the whole line; lower-precedence text faces fill base
  backgrounds elsewhere. Full-line overlays would override text-property
  emphasis backgrounds regardless of low priority, flattening Delta's brighter
  word highlights. This adjustment was discussed during implementation.
- `doom.d/magit-delta-cache.el` retains stripped/propertized file text plus
  relative background-overlay offsets/property lists, never live overlays,
  buffers, markers, or Magit section objects. Cache hits copy text and recreate
  only the few background overlays at current offsets.
- Batch changed files into one Delta process per affected diff section.
  Only misses undergo ANSI parsing, context stripping, marker hiding, and
  background scanning. Unknown input/output boundaries fall back to a full
  render; failed decoders/processes do not cache that result, and errors/quits
  restore unaugmented input.
- Cache identity includes augmented file content, file-local augmentation
  metadata, effective Delta arguments, resolved executable, directory,
  environment, ANSI palette/render settings, source font-lock mode, marker
  visibility, and line colors. Metadata matters: the same augmented text can
  have different synthetic/original boundaries. The `syntax-text-v1` namespace
  invalidates older snapshots containing syntax-overlay spans.
- HEAD context reads use immutable commit-ID/repository/path keys; staged
  and unstaged sections share refresh-local lookups. Working-tree fallback
  is never persisted. Preserve blank lines using `magit-git-insert` plus
  `split-string`: `magit-git-lines` silently discards them.
- Shared FIFO limits: 256 entries / 16 MiB payload. Count string bytes,
  serialized text-property runs, and background span properties/offsets;
  Lisp object overhead is additional. Oversized entries are not retained.
  Render caching requires `--no-gitconfig` to avoid untracked configuration.
  `M-x lg/magit-delta-clear-cache` also handles replacing Delta in place.
- `PROFILING.md` documents measurements, diagnostic recipes, and limits;
  `AGENTS.md` records text-property and background-precedence gotchas.

### Commit boundaries

Per user request, history is organized as TWO implementation commits: first
an independently functional scrolling fix (syntax text faces, without the
persistent cache), then Delta staging/cache optimization. The standalone
intermediate renderer passed real Magit stage/unstage and word-emphasis/
background tests. The final code is unchanged by this history reorganization;
all regression tests and the combined decision record accompany the second
commit. Backup refs retain the prior histories. Unrelated working files were
not staged or changed.

### Validation and measurements

- All 25 ERT tests in `doom.d/tests/magit-delta-cache-test.el` pass with
  Emacs 30.1, Delta 0.18.2, Magit 4.3.5, upstream magit-delta, and xterm-color.
  Dependencies were extracted under `/tmp`, not installed into the repo.
- Real Git/Magit tests check syntax faces and zero syntax overlays after
  refresh, section painting/highlighting, fontification requests,
  collapse/expand, staging and unstaging. Index correctness and background
  overlay bounds are checked. Unchanged files are neither recolored nor
  decoded; undoing staging reuses the original cached text.
- Cold/warm colors match the old full-buffer ANSI/overlay pipeline at each
  character using merged face attributes, not identical implementation
  objects. Tests cover hidden markers, relocated files, word-emphasis
  backgrounds, newline extension, palette/font-lock behavior, blank context
  lines, errors/fallbacks, cache identity, and memory bounds. Raw Delta cases
  also cover additions/deletions, renames, binary files, spaces, and Unicode.
- Same-input synthetic benchmark: 50 files, ~6k diff lines, five warm runs.
  Parsed syntax-overlay baseline: 23,950 syntax overlays + 100 backgrounds,
  ~153 ms/wrapper run, 11 GCs across five runs. Text-face implementation:
  ZERO syntax overlays + 100 backgrounds, ~18 ms/wrapper run, 2 GCs.
  These are local headless wrapper timings, NOT full staging or interactive
  navigation measurements. Earlier ANSI-only-to-parsed-span benchmark was
  ~429 ms versus ~142 ms, but that intermediate representation still lagged.
- Cache helper byte-compiles with warnings treated as errors; changed Lisp
  passes `check-parens`; `git diff --check` passes.

### Remaining limits and user verification

Restart Emacs (no `doom sync` needed), open review to warm caches, then test
navigation and one-hunk stage/unstage. Check xterm-color overlay count stays
zero. User confirmed after completely restarting Emacs that scrolling is
significantly smoother. This verifies the scrolling fix.

Subsequent focused real-session ELP capture:

| Operation | Total | Refresh | Git calls/time | Delta wrapper | Snapshot insertion |
| --- | --- | --- | --- | --- | --- |
| Stage | 719 ms | 686 ms | 27 / 414 ms | 142 ms | 91 / 23 ms |
| Unstage | 645 ms | 599 ms | 30 / 458 ms | 67 ms | 89 / 22 ms |

Stage had one Delta invocation (~36 ms) and one decoded file (~9 ms);
unstage had neither, confirming a fully cached unstage. The stage capture
was therefore not an entirely warm-cache result despite the requested
warm-up. A repeated unchanged round trip then confirmed fully warm behavior:

| Operation | Total | Refresh | Git calls/time | Delta wrapper | Snapshot insertion |
| --- | --- | --- | --- | --- | --- |
| Stage | 644 ms | 603 ms | 26 / 430 ms | 72 ms | 87 / 22 ms |
| Unstage | 661 ms | 588 ms | 27 / 439 ms | 65 ms | 87 / 21 ms |

Neither repeated operation invoked Delta or decoded a file. There is no
observed recurring cache miss in that round trip. Git accounts for ~67% of
each command's instrumented wall-clock time and is the largest measured
component. These are ELP command timings, not input-to-display latency.
Further refresh optimization should log actual Git commands and call sites
before changing sections or adding caches. No additional optimization beyond
the scrolling fix has been approved.

Git subprocesses, section refresh, source augmentation, text-property copying,
and native redisplay still have costs. Use focused ELP entry points rather
than instrumenting thousands of tiny helpers. No near-instant staging or
permanent visual-fill-column fix is claimed.
