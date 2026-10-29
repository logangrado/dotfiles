# Task: fix-magit-again

**Status**: complete
**Branch**: (none — no-worktree mode)
**Created**: 2026-10-27 08:49

## Objective

Make large Magit reviews navigable and avoid repeated Delta work during
staging, while preserving syntax colors, line backgrounds, and review state.

## Summary

### Remote-free review base selection

The origin-only detector raised an error before showing the base-selection
prompt in repositories without remotes. The approved narrow fix preserves
origin HEAD, then origin/main-master-dev priority; it falls back to local
main/master/dev, verified under refs/heads/ so tags cannot masquerade as
branches. With no conventional default, detection returns nil and the normal
branch completion prompt still opens. Empty input without a default raises
a selection error rather than passing nil into review setup. Explicit branch
choices and empty-input defaults remain unchanged.

Only the two base-selection helpers in `doom.d/packages/review.el` changed;
session/worktree mutation and review lifecycle are untouched. Eight focused
regressions cover origin precedence, local fallback order, tag rejection,
no-default completion, and input handling, including a disposable real Git
repository with no remote. Combined suites pass48/48.
The gitignored `profiles/reload-review-base.el` reloads only these two helpers
without restarting Emacs, resetting sessions, or rerunning review hooks.


### Final decision: no coloring overlays

All Delta syntax colors, emphasis backgrounds, hidden markers, and full-width
line backgrounds now use cached text properties. No syntax or background
coloring overlays are created or restored. Keep normal Magit section,
selection, and review-comment overlays; these were not the problem.

- Use `face`, not `font-lock-face`: Magit overwrites the latter during
  washing and section painting.
- Add lower-precedence background text faces, including newlines and
  `:extend t`, without masking brighter Delta word-emphasis colors.
- `lg/magit-delta--extend-line-backgrounds` now applies text faces ONLY.
  `lg/magit-delta--decode-file` creates text-only snapshots.
  `lg/magit-delta--insert-snapshot` inserts their text and ignores old spans.
- The snapshot struct retains its legacy `spans` slot for compatibility with
  already-live cache entries; new snapshots leave it nil. Old syntax-text-v1
  entries already contain the required text faces, so they can be reused
  without cache invalidation or re-decoding. Never replay their span data.
- Retain widened, targeted background-overlay cleanup at full refresh entry
  to repair old live buffers. Preserve narrowing, readonly state, Magit
  sections, and review comments. Cleanup still happens if refresh then fails.

### What actually went wrong

Three distinct problems were found through live isolation, not one universal
performance issue:

1. Upstream magit-delta uses xterm-color overlays for each contiguous color
   run (often token-sized). Large diffs created thousands. Removing only
   syntax overlays restored navigation; cached computation alone did not
   fix the display-time cost. Syntax text properties removed those overlays.
2. Our custom backgrounds added an overlay per added/removed line. They
   originally lacked evaporation and survived buffer erasure: one live
   review had 32,779 lines and 613,890 background overlays. Removing those
   backgrounds immediately restored navigation. A lifetime fix prevented
   accumulation and cleaned legacy survivors.
3. That lifetime fix was necessary but insufficient. A larger review still
   had 50,897 nonempty, evaporating backgrounds, zero empty backgrounds,
   and zero syntax overlays. Clearing them was instantly snappy; refreshing
   recreated exactly 50,897 and lag returned. With them removed, ~299 other
   Magit/review overlays remained and ALL diffs still rendered properly.
   The backgrounds were redundant: text faces already supplied the visuals.
   The final fix therefore eliminates background creation/restoration too.

The earlier small-case background-removal test did not show a speed change;
that did not justify assuming one overlay per line would scale. Earlier
appearance/bounds tests also failed to assert overlay counts across repeated
refreshes in the SAME buffer. Test both scale and repetition, not merely
bounded growth on tiny fixtures.

Initial visual-fill-column window adjustment was also expensive and disabling
it helped, but no permanent visual-fill-column setting was changed. A visible
Markdown README still had it enabled when background removal alone restored
navigation, isolating the latter issue.

### Caching and correctness contracts

`doom.d/magit-delta-cache.el` caches stripped, propertized per-file text,
never live buffers, markers, overlays, or Magit section objects. Cache misses
are batched into one Delta process per affected diff section; hits skip ANSI
conversion, context stripping, marker hiding, and background scanning.
Magit's synchronous refresh/section parsing remains intact.

- Keys cover augmented input, file-local synthetic-context metadata,
  effective Delta arguments, executable, directory, environment, palette /
  conversion settings, marker visibility, and background colors. Identical
  augmented text can have different original/synthetic boundaries.
- HEAD source data is keyed by immutable commit ID/repository/path and
  shared within a refresh. Working-tree fallback is not persisted.
  Preserve blank source lines with `magit-git-insert` plus `split-string`;
  `magit-git-lines` silently drops them.
- FIFO limits: 512 entries and 128 MiB payload. Count text, serialized
  text-property runs, and any ignored legacy span data still retained;
  Lisp container overhead is additional. Oversized entries are not retained.
- Render caching requires `--no-gitconfig`. Unknown formats fall back to
  full rendering. Failed results are not cached; errors/quits restore raw,
  unaugmented input. Clear the cache after replacing Delta in place.
- Original scrolling and Delta staging changes remain separate commits in
  that order, per user request. Later narrow fixes are separate commits.

### Larger-review staging: capacity, not another rendering rewrite

A subsequent ~150-file review had 2.715M characters /51,592 lines and zero
coloring overlays, but staged slowly. The old 16 MiB cache repeatedly evicted
the working set: all 192 inserted snapshots were decoded again.

| Instrumented stage capture | Old 16 MiB | Warm 64 MiB experiment |
| --- | ---: | ---: |
| Stage command | 5.2669 s | 1.1295 s |
| Delta/context wrapper | 4.5315 s | 0.4640 s |
| Render-cache pass | 3.9063 s | 0.0132 s |
| Payload accounting | 1.7518 s /192 calls | no calls |
| Decode | 1.0663 s /192 calls | no calls |
| Delta execution | 0.7675 s /2 calls | no calls |
| Git | 0.7444 s /41 calls | 0.3911 s /24 calls |
| Prefix source reads | 0.3093 s | 0.0110 s |

Times overlap; do not add rows. These were different stage operations, not
a controlled identical-input speedup guarantee. Nevertheless, cache state
stayed unchanged across the warm capture and recomputation disappeared.
The actual working set was 209 entries (17 sources +192 renders) and
32,999,188 payload bytes (~31.47 MiB), almost twice the old byte budget.
The 256-entry cap was not the binding constraint in this case.

The user approved defaults of **128 MiB and512 entries**, keeping both guards
with more headroom. An entry is a source or a particular rendered version,
not a unique path; one file can consume several entries. The entry cap limits
fixed per-entry Lisp/key/queue overhead and FIFO bookkeeping even if payloads
are tiny. The byte counter is not an exact heap-memory budget.
No eviction-policy change, new metadata cache, payload-sizing optimization,
or Magit-refresh rewrite was needed.

A new regression retains 280 rendered snapshots +24 HEAD-source entries
(>256 entries and >16 MiB payload) through repeated warm render passes with
no extra Delta or decode calls. Another independently exercises byte and entry
eviction. Tests inherit and isolate the real production budget defaults.

The temporary gitignored helper `profiles/magit-stage-benchmark.el` measures
the next normal stage command, reports focused ELP/cache/buffer inventories,
and saves `profiles/stage-benchmark.txt`. It neither stages anything itself
nor clears caches; instrumentation is removed automatically. Two smoke tests
cover normal return and stage errors. Its ELP magit-stage row can show zero;
use the separately timed command header. Timings exclude later redisplay.
The temporary runtime was restored under `/tmp/magit-emacs` after a session
reset; no system packages were installed.

For an already-running Emacs, explicitly set both budget variables; reloading
their `defvar` forms does not override old live values. No cache clearing is
needed, and normal startup initializes the new defaults.

### Git profiling and separate worktree-reader fix

Normal warm command captures measured ~644 ms stage / ~661 ms unstage, with
~430/~439 ms in Git, ~72/~65 ms in the coloring wrapper, and no Delta or
decoding calls. These predate the final background removal and are not an
input-to-display latency guarantee. Initial staging was ~995 ms, but different
workloads prevent treating that comparison as a controlled speedup claim.

Temporary gitignored `profiles/magit-git-profile.el` records one stage/unstage
command's synchronous process arguments, directories, durations, and caller
names, then removes advice automatically. It excludes direct stdin-based
apply and Delta; the supplied traces were whole-file add/reset operations.
Each trace had 26 calls (~578/~586 ms summed process time). Important groups:
diffs ~109/~107 ms, recent commits/decorations ~124/~152 ms, untracked checks
~57/~63 ms, two worktree lists ~39/~36 ms. Tracing adds overhead.

The duplicated worktree call was a genuine custom-config collision:
`magit.el` and `persp-worktree.el` both defined
`lg/worktree--porcelain-list`. Magit's cached consumer now has the unique name
`lg/magit--worktree-path-branches`; tab-bar behavior/cache lifetime remain
unchanged. Six load-order/freshness tests cover it. A pre-existing trailing-
slash mismatch in current-worktree exclusion was explicitly left out of scope.

Do not infer a machine-wide slow-spawn problem from traced Git durations.
A controlled 30-call comparison gave raw ~8.91 ms, Magit args ~7.72 ms,
Magit environment/coding ~7.76 ms, full wrapper ~7.57 ms. Earlier apparent
wrapper penalties did not reproduce. These were batch means, not variability
distributions. No machine/process configuration changes, broad recent-commit
caching, batching, parallelism, or selective-refresh rewrite were approved.

### Validation and operating notes

- 48 tests pass: 34 Delta, six worktree-cache, and eight review-base tests, using extracted
  Emacs 30.1, Delta 0.18.2, Magit 4.3.5, upstream magit-delta and xterm-color.
  Dependencies stayed under `/tmp`; no system packages were installed.
- A 50,000-added-line fixture has ZERO coloring overlays on cold and repeated
  warm renders, with only one Delta/decode call. Real Magit performs 20
  refreshes plus ten stage/unstage round trips in ONE buffer with zero coloring
  overlays and correct index state. Cached old span records are exercised
  through both insertion and a seeded render-cache hit: colors survive,
  no spans replay, no forced re-decoding.
- Visual-attribute equivalence to the original full-buffer ANSI/overlay
  pipeline, marker display, word emphasis, full-width/unterminated backgrounds,
  source/settings invalidation, errors/fallbacks, unrelated overlay preservation,
  and payload limits remain covered. Raw cases include renames, binaries,
  additions/deletions, spaces and Unicode.
- Use `overlays-in` on a widened buffer for diagnostics; `overlay-lists`
  omits empty overlays exactly at point-max.
- Cache helper byte-compiles with warnings treated as errors; changed Lisp
  passes `check-parens`; `git diff --check` passes.
- Current-session gitignored loader:
  `profiles/reload-magit-overlay-lifetime.el`. It reloads only four renderer /
  refresh function definitions from source, preserving review sessions and
  warm caches. Press `g` afterward to clear old backgrounds and redraw.
  Normal startup loads the fix; no `doom sync` needed.

The user verified fast navigation and proper visuals after manually removing
all coloring overlays. Latest code automatically reproduces that state;
sustained responsiveness in the real large review still needs confirmation.
Git queries, section reconstruction, and text-property display still cost
time; no near-instant staging or permanent visual-fill-column fix is claimed.
