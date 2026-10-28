# Profiling Emacs/Doom config for real slowness

A data-driven approach for "this feels slow" complaints (magit, but the
method generalizes to anything in this config). Discipline: **measure, find
what's slow, fix, re-measure.** Never fix based on a guess — every fix in
this doc's case study was re-measured, and two of the three "obvious" fixes
turned out wrong or incomplete on first attempt.

## Why not just use the CPU profiler?

Emacs's built-in sampling profiler (`profiler-start`/`profiler-stop`) is
`ITIMER_PROF`-based — it only samples while Emacs is actively burning CPU.
Most slowness in this config (magit, git-backed anything) is Emacs blocked
in a synchronous subprocess call (`call-process`/`process-file`), asleep in
`waitpid`. That time is **invisible to a CPU sampler** — you'll get noisy,
inconclusive profiles that don't point at the real cost.

Use **`elp`** instead: it wraps each instrumented function and times real
wall-clock elapsed time via `float-time`, regardless of what the call was
blocked on. This is what actually finds subprocess-bound bottlenecks.

## Tool 1: `elp` — per-function wall-clock timing

Instruments every function in a package prefix (e.g. `"magit-"`, `"lg/"`)
and reports call count / total time / average time per function.

**Caveat**: `elp`'s output is flat, not hierarchical. A parent function's
total time includes all descendant time, so you can't sum rows to explain
the overall total — only genuine "leaf" functions (nothing else they call is
also instrumented) give a trustworthy, non-overlapping cost. When a
function's own cost is ambiguous, cross-check with Tool 2 below.

### Quick one-shot (paste, trigger the slow thing, results print immediately)

```elisp
(progn
  (elp-restore-all)
  (elp-instrument-package "magit-")
  (elp-instrument-package "lg/")
  (let ((default-directory (magit-toplevel)))
    (benchmark-run 1 (magit-status default-directory)))
  (elp-results)
  (elp-restore-all))
```

Swap the `benchmark-run` body for whatever you're profiling. Use this form
when you're happy calling the target function directly (non-interactive,
deterministic entry point).

### When you need the *real* entry point (keybinding, transient, etc.)

Sometimes calling the function directly isn't representative — you want to
know what happens when you actually press the key. Split instrumentation
from result-reading into two eval-regions:

```elisp
(require 'elp)
(elp-instrument-package "magit-")
(elp-instrument-package "lg/")
```

Then trigger the slow thing exactly the way you normally would (keybinding,
`SPC g l`, whatever it is) — don't call a function name directly. Once
it's fully rendered, eval:

```elisp
(sit-for 1)
(elp-results)
(elp-restore-all)
```

(Bump `sit-for` if the buffer is still rendering when results print — you'll
see suspiciously low totals if you read results too early.)

## Tool 2: per-call subprocess logger

For exact, non-ambiguous attribution of *which* call site triggered *which*
git subprocess — the thing `elp`'s flat table can't answer alone. Advises
the actual git-invocation function directly and logs `(duration . args)` per
call.

```elisp
(defvar lg/profile--git-log nil)

(defun lg/profile--record-git-call (orig &rest args)
  (let ((start (float-time)))
    (prog1 (apply orig args)
      (push (cons (- (float-time) start) args) lg/profile--git-log))))

(setq lg/profile--git-log nil)
(advice-add 'magit-process-git :around #'lg/profile--record-git-call)
(let ((default-directory (magit-toplevel)))
  (benchmark-run 1 (magit-status default-directory)))
(advice-remove 'magit-process-git #'lg/profile--record-git-call)

(with-current-buffer (get-buffer-create "*git-call-log*")
  (erase-buffer)
  (dolist (c (reverse lg/profile--git-log))
    (insert (format "%.4f  %S\n" (car c) (cdr c))))
  (pop-to-buffer (current-buffer)))
```

Output looks like:

```
0.0466  ((t nil) ("rev-parse" "--show-toplevel"))
0.0203  ((t nil) ("rev-parse" "--show-toplevel"))
0.0186  (nil ("update-index" "--refresh"))
```

Each line is exactly the args passed to `magit-process-git` for one
subprocess call, so you can see precisely which git subcommands ran, how
many times, and in what order — this is what caught two wrong-hook and
missing-cache bugs during the magit optimization work (see
`.hatchery`/git log for `fix-slow-magit`) that `elp` alone couldn't localize.

Swap `magit-process-git` for a different function if the thing you're
chasing shells out elsewhere.

## Combined, zero-manual-copy version

Runs both instruments against a real entry point, writes both results
straight to files (no switching buffers / manual copy-paste), and cleans up
after itself even if the target errors:

```elisp
(let ((lg/profile-git-log nil)
      (lg/profile-advice
       (lambda (orig &rest args)
         (let ((start (float-time)))
           (prog1 (apply orig args)
             (push (cons (- (float-time) start) args) lg/profile-git-log))))))
  (advice-add 'magit-process-git :around lg/profile-advice)
  (require 'elp)
  (elp-instrument-package "magit-")
  (elp-instrument-package "lg/")
  (unwind-protect
      (progn
        (lg/magit-log-branches)      ; <-- swap for whatever you're profiling
        (sit-for 2))                 ; bump if the buffer isn't done rendering
    (elp-results)
    (with-current-buffer "*elp results*"
      (write-region (point-min) (point-max)
                     (expand-file-name "profiles/elp-out.txt" "~/.dotfiles/")))
    (elp-restore-all)
    (advice-remove 'magit-process-git lg/profile-advice)
    (with-temp-buffer
      (dolist (entry (nreverse lg/profile-git-log))
        (insert (format "%.4f\t%S\n" (car entry) (cdr entry))))
      (write-region (point-min) (point-max)
                     (expand-file-name "profiles/git-out.txt" "~/.dotfiles/")))
    (message "Wrote %d git calls + elp results to profiles/elp-out.txt / profiles/git-out.txt"
             (length lg/profile-git-log))))
```

`profiles/` is gitignored scratch space — treat these files as disposable,
overwrite on every run.

## Profiling stage/unstage refreshes with Delta

Start with `magit-` and `lg/` using the interactive recipe above, reset
with `elp-reset-all`, then stage **one hunk** and wait for redisplay before
collecting results. Do not benchmark opening status as a substitute for
the actual operation. Parent and child totals overlap. For confirmation,
remove broad instrumentation with `elp-restore-all`, then time only
`magit-stage`, `magit-refresh`,
`magit-process-git`, `lg/magit-delta-call-delta-with-context`,
`lg/magit-delta--decode-file`, `lg/magit-delta--insert-snapshot`, and
`lg/magit-delta--run`. Timing thousands of tiny helpers adds overhead.

The October 2026 review-buffer trace measured ~995 ms staging, of which
~946 ms was refresh and only ~50 ms the actual staging Git command.
Delta plus our syntax-context wrapper took ~429 ms, including ~179 ms
fetching preceding source lines. Review comment overlays were negligible.

`doom.d/magit-delta-cache.el` retains stripped per-file text with syntax,
background, and display properties. No coloring overlays are created or
restored: user isolation found both dense syntax overlays and, on larger
reviews, background overlays made navigation slow. Use the separate `face`
text property because Magit rewrites `font-lock-face` during section painting.
Cache keys cover augmented input, file-local
synthetic-context stripping metadata, effective Delta arguments, executable
path, repository directory, process environment, and conversion settings
(font-lock face property, ANSI palette/rendering, marker visibility, line
background colors). Changed files are batched into one process per diff
section. Only misses run ANSI conversion, context stripping, marker hiding,
and background scanning. Magit still regenerates its sections. Insertion
copies propertized text only. Lower-precedence background faces include
newlines and :extend t, filling to the window edge without masking Delta's
word-emphasis colors. The snapshot's old span field remains for compatibility,
but is ignored by insertion; warm old cache entries already have the needed
text properties and do not require a flush. Thus:

- The first refresh is cold; measure subsequent stage/unstage operations.
- `lg/magit-delta--run` counts actual Delta invocations. An unchanged warm
  refresh should need none; staging a hunk generally needs at most one
  per affected staged/unstaged section.
- `lg/magit-delta--decode-file` should run only for changed or uncached files.
  An unchanged warm refresh should need neither Delta nor decoding. Snapshot
  installation only copies the cached text. Both `xterm-color` and
  `lg-magit-delta-line-bg` overlay counts must stay ZERO after highlighting,
  refresh, staging, and unstaging, even with old cached span descriptors.
- Immutable HEAD source contents are also cached, and shared across the
  staged/unstaged rendering passes. Working-tree fallback is not persisted.
- Run `M-x lg/magit-delta-clear-cache` after replacing Delta in place or
  to compare cold versus warm behavior. Rendering-option changes naturally
  miss the cache. Removing `--no-gitconfig` bypasses rendered-output caching.
- The shared FIFO cache is capped at 256 entries and 16 MiB of payload
  (text, serialized text-property runs, and any ignored legacy span data still
  retained; Lisp object overhead is additional). Oversized entries are not
  retained. New snapshots leave the legacy span field empty.
- ANSI-only caching removed repeated Delta computation but left parsing,
  allocation, and native redisplay costs. A local identical-input warm
  wrapper benchmark (50 files, ~6k diff lines, five runs) measured ~429 ms/run
  for ANSI-only caching versus ~142 ms/run for parsed snapshots. This is not
  an end-to-end staging benchmark or a user-machine latency guarantee.
- Parsed spans still recreated thousands of syntax overlays and did not
  resolve navigation lag. A later same-input benchmark (50 files, ~6k lines,
  five runs) compared parsed syntax overlays with syntax text properties:
  ~153 ms versus ~18 ms per warm wrapper run, with syntax overlays dropping
  from 23,950 to zero and background overlays staying at 100. GC collections
  across five runs dropped from 11 to 2. This is also a local wrapper
  benchmark, not an interactive redisplay or full-stage measurement.

Background lifetime matters independently of syntax-overlay density. A later
real review had 32,779 lines but 613,890 background overlays: the old
non-evaporating backgrounds survived text erasure and accumulated on refresh.
Removing only those backgrounds immediately restored navigation. A lifetime
fix prevented accumulation, but a still larger review then had 50,897 valid,
nonempty, evaporating backgrounds and lag returned after every refresh.
Clearing them left ~299 unrelated Magit/review overlays, restored navigation,
and ALL diffs still rendered correctly. Density alone was enough to hurt.

The final renderer therefore creates NO syntax or background overlays and
ignores old cached spans. Full refresh still removes legacy owned background
overlays. Check counts on large diffs AND repeated same-buffer operations;
bounded counts alone are not a performance guarantee. To include empty
overlays at point-max, use `overlays-in` over the widened buffer rather than
`overlay-lists`, which omits that boundary case.

Regression tests: `emacs -Q --batch -l doom.d/tests/magit-delta-cache-test.el
-f ert-run-tests-batch-and-exit`. Unit tests need no Doom setup; real-process
tests additionally need Delta (`DELTA=/path/to/delta` is supported), and
the full integration test needs Magit, magit-delta, and xterm-color on
`load-path`. It stages/unstages a real hunk in a disposable Git repository
and checks unchanged files are neither sent back to Delta nor decoded again.
Cold/warm parsed output is compared to the full-buffer ANSI pipeline at every
character, including resolved color attributes and hidden markers. Real
Magit tests also check syntax faces through painting, highlighting,
fontification requests, collapse/expand, 20 repeated refreshes and 10
stage/unstage round trips in one live buffer with ZERO coloring overlays.
A 50,000-added-line cold/warm fixture checks that scale does not reintroduce
overlays; a seeded legacy cache verifies old spans are ignored without losing
colors or forcing re-decoding. Word emphasis and full-width backgrounds have
separate regression coverage.

## General findings that generalize beyond magit

- **Many subprocesses can dominate a Git-bound operation**, but command
  durations are not pure spawn overhead. Earlier cases measured ~15–45 ms
  per query; a later controlled comparison found ~8 ms through both raw Git
  and Magit. Measure arguments/environment, actual Git work, tracing and
  allocation effects before blaming process launch. Duplicate calls are
  worthwhile to remove, but do not assume batching is trivial or sufficient.
- **A sibling function/section can silently absorb work you thought you
  removed.** Don't judge one function's removability from its `elp` cost in
  isolation — check what happens to overall total time after removing it,
  not just whether that one row disappeared. (Case: removing one magit
  status section only moved its `git log` cost onto a sibling section that
  shared the same underlying computation; net savings were much smaller
  than the isolated cost suggested.)
- **Hooks that only add, don't replace** (`add-hook`,
  `magit-add-section-hook`) can leave both old and new behavior running
  simultaneously if you forget the explicit `remove-hook` — making things
  *slower*, not faster. Always re-profile after a hook-based fix to confirm
  the old path actually stopped running.
- **Advising the specific low-level function that shells out is usually
  less invasive than patching the higher-level caller.** The magit-log fix
  batched `magit-rev-verify` (a small, generic, easily-understood contract:
  ref name in, resolved hash or nil out) rather than touching
  `magit-format-ref-labels` (a large, more complex rendering function) —
  smaller surface area, easier to reason about correctness, same win.
- **Per-refresh caches**: key the cache lifetime to whichever hook is
  guaranteed to run *last* in a refresh cycle (e.g.
  `magit-refresh-buffer-hook`), not a timer — clear it there, populate it
  lazily on first use. This pattern was reused three times (worktree
  porcelain list, worktree branch cache, tag-verify table) across this
  work.
- **`lexical-binding: t` gotcha**: a shared cache `defvar` must textually
  precede any `setq` of it elsewhere in the file, or the byte-compiler may
  treat an earlier `setq` as creating a lexical binding instead of setting
  the special/global variable — silently breaking the cache with no error.

## Case study numbers

`magit-status` on a real multi-worktree repo: 0.971s → ~0.66s (~32%) via
worktree-listing dedup + dropping low-value sections.

`magit-log` (`lg/magit-log-branches`, 1036-commit view): 2.36s → 0.46s
(~80%) via batching 130 individual `rev-parse --verify refs/tags/*` calls
(1.78s, ~75% of total) into one `git for-each-ref refs/tags` call.
