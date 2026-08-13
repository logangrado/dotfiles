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

## General findings that generalize beyond magit

- **Subprocess-spawn fixed overhead is real and dominant**: every observed
  `git` subcommand cost ~0.015–0.045s almost regardless of what it computed.
  The lever for speedup is reducing subprocess *count*, not per-call speed.
  If `elp` shows N calls to the same underlying operation, batching into one
  call is almost always the highest-leverage fix available.
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
