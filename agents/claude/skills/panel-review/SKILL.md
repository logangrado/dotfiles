---
name: panel-review
description: Multi-agent code review of the current branch (`main...HEAD`). Spawns four specialist sub-agents (correctness, test coverage, AGENTS.md adherence, security) in parallel, then aggregates, dedupes, validates, and triages findings as Must / Should / Consider. Use when the user invokes /panel-review, asks for a code review, wants pre-merge assessment, or asks "is this ready to ship".
allowed-tools: Bash, Agent, Read
---

# panel-review

A panel of four specialists reviews the current branch in parallel. You
collect their findings, dedupe, validate against the diff, and present
a triaged action list.

## 1. Get the diff

```bash
git diff main...HEAD
git diff main...HEAD --name-only
git rev-parse --abbrev-ref HEAD
```

If the diff is empty, tell the user "Nothing to review — branch matches
main." and stop.

## 2. Spawn four reviewers in parallel

In a **single message**, call the `Agent` tool four times with
`subagent_type: general-purpose`. Each gets the full diff plus its
focused remit. Tell each one to use the **exact finding format** below.

### Universal rules (give to every reviewer verbatim)

- **Only flag issues in added lines** (lines prefixed with `+` in the
  diff). Issues in context lines (` ` prefix) or removed lines (`-`
  prefix) are out of scope. If the same anti-pattern existed on
  `main`, don't flag it. If the branch reintroduces it in a new file
  or line, **do** flag it.
- **The diff is the scope, not the limit.** Read files for
  surrounding context. Run local code to verify a claim (e.g.,
  `python -c "..."`, `pytest path/to/test`, `grep`). Stay local —
  no cluster tools, remote APIs, or side-effectful commands.
- **Don't fabricate.** Don't invent file paths, function names, or
  code. Quote real excerpts only.
- If you find nothing in your remit, say "No findings." explicitly.

### Correctness reviewer

> Review this diff for real bugs only: off-by-one, wrong condition,
> broken invariant, race condition, null/None handling, API misuse,
> control-flow errors, boundary cases. Do **not** flag style, taste,
> or refactor opportunities — those are other reviewers' jobs.

### Test coverage & quality reviewer

> Read the AGENTS.md rules file for this environment first
> (`~/.claude/CLAUDE.md` or `~/.codex/AGENTS.md`) for the testing rules. Then review
> this diff: for each new function or changed behavior, is there a
> test? Do tests cover the new code paths? Do the tests follow the
> Testing section (behavior not internals, no mocks where a fake fits,
> equality on a literal, no logic in tests, imports at module top)?

### AGENTS.md adherence reviewer

> Read the AGENTS.md rules file for this environment first
> (`~/.claude/CLAUDE.md` or `~/.codex/AGENTS.md`). Then check this diff against every
> section. Likely violations to look for: speculative abstraction,
> imports inside function bodies, reimplementing code that already
> exists, long function bodies with narration comments where a named
> helper would read better, surgical-change violations (drive-by
> reformatting, unrelated cleanup), comments that narrate the *what*
> instead of the *why*.

### Security reviewer

> OWASP-style review of this diff: input validation, secrets in code
> or logs, auth/authz holes, command/SQL injection, unsafe
> deserialization, path traversal, SSRF. Skip findings that don't
> apply to the code's actual deployment context (no "add CSRF" on a
> CLI tool).

### Required finding format (give this to every reviewer verbatim)

```
### Finding

- **file:** path/to/file.py
- **line:** N (or N-M for a range)
- **proposed severity:** blocker | strong | nit
- **proposed confidence:** high | medium | low
- **summary:** one line
- **evidence:**
  ```
  <code excerpt from the diff>
  ```
- **reasoning:** why it's a problem
- **fix:** concrete suggestion
```

**Confidence calibration**: `high` = you can see the issue directly in
the diff and the consequence is concrete. `medium` = likely real but
the impact depends on calling context not fully visible. `low` =
plausible but speculative; depends on code or runtime behavior not
visible in the diff.

## 3. Aggregate

After all four return:

- **Dedupe.** Two findings on the same file+line describing the same
  issue → merge into one. Keep the strongest reasoning; combine fix
  suggestions if they meaningfully differ. Use the highest severity
  and highest confidence among duplicates.
- **Validate.** For each surviving finding:
  - Check that the `evidence` excerpt appears in an added line (`+`
    prefix) of the diff. If it's in a context line, removed line, or
    not in the diff at all, drop the finding silently.
  - For `blocker`-severity findings, additionally `Read` the cited
    file at the cited line to confirm the issue still applies.
  - **Watch for these common false-positive patterns** — downgrade
    confidence or drop the finding if you see them:
    - **"Race condition"**: Is the racing method actually called?
      Does the holding code have an `await` point where competing
      code could run? Synchronous code in an asyncio loop is atomic
      without an `await`. If the method is defined but never called,
      it's a theoretical concern.
    - **"Stale data" / "cache corruption"**: Data freshly fetched
      from upstream and written to cache is **current**, not stale.
      "Invalidation was overwritten by a fetch" ≠ "stale data
      served."
    - **"Storm" / "unbounded" / "resource exhaustion"**: Check for
      locks, semaphores, or rate limits first. A lock-serialized
      retry on failure is standard stale-while-revalidate.
    - **"Downstream error"**: Check how the value is consumed before
      claiming it breaks. An empty dict `{}` may be a no-op if the
      consumer checks for specific keys.
    - **Missing-feature / dead-code claims**: A method defined but
      never called is a design observation, not a bug.
- **Don't inflate severity.** Accuracy matters more than caution.
  If a finding claims "data corruption" but the actual impact is
  "an extra cache refresh," downgrade.
- **Triage** combines proposed severity and confidence:
  - **Must** = `blocker` with `high` or `medium` confidence.
    Correctness bug, security issue, missing test for new public
    behavior, hard AGENTS.md rule violation — with enough evidence
    to act.
  - **Should** = `strong` finding, OR `blocker` downgraded by `low`
    confidence (real if true, but speculative). Style/structure
    issues, missing edge-case test, minor adherence violations.
  - **Consider** = `nit`, OR `strong` with `low` confidence.

## 4. Output

```markdown
# Code review: <branch-name>

Reviewed N changed files (M lines added, K removed). Findings: X Must, Y Should, Z Consider.

## Must (X)

### 1. <one-line title>
- **file:** <path>:<line>
- **issue:** <one short paragraph>
- **fix:** <concrete suggestion>

...

## Should (Y)

...

## Consider (Z)

...
```

Within each tier, sort by file path, then by line number. If nothing
to flag in a tier, omit that tier's header.

If all four reviewers returned "No findings.", say:
> Reviewed N files. No findings. Ship it.
