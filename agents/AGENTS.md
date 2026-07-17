# AGENTS.md

## 1. Think Before Coding

**Don't assume. Surface tradeoffs. Push back when warranted.**

- State assumptions explicitly. If uncertain, ask.
- If multiple interpretations exist, present them — don't pick silently.
- If something is unclear, name what's confusing and stop.
- **Ask before big moves**: multi-file refactors, destructive git ops, dependency changes, anything irreversible.
- If I propose something dumb, say so with a one-line reason. Don't just comply.

## 2. Simplicity First

**Minimum code that solves the problem. Nothing speculative.**

- No features beyond what was asked.
- No abstractions for single-use code. Three repeated lines beat a premature helper.
- No defensive code for impossible states. Validate at system boundaries; trust internal callers.
- Comments are rare — only when *why* is non-obvious. Never narrate the *what*.

## 3. Surgical Changes

**Touch only what you must. Clean up only your own mess.**

- Don't "improve" adjacent code, reformat, or refactor unprompted.
- Match existing style even if you'd do it differently.
- If you notice unrelated dead code, mention it — don't delete it.
- Every changed line should trace back to what I asked for.

## 4. Read Before Editing

**Don't edit a function based on `grep` output. Read the file.**

- Especially for non-trivial code where local invariants matter.
- Skim nearby code to learn what conventions apply before you write anything new.

## 5. Verify Before Claiming Done

**Don't claim a change works without running it.**

- A diff that looks right is not a diff that works.
- If you can't run it (no test infra, can't reach a service), say "I haven't verified this" explicitly.
- For UI: open the page. For backend: run the function or the test. For CLI: invoke it.

## 6. On Failure, Investigate

**Tests, lint, and hook failures are signals — not obstacles.**

- Don't suppress (`# noqa`, `--no-verify`, retry with different flags) without understanding why.
- The fix is usually in the code under test, not the test or the harness.

## 7. Be Terse

**Short answers. No preamble. I read diffs.**

- No recap of what you just did unless I ask.
- If you picked between two reasonable approaches, say so in one line.
- Match response length to the task. A simple question gets a direct answer, not headers and sections.

## 8. Commit Messages

**Use Conventional Commits. The commit body IS the changelog.**

- Format: `<type>(<scope>): <description>`. Body explains *why*, not what — write for the future maintainer reading `git log` six months out.
- **`fix:` is for things broken on `main`** — never for code you introduced in this branch.
- Don't amend published commits. Don't force-push without asking, never to `main` / `master`.

## 9. Testing

**Tests are required when the repo has a test framework. Write them first. Test behavior, never internals.**

- TDD: for bug fixes, write a failing test that reproduces the bug, then fix it. For features, write the test for the public contract first.
- Test names describe behavior: `test_returns_404_for_unknown_variant`, not `test_handle_unknown`. Reads as a sentence in `pytest -v`.
- **Never test internals.** If renaming a private helper breaks a test, the test was wrong.
- **Assert with one equality on a literal**, not N field-by-field asserts — `pytest`'s diff shows the whole picture. For dynamic fields (timestamps, IDs), pop them, assert shape, then equality on the rest.
- Doubles, most-to-least preferred: **real impl** → **fake** (working in-memory) via DI → **stub** (canned response) via DI → **monkeypatched** fake/stub (clock, env, stdio only) → **mock** (`assert_called_with`). Mocks couple tests to implementation; avoid.

## 10. Code Structure

**Reuse before reinventing. Extract helpers to name steps, not comment them.**

- **Imports at module top.** Inside a function body only to guard the import (optional dep, circular import).
- **Reuse before reimplementing.** Search for an existing function before writing a new one. If a private helper elsewhere does what you need, promote it to a public utility — don't paste-copy or reinvent.
- **Extract helpers to name steps, not to anticipate reuse.** If a block needs a multi-line comment to explain what it does, the block wants to be a named function with a docstring. A good function name documents better than a comment ever can.
