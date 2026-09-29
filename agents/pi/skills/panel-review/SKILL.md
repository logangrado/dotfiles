---
name: panel-review
description: Run a Pi-native multi-agent review of a supplied diff or current branch. Uses three fresh built-in reviewers for correctness, test coverage and quality, and cleanliness and readability, plus conditional risk specialists, then validates and triages their findings. Use when the user asks for panel review, pre-merge review, or whether a change is ready to ship.
---

# Panel Review

This is a parent orchestration skill. Reuse the built-in `reviewer` execution envelope; specialize each child through its task prompt. Do not create persistent agents for temporary review perspectives.

## 1. Establish the review contract

Honor an explicit user-supplied target. Otherwise review the current branch against the repository's default branch plus the current working-tree delta. Resolve the default branch from the remote HEAD when available, then fall back to an existing `main` or `master`; ask rather than guessing if it remains ambiguous.

Before launch, record:

- repository, cwd, base ref, HEAD, and working-tree state;
- the approved goal, plan, or task statement the change must satisfy;
- applicable global and project `AGENTS.md` instructions;
- changed files and the exact diff under review.

Capture the diff once. For a non-trivial diff, write it to a uniquely named temporary file outside the repository and give every reviewer its absolute path. Do not ask reviewers to infer a committed range: the built-in reviewer can inspect files but does not have general Git or shell access.

If the diff is empty, report that there is nothing to review and stop. If it is too large for a useful review, partition it by coherent source seam rather than truncating it silently.

## 2. Select reviewers

Every non-trivial code review uses these three fresh reviewers:

### Correctness

This is the primary review. Determine whether the implementation fully accomplishes the approved goal while preserving existing contracts and behavior outside scope. Inspect logic, invariants, boundaries, failure paths, API use, call sites, and reachable regressions. Flag missing portions of the approved plan. Do not spend the review on style or test aesthetics.

### Tests

Review both coverage and test quality:

- every changed behavior has meaningful success, failure, boundary, and regression coverage;
- regression tests would fail without the fix they protect;
- tests exercise public behavior rather than private implementation details;
- doubles follow real implementation → fake → stub → monkeypatch → mock preference;
- monkeypatching is limited to true process boundaries when practical;
- assertions compare complete expected objects where practical instead of checking fields piecemeal;
- repeated setup uses clear fixtures or builders, while one-off data remains local;
- validation evidence actually proves the changed behavior.

The reviewer cannot run shell commands. It must distinguish inspected evidence from commands the parent should run.

### Cleanliness and readability

Review whether the implementation is easy to understand and no more complex than necessary. Inspect function cohesion and size, intention-revealing names, distinct phases that should be private helpers, comments that narrate mechanics instead of explaining constraints, duplication, speculative abstraction, and unrelated cleanup. A multi-line comment explaining a section of a function is strong evidence that the section should become a named helper. Do not request abstraction for one-off code merely for symmetry.

Add a separate fresh specialist only when the changed code creates that risk:

- security;
- concurrency;
- performance;
- public API compatibility;
- migration or data integrity.

Do not spend a specialist slot on a generic checklist with no relevant attack surface or runtime path.

## 3. Launch one review wave

First confirm executable agents with `subagent({ action: "list", capabilities: true })`. Then make exactly one top-level asynchronous `workflowScript` call containing the three core `reviewer` tasks and any selected specialists. Every task must include the repository/cwd, exact target, approved goal or plan, absolute diff artifact, changed-file list, applicable instructions, review-only authority, and output contract.

Use this shape, replacing placeholders with concrete context and adding only justified specialist tasks:

```javascript
const common = "Repository: <absolute repo>. Target: <base/head and worktree state>. Approved goal/plan: <contract>. Diff artifact: <absolute temporary path>. Changed files: <list>. Apply the inherited global and project instructions. Review only; do not modify project/source files. Inspect relevant source for context. Report only issues introduced or made reachable by this target. Every finding needs severity P0/P1/P2, path and line, evidence, consequence, and the smallest safe fix. Say exactly 'No issues found.' when nothing qualifies. End with Merge verdict: BLOCK, OK, or OK with notes.";
const tasks = [
  { key: "correctness", label: "Review correctness and regressions", agent: "reviewer", task: common + "\n\nReview only goal fidelity, correctness, invariants, boundary and failure behavior, call sites, and reachable regressions.", output: false },
  { key: "tests", label: "Review test coverage and quality", agent: "reviewer", task: common + "\n\nReview only behavioral coverage and test quality. Enforce the inherited Testing rules, including regression proof, real/fake/stub preference, minimal monkeypatching and mocking, complete-object assertions, and restrained fixture or builder reuse.", output: false },
  { key: "readability", label: "Review cleanliness and readability", agent: "reviewer", task: common + "\n\nReview only cleanliness and readability. Enforce the inherited Simplicity, Surgical Changes, and Code Structure rules, especially cohesive bounded functions and extracting named helpers for separately explained phases.", output: false }
];
// Example only when the diff has a real security surface:
// tasks.push({ key: "security", label: "Review security", agent: "reviewer", task: common + "\n\nReview only deployment-relevant security risks in the changed behavior.", output: false });
const reviews = await runs.all(tasks);
return reviews.map(({ key, output }) => ({ key, output }));
```

Launch with `async: true` and `context: "fresh"`. Return control when no safe independent parent work remains; ordinary async completion will wake the session.

## 4. Validate and disposition

When the wave completes:

1. Deduplicate findings that describe the same defect and location.
2. Check every finding against the supplied diff, current source, approved goal, and applicable instructions.
3. Reject findings not introduced or made reachable by the target.
4. Classify each as valid, stale, invalid, speculative, or out of scope.
5. Keep severity proportional to demonstrated impact.
6. Run the narrowest useful reproduction for claimed blockers when practical.
7. Send accepted fixes to the worker, then re-run focused validation.
8. Re-review only the changed blast radius, and at most once unless the user requested a longer loop.
9. Remove the temporary diff artifact after synthesis.

The parent owns disposition. Reviewer consensus is evidence, not authority.

## 5. Report

Map validated severities to:

- **Must** — valid P0 blockers.
- **Should** — valid P1 issues worth fixing before release.
- **Consider** — valid P2 notes or explicitly deferred improvements.

For each retained finding, report `path:line`, issue, evidence, impact, and smallest fix. Briefly list rejected findings only when explaining a consequential disagreement. End with one of:

- `Verdict: BLOCK`
- `Verdict: READY WITH NOTES`
- `Verdict: READY`

If no finding survives validation, say plainly that the reviewed target has no actionable findings.
