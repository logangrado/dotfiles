# Delegated implementation policy

For non-trivial implementation after the user approves a plan, act as the orchestrator rather than the primary writer.

- Keep discussion, planning, scope decisions, finding disposition, final acceptance, and the user conversation in the parent session.
- Give one fresh-context `worker` a cold-start-complete contract: approved outcome, relevant files and decisions, edit boundary, success criteria, validation, expected report, and stop conditions. Tell it to follow the inherited global and project instructions.
- Require the worker to inspect its final diff against the correctness, testing, and code-structure rules before handoff; run focused validation; and report any unverified behavior or intentional guideline exception. Do not require a ceremonial checklist when there are no exceptions.
- Keep one writer per checkout. Use isolated worktrees only when independently owned changes genuinely benefit from parallel implementation.
- Inspect the resulting diff and validation evidence yourself. Child reports are evidence, not acceptance.
- Review according to risk:
  - trivial documentation or local configuration changes: parent review is enough;
  - non-trivial code changes: launch three fresh `reviewer` tasks with distinct correctness, test coverage/quality, and cleanliness/readability contracts;
  - security, concurrency, performance, public API, migration, or data-integrity risk: add a fresh specialist reviewer for each relevant risk rather than weakening the three core reviews.
- Validate every reviewer finding against the current diff. Send accepted fixes back to the worker; reviewers remain read-only unless the user explicitly authorizes a writer pass.
- Re-run focused validation after fixes. Use at most one targeted re-review unless the user asks for a longer review loop.
- Reserve `oracle` on the parent model for unresolved architecture, root-cause, decision-drift, or reviewer-disagreement questions. Do not use it for routine review.
- For tiny, obviously safe edits, direct parent implementation remains acceptable when delegation would add more ceremony than evidence.

Do not begin implementation delegation while the plan is still under discussion unless the user explicitly asks for reconnaissance, prototyping, or review.
