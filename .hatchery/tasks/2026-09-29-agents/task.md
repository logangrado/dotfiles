# Task: agents

**Status**: complete
**Branch**: (none — no-worktree mode)
**Created**: 2026-09-29 10:56

## Objective

Create a tracked global Pi configuration for a workflow where a high-capability parent discusses and approves a plan, delegates implementation, reviews the result through independent angles, and returns to the user with validated work.

## Context

Pi separates three concerns that other harnesses may combine:

- An **agent definition** is a stable execution envelope: model, thinking level, tools, context inheritance, permissions, defaults, and base system prompt.
- A **launch task** gives an available agent its temporary, specific assignment. One read-only `reviewer` can therefore serve as a correctness, testing, readability, or security specialist without four persistent definitions.
- A **skill** is reusable guidance or an orchestration recipe. A parent skill can launch several task-specialized agents and synthesize their results.

The desired default is one expensive parent retaining discussion, decisions, orchestration, finding disposition, and final acceptance. Non-trivial implementation goes to one fresh-context worker. Every non-trivial code review uses independent correctness, test-quality, and readability angles, with extra specialists selected only for relevant risk.

Agent resources are organized first by whether they are genuinely shared, then by harness. This prevents a skill written for one orchestration API from being exposed to an incompatible harness.

## Summary

The tracked layout is:

```text
agents/
├── shared/
│   ├── AGENTS.md
│   └── skills/agent-comm/
├── claude/
│   ├── statusline-command.sh
│   └── skills/panel-review/
└── pi/
    ├── APPEND_SYSTEM.md
    ├── settings.json
    ├── extensions/
    └── skills/panel-review/
```

- `agents/shared/AGENTS.md` is the canonical engineering contract linked into Claude, Codex, and Pi. It requires goal and contract fidelity, success/failure/boundary verification, regression tests that fail without the fix, behavioral rather than internal tests, real implementations and fakes over mocks, minimal monkeypatching, complete-object assertions, restrained fixture/builder reuse, cohesive bounded functions, and named helpers instead of large explanatory comments.
- `agents/shared/skills/agent-comm` is linked into Claude and Codex. Pi uses its native supervisor and subagent workflow instead.
- `agents/claude/skills/panel-review` and `agents/claude/statusline-command.sh` are Claude-specific. The Claude panel is no longer linked into Codex because it depends on Claude's `Agent` tool and `subagent_type` contract.
- `agents/pi/settings.json` configures Pi packages and model routing. Built-in `worker` and `reviewer` roles use Terra with high thinking and receive the global instruction file through `inheritGlobalContext`; this does not inherit parent conversation history, and their launch context remains fresh.
- `agents/pi/APPEND_SYSTEM.md` keeps planning and acceptance with the parent, assigns one fresh worker, requires worker self-review and focused validation, and makes three fresh reviews standard for non-trivial code. Accepted fixes return to the worker; one targeted re-review is the default limit.
- `agents/pi/skills/panel-review` always launches distinct correctness, test coverage/quality, and cleanliness/readability reviewers. Security, concurrency, performance, public API, migration, and data-integrity reviewers are conditional on relevant risk.
- The Pi panel captures the exact committed/worktree diff once and gives all reviewers the same approved goal, changed-file list, instructions, and diff artifact. The parent validates, deduplicates, and dispositions findings rather than treating reviewer consensus as authority.
- No custom Pi agent definitions were needed. Future global custom definitions should live under `agents/pi/agents/` and be linked to `~/.pi/agent/agents/` when the first one is added.
- `link.sh` composes shared and harness-specific resources at their final destinations instead of linking one mixed skills directory into multiple harnesses.
- Validation passed for shell syntax, JSON settings, every isolated-HOME symlink, exclusion of Claude's panel from Codex, and Pi RPC discovery of its native panel skill.

Gotchas:

- The built-in Pi reviewer has read access but no general shell/Git access. Reviewers distinguish inspected evidence from commands the parent should run, and the panel supplies a temporary diff artifact rather than asking children to infer a committed range.
- Concrete model IDs are deployment policy. Update `agents/pi/settings.json` if Sol or Terra identifiers change or are unavailable in another provider registry.
- The validation environment runs Pi 0.85.1 while the installed extensions prefer newer dynamic-tool activation APIs. Both extensions used their supported eager-activation fallback; upgrading Pi removes those compatibility warnings.
- Pre-existing uncommitted changes to `context-usage-bar.ts` were preserved at `agents/pi/extensions/context-usage-bar.ts` and intentionally left out of the organization commits.
- Local untracked content under the former `agents/skills/` directory was not moved, tracked, or linked by this task.
