# Task: link-agent-files

**Status**: complete
**Branch**: hatchery/link-agent-files
**Created**: 2026-06-18 13:59

## Objective

I want to ensure my agent-setup files are managed here and linked in.

We want to manage:

- claude code prompt
- AGENTS.md file, symlinked into claude and codex dirs

## Summary

Added a top-level `agents/` directory (mirrors the `git/`, `ruff/`, `tmux/`
pattern in `link.sh`):

- `agents/AGENTS.md` — global agent prompt, symlinked to both
  `~/.claude/CLAUDE.md` and `~/.codex/AGENTS.md`. One source, two
  destinations, so Claude Code and Codex stay in sync.
- `agents/statusline-command.sh` — Claude Code's statusline script,
  symlinked to `~/.claude/statusline-command.sh` only. It parses Claude
  Code's own statusline JSON schema (`.model.display_name`,
  `.context_window.remaining_percentage`, etc.) — Codex has no equivalent
  hook or format, so it isn't linked there.
- `agents/skills/panel-review/SKILL.md` — a multi-agent code-review skill
  added later on this branch. `link.sh` now auto-discovers every directory
  under `agents/skills/*/` and symlinks it to `~/.claude/skills/<name>`,
  so future skills dropped in that directory link automatically without
  further `link.sh` edits.

`AGENTS.md` itself was restructured from a flat prompt into 10 numbered
directives (think-before-coding, simplicity, surgical changes,
verify-before-done, testing, code structure, etc.) — Karpathy-style,
imperative, one rule per section with a **Why** framed as a bolded lead
sentence rather than prose.

Gotcha: `link.sh`'s `DOT_DIR=$PWD` had to move earlier (before the OS
`case` block) so the new skills-discovery loop could reference it when
building the `ALL` array.

Left alone: `.claude/skills/hatchery-done/SKILL.md` at the worktree root
is harness-injected session tooling, not repo content — not committed.
