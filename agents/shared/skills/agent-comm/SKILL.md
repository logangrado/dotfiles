---
name: agent-comm
description: Set up and run a file-based comm channel between a coordinating agent and one or more implementer agents in separate worktrees/containers. Use when starting multi-agent work, priming a new worker, or when asked about the comm protocol, outbox/ack discipline, or how to review another agent's work.
allowed-tools: Read, Write, Glob, Grep, Bash(date:*), Bash(ls:*), Bash(mkdir:*), Bash(comm:*), Bash(git log:*), Bash(git branch:*)
---

A durable, clobber-proof channel for agents that cannot see each other's context. Each agent
writes only to its own directory; both read freely. The archive survives context compaction, which
is most of why it works.

## Roles

- **Architect** — owns scope, reviews every result, and is the *only* agent that talks to the
  human. Decides rather than deferring downward.
- **Implementer(s)** — build, measure, report. Never talk to the human directly.

One human, one architect, N implementers. Implementers do not message each other.

## Layout

Each agent owns a `comm/` in its **own** worktree and writes only there. If the containers mount
each other read-only, this is enforced rather than merely agreed.

```
<architect-worktree>/comm/
  outbox/worker-1/   architect -> worker-1
  outbox/worker-2/   architect -> worker-2
  acks/              architect's acks of messages received
<worker-N-worktree>/comm/
  outbox/architect/  worker -> architect
  outbox/worker-M/   worker -> worker-M   (only if the architect has authorised it)
  acks/              worker's acks of messages received
```

Each agent creates its own `comm/` on first use. Never create or write another agent's.

**One outbox subdirectory per recipient, always — no exceptions, including agents with a single
correspondent.** Two reasons. The unacked check below needs it: against a flat architect outbox,
every message addressed to worker-2 reads as unacked by worker-1 forever, and the false positives
grow with every message. And a uniform rule — *write to `outbox/<recipient>/`* — is what survives
someone extending the protocol later; a "workers skip the level because they only have one
recipient" special case is the thing that gets got wrong when a second recipient appears.

The rule is **write to one recipient's directory, read any of them.**

**Cross-reading is a feature.** A worker noticing a contradiction in an instruction sent to its
sibling is a real failure-detection path, and it catches architect errors that neither the architect
nor the addressee spots. It also lets a worker check its own work against a constraint imposed on
another without anyone sending a message.

**Worker-to-worker messaging is off by default, and the structure supports it anyway.** The default
is that implementers report to the architect and not to each other — an architect in the path is
what catches a mis-scoped "not a blocker" or a signature pin that would reject a working model
before either reaches a branch. But the directory shape allows it, so authorising it for a specific
purpose (a handoff, a rebase coordination) is a sentence rather than a protocol change. Note that
unlike a private channel it stays fully auditable: the architect reads every directory regardless,
so permitting it costs visibility nothing.

## Messages

One file per message, **write-once — never edit or delete after creating.**

```
<UTC>--<sender>--<slug>.md      e.g. 20260810T193300Z--architect--assignment-registry-assertions.md
```

Stamp with `date -u +%Y%m%dT%H%M%SZ`. Sorts chronologically, collision-free, and `ls` alone is a
usable index.

```
---
from: architect | worker-1
to: worker-1 | architect
subject: one line that states the finding, not the topic
re: <filename being replied to>        # optional
supersedes: <filename this replaces>   # optional
status: ACTION | NEEDS-REVIEW | BLOCKED | FYI | ACK-ONLY
---
```

A fresh unique filename means nothing can be clobbered, so `>` is safe:

```bash
cat > comm/outbox/$(date -u +%Y%m%dT%H%M%SZ)--architect--<slug>.md <<'EOF'
---
from: architect
to: worker-1
subject: ...
status: ACTION
---
...
EOF
```

If the target exists, you have a slug collision — pick another. Never overwrite.

**Changed your mind? New file with `supersedes:`.** This applies to your own errors too: if you
put something wrong in writing, correct it in a new message that says so plainly. Write-once means
the wrong version stays readable, so the correction has to be findable.

## Acks — always

Every message gets one. A file in **your own** `acks/`, named exactly after the message received,
containing one line: `READ`, `ACTING`, `DISAGREE`, or `BLOCKED`, plus a few words.

Because ack filenames mirror message filenames, either side can list what the other has not seen:

```bash
# architect: what has worker-1 not acked?
comm -23 <(ls comm/outbox/worker-1) <(ls ../worker-1/comm/acks)
# worker: what has the architect not acked?
comm -23 <(ls comm/outbox) <(ls ../<architect>/comm/acks)
```

Empty means current. **Run this before escalating** — without acks, "working on it" and "never saw
it" look identical. This only works if the architect's outbox is split per recipient; against a flat
outbox it reports every message sent to a sibling as unacked.

## Monitoring

Every agent watches the other side's `outbox/`, persistently, for the life of the session. The
architect's version tolerates worker directories that do not exist yet:

```bash
snap() { for w in worker-1 worker-2; do d="../$w/comm/outbox"; [ -d "$d" ] && ls "$d" | sed "s|^|$w: |"; done | sort; }
snap > /tmp/seen; while true; do snap > /tmp/now; comm -13 /tmp/seen /tmp/now; cp /tmp/now /tmp/seen; sleep 20; done
```

Seed the baseline first so existing messages do not replay.

## Priming a new implementer

Keep it short. State only what does damage **before the first review can happen** — there is a
window between spawn and your first reply where the agent acts unsupervised. Everything else is
enforceable in review, and a constraint explained when it bites is understood, where the same rule
in a priming doc is a line someone skimmed.

Include:
- Identity, who the architect is, who the siblings are.
- **Irreversible constraints only**: which worktree/branch/`comm/` are theirs; what they must never
  write; which files another agent currently has open; anything destructive.
- The protocol above.
- "Read the last N messages in `<other-worker>/comm/outbox/`. That is the working standard. Match
  it." An archive of real messages teaches more than an abstraction of it.
- **"Arm a persistent watcher on the architect's `outbox/` before you stand by."**
- "Then send one `ACK-ONLY` and stand by. Do not explore, do not start, do not propose."

Do **not** include style rules, quality preferences, or architectural constraints. Those go in the
first assignment or in review.

**The watcher is not optional, and it belongs in priming rather than the first assignment.** A
worker that reads the architect's outbox once at spawn can miss an assignment written moments
later, conclude none exists, and park forever — while the architect sees an idle worker and assumes
it is working. The human then has to hand-deliver filenames. A standing-by agent with no watcher is
a stalled agent.

### Priming template

Paste at spawn, filling the bracketed fields. This is the complete worker-facing subset — nothing
else needs to reach an implementer before its first assignment.

```markdown
You are **<WORKER_NAME>**, an implementer on <ONE_LINE_PROJECT_DESCRIPTION>. An **architect** agent
sets scope and reviews your work, and is the only one who talks to <HUMAN_NAME>, the human owner.
<SIBLING_NAMES_OR_"You are the only implementer.">

**Isolation — these bind before your first action:**
- Write only inside your own worktree and your own `comm/outbox`. Never write another agent's
  branch, worktree, or `comm/`. Read theirs freely.
- <READ_ONLY_PATHS>
- Your branch is `<WORKER_BRANCH>`. Never push to <BRANCHES_OWNED_BY_OTHERS>.
- Do not edit <FILES_ANOTHER_AGENT_HAS_OPEN>.
- <ANY_OTHER_DESTRUCTIVE_OR_IRREVERSIBLE_CONSTRAINT>

**Comm protocol:** one file per message in your own `comm/outbox/`, acks in your own `comm/acks/`.
Filename `<UTC>--<WORKER_NAME>--<slug>.md`, stamped with `date -u +%Y%m%dT%H%M%SZ`. Write-once —
never edit a sent message; reverse it with a new one carrying `supersedes:`. Frontmatter: `from`,
`to`, `subject`, `re`, `status` ∈ {`ACTION`, `NEEDS-REVIEW`, `BLOCKED`, `FYI`, `ACK-ONLY`}. Ack
every message you receive: a file in your own `acks/` named exactly after the message, containing
one line — `READ`, `ACTING`, `DISAGREE`, or `BLOCKED` — plus a few words.

**Orientation.** Read the last ten messages in `<PEER_OUTBOX_PATH>`. That is the working standard
on this project — what gets measured rather than argued, how findings are reported, how
corrections are handled. Match it.

**Arm a watcher before you stand by**, so you notice your assignment arriving:

    A=<ARCHITECT_OUTBOX_PATH>/<WORKER_NAME>
    ls $A > /tmp/seen
    while true; do ls $A > /tmp/now; comm -13 /tmp/seen /tmp/now; cp /tmp/now /tmp/seen; sleep 20; done

Run it in the background, persistently, for the whole session.

Then send a single `ACK-ONLY` message confirming you are primed, and **stand by**. Do not explore
the repo, do not start work, do not propose anything.
```

If there is no peer yet — the first implementer on a project — replace the orientation line with
the **working standard** section below, quoted directly. It is the only part of this skill an
implementer needs, and it is worth stating in full when there is no archive to point at.

## The working standard

This is what makes the output trustworthy. Hold implementers to it explicitly; it does not arrive
by itself.

- **Run it, don't reason about it.** Report the measured number, not that a bound passed. Confident
  readings of source are wrong often enough to matter; empirical checks are not.
- **Find the cause before accepting a tolerance.** A small discrepancy that looks like float noise
  needs a *cause* before it gets a bound. "Small and plausible" is where real bugs hide.
- **Diff sets, never counts.** Two offsetting differences net to an equal count and read as
  success. And never normalize away the thing you are trying to detect.
- **A parity harness built from your own assumptions can only confirm you agree with yourself.**
  Build the reference from the real artifact, not from your port's config.
- **Unverified stays unverified.** If it cannot be checked locally, say so and say why. "Researched
  and plausible" must not quietly become "established."
- **Report your own errors and corrected tests.** A silently fixed mistake is indistinguishable
  from one that never happened.
- **`BLOCKED` rather than guess.** Stop when something would materially change the plan.
- **Report before expensive or irreversible steps** — cluster runs, deploys, anything with a slow
  feedback loop.

## Architect behaviours

- **Accept explicitly, and name what was done well and why.** This is not politeness; it is what
  makes the corrections land, and it tells the implementer which instincts to keep.
- **Decide.** An implementer that surfaces a real design fork wants an answer, not options handed
  back.
- **Scope tightly when accepting growth.** Approve the mechanism, require proof of the shortcut,
  and say what is out of scope in the same message.
- **State the reachability argument** when telling someone to preserve a known bug or a limitation.
  "Do it this way" decays; "do it this way because X is unreachable at scale" survives.
- **Consolidate scattered caveats.** Four limitations documented in four files are collectively
  invisible. Put an index where the user of the thing will land.
- **Correct yourself in writing.** You will be wrong; the archive is permanent; a future reader
  will otherwise cite the wrong version.

## What does NOT belong here

Decisions that outlive the task. The comm channel is scaffolding and should be treated as losable.
If a message contains a decision worth keeping, the same message should name the committed file it
is going into — a doc, an ADR, or a citation comment in the code.
