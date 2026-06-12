# Task: fix-terminal-history

**Status**: complete
**Branch**: hatchery/fix-terminal-history
**Created**: 2026-06-10 09:19

## Objective

When in vterm in emacs, my arrow keys (in insert mode) do not scroll through history as expected. Up arrow will often return commands far back in history, skipping keys.

Is this an issue with my vterm? zsh? Other?

## Context

`zsh/zshrc` was binding up/down to `up-line-or-history`. Two real causes
of the "skipping" feel:

- **Multi-line history entries**: `up-line-or-history` first moves up
  *within the current buffer*. A prior command containing literal newlines
  (heredoc, pasted snippet) makes the first N up-presses cycle through that
  command's internal lines before history advances — looks like skipping.
- **Non-adjacent duplicates**: oh-my-zsh sets only `HIST_IGNORE_DUPS`
  (adjacent only); a cluster of identical non-adjacent commands all stay in
  history, so scrolling past them feels stuck.

The bug is in zsh's choice of widget, not in vterm. A pure-emacs
replacement was considered and rejected: vterm's buffer is rendered by the
libvterm C module and is largely opaque to emacs — intercepting up/down
would require parsing the display grid and round-tripping replacement keys
back into vterm. `company-mode` cannot inspect vterm for the same reason.

## Summary

Hybrid fix landed:

- **zsh side** (`zsh/zshrc`) — arrow keys + the existing `^[i`/`^[k`
  chords now invoke the zsh-builtin `up-line-or-beginning-search` /
  `down-line-or-beginning-search` widgets (`autoload`'d + `zle -N`'d).
  Behavior: empty prompt → plain history scroll; typed text → only
  matching-prefix entries; multi-line entries no longer cycle line-by-line.
  Both `\eOA` (application keypad / DECCKM set) and `\e[A` (cursor mode)
  encodings are bound so the fix is mode-agnostic.

  History options tightened: kept `nosharehistory` (per-window history is
  an explicit choice), added `inc_append_history` (pairs with no-share so
  HISTFILE updates immediately instead of on shell exit),
  `hist_ignore_all_dups` (drop older copy when a duplicate lands),
  `hist_find_no_dups`, `hist_reduce_blanks`, `hist_verify`.

- **emacs side** (`doom.d/packages/vterm.el`) — `SPC s h` ("search
  history") is bound to a small dispatcher `lg/consult-history-dwim`:
  - In `vterm-mode` → calls `lg/vterm-consult-history`, which reads
    `~/.zsh_history` (stripping `extended_history` `: ts:elapsed;`
    prefixes), pops a vertico minibuffer, and sends the choice to the
    live prompt via `vterm-send-string` so it can be edited before Enter.
  - Elsewhere → falls back to vanilla `consult-history` (handles
    eshell/comint/term/minibuffer via the built-in
    `consult-mode-histories` alist).

  `C-r` is intentionally **left unbound on the emacs side** so it falls
  through to zsh's native `history-incremental-search-backward` (vterm's
  default passthrough) — preserving universal shell muscle memory.

  **Scope asymmetry, by design:** arrow keys + zsh `C-r` operate on this
  shell's in-memory history (with `nosharehistory`, that means commands
  loaded from `~/.zsh_history` at shell start + commands typed in this
  session). `SPC s h` reads the on-disk file, so it spans **all**
  sessions. The two mechanisms are complementary: reach for `SPC s h`
  when you suspect the command came from another vterm.

### Files changed

- `zsh/zshrc` (lines 96–122 area) — full rewrite of the history + arrow
  key bindkey block.
- `doom.d/packages/vterm.el` — added `lg/vterm--read-zsh-history`,
  `lg/vterm-consult-history`, `lg/consult-history-dwim` helpers and a
  new `SPC s h` leader binding.

### Gotchas / notes for future work

- **`up-line-or-beginning-search` matches by *anchored prefix***, not
  substring. If substring matching becomes desirable, install
  `zsh-history-substring-search` and swap the widget names — the bindkey
  lines are otherwise identical. This was deliberately skipped to keep
  the zshrc plugin-free.
- **`inc_append_history` + `nosharehistory`**: each command lands in the
  shared file immediately, but in-memory history per session does **not**
  pull in commands from other sessions. The intent is that newly spawned
  vterms see everything, but existing windows stay isolated. If you ever
  want cross-window live sync, swap `inc_append_history` for
  `share_history` (and drop `nosharehistory`).
- **Why a custom wrapper instead of stock `consult-history`**: stock
  `consult-history` has no entry for `vterm-mode` in `consult-mode-
  histories` (it covers eshell/comint/term), so it errors with "No
  history configured for vterm-mode". Even if you add an entry, the
  function inserts the selection via `insert` — wrong for vterm, since
  vterm's display is owned by libvterm and `insert` doesn't reach the
  shell. Hence `lg/vterm-consult-history` reads the file directly and
  pushes via `vterm-send-string`.
- **`consult` is loaded transitively via Doom's `vertico` module** (see
  `doom.d/init.el`). The wrapper uses `consult--read` (technically
  internal but stable across consult versions in practice). If consult is
  ever swapped out, replace the body with plain `completing-read` —
  vertico will provide the UI either way; you just lose the sort-off
  `:sort nil` guarantee and would need metadata trickery to preserve
  newest-first order.
- **Multi-line history entries** (zsh `extended_history` writes
  backslash-continuations) are returned as separate lines by the parser.
  Rare enough in practice that this is the v1 trade-off; fix it by
  detecting trailing `\` and joining the next line, if it becomes a
  problem.
- **DECCKM coverage**: both `\eOA` and `\e[A` are bound. vterm normally
  sends `\eOA` once zsh's `zle-line-init` enables application keypad mode,
  but some terminals (iTerm2 with non-default settings, raw TTYs) stay in
  cursor mode. Binding both is cheap insurance — keep them both if this
  block is ever refactored.
