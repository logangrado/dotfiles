# Task: fix-git-delta

**Status**: in-progress
**Branch**: hatchery/fix-git-delta
**Created**: 2026-05-20 12:32

## Objective

git-delta combined with magit give great, colored diffs. However, there is one big bug.

If say I have this code:

1 def func():
2    """
3     some doctring
4     """
5     x = 32
6     y = 10
7     answer_to_life = x + y

And the diff for a chunk runs from line 3 to 7: 

  3     some doctring
  4     """
  5     x = 32
  6     y = 10
  7     answer_to_life = x + y

Then syntax highlighting is wrong. It highlights line 3 like syntax, and lines 4-7 as a text block.

Can we fix this?

## Agreed Plan

git-delta does per-hunk syntax highlighting via syntect, which is stateless. When
a hunk begins mid-multi-line construct (e.g. inside a Python docstring), delta
misidentifies the syntactic state. Upstream has no flag for this.

Fix: wrap `magit-delta-call-delta-and-convert-ansi-escape-sequences` with
`:around` advice that augments each hunk with N preceding lines (from
`git show HEAD:<path>` or working tree) before delta sees the diff, then strips
those synthetic lines after delta colorizes (but before xterm-color converts
ANSI → faces in the same function call).

Steps:
1. Add `lg/magit-delta--extra-context-lines` (defvar, default 30).
2. Add `lg/magit-delta--read-prefix-lines` — fetch N lines preceding a hunk
   from HEAD or worktree.
3. Add `lg/magit-delta--augment-buffer` — walk buffer, prepend ` `-prefixed
   context lines to each hunk, rewrite `@@` header, record undo offsets.
4. Add `lg/magit-delta--strip-augmentations` — remove the prepended lines and
   restore original `@@` header.
5. Add `lg/magit-delta-call-delta-with-context` — `:around` advice wiring 3 + 4
   around delta with `unwind-protect`.
6. Install advice in the existing `(use-package! magit-delta …)` block.

All edits land in `doom.d/packages/magit.el`.

## Progress Log

- [x] Step 1: defvar for context lines count
- [x] Step 2: prefix-line reader
- [x] Step 3: buffer-augmentation walker
- [x] Step 4: augmentation-stripper
- [x] Step 5: `:around` advice with unwind-protect
- [x] Step 6: install advice in use-package block
- [ ] Verification: manual test with docstring repro, multi-hunk, new file, deleted file

## Summary

*(Fill in on completion — then remove Agreed Plan and Progress Log above.
Cover: key decisions made, patterns established, files changed, gotchas,
and anything a future agent working in this repo should know.)*
