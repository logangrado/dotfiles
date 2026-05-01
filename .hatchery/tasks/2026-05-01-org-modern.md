# Task: org-modern

**Status**: complete
**Branch**: hatchery/org-modern
**Created**: 2026-05-01 09:15

## Objective

org-modern-indent provides a way for src blocks to be indented in org mode under headings AND bullets.

I want that functionality. I do not currently have org-modern installed.

Let's investigate adding org-modern and org-modern-indent, or just org-modern-indent.

## Summary

**Decision**: Added org-modern-indent standalone (without enabling org-modern) for visual bracket decorations on src blocks, plus fixed the core org indentation setting that Doom was overriding.

**Key insight**: org-modern-indent only provides visual bracket styling (╭│╰) around blocks — it does NOT indent block contents. The actual content indentation is handled by org's native indentation system, which requires `org-src-preserve-indentation` to be `nil`. Doom's org module sets this to `t`, which was the root cause of content not being indented.

**Files changed**:
- `doom.d/packages.el` — added `(package! org-modern-indent :recipe (:host github :repo "jdtsmith/org-modern-indent"))`. GitHub recipe needed since it's not on MELPA.
- `doom.d/packages/org-config.el` — two additions:
  1. `(after! org (setq org-src-preserve-indentation nil))` — overrides Doom's default of `t` so org can re-indent block content within lists. Must use `after!` rather than `setq-default` to run after Doom's module config.
  2. `(use-package! org-modern-indent :after org :config (add-hook 'org-mode-hook #'org-modern-indent-mode 90))` — loads org-modern-indent after org, with hook priority 90 so it activates after org-indent-mode.

**Gotchas**:
- Doom Emacs sets `org-src-preserve-indentation t` in its org module. A `setq-default` in the user's `:config` block is NOT sufficient to override it — must use `(after! org (setq ...))`.
- org-modern-indent hook priority 90 matters — it must run after org-indent-mode.
- Existing src blocks under bullets need re-indenting: `M-S-right` on the `#+begin_src` line runs `org-indent-block`.
- New blocks: `C-c C-,` then `s` creates a properly indented block; `C-c '` opens the dedicated edit buffer where language-mode indentation works natively.
- org-modern remains declared in packages.el but unconfigured — can be enabled later for heading/keyword/table styling.
