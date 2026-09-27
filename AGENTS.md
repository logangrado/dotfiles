# AGENTS.md

This file captures context for AI agents working in this repository.
It is updated at the end of each task.

## Architecture Overview

The active Emacs configuration is Doom Emacs in `doom.d/`. Package declarations
live in `doom.d/packages.el`; package-specific configuration files in
`doom.d/packages/` are loaded by `doom.d/config.el`.

## Conventions

Declare third-party packages with `package!` and place their configuration in a
same-named file under `doom.d/packages/`. Keep machine-specific settings and
secrets in `doom.d/computer-locals.el` or `auth-source`, not the repository.
Load reusable macros from `doom.d/custom_funcs.el` before package files that
expand them.

## Task History

- Vterm Visual yanks: `lg/vterm-yank-region` removes right-edge terminal-cell
  padding while preserving every visible vterm row as a newline.  Bind it only
  in `vterm-copy-mode-map`; keep ordinary Evil Visual `y` bound to `evil-yank`.
  This is the no-native-rebuild fallback for redraw-heavy terminal UIs such as
  Pi, Codex, and Claude; it intentionally leaves visual wraps as newlines.

- Jira integration: added MELPA's maintained `jira.el`, configured for REST v3,
  and bound the issue list to `SPC j j`. `lg/define-transient-map` is the single
  source of truth for Jira actions: it generates the `h`/`?` transient and
  matching explicit Evil normal/visual bindings. List `l` opens filters; `RET` opens
  details; list `U` fetches the selected issue before opening the package's
  field-update picker. `n` creates an issue by selecting a project (default
  `FLWT`) and issue type from Jira metadata; detail `+` adds a comment.
  The macro converts `#'command` declarations to the bare command symbols
  Transient requires, and wraps noninteractive comment helpers before binding.
  Jira's find-issue command has no autoload, so use `lg/jira-find-issue` from
  menus that can load before `jira-detail`.
  Register generated bindings with `evil-define-key` and normalize on each
  Jira mode hook; do not add regular-map bindings, which affect insert state.
  Mark the generated action maps as normal/visual intercept maps so they take
  precedence over Tablist's Evil minor-mode bindings.
  Convert generated direct key descriptions with `kbd`; otherwise multi-event
  names such as `RET` become literal character sequences.
  Personal filters and views live in ignored `doom.d/custom.el`. Filters own
  JQL and their default view; views own visible columns and local sort fields.
  `f` opens the filter transient and `,` opens the view transient, both in
  Normal state only. A view reuses cached issues when its fields are already
  loaded; hidden sort fields are requested with the view. `SPC j j` opens the
  saved default filter. Combined legacy views are intentionally unsupported.
  Attach Jira list and detail maps with `after! jira-issues` and `after!
  jira-detail`: `SPC j j` autoloads `jira-issues`, not the top-level `jira`
  feature.
  Wrap Jira's field-update and subtask helpers in interactive commands before
  binding them; those helpers are not commands themselves.
