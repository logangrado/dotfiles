# Task: jira

**Status**: complete
**Branch**: (none — no-worktree mode)
**Created**: 2026-09-11 09:13

## Objective

Please help me identify, install, and test the best jira extensions for emacs

## Context

Jira actions need to work reliably under Evil while remaining discoverable.
Defining direct bindings and a transient separately had caused their advertised
keys to diverge. Jira's find-issue function is defined in `jira-detail.el`
without an autoload, so the list transient cannot refer to it directly before
detail mode loads.

`map! :nv` did not override Evil's state-specific bindings in Jira list mode,
so generated direct actions could fall through to inherited bindings. Regular
major-mode bindings would also apply in insert state, so the repair must remain
in Evil's normal and visual layers.

Jira's auxiliary keymap is active but Tablist's Evil minor-mode map has higher
precedence. The generated Jira action map must be an intercept map in normal
and visual states. `custom_funcs.el` is the correct package-neutral home, but
must load before `packages/*.el` when it provides macros for package
configuration.

Personal Jira views should persist in `doom.d/custom.el`, not in repository
configuration. View commands need normal-state-only bindings to preserve Evil
Visual mode.

Filters and views are separate persistent records: filters select issues with
JQL and choose a default view; views define sorting and columns. Existing
combined views are intentionally not supported and should be recreated.

## Summary

`jira.el` was selected over `org-jira`: it is actively maintained and provides
native issue lists, filters, editing, status changes, worklogs, and exports.
It is declared in `doom.d/packages.el` and configured in
`doom.d/packages/jira.el` for Jira Cloud's REST v3 API. `SPC j j` opens the
issue list.

Set the instance URL outside version control in `doom.d/computer-locals.el`:

```elisp
(setq jira-base-url "https://your-site.atlassian.net")
```

Store the matching email and API token in `~/.authinfo.gpg` (or `~/.authinfo`):

```
machine your-site.atlassian.net login you@example.com port https password API_TOKEN
```

Then run `doom sync`, restart Emacs, and use `SPC j j`. The clean-container
verification installed Jira and all MELPA dependencies and loaded `jira.el`
successfully under Emacs 30.1. The container did not include the Doom command
or Jira credentials, so the final Doom sync and live request must be performed
on the configured machine. The first list operation is read-only; press `?` in
the issue list to discover its actions.

`lg/define-transient-map` is package-neutral: it takes a keymap, transient
name, and grouped action declarations, then generates both the
sectioned transient and matching explicit Evil normal/visual bindings. In Jira list and
detail buffers, `h` and `?` open that transient; every action it presents is
also available directly. This makes the declaration the sole source of truth.
The list's `l` action opens the upstream filter transient, which can later be
extended as a nested menu. `RET` opens the selected issue detail.

The macro's generated transient and bindings were verified by loading the real
Transient definition with representative interactive Jira command stubs. The
list transient was also loaded with `jira-detail` deliberately absent. It
converts the conventional `#'command` declaration form into Transient's
required bare command symbol. `lg/jira-find-issue` loads `jira-detail` only
when invoked. The configuration passed Emacs parenthesis and Git whitespace
checks. Reload Doom before live keymap verification.

The builder uses `evil-define-key` for normal and visual states, rather than
`map! :nv`, and Jira mode hooks normalize Evil keymaps when buffers start. This
ensures Jira actions override inherited bindings such as Tablist's `U` command
without affecting insert state. A disposable environment with the real Evil
package verified normal-state `U` resolves to `lg/jira-update-selected-issue`.
Because Tablist's Evil minor-mode map otherwise has higher precedence than a
Jira auxiliary map, the builder marks each generated normal/visual action map
as an Evil intercept map. This was verified against a simulated Tablist
normal-state `U` binding.

Generated direct bindings pass their key descriptions through `kbd`, while
Transient uses those same descriptions directly. This preserves the single
action declaration while correctly handling multi-event keys such as `RET`.
The real-Evil test verified both `RET` and `U` resolve to their Jira commands.

`lg/define-transient-map` lives in `doom.d/custom_funcs.el`, not the Jira
package configuration, and `doom.d/config.el` loads that file before
`packages/*.el`. It is therefore available to any future package configuration
without creating a Jira dependency. Loading the shared file followed by Jira's
configuration passed the real Evil/Transient `RET` and `U` test.

Saved filters and views are independent Custom variables in ignored
`doom.d/custom.el`. A filter owns JQL and its default view; a view owns its
columns, sort fields, and optional custom status order. In the issue list,
`f` opens the normal-state filter transient (`o` open, `n` save, `d` assign a
default view, `D` set the `SPC j j` startup filter). `,` opens the view
transient (`o` open, `n` save, `s` choose sort fields, `c` choose columns).
Views reuse cached results when all their fields are present; otherwise they
fetch once. Hidden sort fields are still included in that request. Local status
sorting is case-insensitive and stable, preserving JQL's server ordering such
as Rank within each status. There is intentionally no migration from the former
combined `lg/jira-views` format: recreate those saved records after reloading.
The split model, default startup fetch, and transient commands passed an Emacs
stub test with real Evil and Transient.

`jira-detail--update-field` and `jira-detail--create-subtask` are internal
non-interactive helpers. Bindings now call them through interactive wrappers;
directly binding these helpers causes Emacs to signal `wrong-type-argument
commandp`. The comment edit and remove helpers need wrappers for the same
reason; otherwise Transient rejects them while parsing its suffixes.

`SPC j j` autoloads `jira-issues`, bypassing the top-level `jira` feature.
The Jira bindings are therefore attached with `after! jira-issues` and
`after! jira-detail`, ensuring the relevant keymaps exist and the bindings
install when those features load.

List view `U` fetches the selected issue, populates its detail context, and
then invokes Jira's existing field-update picker. Its asynchronous callback
flow was verified with an Emacs stub; verify the live Jira request after
reloading Doom.

Jira issue lists prioritize filtering over horizontal motion: `l` opens the
filter menu. `h` is reserved for the generated Jira command transient.

`n` in Jira issue lists and detail buffers creates an issue. It prompts
for a project (defaulting to `FLWT`), then an issue type, and finally every
field Jira requires for that type. Detail `+` adds a comment. The creation
selection flow passed an Emacs stub test; verify an actual submission after
reloading Doom.
