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
