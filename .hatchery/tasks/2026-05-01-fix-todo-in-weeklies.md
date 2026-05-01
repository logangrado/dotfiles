# Task: fix-todo-in-weeklies

**Status**: complete
**Branch**: hatchery/fix-todo-in-weeklies
**Created**: 2026-05-01 11:01

## Objective

we are capturing TODO items into weekly notes. However, those todo items are not showing up in our custom agenda.

## Summary

**Root cause:** `lg/org-roam-weeklies--new-skeleton` generated the node ID as a `#+id:` file keyword. Org-roam only recognizes IDs in a `:PROPERTIES:` drawer, so weekly files were never indexed into the org-roam DB. The vulpea-based agenda query (`lg/vulpea-project-files`) queries the `nodes` table, so unindexed files never appeared in the agenda.

**Fix:** Changed the skeleton template in `doom.d/packages/org-roam.el` to emit a proper `:PROPERTIES:` drawer with `:ID:` before the `#+title:` line. Also added `#+filetags: :project:` to the skeleton so new weeklies are pre-tagged.

**Gotcha for existing files:** Weekly files created before this fix still have the old `#+id:` format and won't be indexed until manually converted to use a `:PROPERTIES:` drawer.
