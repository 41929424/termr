# Migration to 1.0

This note records the compatibility position for the 0.9 development series.
There are no mandatory API migrations in this series: no exported function or
class has been renamed or removed, and no exported symbol is currently marked
deprecated. The 1.0 release plan is to keep the documented stable API listed
in [API stability](stability.md).

## Names and compatibility

No naming change was justified by the pre-1.0 audit. In particular,
`browse_data()` and `data_browser()` are related but distinct, and pairs such
as `tabs()` / `tab()` or `run_worker()` / `run_process()` are not aliases.
They need no migration. Existing argument names and documented behavior
remain the migration baseline.

The historical package rename from `retui` to `termr` happened in 0.3.0 and
did not provide compatibility aliases. Current code should use `termr`,
`termr_progress()`, `termr.*` options, and `TERMR_*` environment variables;
this is historical guidance, not a new 1.0 change.

## Before upgrading

Applications using only the stable API should not need source changes for
1.0. Applications using APIs marked experimental in the [stability policy](stability.md)
should review release NEWS for changes, especially for text editing, reactive
signals, workers, custom layouts, lazy table sources, database adapters and
static exports. External extensions should use only the public points in
[Extending termr](extensions.md), not `termr:::` internals.

When 1.0 changes require action, this page and NEWS will list the old call,
its replacement, and the first version containing the change. Until then,
there are no deprecated aliases to update.

