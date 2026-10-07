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

## Behavior changes from the pre-1.0 audit

No call has to change, but these behaviors did:

| Before | Now | Action |
|---|---|---|
| Moving a widget with `mount()` ended its `bind_reactive()` bindings, widget timers and owned workers. | A move keeps them; only `remove()` ends them. | Code that re-created bindings or timers after a move can drop that workaround. |
| A password `input()` showed its value in `print()`, event logs, the debug overlay and `inspect_widget()`, and Ctrl+C copied it. | The value is shown as `<hidden>` and is never copied. | Read `widget$value` directly where the value is needed. |
| `render_html(trim = TRUE)` removed trailing spaces of every styled run. | Only the end of each row is trimmed. | Regenerate stored HTML snapshots. |

`widget$on()` additionally accepts `widget$on(type, selector, handler)`, the
argument order of `on()` and `app$on()`; the existing order keeps working.

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

