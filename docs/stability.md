# API stability

This page applies to termr 1.0.0, the first stable release. It defines
which parts of the package are intended for application authors, which remain
experimental, and which are implementation details. The [API inventory](api-inventory.md)
lists every current export, its implementation and its Rd topic.

## Stable API

These APIs are stable in termr 1.0. Bug fixes may
correct behavior that contradicts their documentation; incompatible changes
will be called out in NEWS and migration notes.

- Core app and widget construction: `app()`, `run()`, `label()`, `button()`,
  `input()`, `checkbox()`, `radio_set()`, `radio_button()`, `dropdown()`,
  `option_list()`, `panel()`, `vertical()`, `horizontal()`,
  `grid_layout()`, `scroll_view()`, `tabs()`, `tab()`, `modal()`,
  `confirm_dialog()`, `alert_dialog()`, `split_pane()`, `tree_view()`,
  `tree_node()`, and the display widgets documented in [Widgets](widgets.md).
- Events and commands: `on()`, `bind()`, `command()`, event dispatch and
  documented widget/app methods.
- Styling and themes: `style()`, `stylesheet()`, `stylesheet_file()`,
  `termr_theme()`, and documented built-in theme names.
- In-memory data tables: `data_table()` with a data frame or matrix,
  `column()`, sorting, filtering, selection and documented table events.
- App testing and basic rendering: `test_app()`, `render_widget()`,
  `render_text()`, `render_lines()`, `strip_ansi()`, and the documented pilot
  methods.
- Terminal capability queries and documented run/cleanup behavior described
  in [Terminal](terminal.md). Host coverage varies; see the
  [platform matrix](platform-testing.md).

The public R6 class names are documented for type checks and advanced
subclassing. Use the constructors for ordinary application code.

## Experimental API

These APIs are public and documented, but their details may change before 1.0.
Every exported symbol not named in the stable section is experimental unless
it is explicitly identified as internal below. Changes will be recorded in
NEWS and in [migration notes](migration-to-1.0.md).

- `text_area()` and syntax-highlighter callbacks (`r_highlighter()`,
  `sql_highlighter()`).
- Reactive graph functions: `signal()`, `computed()`, `watch()`, `batch()`,
  `peek()`, `untracked()`, `dispose()`, and `update_signal()`.
- Workers and subprocesses: `run_worker()`, `run_process()`, and worker
  lifecycle details.
- Custom widget construction and low-level widget subclassing through
  `widget()`, `Widget`, `ScreenBuffer`, and `region()`.
- Custom layout registration through `register_layout()` and
  `unregister_layout()`.
- Lazy table sources and database support: `table_source()`,
  `db_table_source()`, `db_query_source()`, `db_connection()`,
  `db_metadata()`, `db_explorer()`, `sql_editor()`, and SQL query events.
- Workflow widgets added in 0.9: `status_bar()`, `property_grid()`,
  `record_view()`, `data_profile()`, `json_view()` and their classes.
- Static export and serialization: `render_markdown()`, `render_html()`,
  `render_svg()`, `screen_snapshot()`, `screen_snapshot_json()`,
  `widget_snapshot()`, `widget_snapshot_json()`, `knit_termr()`, and
  `write_rendered()`.
- Diagnostic and low-level rendering APIs such as `inspect_widget()`,
  `inspect_signal()`, `patch_to_ansi()`, and `diff_screen()`.

Optional packages such as DBI, RSQLite, jsonlite and knitr remain optional;
they are not runtime dependencies.

## Internal API

Names not listed by `NAMESPACE` are internal, even when their spelling looks
like a user-facing function. This includes driver parsers, validation and
painting helpers, caches, private callbacks, and functions documented only
for internal package use. They may change without notice. Do not call them
with `termr:::` or rely on their implementation details. See the
[extension guide](extensions.md) for the supported external extension points.

S3 methods registered in `NAMESPACE` are public only through their documented
generic behavior; their implementation names are not separate extension
points.

## Versioning after 1.0

termr follows semantic versioning from 1.0.0 on:

- **Patch releases** (1.0.x) fix bugs. They change behavior only where it
  contradicted the documentation.
- **Minor releases** (1.x.0) add features. Stable APIs keep working; a stable
  API that must go is first deprecated: it keeps working and warns (once per
  session) for at least one minor release and six months, and NEWS names its
  replacement.
- **Major releases** (2.0.0) may remove deprecated or change stable APIs.

Experimental APIs are excluded: they may change in a minor release, with a
NEWS entry, and without a deprecation period when one is impractical. An
experimental API becomes stable by being moved to the stable list here.

Event names and the fields of `event$data` listed in the
[event reference](events.md#widget-and-subsystem-messages) are frozen as they
are: names are not renamed, and documented fields are not removed or renamed
within a major version (new fields may be added). This holds for messages of
experimental widgets too, as long as the widget exists. Documented key
bindings and command names are stable as well.

Snapshot formats carry a `schema_version`. Adding fields does not change it;
renaming, removing or re-typing a field increments it, in a minor release at
the earliest, and is listed in NEWS. Consumers should ignore unknown fields.

Extension points (custom widgets, layouts, themes, highlighters, table
sources and database adapters) are experimental for 1.0. Extensions should
use only exported functions and documented callback contracts; anything
reached with `termr:::` can change in any release.

## Naming and aliases

The existing snake_case constructors correspond to CamelCase R6 classes.
Paired names such as `vertical()` / `horizontal()`, `tabs()` / `tab()`, and
`run_worker()` / `run_process()` describe related operations rather than
deprecated aliases. `browse_data()` and `data_browser()` also have different
return behavior. No exported name is currently deprecated. Before 1.0 the
0.9 additions `snapshot_json()`, `inspect_json()` and
`db_table_source(own_connection =)` were renamed to `screen_snapshot_json()`,
`widget_snapshot_json()` and `owned =` without aliases (see
[migration notes](migration-to-1.0.md)). Event names were not renamed; see
[Events](events.md).

