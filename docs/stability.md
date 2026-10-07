# API stability

termr is in 0.9 development, before its first 1.0 release. This page defines
which parts of the package are intended for application authors, which remain
experimental, and which are implementation details. The [API inventory](api-inventory.md)
lists every current export, its implementation and its Rd topic.

## Stable API

These APIs are intended to remain compatible through 1.0. Bug fixes may
correct behavior that contradicts their documentation; incompatible changes
will be called out in NEWS and migration notes.

- Core app and widget construction: `app()`, `run()`, `label()`, `button()`,
  `input()`, `checkbox()`, `radio_set()`, `radio_button()`, `dropdown()`,
  `option_list()`, `select`, `panel()`, `vertical()`, `horizontal()`,
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
  `db_table_source()`, `db_connection()`, `db_metadata()`, `db_explorer()`,
  `sql_editor()`, and SQL query events.
- Static export and serialization: `render_markdown()`, `render_html()`,
  `render_svg()`, `snapshot_json()`, `inspect_json()`, `widget_snapshot()`,
  `screen_snapshot()`, `knit_termr()`, and `write_rendered()`.
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

## Naming and aliases

The existing snake_case constructors correspond to CamelCase R6 classes.
Paired names such as `vertical()` / `horizontal()`, `tabs()` / `tab()`, and
`run_worker()` / `run_process()` describe related operations rather than
deprecated aliases. `browse_data()` and `data_browser()` also have different
return behavior. No exported name is currently deprecated. We found no
clearly bad name whose benefit would justify a rename ahead of 1.0, so this
release introduces no rename or compatibility alias.

