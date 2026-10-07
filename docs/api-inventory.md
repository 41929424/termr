# Public API inventory

Generated from the checked-in `NAMESPACE`, top-level definitions in `R/`, and current Rd files. This is an inventory, not a recommendation to rename or remove symbols. It describes direct `export()` symbols separately from registered S3 methods.

## Scope and counting

- Explicit `export()` symbols: **155**.
- Registered S3 methods: **29** (listed separately; they are registrations, not `export(name)` directives).
- Exported symbols without a matching Rd alias: **0**.
- Exports without an Rd usage section: **0**.
- Exports without an Rd examples section: **72**.
- Exports with no direct occurrence in README/docs/examples/tests: **24**.

?Used? below means a textual symbol occurrence in the named area; it does not prove runtime reachability. S3 methods are not counted as direct exports. `Rd usage` means a top-level Rd `\usage{}` section or an R6 method usage subsection. R6 class entries can be types returned by constructors even when their class names do not occur directly in examples.

## Exported symbols

| Name | Type | Implementation | Rd | Usage | Example | Direct references |
|---|---|---|---|---|---|---|
| `Animation` | R6 class | [animation.R](../R/animation.R#L45) | [yes](../man/Animation.Rd) | yes | no | ? |
| `App` | R6 class | [app.R](../R/app.R#L23) | [yes](../man/App-class.Rd) | yes | no | tests |
| `BlurEvent` | R6 class | [events.R](../R/events.R#L187) | [yes](../man/Event.Rd) | yes | no | ? |
| `Button` | R6 class | [widget-button.R](../R/widget-button.R#L13) | [yes](../man/Button-class.Rd) | yes | no | docs, tests |
| `Checkbox` | R6 class | [widget-controls.R](../R/widget-controls.R#L7) | [yes](../man/Checkbox-class.Rd) | yes | no | ? |
| `DataProfile` | R6 class | [widget-workflows.R](../R/widget-workflows.R#L420) | [yes](../man/DataProfile-class.Rd) | yes | no | tests |
| `DataTable` | R6 class | [widget-datatable.R](../R/widget-datatable.R#L88) | [yes](../man/DataTable-class.Rd) | yes | no | docs |
| `Event` | R6 class | [events.R](../R/events.R#L20) | [yes](../man/Event.Rd) | yes | no | docs |
| `FocusEvent` | R6 class | [events.R](../R/events.R#L175) | [yes](../man/Event.Rd) | yes | no | ? |
| `Grid` | R6 class | [widget-containers.R](../R/widget-containers.R#L46) | [yes](../man/Grid.Rd) | yes | no | docs, tests |
| `Horizontal` | R6 class | [widget-containers.R](../R/widget-containers.R#L34) | [yes](../man/Horizontal-class.Rd) | yes | no | docs, tests |
| `Input` | R6 class | [widget-input.R](../R/widget-input.R#L5) | [yes](../man/Input-class.Rd) | yes | no | README, docs, tests |
| `KeyEvent` | R6 class | [events.R](../R/events.R#L94) | [yes](../man/Event.Rd) | yes | no | docs, tests |
| `KeyValue` | R6 class | [widget-display.R](../R/widget-display.R#L496) | [yes](../man/KeyValue-class.Rd) | yes | no | ? |
| `Label` | R6 class | [widget-label.R](../R/widget-label.R#L5) | [yes](../man/Label-class.Rd) | yes | no | docs, tests |
| `LogView` | R6 class | [widget-log.R](../R/widget-log.R#L7) | [yes](../man/LogView-class.Rd) | yes | no | tests |
| `MarkdownView` | R6 class | [markdown.R](../R/markdown.R#L351) | [yes](../man/MarkdownView-class.Rd) | yes | no | ? |
| `MessageEvent` | R6 class | [events.R](../R/events.R#L223) | [yes](../man/Event.Rd) | yes | no | docs |
| `Metric` | R6 class | [widget-display.R](../R/widget-display.R#L397) | [yes](../man/Metric-class.Rd) | yes | no | ? |
| `ModalScreen` | R6 class | [screens.R](../R/screens.R#L64) | [yes](../man/ModalScreen.Rd) | yes | no | tests |
| `MountEvent` | R6 class | [events.R](../R/events.R#L199) | [yes](../man/Event.Rd) | yes | no | ? |
| `MouseEvent` | R6 class | [mouse.R](../R/mouse.R#L23) | [yes](../man/Event.Rd) | yes | no | docs, tests |
| `OptionList` | R6 class | [widget-list.R](../R/widget-list.R#L178) | [yes](../man/OptionList-class.Rd) | yes | no | ? |
| `Panel` | R6 class | [screens.R](../R/screens.R#L121) | [yes](../man/Panel-class.Rd) | yes | no | tests |
| `PasteEvent` | R6 class | [events.R](../R/events.R#L148) | [yes](../man/Event.Rd) | yes | no | docs, tests |
| `ProcessView` | R6 class | [widget-process.R](../R/widget-process.R#L5) | [yes](../man/ProcessView-class.Rd) | yes | no | ? |
| `ProgressBar` | R6 class | [widget-display.R](../R/widget-display.R#L9) | [yes](../man/ProgressBar-class.Rd) | yes | no | ? |
| `RadioButton` | R6 class | [widget-controls.R](../R/widget-controls.R#L88) | [yes](../man/RadioButton.Rd) | yes | no | ? |
| `RadioSet` | R6 class | [widget-controls.R](../R/widget-controls.R#L148) | [yes](../man/RadioSet-class.Rd) | yes | no | ? |
| `ResizeEvent` | R6 class | [events.R](../R/events.R#L128) | [yes](../man/Event.Rd) | yes | no | docs, tests |
| `PropertyGrid` | R6 class | [widget-workflows.R](../R/widget-workflows.R#L163) | [yes](../man/PropertyGrid-class.Rd) | yes | no | tests |
| `Rule` | R6 class | [widget-display.R](../R/widget-display.R#L214) | [yes](../man/Rule-class.Rd) | yes | no | ? |
| `Screen` | R6 class | [widget-containers.R](../R/widget-containers.R#L6) | [yes](../man/Screen.Rd) | yes | no | tests |
| `ScreenBuffer` | R6 class | [screen-buffer.R](../R/screen-buffer.R#L16) | [yes](../man/ScreenBuffer.Rd) | yes | no | docs, examples, tests |
| `ScrollView` | R6 class | [widget-scroll.R](../R/widget-scroll.R#L6) | [yes](../man/ScrollView-class.Rd) | yes | no | docs |
| `Select` | R6 class | [widget-controls.R](../R/widget-controls.R#L278) | [yes](../man/Select-class.Rd) | yes | no | docs, examples, tests |
| `Sparkline` | R6 class | [widget-display.R](../R/widget-display.R#L285) | [yes](../man/Sparkline-class.Rd) | yes | no | ? |
| `Spinner` | R6 class | [widget-display.R](../R/widget-display.R#L133) | [yes](../man/Spinner-class.Rd) | yes | no | ? |
| `SplitHandle` | R6 class | [widget-split.R](../R/widget-split.R#L6) | [yes](../man/SplitHandle-class.Rd) | yes | no | ? |
| `SplitPane` | R6 class | [widget-split.R](../R/widget-split.R#L82) | [yes](../man/SplitPane-class.Rd) | yes | no | ? |
| `StatusBar` | R6 class | [widget-workflows.R](../R/widget-workflows.R#L7) | [yes](../man/StatusBar-class.Rd) | yes | no | tests |
| `TabPane` | R6 class | [widget-tabs.R](../R/widget-tabs.R#L4) | [yes](../man/TabPane.Rd) | yes | no | ? |
| `Tabs` | R6 class | [widget-tabs.R](../R/widget-tabs.R#L34) | [yes](../man/Tabs-class.Rd) | yes | no | docs |
| `TextArea` | R6 class | [widget-textarea.R](../R/widget-textarea.R#L5) | [yes](../man/TextArea-class.Rd) | yes | no | docs, tests |
| `Timer` | R6 class | [timers.R](../R/timers.R#L8) | [yes](../man/Timer.Rd) | yes | no | ? |
| `Toast` | R6 class | [notifications.R](../R/notifications.R#L30) | [yes](../man/Toast.Rd) | yes | no | ? |
| `TreeNode` | R6 class | [widget-treeview.R](../R/widget-treeview.R#L13) | [yes](../man/TreeNode.Rd) | yes | no | ? |
| `TreeView` | R6 class | [widget-treeview.R](../R/widget-treeview.R#L202) | [yes](../man/TreeView-class.Rd) | yes | no | ? |
| `UnmountEvent` | R6 class | [events.R](../R/events.R#L211) | [yes](../man/Event.Rd) | yes | no | ? |
| `Vertical` | R6 class | [widget-containers.R](../R/widget-containers.R#L21) | [yes](../man/Vertical-class.Rd) | yes | no | docs, tests |
| `Widget` | R6 class | [widget.R](../R/widget.R#L23) | [yes](../man/Widget-class.Rd) | yes | no | README, docs, tests |
| `Worker` | R6 class | [workers.R](../R/workers.R#L23) | [yes](../man/Worker.Rd) | yes | no | docs |
| `alert_dialog` | function | [screens.R](../R/screens.R#L218) | [yes](../man/modal.Rd) | yes | yes | docs, tests |
| `animate` | function | [animation.R](../R/animation.R#L164) | [yes](../man/animate.Rd) | yes | yes | docs, tests |
| `app` | function | [app.R](../R/app.R#L1183) | [yes](../man/app.Rd) | yes | yes | README, docs, examples, tests |
| `batch` | function | [signals.R](../R/signals.R#L290) | [yes](../man/signals.Rd) | yes | yes | README, docs, examples, tests |
| `bind` | function | [bindings.R](../R/bindings.R#L27) | [yes](../man/bind.Rd) | yes | yes | docs, examples, tests |
| `browse_data` | function | [browse-data.R](../R/browse-data.R#L26) | [yes](../man/browse_data.Rd) | yes | yes | README, docs, examples |
| `button` | function | [widget-button.R](../R/widget-button.R#L127) | [yes](../man/button.Rd) | yes | yes | README, docs, examples, tests |
| `cell` | helper | [cell.R](../R/cell.R#L42) | [yes](../man/cell.Rd) | yes | yes | README, docs, examples, tests |
| `checkbox` | function | [widget-controls.R](../R/widget-controls.R#L81) | [yes](../man/checkbox.Rd) | yes | yes | README, docs, examples, tests |
| `column` | helper | [widget-datatable.R](../R/widget-datatable.R#L30) | [yes](../man/column.Rd) | yes | yes | README, docs, examples, tests |
| `command` | function | [commands.R](../R/commands.R#L23) | [yes](../man/command.Rd) | yes | yes | README, docs, examples, tests |
| `computed` | function | [signals.R](../R/signals.R#L233) | [yes](../man/signals.Rd) | yes | yes | README, docs, examples, tests |
| `confirm_dialog` | function | [screens.R](../R/screens.R#L188) | [yes](../man/modal.Rd) | yes | yes | docs, examples, tests |
| `current_app` | function | [app.R](../R/app.R#L1085) | [yes](../man/current_app.Rd) | yes | no | tests |
| `data_browser` | function | [browse-data.R](../R/browse-data.R#L34) | [yes](../man/browse_data.Rd) | yes | yes | README, docs, examples, tests |
| `data_profile` | function | [widget-workflows.R](../R/widget-workflows.R#L506) | [yes](../man/data_profile.Rd) | yes | yes | docs, examples, tests |
| `data_table` | function | [widget-datatable.R](../R/widget-datatable.R#L2114) | [yes](../man/data_table.Rd) | yes | yes | README, docs, examples, tests |
| `db_connection` | function | [sql.R](../R/sql.R#L135) | [yes](../man/db_connection.Rd) | yes | no | README, docs, examples, tests |
| `db_explorer` | function | [database-explorer.R](../R/database-explorer.R#L61) | [yes](../man/db_explorer.Rd) | yes | yes | README, docs, examples, tests |
| `db_metadata` | helper | [database-explorer.R](../R/database-explorer.R#L14) | [yes](../man/db_metadata.Rd) | yes | no | docs, tests |
| `db_query_source` | function | [datatable-source-query.R](../R/datatable-source-query.R#L30) | [yes](../man/db_query_source.Rd) | yes | no | docs, tests |
| `db_table_source` | function | [datatable-source-sqlite.R](../R/datatable-source-sqlite.R#L16) | [yes](../man/db_table_source.Rd) | yes | no | docs, tests |
| `diff_screen` | helper | [diff.R](../R/diff.R#L24) | [yes](../man/diff_screen.Rd) | yes | yes | examples, tests |
| `dispose` | function | [signals.R](../R/signals.R#L275) | [yes](../man/signals.Rd) | yes | yes | docs, tests |
| `dropdown` | function | [widget-controls.R](../R/widget-controls.R#L386) | [yes](../man/dropdown.Rd) | yes | yes | README, docs, examples, tests |
| `grid_layout` | function | [widget-containers.R](../R/widget-containers.R#L81) | [yes](../man/grid_layout.Rd) | yes | yes | docs, examples, tests |
| `horizontal` | function | [widget-containers.R](../R/widget-containers.R#L114) | [yes](../man/vertical.Rd) | yes | yes | README, docs, examples, tests |
| `input` | function | [widget-input.R](../R/widget-input.R#L479) | [yes](../man/input.Rd) | yes | yes | README, docs, examples, tests |
| `inspect_json` | helper | [export.R](../R/export.R#L253) | [yes](../man/inspect_json.Rd) | yes | no | docs, tests |
| `inspect_signal` | helper | [signals.R](../R/signals.R#L334) | [yes](../man/inspect_signal.Rd) | yes | no | docs, tests |
| `inspect_widget` | helper | [devtools.R](../R/devtools.R#L18) | [yes](../man/inspect_widget.Rd) | yes | yes | docs, tests |
| `json_view` | function | [widget-workflows.R](../R/widget-workflows.R#L555) | [yes](../man/json_view.Rd) | yes | yes | docs, tests |
| `key_event` | helper | [events.R](../R/events.R#L248) | [yes](../man/key_event.Rd) | yes | yes | tests |
| `key_value` | function | [widget-display.R](../R/widget-display.R#L553) | [yes](../man/key_value.Rd) | yes | yes | README, docs, examples, tests |
| `knit_termr` | function | [export.R](../R/export.R#L268) | [yes](../man/knit_termr.Rd) | yes | no | docs, tests |
| `label` | function | [widget-label.R](../R/widget-label.R#L63) | [yes](../man/label.Rd) | yes | yes | README, docs, examples, tests |
| `log_view` | function | [widget-log.R](../R/widget-log.R#L141) | [yes](../man/log_view.Rd) | yes | yes | README, docs, examples, tests |
| `markdown_view` | function | [markdown.R](../R/markdown.R#L457) | [yes](../man/markdown_view.Rd) | yes | yes | README, docs, examples, tests |
| `metric` | function | [widget-display.R](../R/widget-display.R#L484) | [yes](../man/metric.Rd) | yes | yes | README, docs, examples, tests |
| `modal` | function | [screens.R](../R/screens.R#L176) | [yes](../man/modal.Rd) | yes | yes | README, docs, examples, tests |
| `motion_reduced` | function | [animation.R](../R/animation.R#L16) | [yes](../man/motion_reduced.Rd) | yes | yes | docs, examples, tests |
| `on` | function | [bindings.R](../R/bindings.R#L89) | [yes](../man/on.Rd) | yes | yes | README, docs, examples, tests |
| `option_list` | function | [widget-list.R](../R/widget-list.R#L255) | [yes](../man/option_list.Rd) | yes | yes | README, docs, examples, tests |
| `panel` | function | [screens.R](../R/screens.R#L144) | [yes](../man/panel.Rd) | yes | yes | README, docs, examples, tests |
| `patch_to_ansi` | helper | [renderer.R](../R/renderer.R#L16) | [yes](../man/patch_to_ansi.Rd) | yes | yes | examples, tests |
| `peek` | function | [signals.R](../R/signals.R#L304) | [yes](../man/signals.Rd) | yes | yes | docs, tests |
| `process_view` | function | [widget-process.R](../R/widget-process.R#L152) | [yes](../man/process_view.Rd) | yes | yes | README, docs, tests |
| `progress_bar` | function | [widget-display.R](../R/widget-display.R#L115) | [yes](../man/progress_bar.Rd) | yes | yes | README, docs, examples, tests |
| `property_grid` | function | [widget-workflows.R](../R/widget-workflows.R#L255) | [yes](../man/property_grid.Rd) | yes | yes | docs, tests |
| `r_highlighter` | function | [widget-textarea.R](../R/widget-textarea.R#L1582) | [yes](../man/r_highlighter.Rd) | yes | no | docs, tests |
| `radio_button` | function | [widget-controls.R](../R/widget-controls.R#L239) | [yes](../man/radio_set.Rd) | yes | yes | docs, examples, tests |
| `radio_set` | function | [widget-controls.R](../R/widget-controls.R#L230) | [yes](../man/radio_set.Rd) | yes | yes | README, docs, examples, tests |
| `reactive` | function | [reactive.R](../R/reactive.R#L17) | [yes](../man/reactive.Rd) | yes | yes | README, docs, examples, tests |
| `record_view` | function | [widget-workflows.R](../R/widget-workflows.R#L302) | [yes](../man/record_view.Rd) | yes | yes | docs, examples, tests |
| `region` | helper | [geometry.R](../R/geometry.R#L17) | [yes](../man/region.Rd) | yes | yes | README, docs, examples, tests |
| `register_layout` | function | [layout.R](../R/layout.R#L271) | [yes](../man/register_layout.Rd) | yes | no | docs, tests |
| `render_html` | helper | [export.R](../R/export.R#L151) | [yes](../man/render_html.Rd) | yes | yes | README, docs, tests |
| `render_lines` | helper | [export.R](../R/export.R#L29) | [yes](../man/render_text.Rd) | yes | yes | docs, tests |
| `render_markdown` | helper | [export.R](../R/export.R#L42) | [yes](../man/render_markdown.Rd) | yes | yes | README, docs, tests |
| `render_svg` | helper | [export.R](../R/export.R#L182) | [yes](../man/render_svg.Rd) | yes | no | docs, tests |
| `render_text` | helper | [export.R](../R/export.R#L22) | [yes](../man/render_text.Rd) | yes | yes | docs, tests |
| `render_widget` | helper | [testing.R](../R/testing.R#L14) | [yes](../man/render_widget.Rd) | yes | yes | docs, tests |
| `rule` | function | [widget-display.R](../R/widget-display.R#L277) | [yes](../man/rule.Rd) | yes | yes | docs, examples, tests |
| `run` | function | [app.R](../R/app.R#L1203) | [yes](../man/run.Rd) | yes | no | README, docs, examples, tests |
| `run_example` | function | [examples.R](../R/examples.R#L9) | [yes](../man/run_example.Rd) | yes | yes | README, docs, examples, tests |
| `run_process` | function | [workers.R](../R/workers.R#L386) | [yes](../man/run_worker.Rd) | yes | yes | README, docs, tests |
| `run_worker` | function | [workers.R](../R/workers.R#L370) | [yes](../man/run_worker.Rd) | yes | yes | README, docs, examples, tests |
| `screen_buffer` | helper | [screen-buffer.R](../R/screen-buffer.R#L263) | [yes](../man/screen_buffer.Rd) | yes | yes | examples, tests |
| `screen_snapshot` | helper | [export.R](../R/export.R#L82) | [yes](../man/screen_snapshot.Rd) | yes | yes | docs, tests |
| `scroll_view` | function | [widget-scroll.R](../R/widget-scroll.R#L305) | [yes](../man/scroll_view.Rd) | yes | yes | README, docs, examples, tests |
| `set_interval` | function | [app.R](../R/app.R#L1114) | [yes](../man/set_timeout.Rd) | yes | yes | docs, examples, tests |
| `set_timeout` | function | [app.R](../R/app.R#L1108) | [yes](../man/set_timeout.Rd) | yes | yes | docs, tests |
| `signal` | function | [signals.R](../R/signals.R#L210) | [yes](../man/signals.Rd) | yes | yes | README, docs, examples, tests |
| `snapshot_json` | helper | [export.R](../R/export.R#L97) | [yes](../man/snapshot_json.Rd) | yes | no | docs, tests |
| `span` | function | [span.R](../R/span.R#L21) | [yes](../man/span.Rd) | yes | yes | docs, examples, tests |
| `sparkline` | function | [widget-display.R](../R/widget-display.R#L384) | [yes](../man/sparkline.Rd) | yes | yes | README, docs, examples, tests |
| `spinner` | function | [widget-display.R](../R/widget-display.R#L206) | [yes](../man/spinner.Rd) | yes | yes | README, docs, examples, tests |
| `split_pane` | function | [widget-split.R](../R/widget-split.R#L248) | [yes](../man/split_pane.Rd) | yes | yes | README, docs, examples, tests |
| `sql_editor` | function | [sql.R](../R/sql.R#L207) | [yes](../man/sql_editor.Rd) | yes | no | README, docs, examples, tests |
| `sql_highlighter` | function | [sql.R](../R/sql.R#L9) | [yes](../man/sql_highlighter.Rd) | yes | no | docs, tests |
| `status_bar` | function | [widget-workflows.R](../R/widget-workflows.R#L111) | [yes](../man/status_bar.Rd) | yes | yes | docs, examples, tests |
| `strip_ansi` | helper | [ansi.R](../R/ansi.R#L103) | [yes](../man/strip_ansi.Rd) | yes | yes | tests |
| `style` | function | [style.R](../R/style.R#L54) | [yes](../man/style.Rd) | yes | yes | README, docs, examples, tests |
| `stylesheet` | function | [stylesheet.R](../R/stylesheet.R#L61) | [yes](../man/stylesheet.Rd) | yes | yes | docs, tests |
| `stylesheet_file` | function | [stylesheet.R](../R/stylesheet.R#L68) | [yes](../man/stylesheet.Rd) | yes | yes | tests |
| `tab` | function | [widget-tabs.R](../R/widget-tabs.R#L287) | [yes](../man/tabs.Rd) | yes | yes | README, docs, examples, tests |
| `table_filter` | helper | [widget-datatable.R](../R/widget-datatable.R#L67) | [yes](../man/table_filter.Rd) | yes | no | docs, tests |
| `table_source` | function | [datatable-source.R](../R/datatable-source.R#L33) | [yes](../man/table_source.Rd) | yes | no | docs, tests |
| `tabs` | function | [widget-tabs.R](../R/widget-tabs.R#L280) | [yes](../man/tabs.Rd) | yes | yes | README, docs, examples, tests |
| `terminal_capabilities` | helper | [capabilities.R](../R/capabilities.R#L29) | [yes](../man/terminal_capabilities.Rd) | yes | yes | docs, tests |
| `termr_theme` | function | [theme.R](../R/theme.R#L79) | [yes](../man/termr_theme.Rd) | yes | yes | docs, tests |
| `test_app` | helper | [testing.R](../R/testing.R#L94) | [yes](../man/test_app.Rd) | yes | yes | README, docs, tests |
| `text_area` | function | [widget-textarea.R](../R/widget-textarea.R#L1563) | [yes](../man/text_area.Rd) | yes | yes | README, docs, examples, tests |
| `tree_node` | function | [widget-treeview.R](../R/widget-treeview.R#L194) | [yes](../man/tree_node.Rd) | yes | yes | docs, examples, tests |
| `tree_view` | function | [widget-treeview.R](../R/widget-treeview.R#L498) | [yes](../man/tree_view.Rd) | yes | yes | README, docs, examples, tests |
| `unregister_layout` | function | [layout.R](../R/layout.R#L289) | [yes](../man/unregister_layout.Rd) | yes | no | docs, tests |
| `untracked` | function | [signals.R](../R/signals.R#L311) | [yes](../man/signals.Rd) | yes | yes | docs, tests |
| `update_signal` | function | [signals.R](../R/signals.R#L321) | [yes](../man/signals.Rd) | yes | yes | docs, tests |
| `vertical` | function | [widget-containers.R](../R/widget-containers.R#L108) | [yes](../man/vertical.Rd) | yes | yes | README, docs, examples, tests |
| `watch` | function | [signals.R](../R/signals.R#L247) | [yes](../man/signals.Rd) | yes | yes | README, docs, examples, tests |
| `widget` | function | [widget-factory.R](../R/widget-factory.R#L44) | [yes](../man/widget.Rd) | yes | yes | README, docs, examples, tests |
| `widget_snapshot` | helper | [export.R](../R/export.R#L241) | [yes](../man/widget_snapshot.Rd) | yes | no | docs, tests |
| `write_rendered` | helper | [export.R](../R/export.R#L284) | [yes](../man/write_rendered.Rd) | yes | no | docs, examples, tests |

## Registered S3 methods

The following 29 `S3method()` registrations are part of package dispatch behavior. They are listed separately from the explicit `export()` symbols.

| Registration | Method definition | Implementation |
|---|---|---|
| `as.character,termr_text` | `as.character.termr_text` | [span.R](../R/span.R#L58) |
| `c,termr_text` | `c.termr_text` | [span.R](../R/span.R#L39) |
| `format,termr_binding` | `format.termr_binding` | [bindings.R](../R/bindings.R#L41) |
| `format,termr_cell` | `format.termr_cell` | [cell.R](../R/cell.R#L61) |
| `format,termr_command` | `format.termr_command` | [commands.R](../R/commands.R#L40) |
| `format,termr_patch` | `format.termr_patch` | [diff.R](../R/diff.R#L79) |
| `format,termr_reactive` | `format.termr_reactive` | [reactive.R](../R/reactive.R#L23) |
| `format,termr_rect` | `format.termr_rect` | [geometry.R](../R/geometry.R#L35) |
| `format,termr_size` | `format.termr_size` | [style.R](../R/style.R#L202) |
| `format,termr_style` | `format.termr_style` | [style.R](../R/style.R#L177) |
| `format,termr_stylesheet` | `format.termr_stylesheet` | [stylesheet.R](../R/stylesheet.R#L83) |
| `format,termr_text` | `format.termr_text` | [span.R](../R/span.R#L63) |
| `format,termr_theme` | `format.termr_theme` | [theme.R](../R/theme.R#L108) |
| `print,termr_binding` | `print.termr_binding` | [bindings.R](../R/bindings.R#L47) |
| `print,termr_capabilities` | `print.termr_capabilities` | [capabilities.R](../R/capabilities.R#L81) |
| `print,termr_cell` | `print.termr_cell` | [cell.R](../R/cell.R#L73) |
| `print,termr_command` | `print.termr_command` | [commands.R](../R/commands.R#L46) |
| `print,termr_computed` | `print.termr_computed` | [signals.R](../R/signals.R#L356) |
| `print,termr_inspection` | `print.termr_inspection` | [devtools.R](../R/devtools.R#L57) |
| `print,termr_patch` | `print.termr_patch` | [diff.R](../R/diff.R#L88) |
| `print,termr_reactive` | `print.termr_reactive` | [reactive.R](../R/reactive.R#L29) |
| `print,termr_rect` | `print.termr_rect` | [geometry.R](../R/geometry.R#L40) |
| `print,termr_signal` | `print.termr_signal` | [signals.R](../R/signals.R#L350) |
| `print,termr_style` | `print.termr_style` | [style.R](../R/style.R#L190) |
| `print,termr_stylesheet` | `print.termr_stylesheet` | [stylesheet.R](../R/stylesheet.R#L88) |
| `print,termr_text` | `print.termr_text` | [span.R](../R/span.R#L68) |
| `print,termr_theme` | `print.termr_theme` | [theme.R](../R/theme.R#L114) |
| `print,termr_watch` | `print.termr_watch` | [signals.R](../R/signals.R#L363) |
| `print,termr_widget_type` | `print.termr_widget_type` | [widget-factory.R](../R/widget-factory.R#L130) |

## Public-looking top-level functions that are not exported

The audit found no top-level non-exported function with its own public Rd topic. These names are easy to mistake for extension APIs from their spelling, but their call sites and purpose are internal; treat them as implementation details unless separately documented:

| Symbol | Implementation | Why it looks public / audit note |
|---|---|---|
| `headless_capabilities` | [capabilities.R](../R/capabilities.R#L73) | Internal capability, app command, parsing, query or frame-composition implementation; no public Rd topic/export. |
| `open_command_palette` | [command-palette.R](../R/command-palette.R#L92) | Internal capability, app command, parsing, query or frame-composition implementation; no public Rd topic/export. |
| `open_help` | [commands.R](../R/commands.R#L151) | Internal capability, app command, parsing, query or frame-composition implementation; no public Rd topic/export. |
| `search_rows` | [browse-data.R](../R/browse-data.R#L129) | Internal capability, app command, parsing, query or frame-composition implementation; no public Rd topic/export. |
| `column_summary` | [browse-data.R](../R/browse-data.R#L141) | Internal capability, app command, parsing, query or frame-composition implementation; no public Rd topic/export. |
| `stats_quantiles` | [browse-data.R](../R/browse-data.R#L158) | Internal capability, app command, parsing, query or frame-composition implementation; no public Rd topic/export. |
| `parse_uint32` | [terminal-windows.R](../R/terminal-windows.R#L24) | Internal capability, app command, parsing, query or frame-composition implementation; no public Rd topic/export. |
| `visible_area` | [testing.R](../R/testing.R#L30) | Internal capability, app command, parsing, query or frame-composition implementation; no public Rd topic/export. |
| `compose_frame` | [testing.R](../R/testing.R#L41) | Internal capability, app command, parsing, query or frame-composition implementation; no public Rd topic/export. |

There are many additional non-exported parsing, layout, painting, ANSI, validation, and cache helpers. This short list is the externally readable-looking subset selected for review, not a complete index of private functions.

## `termr:::` references

- In `tests/`, `inst/examples/`, `R/`, and internal helper code: **0** occurrences of `termr:::`.
- In user documentation: **1** occurrence, in [docs/extensions.md](extensions.md#compatibility-and-testing), which tells extension authors that triple-colon access is internal.

## Similar names, aliases, and overlaps

These exported names share topics or neighboring roles; none is marked deprecated in the current source/docs:

- `vertical()` / `horizontal()` construct `Vertical` / `Horizontal` R6 widgets; the constructors share one Rd topic. The CamelCase class and snake_case constructor convention is consistent across widgets.
- `browse_data()` runs the viewer; `data_browser()` builds and returns its `App`. They are related APIs with intentionally different behavior, not aliases.
- `modal()`, `confirm_dialog()`, and `alert_dialog()` share an Rd topic; the latter two are modal presets.
- `radio_set()` and `radio_button()` share an Rd topic but construct a group and an individual choice respectively.
- `tabs()` / `tab()` build a tab group and one pane; `Tabs` / `TabPane` are the corresponding classes.
- `run_worker()` / `run_process()` are parallel worker entry points for an R function and an external process.
- `set_timeout()` / `set_interval()` are paired timer helpers.
- `stylesheet()` / `stylesheet_file()` parse stylesheet text or a file.
- `render_text()` / `render_lines()` overlap; one returns a newline-joined scalar, the other a vector of lines. `render_widget()` returns the lower-level `ScreenBuffer`.
- `inspect_widget()`, `widget_snapshot()`, and `inspect_json()` cover rich inspection, a redacted structural snapshot, and JSON serialization of that snapshot.
- `table_source()` is the generic source protocol; `db_table_source()` supplies the SQLite/DB-backed adapter.
- Related highlighters use different naming patterns: `r_highlighter()` and `sql_highlighter()`; `sql_editor()` consumes the latter.
- `app()` constructs an `App`; `run()` starts it. `ScreenBuffer` / `screen_buffer()` similarly pair a class and factory.
- Low-level exported helpers include `patch_to_ansi()`, `diff_screen()`, and `strip_ansi()` alongside the widget-facing rendering APIs; their abstraction level differs from most constructors.

### Deprecated or legacy names

No exported function or class is marked deprecated in the current R source, Rd files, README, docs, examples, or NEWS. NEWS states that the 0.3 package rename introduced no compatibility aliases. The stylesheet property aliases and accepted color/key spellings are input synonyms, not deprecated exported symbols.

## TODO markers

No `TODO`, `FIXME`, `HACK`, or `XXX` markers were found in `R/`, `tests/`, or `docs/`.

## Review notes

- `24` direct exports have no textual reference in README/docs/examples/tests: `Animation`, `BlurEvent`, `Checkbox`, `FocusEvent`, `KeyValue`, `MarkdownView`, `Metric`, `MountEvent`, `OptionList`, `ProcessView`, `ProgressBar`, `RadioButton`, `RadioSet`, `Rule`, `Sparkline`, `Spinner`, `SplitHandle`, `SplitPane`, `TabPane`, `Timer`, `Toast`, `TreeNode`, `TreeView`, `UnmountEvent`.
  All are R6 class names; several are returned by constructors or represent event/widget types. This is a ?no direct reference? metric, not evidence that the classes are dead.
- `72` exported symbols lack examples in Rd (including `DataProfile`, `PropertyGrid`, `StatusBar` and `db_query_source`, added in 0.9): `Animation`, `App`, `BlurEvent`, `Button`, `Checkbox`, `DataTable`, `Event`, `FocusEvent`, `Grid`, `Horizontal`, `Input`, `KeyEvent`, `KeyValue`, `Label`, `LogView`, `MarkdownView`, `MessageEvent`, `Metric`, `ModalScreen`, `MountEvent`, `MouseEvent`, `OptionList`, `Panel`, `PasteEvent`, `ProcessView`, `ProgressBar`, `RadioButton`, `RadioSet`, `ResizeEvent`, `Rule`, `Screen`, `ScreenBuffer`, `ScrollView`, `Select`, `Sparkline`, `Spinner`, `SplitHandle`, `SplitPane`, `TabPane`, `Tabs`, `TextArea`, `Timer`, `Toast`, `TreeNode`, `TreeView`, `UnmountEvent`, `Vertical`, `Widget`, `Worker`, `current_app`, `db_connection`, `db_metadata`, `db_table_source`, `inspect_json`, `inspect_signal`, `knit_termr`, `r_highlighter`, `register_layout`, `render_svg`, `run`, `snapshot_json`, `sql_editor`, `sql_highlighter`, `table_filter`, `table_source`, `unregister_layout`, `widget_snapshot`, `write_rendered`.
- `0` exported symbols lack Rd usage: none.

## Reproduction

Counts and mappings above are based on `NAMESPACE`, top-level assignments under `R/`, `man/*.Rd`, and direct textual search in README, `docs/`, `inst/examples/`, and `tests/`. Re-run the inventory after changing exports or documentation; direct textual use is intentionally not treated as a semantic call graph.

