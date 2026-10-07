# printing shows the tree

    Code
      print(ui)
    Output
      <Vertical>
      <U+251C><U+2500><U+2500> <Label #greeting> "Hello"
      <U+2514><U+2500><U+2500> <Horizontal>
          <U+2514><U+2500><U+2500> <Label .x> "a"

# render_widget snapshot of a nested layout

    Code
      print(render_widget(ui, 40, 8))
    Output
      <ScreenBuffer 40x8>
      +----------------------------------------+
      |                 termr                  |
      |+------------------++------------------+|
      ||left pane         ||       right pane ||
      ||                  ||                  ||
      ||                  ||                  ||
      |+------------------++------------------+|
      |  footer                                |
      |                                        |
      +----------------------------------------+

