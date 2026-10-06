# printing shows the tree

    Code
      print(ui)
    Output
      <Vertical>
      ├── <Label #greeting> "Hello"
      └── <Horizontal>
          └── <Label .x> "a"

# render_widget snapshot of a nested layout

    Code
      print(render_widget(ui, 40, 8))
    Output
      <ScreenBuffer 40x8>
      +----------------------------------------+
      |                 termr                  |
      |╭──────────────────╮┌──────────────────┐|
      |│left pane         ││       right pane │|
      |│                  ││                  │|
      |│                  ││                  │|
      |╰──────────────────╯└──────────────────┘|
      |  footer                                |
      |                                        |
      +----------------------------------------+

