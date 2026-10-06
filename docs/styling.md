# Styling

## Inline styles

```r
button("Run", style = style(
  width = 20, padding = c(0, 2), border = "round", align = "center",
  foreground = "white", background = "$primary", bold = TRUE,
  focus = style(border_color = "$accent"),
  hover = style(reverse = TRUE),
  disabled = style(foreground = "$muted"),
  states = list(pressed = style(background = "$success"))
))
```

Colours: ANSI names (`"red"`, `"bright_cyan"`, `"grey"`), 0-255, hex
(`"#ff8700"`), R colour names (`"steelblue"`), or theme colours
(`"$primary"`). They are downgraded automatically for 256 / 16 colour
terminals; `NO_COLOR` disables colour.

## Stylesheets (RTCSS)

A small CSS-like language, named RTCSS because it is only CSS-like:

```css
/* comments */
Button { border: round; padding: 0 2; }
Button.danger { background: $error; }
#save { width: 20; }
Input:focus { border: heavy $accent; }
Input:invalid { border-color: $error; }
Vertical > Label { bold: true; }
Horizontal Label, .note { color: $muted; }
```

```r
app(ui, stylesheet = "Button { border: round; }")
app(ui, stylesheet = "styles/app.rtcss")         # a file
app$add_stylesheet(stylesheet(text))
```

Properties are the `style()` arguments (`border-color` or `border_color`;
also `color`, `text-align`, `background-color`). `border` accepts a
colour: `border: round cyan`. Pseudo classes: `:focus`, `:hover`,
`:disabled`, `:pressed`, `:invalid`.

Errors point at the problem: `<stylesheet>:2:3: property "border": ...`.

### Cascade

From lowest to highest priority:

1. the widget type's built-in style (and its state styles);
2. stylesheet rules, by specificity (ids, then classes and pseudo
   classes, then types), then source order;
3. the widget's own `style`;
4. the state styles of the widget's own `style`.

## Themes

```r
app(ui, theme = "dark")                         # "default", "dark", "light"
app$theme <- termr_theme("light", primary = "#8a2be2")
```

Theme colours: `foreground`, `background`, `surface`, `muted`, `primary`,
`on_primary`, `accent`, `success`, `warning`, `error`, `information`,
`stripe`, plus your own. The `default` theme uses the terminal's 16-colour
palette. Named `termr_theme()` so it does not mask `ggplot2::theme()`.

## Themes, contrast and colour-free terminals

Built-in themes: `"default"`, `"dark"`, `"light"`, `"high-contrast"`. Without
colour (`NO_COLOR`) highlights become reverse video, errors bold underline and
the focused widget gets a heavier border. See [terminal.md](terminal.md).
