# theme.R: creates the custom Bootstrap theme.

library(bslib)

theme <- bs_theme(
  bg = "#cbcdcd",
  fg = "#4c5355",
  primary = "#c44b4f",
  secondary = "#607d86",
  base_font = font_google("Open Sans"),
  heading_font = font_google("Open Sans"),
  code_font = font_google("Source Code Pro"),
  # Border for interactables
  "input-border-color" = "#c44b4f"
)
# if (interactive()) {
#   bs_theme_preview(theme)
# }

theme <- theme %>%
  # Inspector elements margin
  bs_add_rules(".row { margin: 30px 0px 30px 0px; }")
