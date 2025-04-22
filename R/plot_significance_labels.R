# Significance labels for plots -------------------------------------------
plot_significance_labels <- function(
    plot = plot,
    # stars
    label = label,
    size = 8,
    # label placement
    # star
    x_star = 1.5,
    y_star = 1.05,
    # line
    x_line = 1,
    x_line_end = 1.95,
    y_line = 1.03,
    ...
){
  list(
    # ─── Significance labels ───────────────────────────────────────────────────
    ggplot2::geom_text(
      x = x_star,
      y = y_star,
      label = label,
      # inherit.aes = FALSE,
      color = "black",
      size = size
    ),
    ggplot2::geom_segment(
      x = x_line,
      xend = x_line_end,
      y    = y_line,
      yend = y_line,
      # inherit.aes = FALSE,
      color = "black",
      linewidth = .5
    )
  )
}