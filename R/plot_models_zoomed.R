library(ggplot2)

# Plotting Groups and Congruence, zoomed ----------------------------------
plot_models_zoomed <- function(
    df, 
    group,
    estimates,
    dw    = .5,
    size  = 3,
    y_min = 400,
    y_max = 1110,
    ...
) {
  df |> 
    ggplot(
      aes(
        x = congruence, 
        y = rt, 
        color = .data[[group]], 
        fill = .data[[group]]
      )
    ) +
    geom_line(
      data        = estimates,
      mapping     = aes(y = median, group = .data[[group]]),
      position    = position_dodge(width = dw),
      linewidth   = 1.5,
      show.legend = FALSE
    ) +
    geom_errorbar(
      data      = estimates,
      mapping   = aes(y = median, ymin = median - SE, ymax = median + SE),
      position  = position_dodge(width = dw),
      width     = 0,
      linewidth = 1,
      color     = "black",
      show.legend = FALSE
    ) +
    see::geom_point2(
      data     = estimates,
      mapping  = aes(y = median),
      position = position_dodge(width = dw),
      size     = size,
      shape    = 21,
      stroke   = 1,
      color    = "black"
    ) +
    labs(x = NULL, y = NULL, fill = NULL) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 8)) +
    theme(
      axis.text.y        = element_text(size = 10),
      panel.grid.major.y = element_line(linewidth = 0.5, color = "grey90"),
      panel.grid.minor.y = element_line(linewidth = 0.5, color = "grey95")
    )
}