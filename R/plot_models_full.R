library(ggplot2)

# Plotting Groups and Congruence, full view ------------------------------------
plot_models_full <- function(
    df, 
    group,
    estimates,
    dw    = .4,
    size  = 3,
    nudge = .11,
    y_min = 400,
    y_max = 1110,
    alpha_subj = .3,
    ...
) {
  
  if (group == "aphantasia"){
    df |> 
      ggplot(
        aes(
          x     = congruence, 
          y     = rt, 
          color = .data[[group]], 
          fill  = .data[[group]]
        )
      ) +
      see::geom_violinhalf(
        trim  = TRUE,
        flip  = c(1, 2),
        alpha = .5,
        scale = "width",
        position    = position_dodge(width = dw),
        show.legend = FALSE
      ) +
      # individual subject lines
      geom_line(
        data     = df |> filter(.data[[group]] == "Aphantasia"),
        mapping  = aes(group = interaction(subjectid, .data[[group]])),
        position = position_nudge(nudge),
        alpha    = alpha_subj,
        show.legend = FALSE
      ) +
      geom_line(
        data     = df |> filter(.data[[group]] == "Control"),
        mapping  = aes(group = interaction(subjectid, .data[[group]])),
        position = position_nudge(-nudge),
        alpha    = alpha_subj,
        show.legend = FALSE
      ) +
      # individual subject points
      ggbeeswarm::geom_quasirandom(
        color = "white",
        pch   = 21,
        width = 0.05,
        size  = 1.8,
        alpha = 0.6,
        dodge.width = dw,
        show.legend = FALSE
      ) +
      # group median lines
      geom_line(
        data      = estimates,
        mapping   = aes(y = median, group = .data[[group]]),
        position  = position_dodge(width = dw),
        linewidth = 1,
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
      labs(x = NULL, fill = NULL) +
      scale_y_continuous(
        name   = "Response time (ms)",
        breaks = scales::breaks_pretty(),
        limits = c(y_min, y_max)
      ) +
      theme(
        axis.ticks.y       = element_line(linewidth = 0.5),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey90"),
        panel.grid.minor.y = element_line(linewidth = 0.5, color = "grey95")
      )
  } else {
    df |> 
      ggplot(
        aes(
          x     = congruence, 
          y     = rt, 
          color = .data[[group]], 
          fill  = .data[[group]])) +
      see::geom_violinhalf(
        trim     = TRUE,
        flip     = c(1, 2, 3),
        alpha    = .5,
        scale    = "width",
        position = position_dodge(width = dw),
        show.legend = FALSE
      ) +
      # group median lines
      geom_line(
        data        = estimates,
        mapping     = aes(y = median, group = .data[[group]]),
        position    = position_dodge(width = dw),
        linewidth   = 1,
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
      labs(x = NULL, fill = NULL) +
      scale_y_continuous(
        name   = "Response time (ms)",
        breaks = breaks_pretty(),
        limits = c(y_min, y_max)
      ) +
      theme(
        axis.ticks.y       = element_line(linewidth = 0.5),
        panel.grid.major.y = element_line(linewidth = 0.5, color = "grey90"),
        panel.grid.minor.y = element_line(linewidth = 0.5, color = "grey95")
      )
  }
}
