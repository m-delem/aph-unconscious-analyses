pacman::p_load(dplyr, ggplot2)

# Plotting fixed effects --------------------------------------------------
plot_fixed_effects <- function(fit){
  bayesplot::mcmc_areas(
    brms::fixef(fit, summary = FALSE), 
    pars = vars(!tidyselect::contains("Intercept"))
  )
}

# Plotting varying effects ------------------------------------------------
plot_varying_effects <- function(fit, df) {
  fit |> 
    tidybayes::spread_draws(b_Intercept, r_subjectid[subjectid, ]) |>
    mutate(mu = b_Intercept + r_subjectid) |>
    ungroup() |> 
    right_join(
      df |> 
        select(subjectid, aphantasia) |> 
        distinct(), 
      by = "subjectid"
    ) |> 
    ggplot(aes(
      x     = mu, 
      y     = reorder(subjectid, mu),
      color = aphantasia,
      fill  = aphantasia
    )) +
    geom_vline(
      xintercept = brms::fixef(fit)[1, 1], 
      color      = "#839496", 
      linewidth  = 1
    ) +
    geom_vline(
      xintercept = brms::fixef(fit)[1, 3:4], 
      color      = "#839496", 
      linetype   = 2
    ) +
    ggdist::stat_halfeye(
      .width     = c(.5),
      size       = 2,
      slab_alpha = .6,
    ) +
    scale_x_continuous(breaks = scales::breaks_pretty()) +
    theme(
      axis.ticks.x = element_line(linewidth = 0.5),
      axis.text.y  = element_text(size = 6)
    )
}

# Plotting Groups and Congruence, full view -------------------------------
plot_models_full_bayes <- function(
    df, 
    estimates,
    dw    = .4,
    size  = 3,
    nudge = .11,
    y_min = 400,
    y_max = 1110,
    alpha_subj = .3,
    ...
) {
  df |> 
    ggplot(aes(x = congruence, y = rt, color = aphantasia, fill = aphantasia)) +
    see::geom_violinhalf(
      trim     = TRUE,
      flip     = c(1, 2),
      alpha    = .5,
      scale    = "width",
      position = position_dodge(width = dw),
      show.legend = FALSE
    ) +
    # individual subject lines
    geom_line(
      data     = df |> filter(aphantasia == "Aphantasia"),
      mapping  = aes(group = interaction(subjectid, aphantasia)),
      position = position_nudge(nudge),
      alpha    = alpha_subj,
      show.legend = FALSE
    ) +
    geom_line(
      data     = df |> filter(aphantasia == "Control"),
      mapping  = aes(group = interaction(subjectid, aphantasia)),
      position = position_nudge(-nudge),
      alpha    = alpha_subj,
      show.legend = FALSE
    ) +
    # individual subject points
    ggbeeswarm::geom_quasirandom(
      color = "white",
      pch   = 21,
      width = .05,
      size  = 1.8,
      alpha = .6,
      dodge.width = .4,
      show.legend = FALSE
    ) +
    # group median lines
    geom_line(
      data      = estimates,
      mapping   = aes(y = median, group = aphantasia),
      position  = position_dodge(width = dw),
      linewidth = 1,
      show.legend = FALSE
    ) +
    geom_errorbar(
      data = estimates,
      aes(y = median, ymin = lower.HPD, ymax = upper.HPD),
      position = position_dodge(width = dw),
      width = 0,
      linewidth = 1,
      color = "black",
      show.legend = FALSE
    ) +
    see::geom_point2(
      data = estimates,
      aes(y = median),
      position = position_dodge(width = dw),
      size = size,
      shape = 21,
      stroke = 1,
      color = "black"
    ) +
    labs(x = NULL, fill = NULL) +
    scale_y_continuous(
      name = "Response time (ms)",
      breaks = scales::breaks_pretty(),
      limits = c(y_min, y_max)
    ) +
    theme(
      axis.ticks.y = element_line(linewidth = 0.5),
      panel.grid.major.y = element_line(linewidth = 0.5, color = "grey90"),
      panel.grid.minor.y = element_line(linewidth = 0.5, color = "grey95")
    )
}

# Plotting Groups and Congruence, zoomed ----------------------------------

plot_models_zoomed_bayes <- function(
    df, 
    estimates,
    dw    = .5,
    size  = 3,
    y_min = 400,
    y_max = 1110,
    ...
) {
  df |> 
    ggplot(aes(x = congruence, y = rt, color = aphantasia, fill = aphantasia)) +
    geom_line(
      data      = estimates,
      mapping   = aes(y = median, group = aphantasia),
      position  = position_dodge(width = dw),
      linewidth = 1.5,
      show.legend = FALSE
    ) +
    geom_errorbar(
      data     = estimates,
      mapping  = aes(y = median, ymin = lower.HPD, ymax = upper.HPD),
      position = position_dodge(width = dw),
      width = 0,
      linewidth = 1,
      color = "black",
      show.legend = FALSE
    ) +
    see::geom_point2(
      data = estimates,
      aes(y = median),
      position = position_dodge(width = dw),
      size = size,
      shape = 21,
      stroke = 1,
      color = "black"
    ) +
    labs(x = NULL, y = NULL, fill = NULL) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 8),
    ) +
    theme(
      axis.text.y = element_text(size = 10),
      # axis.ticks.y = element_line(linewidth = 0.5),
      panel.grid.major.y = element_line(linewidth = 0.5, color = "grey90"),
      panel.grid.minor.y = element_line(linewidth = 0.5, color = "grey95")
    )
}
