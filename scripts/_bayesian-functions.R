# Functions shared across scripts are placed here


# Define contrasts inside a dplyr pipe ------------------------------------

define_contrasts <- function(df, col, contrast) {
  contrasts(df[[col]]) <- contrast
  return(df)
}


# Cleaning and labelling variables and factors ----------------------------

clean_variables <- function(df){
  df |>
    mutate(
      aphantasia = factor(
        aphantasia, 
        levels = c("no", "yes"), 
        labels = c("Control", "Aphantasia")),
      color = factor(
        color, 
        levels = c("uncoloured", "coloured"),
        labels = c("Uncoloured", "Coloured")),
      congruence = factor(
        congruence, 
        levels = c("uncongruent", "congruent"),
        labels = c("Incongruent", "Congruent"))
    ) |>
    define_contrasts("aphantasia", c(-0.5, 0.5)) |>
    define_contrasts("congruence", c(-0.5, 0.5)) |>
    define_contrasts("color", c(-0.5, 0.5)) |>
    set_variable_labels(
      subjectid = "Subject",
      age = "Age",
      aphantasia = "Group",
      color = "Colour",
      congruence = "Congruence",
      rt = "Response Time (s)"
    )
}


# Custom pp_check function ------------------------------------------------

custom_pp_check <- function(fit, ndraws = 50) {
  pp_check(
    fit, 
    ndraws = ndraws,
    type = "dens_overlay_grouped",
    group = "aphantasia"
    ) +
    scale_x_continuous(limits = c(0, 3), breaks = breaks_pretty()) +
    labs(
      x = "Response Time (s)",
      y = "Density"
    ) +
    theme(
      axis.ticks.x = element_line(linewidth = 0.5),
      panel.grid.major.x = element_line(linewidth = 0.5, color = "grey90"),
      panel.grid.minor.x = element_line(linewidth = 0.5, color = "grey95"),
      panel.spacing = unit(3, "lines")
    )
}


# Plotting fixed effects --------------------------------------------------

plot_fixed_effects <- function(fit){
  mcmc_areas(
    fixef(fit, summary = FALSE), 
    pars = vars(!contains("Intercept"))
  )
}


# Plotting varying effects ------------------------------------------------

plot_varying_effects <- function(fit, df) {
  fit |> 
    spread_draws(b_Intercept, r_subjectid[subjectid, ]) |>
    mutate(
      mu = b_Intercept + r_subjectid
    ) |>
    ungroup() |> 
    right_join(
      df |> 
        select(subjectid, aphantasia) |> 
        distinct(), 
      by = "subjectid"
    ) |> 
    ggplot(aes(
      x = mu, 
      y = reorder(subjectid, mu),
      color = aphantasia,
      fill = aphantasia
    )) +
    geom_vline(
      xintercept = fixef(fit)[1, 1], 
      color = "#839496", 
      linewidth = 1
    ) +
    geom_vline(
      xintercept = fixef(fit)[1, 3:4], 
      color = "#839496", 
      linetype = 2
    ) +
    stat_halfeye(
      .width = c(.5),
      size  = 2,
      slab_alpha = .6,
    ) +
    scale_x_continuous(breaks = breaks_pretty()) +
    theme(
      axis.ticks.x = element_line(linewidth = 0.5),
      axis.text.y = element_text(size = 6)
    )
}

# Plotting Groups and Congruence, full view -------------------------------

plot_models_full_bayes <- function(
    df, 
    estimates,
    dw = .4,
    size = 3,
    alpha_subj = .3,
    nudge = .11,
    y_min = 400,
    y_max = 1110,
    ...
) {
  
  df |> 
    ggplot(aes(x = congruence, y = rt, color = aphantasia, fill = aphantasia)) +
    geom_violinhalf(
      trim = TRUE,
      flip = c(1, 2),
      alpha = .5,
      scale = "width",
      position = position_dodge(width = dw),
      show.legend = FALSE
    ) +
    # individual subject lines
    geom_line(
      data = df |> filter(aphantasia == "Aphantasia"),
      aes(group = interaction(subjectid, aphantasia)),
      position = position_nudge(nudge),
      alpha = alpha_subj,
      show.legend = FALSE
    ) +
    geom_line(
      data = df |> filter(aphantasia == "Control"),
      aes(group = interaction(subjectid, aphantasia)),
      position = position_nudge(-nudge),
      alpha = alpha_subj,
      show.legend = FALSE
    ) +
    # individual subject points
    geom_quasirandom(
      color = "white",
      pch = 21,
      width = .05,
      size = 1.8,
      alpha = .6,
      dodge.width = .4,
      show.legend = FALSE
    ) +
    # group median lines
    geom_line(
      data = estimates,
      aes(y = median, group = aphantasia),
      position = position_dodge(width = dw),
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
    geom_point2(
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
      breaks = breaks_pretty(),
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
    dw = .5,
    size = 3,
    y_min = 400,
    y_max = 1110,
    ...
) {
  
  df |> 
    ggplot(aes(x = congruence, y = rt, color = aphantasia, fill = aphantasia)) +
    geom_line(
      data = estimates,
      aes(y = median, group = aphantasia),
      position = position_dodge(width = dw),
      linewidth = 1.5,
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
    geom_point2(
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
      breaks = breaks_pretty(n = 8),
    ) +
    theme(
      axis.text.y = element_text(size = 10),
      # axis.ticks.y = element_line(linewidth = 0.5),
      panel.grid.major.y = element_line(linewidth = 0.5, color = "grey90"),
      panel.grid.minor.y = element_line(linewidth = 0.5, color = "grey95")
    )
}
