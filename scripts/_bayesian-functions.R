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
