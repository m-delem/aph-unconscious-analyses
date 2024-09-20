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


# Plotting Groups and Congruence, full view -------------------------------

plot_models_full <- function(
    df, 
    group,
    estimates,
    dw = .4,
    size = 3,
    alpha_subj = .3,
    nudge = .11,
    y_min = 400,
    y_max = 1110,
    ...
    ) {
  
  if (group == "aphantasia"){
    df |> 
      ggplot(aes(x = congruence, y = rt, color = .data[[group]], fill = .data[[group]])) +
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
        data = df |> filter(.data[[group]] == "Aphantasia"),
        aes(group = interaction(subjectid, .data[[group]])),
        position = position_nudge(nudge),
        alpha = alpha_subj,
        show.legend = FALSE
      ) +
      geom_line(
        data = df |> filter(.data[[group]] == "Control"),
        aes(group = interaction(subjectid, .data[[group]])),
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
        dodge.width = dw,
        show.legend = FALSE
      ) +
      # group median lines
      geom_line(
        data = estimates,
        aes(y = median, group = .data[[group]]),
        position = position_dodge(width = dw),
        linewidth = 1,
        show.legend = FALSE
      ) +
      geom_errorbar(
        data = estimates,
        aes(y = median, ymin = median - SE, ymax = median + SE),
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
  }else{
    df |> 
      ggplot(aes(x = congruence, y = rt, color = .data[[group]], fill = .data[[group]])) +
      geom_violinhalf(
        trim = TRUE,
        flip = c(1, 2, 3),
        alpha = .5,
        scale = "width",
        position = position_dodge(width = dw),
        show.legend = FALSE
      ) +
      # group median lines
      geom_line(
        data = estimates,
        aes(y = median, group = .data[[group]]),
        position = position_dodge(width = dw),
        linewidth = 1,
        show.legend = FALSE
      ) +
      geom_errorbar(
        data = estimates,
        aes(y = median, ymin = median - SE, ymax = median + SE),
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
}


# Plotting Groups and Congruence, zoomed ----------------------------------

plot_models_zoomed <- function(
    df, 
    group,
    estimates,
    dw = .5,
    size = 3,
    y_min = 400,
    y_max = 1110,
    ...
    ) {
  
  df |> 
    ggplot(aes(x = congruence, y = rt, color = .data[[group]], fill = .data[[group]])) +
    geom_line(
      data = estimates,
      aes(y = median, group = .data[[group]]),
      position = position_dodge(width = dw),
      linewidth = 1.5,
      show.legend = FALSE
    ) +
    geom_errorbar(
      data = estimates,
      aes(y = median, ymin = median - SE, ymax = median + SE),
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


# Table of a questionnaire model ------------------------------------------

generate_model_summary <- 
  function(model, model_ranked, questionnaire_name, group = "aphantasia"){
    table_summary <-
      estimate_means(model, by = group, p_adjust = "none") |> 
      select(!c(CI_low, CI_high)) |>
      mutate(across(c(Mean, SE), ~as.character(round(.x, digits = 3)))) |> 
      unite("Mean ± SE", Mean:SE, sep = " ± ") |> 
      pivot_wider(
        names_from = group,
        values_from = "Mean ± SE"
      ) |> 
      mutate(Questionnaire = questionnaire_name) |> 
      # rename(
      #   "Aphantasics" = yes,
      #   "Controls" = no
      # ) |> 
      select(Questionnaire, everything()) |> 
      bind_cols(
        estimate_contrasts(
          model_ranked, 
          contrast = group, 
          p_adjust = "none")[,7:9]
      ) 
    
    return(table_summary)
  }

generate_model_means <- 
  function(model, questionnaire_name, group = "aphantasia"){
    table_means <-
      estimate_means(model, by = group, p_adjust = "none") |> 
      select(!c(CI_low, CI_high)) |>
      mutate(across(c(Mean, SE), ~as.character(round(.x, digits = 3)))) |> 
      unite("Mean ± SE", Mean:SE, sep = " ± ") |> 
      pivot_wider(
        names_from = group,
        values_from = "Mean ± SE"
      ) |> 
      mutate(Questionnaire = questionnaire_name) |> 
      select(Questionnaire, everything())
    
    return(table_means)
  }


# Prepare marginal means for display ---------------------------------------

prepare_means <- function(model, questionnaire, group = "aphantasia"){
  estimate_means(model, by = group) |> 
    mutate(Questionnaire = questionnaire)
}

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
    geom_text(
      x = x_star,
      y = y_star,
      label = label,
      # inherit.aes = FALSE,
      color = "black",
      size = size
    ),
    geom_segment(
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

# Plotting correlations ---------------------------------------------------

plot_correlations <- function(df, x, y){
  ggscatter(
    data = df,
    x = x,
    y = y,
    color = "aphantasia",
    shape = "aphantasia",
    mean.point = TRUE,
    mean.point.size = 5,
    star.plot = TRUE,
    star.plot.lwd = .08
    ) +
    geom_smooth(
      method = "lm",
      formula = y ~ x,
      color = "black",
      linewidth = .8,
      alpha = .2,
      fullrange = TRUE
    ) +
    scale_x_continuous(
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    scale_color_okabeito(
      name = "Group:  ", 
      labels = c(" Control   ", " Aphantasic")
    ) +
    scale_shape_manual(
      name = "Group:  ", 
      labels = c(" Control   ", " Aphantasic"),
      values = c(16, 17)
    ) 
}


# Bayesian functions -------------------------------------------------------

# Custom brms update function ----------------------------------------------

custom_update <- function(fit, data, filename, ...) {
  
  # detecting the number of cores to use
  n_cores <- parallel::detectCores() - 4
  
  # defining the number of iterations per chain (+ 1000 warm-up)
  n_iter <- ceiling(40000 / n_cores) + 1000
  
  update(
    fit, 
    newdata = data,
    chains = n_cores,
    cores  = n_cores,
    iter   = n_iter,
    file = here(filename),
    file_compress = "xz",
    ...
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
