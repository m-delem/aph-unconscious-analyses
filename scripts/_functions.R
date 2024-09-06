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
  function(model, model_ranked, questionnaire_name){
    table_summary <-
      estimate_means(model, by = "aphantasia", p_adjust = "none") |> 
      select(!c(CI_low, CI_high)) |>
      mutate(across(c(Mean, SE), ~as.character(round(.x, digits = 3)))) |> 
      unite("Mean ± SE", Mean:SE, sep = " ± ") |> 
      pivot_wider(
        names_from = aphantasia,
        values_from = "Mean ± SE"
      ) |> 
      mutate(Questionnaire = questionnaire_name) |> 
      rename(
        "Aphantasics" = yes,
        "Controls" = no
      ) |> 
      select(Questionnaire, everything()) |> 
      bind_cols(
        estimate_contrasts(
          model_ranked, 
          contrast = "aphantasia", 
          p_adjust = "none")[,7:9]
      ) 
    
    return(table_summary)
  }


# Prepare marginal means for display ---------------------------------------

prepare_means <- function(model, questionnaire){
  estimate_means(model, by = "aphantasia") |> 
    mutate(Questionnaire = questionnaire)
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
