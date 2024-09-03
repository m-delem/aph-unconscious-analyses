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


# Plotting Groups and Congruence, zoomed ----------------------------------

plot_models_zoomed <- function(
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
