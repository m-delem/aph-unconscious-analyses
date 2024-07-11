# functions shared across scripts are placed here


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
      aphantasia = "Group",
      color = "Colour",
      congruence = "Congruence",
      rt = "Response Time (s)"
    )
}

