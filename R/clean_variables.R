# Cleaning and labelling variables ---------------------------------------------
clean_variables <- function(df){
  # Helper to define contrasts inside a pipe
  define_contrasts <- function(df, col, contrast) {
    contrasts(df[[col]]) <- contrast
    return(df)
  }
  
  df |>
    dplyr::mutate(
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
    labelled::set_variable_labels(
      subjectid = "Subject",
      age = "Age",
      aphantasia = "Group",
      color = "Colour",
      congruence = "Congruence",
      rt = "Response Time (s)"
    )
}