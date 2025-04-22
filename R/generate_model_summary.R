library(dplyr)

# Table of a questionnaire model ------------------------------------------
generate_model_summary <- function(
    model, 
    model_ranked, 
    questionnaire_name, 
    group = "aphantasia"
){
  table_summary <-
    modelbased::estimate_means(model, by = group, p_adjust = "none") |> 
    select(!c(CI_low, CI_high)) |>
    mutate(across(c(Mean, SE), ~as.character(round(.x, digits = 3)))) |> 
    tidyr::unite("Mean ± SE", Mean:SE, sep = " ± ") |> 
    tidyr::pivot_wider(
      names_from = group,
      values_from = "Mean ± SE"
    ) |> 
    mutate(Questionnaire = questionnaire_name) |> 
    select(Questionnaire, everything()) |> 
    bind_cols(
      modelbased::estimate_contrasts(
        model_ranked, 
        contrast = group, 
        p_adjust = "none")[,7:9]
    ) 
  
  return(table_summary)
}

generate_model_means <- function(
    model, 
    questionnaire_name, 
    group = "aphantasia"
){
  table_means <-
    modelbased::estimate_means(model, by = group, p_adjust = "none") |> 
    select(!c(CI_low, CI_high)) |>
    mutate(across(c(Mean, SE), ~as.character(round(.x, digits = 3)))) |> 
    tidyr::unite("Mean ± SE", Mean:SE, sep = " ± ") |> 
    tidyr::pivot_wider(
      names_from = group,
      values_from = "Mean ± SE"
    ) |> 
    mutate(Questionnaire = questionnaire_name) |> 
    select(Questionnaire, everything())
  
  return(table_means)
}