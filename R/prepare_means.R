# Prepare marginal means for display ---------------------------------------
prepare_means <- function(model, questionnaire, group = "aphantasia"){
  model |> 
    modelbased::estimate_means(by = group) |> 
    dplyr::mutate(Questionnaire = questionnaire)
}