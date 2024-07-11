# install.packages("here")
library(here)
source(here("scripts/_setup.R"))

# Explicit task -----------------------------------------------------------


df_e_rt <-
  df_explicit |>
  clean_variables() |> # see _functions.R
  # filtering out...
  filter(
    # participants identified with with high error rates
    !(subjectid %in% c( 
      "subject_7",
      "subject_94", 
      "subject_25", 
      "subject_4",
      "subject_97")) &
    # participants with aberrant means
    !(subjectid %in% c(
      "subject_49",
      "subject_59",
      "subject_107",
      "subject_100",
      "subject_73",
      "subject_106",
      "subject_119"
    )) &
    correct_explicit == 1
  ) |>  
  # removing irrelevant variables
  select(-c(sex, vviq80, orientation, response, correct_explicit)) |>  
  # filtering out extreme RTs
  filter(rt > .3 & rt < 3)


# Implicit task -----------------------------------------------------------


df_i_rt <- 
  df_implicit |>
  clean_variables() |>
  # filtering out...
  filter(
    # participants identified with with high error rates
    !(subjectid %in% c(
      "subject_21",
      "subject_56",
      "subject_9")) &
      # participants with aberrant means
      !(subjectid %in% c(
        "subject_49",
        "subject_107",
        "subject_30",
        "subject_120",
        "subject_127"
      )) &
      correct_implicit == 1
  ) |>  
  # removing irrelevant variables
  select(-c(sex, vviq80, orientation, response, correct_implicit)) |>  
  # filtering out extreme RTs
  filter(rt > .3 & rt < 3)

# saving
write.xlsx(
  list(
    "data_implicit_preprocessed" = df_e_rt,
    "data_explicit_preprocessed" = df_i_rt
  ),
  "data/data-transformed/priming-data-preprocessed.xlsx",
  asTable = TRUE,
  colNames = TRUE,
  colWidths = "auto",
  borders = "all",
  tableStyle = "TableStyleMedium16"
)
