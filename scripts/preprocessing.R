# install.packages("here")
library(here)
source(here("scripts/_setup.R"))

# Questionnaires
df_questionnaires <- 
  read_excel(
    "data/data-raw/priming-data-raw.xlsx",
    sheet = "data_questionnaires"
  ) |> 
  set_variable_labels(
    subjectid = "Subject",
    age = "Age",
    sex = "Sex",
    aphantasia = "Group",
    vviq80 = "VVIQ",
    osiq_o75 = "OSIQ-Object",
    osiq_s75 = "OSIQ-Spatial",
    suis60 = "SUIS"
  )

# Explicit task -----------------------------------------------------------

df_e_acc <- 
  read_excel(
    "data/data-raw/priming-data-raw.xlsx",
    sheet = "data_explicit"
  ) |>
  clean_variables() |> # see _functions.R
  set_variable_labels(correct_explicit = "Correct response") |> 
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
    )) 
  ) |>  
  # removing irrelevant variables
  select(-c(sex, vviq80, orientation, response)) |>  
  # filtering out extreme RTs
  filter(rt > .3 & rt < 3)

df_e_rt <- 
  df_e_acc |> 
  filter(correct_explicit == 1) |> 
  select(!correct_explicit)


# Implicit task -----------------------------------------------------------


df_i_acc <- 
  read_excel(
    "data/data-raw/priming-data-raw.xlsx",
    sheet = "data_implicit"
  ) |> 
  clean_variables() |>
  set_variable_labels(correct_implicit = "Correct response") |>
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
      ))
  ) |>  
  # removing irrelevant variables
  select(-c(sex, vviq80, orientation, response)) |>  
  # filtering out extreme RTs
  filter(rt > .3 & rt < 3)

df_i_rt <-
  df_i_acc |> 
  filter(correct_implicit == 1) |> 
  select(!correct_implicit)


# Exporting to .xlsx ------------------------------------------------------
write.xlsx(
  list(
    "data_implicit_preprocessed" = df_i_acc,
    "data_implicit_no_errors" = df_i_rt,
    "data_explicit_preprocessed" = df_e_acc,
    "data_explicit_no_errors" = df_e_rt
  ),
  "data/data-transformed/priming-data-preprocessed.xlsx",
  asTable = TRUE,
  colNames = TRUE,
  colWidths = "auto",
  borders = "all",
  tableStyle = "TableStyleMedium16"
)
