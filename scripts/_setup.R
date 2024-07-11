
# Packages ----------------------------------------------------------------

# pacman allows to check/install/load packages with a single call
# if (!require("pacman")) install.packages("pacman") # already in renv.lock
library("pacman")

# packages to load (and install if needed) -------------------------------
pacman::p_load(
  here,      # easy file paths
  see,       # theme_modern and okabeito palette
  report,    # reporting various info 
  labelled,  # labelled data
  # --- packages specific to this project ---
  brms,
  rtdists,
  cmdstanr,
  simr,
  readxl,
  openxlsx,
  # ---
  tidyverse  # modern R ecosystem
)


# Custom functions shared across scripts ----------------------------------
source(here("scripts/_functions.R"))


# Global cosmetic theme ---------------------------------------------------

theme_set(theme_modern(base_size = 14)) # from see in easystats

# setting my favourite palettes as ggplot2 defaults
options( 
  ggplot2.discrete.colour   = scale_colour_okabeito,
  ggplot2.discrete.fill     = scale_fill_okabeito,
  ggplot2.continuous.colour = scale_colour_viridis_c,
  ggplot2.continuous.fill   = scale_fill_viridis_c
)


# Fixing a seed for reproducibility ---------------------------------------
set.seed(14051998)


# Adding all packages' citations to a .bib --------------------------------
knitr::write_bib(c(.packages()), file = here("bibliography/packages.bib"))


# Loading mandatory datasets ----------------------------------------------

# Implicit task
df_implicit <- 
  read_excel(
    "data/data-raw/priming-data-raw.xlsx",
    sheet = "data_implicit"
  )

# Explicit task
df_explicit <- 
  read_excel(
    "data/data-raw/priming-data-raw.xlsx",
    sheet = "data_explicit"
  )

# Questionnaires
df_questionnaires <- 
  read_excel(
    "data/data-raw/priming-data-raw.xlsx",
    sheet = "data_questionnaires"
  )


# Parallel processing -----------------------------------------------------

# detecting the number of cores to use
n_cores <- parallel::detectCores() - 1

# defining the number of iterations per chain (+ 1000 warm-up)
n_iter <- ceiling(40000 / n_cores) + 1000
