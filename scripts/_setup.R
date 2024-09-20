
# Packages ----------------------------------------------------------------

# using a reproducible environment
renv::restore()

# the cmdstanr package for Bayesian modelling has to be installed manually
# install.packages(
#   "cmdstanr", 
#   repos = c('https://stan-dev.r-universe.dev', getOption("repos")))

# The cmdstan backend, if not already installed, has to be installed on your 
# computer first, outside of the project:
# check_cmdstan_toolchain() # check if RTools is setup
# nb_cores <- parallel::detectCores() - 1
# install_cmdstan(cores = nb_cores)

# pacman allows to check/install/load packages with a single call
# if (!require("pacman")) install.packages("pacman") # already in renv.lock
library("pacman")

# packages to load (and install if needed) -------------------------------
pacman::p_load(
  here,      # easy file paths
  see,       # theme_modern and okabeito palette
  report,    # reporting various info 
  labelled,  # labelled data
  quarto,
  # ---- Modelling
  easystats, # modelling package framework
  lme4,      # mixed-effects models
  car,       # companion to lme4
  simr,      # power analysis
  statmod,   # power analysis
  emmeans,   # post-hoc tests
  # ---- Bayesian modelling
  brms,      # Bayesian regression models
  tidybayes, # tidy output for brms
  bayesplot, # Bayesian visualisations
  cmdstanr,  # Stan interface
  
  # ---- Visualisations
  qqplotr,    # QQ plots
  scales,     # ggplot2 scales
  latex2exp,  # LaTeX expressions in ggplot2
  ggbeeswarm, # beeswarm plots
  ggpubr,     # publication-ready plots
  patchwork,  # combining plots
  # ---- Data wrangling
  readxl,
  openxlsx,
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

