# install.packages("here")
library(here)
source(here("scripts/preprocessing.R"))

# Parallel processing variables -------------------------------------------

# detecting the number of cores to use
n_cores <- parallel::detectCores() - 1

# defining the number of iterations per chain (+ 1000 warm-up)
n_iter <- ceiling(40000 / n_cores) + 1000


# Formulas ----------------------------------------------------------------

formula_1 <- rt ~ 
