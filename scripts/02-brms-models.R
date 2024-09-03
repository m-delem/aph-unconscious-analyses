# install.packages("here")
library(here)
source(here("scripts/00-preprocessing.R"))

# Parallel processing variables -------------------------------------------

# detecting the number of cores to use
n_cores <- parallel::detectCores() - 4

# defining the number of iterations per chain (+ 1000 warm-up)
n_iter <- ceiling(40000 / n_cores) + 1000


options(cmdstanr_warn_inits = FALSE)


# Model formula -----------------------------------------------------------

formula <- bf(
  rt ~ aphantasia * congruence * color + 
    (congruence * color | subjectid))

formula <- bf(
  rt ~ aphantasia * congruence * color + 
    (1 | subjectid))

# Priors ------------------------------------------------------------------

get_prior(formula, data = df_i_rt, family = shifted_lognormal())
# need to define better priors for the intercept, sigma, b, and sd

prior <- c(
  prior(normal(-1, 0.5), class = 'Intercept'),  # around exp(-1) = 0.36 secs
  prior(normal(0.4, 0.3), class = 'sigma'), # SD of individual rts in log-units
  prior(normal(0, 0.3), class = 'b'), # around exp(-1) - exp(-1 + 0.3) = 150 ms 
  prior(normal(0.3, 0.1), class = 'sd')  # some variability between participants
)

# Implicit task model -----------------------------------------------------

fit_implicit <- 
  brm(
    formula = formula,
    data = df_i_rt,
    family = shifted_lognormal(),
    prior = prior,
    sample_prior = TRUE,
    chains = n_cores,
    cores  = n_cores,
    iter   = n_iter,
    warmup = 1000,
    refresh = 5,
    backend = "cmdstanr",
    stan_model_args = list(stanc_options = list("O1")),
    save_pars = save_pars(all = TRUE),
    file = here("data/r-data-structures/brms-model-implicit.rds"),
    file_compress = "xz"
  )


# Explicit task model -----------------------------------------------------


fit_explicit <- 
  brm(
    formula = formula,
    data = df_e_rt,
    family = shifted_lognormal(),
    prior = prior,
    sample_prior = TRUE,
    chains = n_cores,
    cores  = n_cores,
    iter   = n_iter,
    warmup = 1000,
    refresh = 5,
    backend = "cmdstanr",
    stan_model_args = list(stanc_options = list("O1")),
    save_pars = save_pars(all = TRUE),
    file = here("data/r-data-structures/brms-model-explicit.rds"),
    file_compress = "xz"
  )
