# install.packages("here")
library(here)
source(here("scripts/preprocessing.R"))

# Parallel processing variables -------------------------------------------

# detecting the number of cores to use
n_cores <- parallel::detectCores() - 4

# defining the number of iterations per chain (+ 1000 warm-up)
n_iter <- ceiling(40000 / n_cores) + 1000


options(cmdstanr_warn_inits = FALSE)


# Model 5 -----------------------------------------------------------------

formula_5 <- bf(rt ~ aphantasia * congruence * color)

get_prior(formula_5, data = df_i_rt, family = shifted_lognormal())

prior_5 <- c(
  prior(normal(-1, 0.5), class = 'Intercept'),  # around exp(-1) = 0.36 secs
  prior(normal(0.4, 0.3), class = 'sigma'), # SD of individual rts in log-units
  prior(normal(0, 0.3), class = 'b') # around exp(-1) - exp(-1 + 0.3) = 150 ms 
)

fit_5 <- 
  brm(
    formula = formula_5,
    data = df_i_rt,
    family = shifted_lognormal(),
    prior  = prior_5,
    sample_prior = TRUE,
    chains = n_cores,
    cores  = n_cores,
    iter = n_iter,
    warmup  = 1000,
    refresh = 5,
    backend = "cmdstanr",
    stan_model_args = list(stanc_options = list("O1")),
    save_pars = save_pars(all = TRUE),
    file = here("data/r-data-structures/brms-model-5.rds"),
    file_compress = "xz"
  )

# summary(fit_5)
# custom_pp_check(fit_5)
# plot_fixed_effects(fit_5)
# plot(conditional_effects(fit_5), ask = FALSE)
# hypothesis(fit_5, "aphantasia1:congruence1 > 0") # BF10 = 167

# Model 6 -----------------------------------------------------------------

formula_6 <- bf(rt ~ aphantasia * congruence * color + (1 | subjectid))

get_prior(formula_6, data = df_i_rt, family = shifted_lognormal())

prior_6 <- c(
  prior(normal(-1, 0.5), class = 'Intercept'),  # around exp(-1) = 0.36 secs
  prior(normal(0.4, 0.3), class = 'sigma'), # SD of individual rts in log-units
  prior(normal(0, 0.3), class = 'b'), # around exp(-1) - exp(-1 + 0.3) = 150 ms 
  prior(normal(0.3, 0.1), class = 'sd')  # some variability between participants
)

fit_6 <- 
  brm(
    formula = formula_6,
    data = df_i_rt,
    family = shifted_lognormal(),
    prior = prior_6,
    sample_prior = TRUE,
    chains =n_cores,
    cores  = n_cores,
    iter = n_iter,
    warmup = 1000,
    refresh = 5,
    backend = "cmdstanr",
    stan_model_args = list(stanc_options = list("O1")),
    save_pars = save_pars(all = TRUE),
    file = here("data/r-data-structures/brms-model-6.rds"),
    file_compress = "xz"
  )

# summary(fit_6)
# custom_pp_check(fit_6)
# plot_fixed_effects(fit_6)
# plot(conditional_effects(fit_6), ask = FALSE)
# plot_varying_effects(fit_6, df_i_rt)
# hypothesis(fit_6, "aphantasia1:congruence1 > 0") # BF10 = 329

# Model 7 -----------------------------------------------------------------

formula_7 <- bf(rt ~ aphantasia * congruence * color + (congruence | subjectid))

get_prior(formula_7, data = df_i_rt, family = shifted_lognormal())

fit_7 <- 
  brm(
    formula = formula_7,
    data = df_i_rt,
    family = shifted_lognormal(),
    prior = prior_6,
    sample_prior = TRUE,
    chains = n_cores,
    cores  = n_cores,
    iter   = n_iter,
    warmup = 1000,
    refresh = 5,
    backend = "cmdstanr",
    stan_model_args = list(stanc_options = list("O1")),
    save_pars = save_pars(all = TRUE),
    file = here("data/r-data-structures/brms-model-7.rds"),
    file_compress = "xz"
  )

# summary(fit_7)
# custom_pp_check(fit_7)
# plot_fixed_effects(fit_7)
# plot(conditional_effects(fit_7), ask = FALSE)
# plot_varying_effects(fit_7, df_i_rt)
# hypothesis(fit_7, "aphantasia1:congruence1 > 0") # BF10 = 167

# Model 8 -----------------------------------------------------------------

formula_8 <- bf(
  rt ~ aphantasia * congruence * color + 
    (congruence * color | subjectid))

get_prior(formula_8, data = df_i_rt, family = shifted_lognormal())

fit_8 <- 
  brm(
    formula = formula_8,
    data = df_i_rt,
    family = shifted_lognormal(),
    prior = prior_6,
    sample_prior = TRUE,
    chains = n_cores,
    cores  = n_cores,
    iter   = n_iter,
    warmup = 1000,
    refresh = 5,
    backend = "cmdstanr",
    stan_model_args = list(stanc_options = list("O1")),
    save_pars = save_pars(all = TRUE),
    file = here("data/r-data-structures/brms-model-8.rds"),
    file_compress = "xz"
  )

# summary(fit_8)
# custom_pp_check(fit_8)
# plot_fixed_effects(fit_8)
# plot(conditional_effects(fit_8), ask = FALSE)
# plot_varying_effects(fit_8, df_i_rt)
# hypothesis(fit_8, "aphantasia1:congruence1 > 0") # BF10 = 110

