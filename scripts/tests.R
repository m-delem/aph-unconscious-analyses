# install.packages("here")
library(here)
source(here("scripts/_setup.R"))

data("speed_acc")

df <- 
  speed_acc |> 
  filter(rt > .18, rt < 3) |> 
  mutate(across(c(stim_cat, response), as.character)) |> 
  filter(response != "error" & stim_cat == response)

formula <- bf(rt ~ condition)

get_prior(
  formula = formula,
  data = df,
  family = shifted_lognormal()
)

prior <- c(
  prior(normal(-1, 0.5), class = 'Intercept'),  # around exp(-1) = 0.36 secs
  prior(normal(0.4, 0.3), class = 'sigma'),  # SD of individual rts in log-units
  prior(normal(0, 0.3), class = 'b')  # around exp(-1) - exp(-1 + 0.3) = 150 ms effect in either direction
  # prior(normal(0.3, 0.1), class = 'sd'),  # some variability between participants
  # prior(normal(-5, 0.3), class = 'Intercept', dpar = 'ndt'),  # around exp(-5) = 0.006 secs
  # prior(normal(0.3, 0.1), class = 'sd', dpar = 'ndt')  # some variability between participants
)

make_stancode(
  formula,
  data = df,
  family = shifted_lognormal(),
  prior = prior
)

# options(cmdstanr_warn_inits = FALSE)

fit <- 
  brm(
    formula = formula,
    data = df,
    family = shifted_lognormal(),
    prior = prior,
    sample_prior = TRUE,
    chains = 4,
    cores  = 4,
    iter = 1000,
    refresh = 5,
    # init = function(){list(Intercept_ndt = -5)},
    backend = "cmdstanr",
    stan_model_args = list(stanc_options = list("O1")),
    save_pars = save_pars(all = TRUE)
  )
