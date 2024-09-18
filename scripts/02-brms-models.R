# install.packages("here")
library(here)
source(here("scripts/00-preprocessing.R"))
source(here("scripts/_bayesian-setup.R"))

# Parallel processing variables -------------------------------------------

# detecting the number of cores to use
n_cores <- parallel::detectCores() - 4

# defining the number of iterations per chain (+ 1000 warm-up)
n_iter <- ceiling(40000 / n_cores) + 1000


options(cmdstanr_warn_inits = FALSE)


# Model formula -----------------------------------------------------------

formula <- bf(rt ~ congruence * color + (1 | subjectid))

# Priors ------------------------------------------------------------------

get_prior(formula, data = df_i_rt, family = shifted_lognormal())
# need to define better priors for the intercept, sigma, b, and sd

prior <- c(
  prior(normal(-1, 0.5), class = 'Intercept'),  # around exp(-1) = 0.36 secs
  prior(normal(0.4, 0.3), class = 'sigma'), # SD of individual rts in log-units
  prior(normal(0, 0.3), class = 'b'), # around exp(-1) - exp(-1 + 0.3) = 150 ms 
  prior(normal(0.3, 0.1), class = 'sd')  # some variability between participants
)

# Implicit task models ----------------------------------------------------

# Aphantasia group ---------------------------------------------
fit_implicit_aph <- 
  brm(
    formula = formula,
    data = df_i_rt |> filter(aphantasia == "Aphantasia"),
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
    file = here("data/r-data-structures/brms-model-implicit-aph.rds"),
    file_compress = "xz"
  )

# fit_implicit_aph |> summary()

# Control group ---------------------------------------------
fit_implicit_con <-
  update(
    fit_implicit_aph,
    newdata = df_i_rt |> filter(aphantasia == "Control"),
    chains = n_cores,
    cores  = n_cores,
    iter   = n_iter,
    file = here("data/r-data-structures/brms-model-implicit-con.rds"),
    file_compress = "xz"
  )

# fit_implicit_con |> summary()

# Finer aphantasia group -------------------------------------
fit_implicit_finer_aph <- 
  update(
    fit_implicit_aph,
    newdata = df_i_finer |> filter(sub_group == "Aphantasia"),
    chains = n_cores,
    cores  = n_cores,
    iter   = n_iter,
    file = here("data/r-data-structures/brms-model-implicit-finer-aph.rds"),
    file_compress = "xz"
  )

# fit_implicit_finer_aph |> summary()

# Hypophantasia group -------------------------------------
fit_implicit_hypo <- 
  update(
    fit_implicit_aph,
    newdata = df_i_finer |> filter(sub_group == "Hypophantasia"),
    chains = n_cores,
    cores  = n_cores,
    iter   = n_iter,
    file = here("data/r-data-structures/brms-model-implicit-hypo.rds"),
    file_compress = "xz"
  )

# fit_implicit_hypo |> summary()

# Finer control group -------------------------------------
fit_implicit_finer_con <- 
  update(
    fit_implicit_aph,
    newdata = df_i_finer |> filter(sub_group == "Control"),
    chains = n_cores,
    cores  = n_cores,
    iter   = n_iter,
    file = here("data/r-data-structures/brms-model-implicit-finer-con.rds"),
    file_compress = "xz"
  )

# fit_implicit_finer_con |> summary()


# Explicit task models -----------------------------------------------------

# Aphantasia group ---------------------------------------------
fit_explicit_aph <- 
  update(
    fit_implicit_aph,
    newdata = df_e_rt |> filter(sub_group == "Aphantasia"),
    chains = n_cores,
    cores  = n_cores,
    iter   = n_iter,
    file = here("data/r-data-structures/brms-model-explicit-aph.rds"),
    file_compress = "xz"
  )

# fit_explicit_aph |> summary()

# Control group ---------------------------------------------
fit_explicit_con <- 
  update(
    fit_implicit_aph,
    newdata = df_e_rt |> filter(sub_group == "Control"),
    chains = n_cores,
    cores  = n_cores,
    iter   = n_iter,
    file = here("data/r-data-structures/brms-model-explicit-con.rds"),
    file_compress = "xz"
  )

# fit_explicit_con |> summary()

# Finer aphantasia group -------------------------------------
fit_explicit_finer_aph <- 
  update(
    fit_implicit_aph,
    newdata = df_e_finer |> filter(sub_group == "Aphantasia"),
    chains = n_cores,
    cores  = n_cores,
    iter   = n_iter,
    file = here("data/r-data-structures/brms-model-explicit-finer-aph.rds"),
    file_compress = "xz"
  )

# fit_explicit_finer_aph |> summary()

# Hypophantasia group -------------------------------------
fit_explicit_hypo <- 
  update(
    fit_implicit_aph,
    newdata = df_e_finer |> filter(sub_group == "Hypophantasia"),
    chains = n_cores,
    cores  = n_cores,
    iter   = n_iter,
    file = here("data/r-data-structures/brms-model-explicit-hypo.rds"),
    file_compress = "xz"
  )

# fit_explicit_hypo |> summary()

# Finer control group -------------------------------------
fit_explicit_finer_con <- 
  update(
    fit_implicit_aph,
    newdata = df_e_finer |> filter(sub_group == "Control"),
    chains = n_cores,
    cores  = n_cores,
    iter   = n_iter,
    file = here("data/r-data-structures/brms-model-explicit-finer-con.rds"),
    file_compress = "xz"
  )

# fit_explicit_finer_con |> summary()



# Now checking Bayes Factors for a congruence effect ----------------------

# Implicit task models ----------------------------------------------------

# Aphantasia group
cat(
  "Aphantasia group, evidence against an implicit congruence effect: \n",
  "BF_01 =",
  hypothesis(fit_implicit_aph, "congruence1 = 0")$hypothesis$Evid.Ratio
  )

# Control group
cat(
  "Control group, evidence against an implicit congruence effect: \n",
  "BF_01 =",
  hypothesis(fit_implicit_con, "congruence1 = 0")$hypothesis$Evid.Ratio
  )

# Finer aphantasia group
cat(
  "Finer aphantasia group, evidence against an implicit congruence effect: \n",
  "BF_01 =",
  hypothesis(fit_implicit_finer_aph, "congruence1 = 0")$hypothesis$Evid.Ratio
  )

# Hypophantasia group
cat(
  "Hypophantasia group, evidence against an implicit congruence effect: \n",
  "BF_01 =",
  hypothesis(fit_implicit_hypo, "congruence1 = 0")$hypothesis$Evid.Ratio
  )

# Finer control group
cat(
  "Finer control group, evidence against an implicit congruence effect: \n",
  "BF_01 =",
  hypothesis(fit_implicit_finer_con, "congruence1 = 0")$hypothesis$Evid.Ratio
  )

# Explicit task models ----------------------------------------------------

# Aphantasia group
cat(
  "Aphantasia group, evidence against an explicit congruence effect: \n",
  "BF_01 =",
  hypothesis(fit_explicit_aph, "congruence1 = 0")$hypothesis$Evid.Ratio
  )

# Control group
cat(
  "Control group, evidence against an explicit congruence effect: \n",
  "BF_01 =",
  hypothesis(fit_explicit_con, "congruence1 = 0")$hypothesis$Evid.Ratio
  )

# Finer aphantasia group
cat(
  "Finer aphantasia group, evidence against an explicit congruence effect: \n",
  "BF_01 =",
  hypothesis(fit_explicit_finer_aph, "congruence1 = 0")$hypothesis$Evid.Ratio
  )

# Hypophantasia group
cat(
  "Hypophantasia group, evidence against an explicit congruence effect: \n",
  "BF_01 =",
  hypothesis(fit_explicit_hypo, "congruence1 = 0")$hypothesis$Evid.Ratio
  )

# Finer control group
cat(
  "Finer control group, evidence against an explicit congruence effect: \n",
  "BF_01 =",
  hypothesis(fit_explicit_finer_con, "congruence1 = 0")$hypothesis$Evid.Ratio
  )
