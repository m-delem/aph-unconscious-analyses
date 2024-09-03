# install.packages("here")
library(here)
source(here("scripts/_setup.R"))

# vectors for subjects
id <- seq(1:150)
groups <- rep(c("aphantasia", "control"), 75)

# conditions
congruences <- list("congruent", "incongruent")
colors <- list("colored", "uncolored")
# repeated 4 times each
congruence = rep(congruence, 4)
color = rep(colors, 4)

# creating the simulation df
df <- 
  tibble(
    id = id, 
    group = groups,
    congruence = list(congruence),
    color = list(color)
  ) |> 
  # crossing conditions for 64 trials/subject total
  unnest_longer(congruence) |> 
  unnest_longer(color)

# creating dataframes of regression coefficients
extract_coefficients <- function(effectsize) {
  df_temp <- tribble(
    ~group,   ~congruence,       ~color,   ~y,
    "aphantasia",   "congruent",    "colored",  -30,
    "aphantasia",   "congruent",  "uncolored",    0,
    "aphantasia", "incongruent",    "colored",  -30, 
    "aphantasia", "incongruent",  "uncolored",    0,
    "control",   "congruent",    "colored", -30 - effectsize,
    "control",   "congruent",  "uncolored",   0 - effectsize,
    "control", "incongruent",    "colored",  -30,
    "control", "incongruent",  "uncolored",    0
  )
  
  lm_coef <- lm(y ~ (group + congruence + color)^3, data = df_temp)
  
  # Set numerical errors to 0
  coef(lm_coef)[abs(coef(lm_coef)) < 10^(-10)] <- 0
  
  # converting to ms upon return
  return(coef(lm_coef)/1000)
}

## Fixed effects: initialized at 0
fixed <- c(700,   # intercept
           0,0,0, # main effects
           0,0,0, # 2-way interactions
           0     # 3-way interactions
)/1000

## Random intercepts for participants
v1 <- 0.150

## Random intercept, slopes (main effects only) and their covariances
slope  <- .05
correl <- 0
v2 <- matrix(
  c(1, correl, correl,
    correl, 1, correl,
    correl, correl, 1),
  3)

v2 <- cor_to_cov(v2, sd = c((v1), slope, slope))

## residual std dev
res <- .15

model <- makeLmer(
  formula = y ~ (group + congruence + color)^3 + (congruence + color|id),
  fixef   = fixed, 
  VarCorr = list(v2), 
  sigma = res,
  data  = df
)


# Running simulations with parallel processing ----------------------------


# finding the available cores for parallel processing
n_cores <- parallel::detectCores() - 1

# creating the cluster of cores
parallel_cluster <- 
  parallel::makeCluster(
    n_cores,
    type = "PSOCK"
  )

# registering the cluster for `foreach`
doParallel::registerDoParallel(cl = parallel_cluster)
# checking
# foreach::getDoParRegistered()
# foreach::getDoParWorkers()

# initializing, ready for takeoff
nsim <- 200

# Run calculations for a range of effect sizes
(simulations <-
    foreach (
      effectsize = c(10, 15, 20, 25, 30, 35, 40), 
      .packages = c("tidyverse", "simr")
    ) %dopar% {
      # Calculate regression coefficients corresponding to the effect size and 
      # update the model to simulate from accordingly
      fixef(model) <- extract_coefficients(effectsize)
      
      # Simulate data and estimate power
      powerSim(
        model, 
        nsim = nsim,
        test = simr::fixed(
          xname = "group:congruence", 
          method = 'anova'
        ))
    }) |> system.time() -> time_simulating

# stopping the cluster when we're done
parallel::stopCluster(cl = parallel_cluster)

simulations
# saveRDS(simulations, file = "analyses-results/00-power-analyses.RDS")
# methods(class = "powerSim")

# Test: Type-2 F-test with Satterthwaite degrees of freedom (package lmerTest)
# Based on 200 simulations

simulations_table <-
  tibble(simulations = simulations) |>
  rowwise() |> 
  mutate(summary = list(simulations |> summary() |> as.data.frame())) |> 
  unnest_wider(summary) |>
  bind_cols(tibble(effectsize = c(10, 15, 20, 25, 30, 35, 40))) |>  
  select(!simulations) |> 
  select(effectsize, everything())

# saveRDS(
# simulations_table, 
# file = "analyses-results/00-power-analyses-table.RDS")

p_power <-
  simulations_table |> 
  ggplot(aes(
    x = effectsize,
    y = mean,
    ymin  = lower,
    ymax  = upper,
    label = effectsize
  )) +
  geom_ribbon(fill = "aquamarine", alpha = .2) +
  geom_pointrange(colour = "aquamarine4") +
  geom_point(colour = "aquamarine4") +
  geom_hline(yintercept = .8, color = "black", linetype = 1) +
  geom_line(colour = "aquamarine4") +
  annotate(
    geom  = "text",
    label = "80% power",
    fontface = "italic",
    color = "black",
    x = -Inf,
    y = .8,
    vjust = -.5,
    hjust = -.3,
    size  = 4 
  ) +
  geom_hline(yintercept = .9, color = "gray60", linetype = 2) +
  annotate(
    geom  = "text",
    label = "90% power",
    fontface = "italic",
    color = "grey60",
    x = -Inf,
    y = .9,
    vjust = -.5,
    hjust = -.3,
    size  = 4 
  ) +
  scale_x_continuous(
    name = "Effect size (RT difference in ms)",
    breaks = unique(simulations_table$effectsize)
  ) +
  scale_y_continuous(
    name = "Statistical power",
    breaks = seq(0, 1, .1),
    limits = c(0, 1)
  )

# ggexport(
#   p_power, 
#   filename = "plots/plots-power-analysis.png", 
#   width = 2500,
#   height = 1700,
#   res = 300
#   )
