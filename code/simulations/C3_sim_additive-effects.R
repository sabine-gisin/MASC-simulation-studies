# DESCRIPTION -------------------------------------------------------------

# In this script we simulate datasets WITH ADDITIVE EFFECTS for the constructs risk preference, 
# affect and personality.
#
# Author(s): Alexandra Bagaini and Sabine Gisin 
# Alexandra Bagaini provided the original simulation script for balanced balenced intercept-only simulation scenarios, 
# Sabine Gisin adapted the original script to simulate additive moderating effects


# PACKAGES ---------------------------------------------------------------
library(tidyverse)
library(tidyr)
library(brms)
library(tidybayes)
library(boot) # logit function
library(data.table)
library(gt)

# FUNCTIONS ----------------------------------------------------------------

masc <- function(rel, change, stabch, retest_interval) {
  
  retest <- rel * (change * (stabch^retest_interval - 1) + 1)
  
  return(retest)
  
}

inv_logit <- function(x) {inv.logit(x)}


# CREATING THE DATA SET -------------------------------------------------------
# setting up the true values of the parameters (Risk Preference)
rel <- 0.5
change <- 0.5
stabch <- 0.8
sigma =  0.1

# setting up the true values of the parameters (Affect)
# rel <- 0.6
# change <- 0.6
# stabch <- 0.9
# sigma =  0.1

# setting up the true values of the parameters (Personality)
# rel <- 0.7
# change <- 0.2
# stabch <- 0.25
# sigma =  0.1

# Define the true parameter values
rel_values <- c(0.4, 0.45, 0.5, 0.6, 0.65) # Five different levels of rel
change <- 0.5
stabch <- 0.8
sigma <- 0.1

rel_age_effect <- 0.01
change_age_effect <- 0.04
change_age_eff_linear <- 0.035
stabch_age_effect <- -0.03

age_values <- seq(10, 70, by = 10)
age_dec_c <- (age_values-40)/10
age_quad_values <- age_dec_c^2

# Combine age and age_quad into a dataframe
age_df <- tibble(age = age_values, age_quad = age_quad_values)

# Define the domain values
domains <- c("Domain1", "Domain2", "Domain3", "Domain4", "Domain5")

# Generate example dataset for plotting
num_corr <- 10000  # Adjust number of correlations for the test

# Calculate the number of samples required
n_short_intervals <- round(2/3 * num_corr)
n_long_intervals <- num_corr - n_short_intervals

# Sample the retest intervals with added randomness
set.seed(123)  # For reproducibility
short_intervals <- sample(seq(0.5, 6, by = 0.5), n_short_intervals, replace = TRUE)
long_intervals <- sample(seq(6.5, 20, by = 0.5), n_long_intervals, replace = TRUE)
sample_retest_intervals <- c(short_intervals, long_intervals)

# Repeat age and domain values to match the length of sample_retest_intervals
repeated_age <- rep(age_values, length.out = length(sample_retest_intervals))
# Repeat domain values to match the length of sample_retest_intervals
repeated_domains <- rep(domains, length.out = length(sample_retest_intervals))


m2_data <- tibble(
  retest_interval = sample_retest_intervals,
  domain = repeated_domains,
  age = repeated_age
) %>%
  mutate(
    age_dec_c = (age - 40) / 10,
    age_quad = age_dec_c^2,
    rel = case_when(
      domain == "Domain1" ~ rel_values[1],
      domain == "Domain2" ~ rel_values[2],
      domain == "Domain3" ~ rel_values[3],
      domain == "Domain4" ~ rel_values[4],
      domain == "Domain5" ~ rel_values[5]
    ),
    change = change,
    stabch = stabch,
    rel_age_effect = rel_age_effect,
    change_age_effect = change_age_effect,
    change_age_eff_linear = change_age_eff_linear,
    stabch_age_effect = stabch_age_effect
  ) %>%
  mutate(
    bounded_rel = pmin(1, pmax(0, rel + rel_age_effect * age_quad)),
    bounded_change = pmin(1, pmax(0, change + change_age_effect * age_quad + change_age_eff_linear * age_dec_c)),
    bounded_stabch = pmin(1, pmax(0, stabch + stabch_age_effect * age_quad)),
    retest = masc(
      rel = bounded_rel,
      change = bounded_change,
      stabch = bounded_stabch,
      retest_interval = retest_interval
    ) + rnorm(n(), 0, sigma)
  ) %>%
  mutate(
    retest = case_when(
      retest > 1 ~ 1,
      retest < 0 ~ 0,
      TRUE ~ retest
    )
  ) %>%
  filter(!is.na(retest))

# Generate true_masc data for plotting
true_masc <- expand.grid(
  retest_interval = seq(0.5, 20, by = 0.5),
  domain = domains
) %>%
  mutate(
    rel = case_when(
      domain == "Domain1" ~ rel_values[1],
      domain == "Domain2" ~ rel_values[2],
      domain == "Domain3" ~ rel_values[3],
      domain == "Domain4" ~ rel_values[4],
      domain == "Domain5" ~ rel_values[5]),
    change = 0.5,
    stabch = 0.8
  ) %>%
  mutate(
    retest = masc(
      rel = rel,
      change = change,
      stabch = stabch,
      retest_interval = retest_interval
    )
  )
# Generate true_vals
true_vals <- crossing(
  domain = domains,
  age_df
) %>%
  mutate(
    rel = pmin(pmax(rep(rel_values, each = nrow(age_df)) + rel_age_effect * age_quad, 0), 1),
    change = pmin(pmax(change + change_age_effect * age_quad + change_age_eff_linear * age_dec_c, 0), 1),
    stabch = pmin(pmax(stabch + stabch_age_effect * age_quad, 0), 1)
  ) %>%
  pivot_longer(cols = c(rel, change, stabch), names_to = "nlpar", values_to = "true_val")


# Calculate prior variance for the computation of posterior contraction
# Simulate from the prior on the logit scale
set.seed(123)
logit_samples <- rnorm(100000, mean = 0, sd = 1)
prior_samples <- inv_logit(logit_samples)

# Calculate the empirical variance of the prior samples
prior_variance <- var(prior_samples)


# DATA SIMULATION ---------------------------------------------------------

full_eval_zscore <- NULL
data_list <- NULL


# Set seed once for reproducibility
set.seed(375)


for (n_sim in c(1:50)) {
  for (num_corr in c(50, 100, 200, 500, 1000, 2000, 3000, 5000, 7500, 10000, 15000)) {
    
    # Calculate the number of samples required for each segment
    n_short_intervals <- round(2/3 * num_corr)
    n_long_intervals <- num_corr - n_short_intervals
    
    # Sample the retest intervals
    
    short_intervals <- sample(seq(0.5, 6, by = 0.5), n_short_intervals, replace = TRUE)
    long_intervals <- sample(seq(6.5, 20, by = 0.5), n_long_intervals, replace = TRUE)
    sample_retest_intervals <- c(short_intervals, long_intervals)
    
    age <- rep(seq(10, 70, by = 10), length.out = num_corr)
    age_dec_c <- (age - 40) / 10
    age_quad <- age_dec_c^2
    # Repeat domain values to match the length of sample_retest_intervals
    repeated_domains <- rep(domains, length.out = length(sample_retest_intervals))
    
    
    m2_data <- tibble(
      retest_interval = sample_retest_intervals,
      domain = domain,
      age = age,
      age_dec_c = age_dec_c,
      age_quad = age_quad,
      rel = rep(rel_values, each = num_corr / length(domains)),
      change = rep(change, num_corr),
      stabch = rep(stabch, num_corr),
      rel_age_effect = rel_age_effect,
      change_age_effect = change_age_effect,
      stabch_age_effect = stabch_age_effect,
      change_age_eff_linear = change_age_eff_linear
    ) %>%
      mutate(
        bounded_rel = pmin(1, pmax(0, rel + rel_age_effect * age_quad)),
        bounded_change = pmin(1, pmax(0, change + change_age_effect * age_quad + change_age_eff_linear * age_dec_c)),
        bounded_stabch = pmin(1, pmax(0, stabch + stabch_age_effect * age_quad)),
        retest = masc(
          rel = bounded_rel,
          change = bounded_change,
          stabch = bounded_stabch,
          retest_interval = retest_interval
        ) + rnorm(n(), 0, sigma)
      ) %>%
      mutate(
        retest = case_when(
          retest > 1 ~ 1,
          retest < 0 ~ 0,
          TRUE ~ retest
        )
      )
    
    
    # Define newdata for parameter extraction
    newdata <- crossing(
      retest_interval = 0, 
      domain = unique(m2_data$domain), 
      age_quad = unique(m2_data$age_quad)
    ) %>%
      left_join(m2_data %>% select(age_quad, age, age_dec_c) %>% distinct(), by = "age_quad")
    
    
    
    family <- brmsfamily(
      family = "student", 
      link = "identity"
    )
    
    # Setting up priors
    # Weakly informative priors 
    priors <- prior(normal(0, 1), nlpar = "logitrel", class = "b") +
      prior(normal(0, 1), nlpar = "logitchange", class = "b") +
      prior(normal(0, 1), nlpar = "logitstabch", class = "b") +
      prior(cauchy(0,1), class = "sigma")
    
    
    
    # Setting up model
    formula <- bf(
      retest ~ rel * (change * ((stabch^retest_interval) - 1) + 1),
      nlf(rel ~ inv_logit(logitrel)),
      nlf(change ~ inv_logit(logitchange)),
      nlf(stabch ~ inv_logit(logitstabch)),
      logitrel ~ 1 + domain + age_quad + age_dec_c,     # Include random effects
      logitchange ~ 1 + domain + age_quad + age_dec_c,  
      logitstabch ~ 1 + domain + age_quad + age_dec_c,
      nl = TRUE
    )
    
    # Fit model (adjust arguments where needed; e.g., "control", "cores", "chains")
    m2_fit_masc <- brm(
      formula = formula,
      prior = priors,
      family = family,
      data = m2_data,
      cores = 2,
      chains = 2,
      iter = 3000,
      warmup = 1000,
      backend = "cmdstanr",
      control = list(max_treedepth = 12, adapt_delta = 0.95, step_size = 0.02),
      init = "0")
    
    # Extracting parameter values
    epred_rel <- as.numeric(add_epred_draws(m2_fit_masc, newdata = newdata, nlpar = "rel")$.epred)
    epred_change <- as.numeric(add_epred_draws(m2_fit_masc, newdata = newdata, nlpar = "change")$.epred)
    epred_stabch <- as.numeric(add_epred_draws(m2_fit_masc, newdata = newdata, nlpar = "stabch")$.epred)
    
    # Correctly repeat domain and age_quad to match the number of samples
    n_samples <- length(epred_rel) / nrow(newdata)
    expanded_domain <- rep(newdata$domain, each = n_samples)
    expanded_age_quad <- rep(newdata$age_quad, each = n_samples)
    expanded_age <- rep(newdata$age, each = n_samples) 
    expanded_age_dec_c <- rep(newdata$age_dec_c, each = n_samples)
    
    # Combine the parameter values with the corresponding domain and age_quad values
    est_vals <- tibble(rel = epred_rel,
                       change = epred_change,
                       stabch = epred_stabch,
                       domain = expanded_domain,
                       age_quad = expanded_age_quad,
                       age_dec_c = expanded_age_dec_c,
                       age = expanded_age) %>%
      pivot_longer(rel:stabch, names_to = "nlpar", values_to = "val") %>%
      group_by(nlpar, domain, age_quad, age, age_dec_c) %>%
      summarise(est_mean = mean(val), est_sd = sd(val), .groups = 'drop')
    
    
    eval_zscore <- est_vals %>% left_join(true_vals, by = c("nlpar", "domain", "age_quad", "age"))
    
    # Compare estimated vs. true parameter values ("posterior z-score")
    eval_zscore <- eval_zscore %>%
      mutate(post_zscore = (est_mean - true_val) / est_sd,
             est_true_mean_diff = est_mean - true_val,
             post_contraction = 1 - (est_sd^2 / prior_variance))  # Using empirical prior variance
    
    # Extracting & Storing Model Diagnostics
    rhat_values <- rhat(m2_fit_masc, pars = c("b_logitrel_Intercept", "b_logitchange_Intercept", "b_logitstabch_Intercept"))
    
    np <- nuts_params(m2_fit_masc)
    divergent_transitions <- sum(subset(np, Parameter == "divergent__")$Value)
    
    neff_ratio <- neff_ratio(m2_fit_masc, pars = c("b_logitrel_Intercept", "b_logitchange_Intercept", "b_logitstabch_Intercept"))
    
    model_diagnostics <- tibble(
      nlpar = names(rhat_values),
      R_hat = rhat_values,
      neff_ratio = neff_ratio[names(rhat_values)],
      divergent_transitions = divergent_transitions
    ) %>%
      mutate(nlpar = case_when(
        nlpar == "b_logitrel_Intercept" ~ "rel",
        nlpar == "b_logitchange_Intercept" ~ "change",
        nlpar == "b_logitstabch_Intercept" ~ "stabch"
      ))
    
    eval_zscore <- eval_zscore %>%
      left_join(model_diagnostics, by = "nlpar") %>%
      mutate(n_sim = n_sim, 
             n_cor = num_corr)
    
    # Storing model evaluation metrics and dataset information
    full_eval_zscore <- bind_rows(full_eval_zscore, eval_zscore)
    
    # Define newdata for predicting the MASC curve with varying retest intervals
    domain_levels <- unique(m2_data$domain)
    age_quad_levels <- unique(m2_data$age_quad)
    retest_intervals <- seq(0.5, 20, by = 0.5)
    
    newdata_retest <- crossing(
      retest_interval = retest_intervals,
      domain = domain_levels,
      age_quad = age_quad_levels
    ) %>%
      left_join(m2_data %>% select(age_quad, age, age_dec_c) %>% distinct(), by = "age_quad")
    
    
    
    
    # Predicted MASC curve
    predict_retest <- add_epred_draws(m2_fit_masc, newdata = newdata_retest) %>%
      mean_qi() %>%
      rename(retest_predict = .epred)
    
    # Saving data
    data_list <- tibble(
      n_sim = n_sim,
      n_cor = num_corr,
      model_data = list(m2_data),
      masc_pred = list(predict_retest)
    ) %>%
      bind_rows(data_list) }
  }
}

