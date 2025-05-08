# DESCRIPTION -------------------------------------------------------------

# In this script we simulate intercept-only datasets for the constructs risk preference, 
# affect and personality for different maximum retest intervals.
#
# Author(s): Alexandra Bagaini and Sabine Gisin 
# Alexandra Bagaini provided the original simulation script for balanced simulation scenarios, 
# Sabine Gisin adapted the original script to simulate scenarios for different maximum retest intervals
# and to store dataset informations


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

# data struct
retest_interval = seq(.5,20, by = .5) # 6 month intervals
panel = as.character(1:5000) # (this can vary)

# creating a dataset that has all of the combinations of the variables using the crossings() function
data_struct <- crossing(retest_interval = retest_interval,
                        panel = panel)


m1_data <- data_struct %>% 
  mutate(
    # adding "true" param value to the dataset
    rel = rel,
    change = change,
    stabch = stabch,
    sigma = sigma,
    # calc retest
    retest = masc(rel = rel, #  rel ~ 1
                  change = change, #  change ~ 1
                  stabch = stabch, #  stabch ~ 1
                  retest_interval = retest_interval)  + rnorm(n(), 0, sigma)) %>%  # masc + error/noise (can change noise; closer to real-world data, enabling a more comprehensive evaluation of model performance)
  mutate(retest = case_when(retest > 1 ~ 1,
                            retest < 0 ~ 0,
                            TRUE ~retest)) # retest correlations bounded between 0 & 1


true_masc <-data_struct %>% 
  filter(panel == "1") %>% 
  mutate(
    # adding "true" param value to the dataset
    rel = rel,
    change = change,
    stabch = stabch,
    sigma = sigma,
    # calc retest
    retest = masc(rel = rel, #  rel ~ 1
                  change = change, #  change ~ 1
                  stabch = stabch, #  stabch ~ 1
                  retest_interval = retest_interval)) 


# DATA SIMULATION ---------------------------------------------------------

full_eval_zscore <- NULL

data_list <- NULL

true_vals <- tibble(
  rel = rel,
  change = change,
  stabch = stabch) %>% 
  pivot_longer(1:3, values_to = "true_val", names_to = "nlpar")

obs_num <- 5000


for (n_sim in c(1:50)) {
  
  for (max_rt in c(1, 3, 5, 10, 12.5, 15, 17.5, 20)) {
    
  
    sub_m1_data <- m1_data %>% filter(retest_interval <= max_rt) %>% 
      sample_n(obs_num, replace = TRUE)
    
    
    family <- brmsfamily(
      family = "student", 
      link = "identity"
    )
    
    
    # setting up priors
    # weakly informative priors 
    priors <-
      prior(normal(0, 1), nlpar="logitrel", class = "b") +
      prior(normal(0, 1), nlpar="logitchange", class = "b") +
      prior(normal(0, 1), nlpar="logitstabch", class = "b")
    
    
    # setting up model
    formula <- bf(
      retest ~ rel * (change * ((stabch^retest_interval) - 1) + 1),
      nlf(rel ~ inv_logit(logitrel)),
      nlf(change ~ inv_logit(logitchange)),
      nlf(stabch ~ inv_logit(logitstabch)),
      logitrel ~ 1,
      logitchange ~ 1, 
      logitstabch ~ 1,
      nl = TRUE
    )
    
    
    
    # fit model (adjust arguments where needed; e.g., "control", "cores", "chains")
    m1_fit_masc <- brm(
      formula = formula,
      prior = priors,
      family = family,
      data = sub_m1_data, 
      cores = 2, 
      chains = 2,
      iter = 3000,
      warmup = 1000, 
      backend = "cmdstanr",
      control = list(max_treedepth = 10, adapt_delta = 0.95, step_size = 0.02), 
      init = "0",
      seed = 75672
    )
    
    ### 4. Extracting & Storing Model Diagnostics 
    
    
    #Extract r_hats  
    rhat_values <- rhat(m1_fit_masc, pars = c("b_logitrel_Intercept", "b_logitchange_Intercept", "b_logitstabch_Intercept"))    #pars = NULL would return ALL rhats
    
    #extract divergent transitions
    np <- nuts_params(m1_fit_masc)
    str(np)
    # extract the number of divergence transitions
    divergent_transitions <- sum(subset(np, Parameter == "divergent__")$Value)
    
    #extract neff-ratio
    neff_ratio <- neff_ratio(m1_fit_masc, pars = c("b_logitrel_Intercept", "b_logitchange_Intercept", "b_logitstabch_Intercept"))
    
    
    #create tibble to combine model diagnostics
    model_diagnostics <- tibble(nlpar = names(rhat_values), 
                                R_hat = rhat_values,
                                neff_ratio = neff_ratio[names(rhat_values)],
                                divergent_transitions = divergent_transitions) %>%# Align by names to ensure correct matching) 
      mutate(nlpar = case_when(
        nlpar == "b_logitrel_Intercept" ~ "rel",
        nlpar == "b_logitchange_Intercept" ~ "change",
        nlpar == "b_logitstabch_Intercept" ~ "stabch")) 
    
    
    
    
    
    
    
    # extracting param values
    epred_rel <-  as.numeric(add_epred_draws(m1_fit_masc, newdata = tibble(retest_interval = 0, panel = "1"), nlpar = "rel")$.epred)
    epred_change <-  as.numeric(add_epred_draws(m1_fit_masc, newdata = tibble(retest_interval = 0, panel = "1"), nlpar = "change")$.epred)
    epred_stabch <-  as.numeric(add_epred_draws(m1_fit_masc, newdata = tibble(retest_interval = 0, panel = "1"), nlpar = "stabch")$.epred)
    
    est_vals <- tibble(rel = epred_rel,
                       change = epred_change,
                       stabch = epred_stabch) %>%
      pivot_longer(rel:stabch,names_to = "nlpar", values_to = "val") %>%
      group_by(nlpar) %>%
      summarise(est_mean = mean(val),est_sd = sd(val))
    
    
    
    eval_zscore <- est_vals %>% 
      left_join(true_vals, by = "nlpar") %>% 
      left_join(model_diagnostics, by = "nlpar")
    
    
    
    # compare estimated vs. true param val ( "posterior z-score")
    eval_zscore <- eval_zscore %>% 
      mutate(post_zscore  = (est_mean-true_val)/est_sd,
             est_true_mean_diff = est_mean - true_val)
    
    
    # predicted MASC curve
    predict_retest <-  add_epred_draws(m1_fit_masc, newdata = data_struct %>% filter(panel == "1"))  %>% 
      mean_qi() %>% 
      rename(retest_predict = .epred)
    
    
    # storing model eval. metrics & data set info
    full_eval_zscore <- eval_zscore %>% 
      mutate(      n_sim = n_sim,
                   max_rt = max_rt,
                   n_cor = obs_num) %>% 
      bind_rows(full_eval_zscore)
    
    
    # saving data
    data_list <-  tibble(n_sim = n_sim,
                         n_cor = obs_num,
                         max_rt = max_rt,
                         model_data = list(sub_m1_data),
                         masc_pred = list(predict_retest)) %>% 
      bind_rows(data_list)
    
    
    }
    
  }

