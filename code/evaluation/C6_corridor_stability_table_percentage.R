# DESCRIPTION -------------------------------------------------------------

# In this script I compute the table for percentages within the corridor for w = ±0.07.
#
# Author: Sabine Gisin 



# PACKAGES ---------------------------------------------------------------
library(tidyverse)
library(gt)

# FUNCTIONS ----------------------------------------------------------------

# Function to create the summary table
create_summary_table <- function(df) {
  df %>%
    group_by(nlpar, n_cor) %>%
    count(name = "n_obs") %>%
    left_join(
      df %>%
        group_by(nlpar, n_cor) %>%
        summarise(
          n_inside_corridor = sum(abs(est_true_mean_diff) <= convergence_criterion),
          .groups = 'drop'
        ),
      by = c("nlpar", "n_cor")
    ) %>%
    mutate(perc_inside_corridor = n_inside_corridor / n_obs)
}

# EVALUATION SCRIPT ----------------------------------------------------------------

# Load the simulated data
zscore_eval <- readRDS("zscore_eval.rds")

# Define the convergence criterion
convergence_criterion <- 0.07

# Apply the function to create the summary table
summary_table <- create_summary_table(zscore_eval)

# Ensure all levels of n_cor are represented for each nlpar
all_n_cor <- unique(summary_table$n_cor)
all_nlpar <- unique(summary_table$nlpar)
expanded_grid <- expand.grid(n_cor = all_n_cor, nlpar = all_nlpar)

summary_table <- expanded_grid %>%
  left_join(summary_table, by = c("n_cor", "nlpar")) %>%
  replace_na(list(perc_outside_corridor = 0, n_obs = 0, n_inside_corridor = 0))

# Reshape the data to wide format
wide_summary_table <- summary_table %>%
  select(n_cor, nlpar, perc_inside_corridor) %>%
  pivot_wider(names_from = nlpar, values_from = perc_inside_corridor) %>%
  arrange(n_cor)

# Convert the summary table to a formatted table using gt
formatted_table <- wide_summary_table %>%
  gt() %>%
  cols_label(
    n_cor = "Number of Correlations",
    Change = "Change",
    Reliability = "Reliability",
    Stab.Change = "Stab.Change"
  ) %>%
  fmt_percent(
    columns = c("Reliability", "Change", "Stab.Change"),  # Format these columns as percentages
    decimals = 0
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_options(
    table.width = pct(100)
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(
      columns = everything()
    )
  )