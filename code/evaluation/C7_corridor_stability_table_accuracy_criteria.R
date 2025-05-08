# DESCRIPTION -------------------------------------------------------------

# In this script I compute the table for the different accuracy criteria.
#
# Author: Sabine Gisin 



# PACKAGES ---------------------------------------------------------------
library(tidyverse)
library(gt)

# FUNCTIONS ----------------------------------------------------------------
# Function to create summary table for a given parameter 
create_summary_table <- function(df, convergence_criterion) {
  summary_table <- df %>%
    group_by(nlpar, n_cor) %>%
    count(name = "n_obs") %>%
    left_join(
      df %>%
        group_by(nlpar, n_cor) %>%
        summarise(
          n_outside_corridor = sum(abs(est_true_mean_diff) > convergence_criterion),
          .groups = 'drop'
        ),
      by = c("nlpar", "n_cor")
    ) %>%
    mutate(perc_outside_corridor = n_outside_corridor / n_obs * 100)
  
  return(summary_table)
}
# EVALUATION SCRIPT ----------------------------------------------------------------
# Load the simulated data
zscore_eval <- readRDS("zscore_eval.rds")
# Different widths of the corridor and different percentage criteria
convergence_criteria <- c(0.07, 0.12, 0.2)
percentage_criteria <- c(5, 10, 20)
# Initialize an empty list to store results
results <- list()

# Loop through each combination of convergence_criterion and perc_crit
for (convergence_criterion in convergence_criteria) {
  for (perc_crit in percentage_criteria) {
    summary_table <- create_summary_table(zscore_eval, convergence_criterion)
    
    
    # Find the accuracy thresholds
    convergence_points_mean_diff <- summary_table %>%
      arrange(nlpar, n_cor) %>%
      filter((perc_outside_corridor < perc_crit & lead(perc_outside_corridor, default = Inf) < perc_crit) |
               (perc_outside_corridor < perc_crit & n_cor == max(n_cor, na.rm = TRUE)))
    
    
    # Handle cases where no n_cor meets the criteria
    if (nrow(convergence_points_mean_diff) == 0) {
      cut_off_mean_diff <- tibble(nlpar = unique(summary_table$nlpar), n_cor = NA_real_)
    } else {
      cut_off_mean_diff <- convergence_points_mean_diff %>%
        group_by(nlpar) %>%
        slice_min(n_cor, with_ties = FALSE) %>%
        summarize(nlpar = first(nlpar), n_cor = min(n_cor, na.rm = TRUE), .groups = 'drop')
    }
    
    
    # Calculate the cut-off point where all parameters converge
    if (n_distinct(cut_off_mean_diff$nlpar) == length(unique(zscore_eval$nlpar))) {
      cut_off_all <- cut_off_mean_diff %>%
        summarize(nlpar = "Whole model", n_cor = max(n_cor, na.rm = TRUE))
      if (cut_off_all$n_cor == -Inf) {
        cut_off_all$n_cor <- NA_real_
      }
    } else {
      cut_off_all <- tibble(nlpar = "Whole model", n_cor = NA_real_)  # Use NA_real_ for missing numeric value
    }
    
    
    # Combine individual cut-off points with the overall cut-off point
    final_cut_off_table <- bind_rows(cut_off_mean_diff, cut_off_all) %>%
      mutate(
        convergence_criterion = convergence_criterion,
        perc_crit = perc_crit
      )
    
    # Store the result
    results[[paste0("w_", convergence_criterion, "_perc_", perc_crit)]] <- final_cut_off_table
  }
}

# Combine all results into a single data frame
final_results <- bind_rows(results)

# Ensure the nlpar column is a factor with the desired order
final_results <- final_results %>%
  mutate(nlpar = factor(nlpar, levels = c("Whole model", "Reliability", "Change", "Stab.Change")))

max_value <- max(unique(zscore_eval$n_cor))


# Reshape the data to wide format
reshaped_results <- final_results %>%
  pivot_wider(names_from = c(convergence_criterion, perc_crit), values_from = n_cor) %>%
  arrange(nlpar) %>% 
  mutate(across(starts_with("0.07"), ~ ifelse(is.na(.), paste0(">", max_value), .)))

# Create the gt table
formatted_table <- reshaped_results %>%
  gt() %>%
  # tab_header(
  #   title = "Sample Size Thresholds",
  #   subtitle = html("Risk Preference Dataset, Unbalanced<br>Percentage within the corridor")
  # ) %>%
  cols_label(
    nlpar = "Parameter",
    `0.07_5` = "w = 0.07",
    `0.07_10` = "w = 0.07",
    `0.07_20` = "w = 0.07",
    `0.12_5` = "w = 0.12",
    `0.12_10` = "w = 0.12",
    `0.12_20` = "w = 0.12",
    `0.2_5` = "w = 0.2",
    `0.2_10` = "w = 0.2",
    `0.2_20` = "w = 0.2"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_spanner(
    label = "95%",
    columns = c(`0.07_5`, `0.12_5`, `0.2_5`)
  ) %>%
  tab_spanner(
    label = "90%",
    columns = c(`0.07_10`, `0.12_10`, `0.2_10`)
  ) %>%
  tab_spanner(
    label = "80%",
    columns = c(`0.07_20`, `0.12_20`, `0.2_20`)
  ) %>%
  tab_options(
    table.width = pct(100)
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = everything(),
      rows = nlpar == "Whole model"
    )
  ) %>%
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(2)),
    locations = list(
      cells_column_labels(columns = c(`0.2_5`)),
      cells_column_labels(columns = c(`0.2_10`)),
      cells_column_labels(columns = c(`0.2_20`))
    )
  ) %>%
  tab_style(
    style = cell_borders(sides = "right", color = "black", weight = px(2)),
    locations = list(
      cells_body(columns = c(`0.2_5`)),
      cells_body(columns = c(`0.2_10`)),
      cells_body(columns = c(`0.2_20`))
    )
  )