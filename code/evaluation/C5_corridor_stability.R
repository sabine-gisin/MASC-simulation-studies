# DESCRIPTION -------------------------------------------------------------

# In this script I compute the corridor of stability inspired by Schönbrodt and Perugini (2013).
#
# Author: Sabine Gisin 



# PACKAGES ---------------------------------------------------------------
library(tidyverse)
library(gt)

# EVALUATION SCRIPT ----------------------------------------------------------------

# Load the simulated data
zscore_eval <- readRDS("zscore_eval.rds")

#get n_cor levels
n_cor_levels <- zscore_eval %>% 
  arrange(n_cor) %>% 
  distinct(n_cor) %>% 
  pull(n_cor)


figure <- zscore_eval %>% 
  mutate(n_cor = factor(n_cor, levels = n_cor_levels)) %>% 
  ggplot(aes(n_cor, est_true_mean_diff, color = nlpar)) +
  geom_jitter(position = position_jitter(width = 1), size = 1, alpha = 1) + 
  geom_hline(yintercept = 0.07, linetype = "dashed")+
  geom_hline(yintercept = -0.07, linetype = "dashed") +
  scale_y_continuous(breaks = c(-0.4, -0.2, 0, 0.2, 0.4)) +
  facet_wrap(~nlpar, nrow = 3) +
  labs(
    x = "Number of Correlations",
    y = "Estimated - True Mean Difference"
  ) +
  theme_minimal() +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(family = "Source Sans 3", size = 12, margin = margin(t = 25, b = 20)),
    axis.title.y = element_text(family = "Source Sans 3", size = 12, margin = margin(r = 20)),
    axis.text.x = element_text(size = 10),  # Increase x-axis digits
    axis.text.y = element_text(size = 10),  # Increase y-axis digits
    axis.ticks = element_blank(),
    strip.text = element_text(size = 12, family = "Source Sans 3"),  # Adjust facet label size
    legend.position = "none",
    legend.title = element_blank(),
    legend.text = element_text(size = 12, family = "Source Sans 3"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    panel.grid.major.y = element_line(size = 0.8), 
    panel.grid.major.x = element_line(size = 0.8)# Adjust grid size and color
  )


print(figure)