# Load required library
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Simulate a few data points with smaller slopes
early_life_factor <- c(1, 2, 3)  # 3 levels of early life factor
later_life_factor <- factor(rep(c("Low", "High"), each = 3))  # Two groups: Low and High

# No Interaction: Parallel linear lines with smaller slopes (Independent Effect)
outcome_no_interaction <- c(2, 2.5, 3, 3, 3.5, 4)

# Negative Interaction: Converging lines with smaller slopes (Compensatory Leveling)
outcome_negative_interaction <- c(2, 2.75, 3.5, 3, 3.25, 3.5)

# Positive Interaction: Diverging lines with smaller slopes (Added Protection)
outcome_positive_interaction <- c(2, 2.25, 2.5, 3, 3.75, 4.5)

# Combine data into a single data frame with correctly matched interaction types
concept_data <- data.frame(
  early_life_factor = rep(early_life_factor, 3),
  later_life_factor = rep(later_life_factor, 3),
  outcome = c(outcome_no_interaction, outcome_negative_interaction, outcome_positive_interaction),
  interaction_type = factor(rep(c("Independent effect", "Compensatory leveling", "Added protection"), each = 6),
                            levels = c("Independent effect", "Compensatory leveling", "Added protection")) # Ensuring correct order
)

# Plot linear interaction effects with smaller slopes and legend at bottom
p <- ggplot(concept_data, aes(x = early_life_factor, y = outcome, color = later_life_factor, group = later_life_factor)) +
  geom_line(size = 1) +  # Add linear lines for the interaction
  geom_point() +  # Add points for clarity
  facet_wrap(~interaction_type) +
  labs(x = "Later life stimulating environments",
       y = "Cognitive health",
       color = "Early life stimulating environments") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 16),  # Increase facet label size
        axis.title.x = element_text(size = 14),  # Increase x-axis label size
        axis.title.y = element_text(size = 14),  # Increase y-axis label size
        axis.text.x = element_blank(), # Remove x-axis labels
        axis.text.y = element_blank(), # Remove y-axis labels
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14))    # Increase legend title size

# Display the plot
print(p)

# Save the plot as a PNG file
ggsave(filename = "C:/Users/peng_admin/Dropbox/peng/Academia/Work with Brea/SNAD/SNAD data/Peng/Distal and proximal/P2P/results/concept.png",
       plot = p, bg = "white", width = 10, height = 7, dpi = 300)
