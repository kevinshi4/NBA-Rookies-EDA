setwd("C:/Users/abc12/Downloads/info 332")

df <- read.csv('nba_rookie_data_cleaned.csv')

library(ggplot2)
library(tidyverse)
library(patchwork)

# Histogram for Points Per Game (PTSpg)
hist_pts <- ggplot(df, aes(x = PTSpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.5, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Points Per Game (PTSpg)") +
  xlab("PTSpg") +
  ylab("Density") +
  theme_minimal()

# Histogram for Total Rebounds Per Game (TRBpg)
hist_trb <- ggplot(df, aes(x = TRBpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "olivedrab3", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Total Rebounds Per Game (TRBpg)") +
  xlab("TRBpg") +
  ylab("Density") +
  theme_minimal()

# Histogram for Assists Per Game (ASTpg)
hist_ast <- ggplot(df, aes(x = ASTpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "gold", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Assists Per Game (ASTpg)") +
  xlab("ASTpg") +
  ylab("Density") +
  theme_minimal()

# Histogram for Minutes Per Game (MPpg)
hist_mpg <- ggplot(df, aes(x = MPpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "turquoise4", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Minutes Per Game (MPpg)") +
  xlab("MPpg") +
  ylab("Density") +
  theme_minimal()

# Histogram for Age
hist_age <- ggplot(filter(df, !is.na(Age)), aes(x = Age)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "purple", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Age of Rookies") +
  xlab("Age") +
  ylab("Density") +
  theme_minimal()

# Combine histograms into one plot
combined_histograms <- (hist_pts | hist_trb) /
  (hist_ast | hist_mpg) / 
  (hist_age + plot_layout(tag_level = 'new')) +
  plot_annotation(title = 'Distributions of Key Statistics for Rookies (1980-2020)',
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

# Save the combined histograms plot
ggsave("images/all_rookies_histograms_R.png", plot = combined_histograms, width = 12, height = 15)

# Scatter Plot for PTSpg vs. MPpg
scatter_pts_mpg <- ggplot(df, aes(x = MPpg, y = PTSpg)) +
  geom_point(color = "coral", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") + 
  ggtitle("Points vs. Minutes Per Game") +
  xlab("Minutes Per Game (MPpg)") +
  ylab("Points Per Game (PTSpg)") +
  theme_minimal()

# Scatter Plot for Age vs. PTSpg
df_age_pts <- df %>%
  filter(!is.na(Age) & !is.na(PTSpg))

scatter_age_pts <- ggplot(df_age_pts, aes(x = Age, y = PTSpg)) +
  geom_point(color = "lightgreen", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") + 
  ggtitle("Points Per Game vs. Age") +
  xlab("Age") +
  ylab("Points Per Game (PTSpg)") +
  theme_minimal()

# Combine scatter plots
combined_scatterplots <- scatter_pts_mpg | scatter_age_pts
combined_scatterplots <- combined_scatterplots +
  plot_annotation(title = 'Relationships Between Key Statistics for Rookies (1980-2020)',
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

# Save the combined scatter plots
ggsave("images/all_rookie_scatter_plots_R.png", plot = combined_scatterplots, width = 14, height = 6)

