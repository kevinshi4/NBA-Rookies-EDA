df <- read.csv("nba_rookie_data_cleaned.csv")

library(tidyverse)
library(patchwork)

# Create separate datasets
all_stars_df <- df %>% filter(was_all_star == 1)
non_all_stars_df <- df %>% filter(was_all_star == 0)
all_rookies_df <- df

# ========================
# HISTOGRAM COMPARISONS
# ========================

# Points Per Game (PTSpg) Histograms
hist_pts_all_stars <- ggplot(all_stars_df, aes(x = PTSpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.5, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("All-Stars") +
  xlab("Points Per Game") +
  ylab("Density") +
  theme_minimal() +
  xlim(0, max(df$PTSpg, na.rm = TRUE))

hist_pts_non_all_stars <- ggplot(non_all_stars_df, aes(x = PTSpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.5, fill = "lightcoral", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Non-All-Stars") +
  xlab("Points Per Game") +
  ylab("Density") +
  theme_minimal() +
  xlim(0, max(df$PTSpg, na.rm = TRUE))

hist_pts_all_rookies <- ggplot(all_rookies_df, aes(x = PTSpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 1.5, fill = "lightgray", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("All Rookies") +
  xlab("Points Per Game") +
  ylab("Density") +
  theme_minimal() +
  xlim(0, max(df$PTSpg, na.rm = TRUE))

# Combine Points histograms
combined_pts_histograms <- hist_pts_all_stars | hist_pts_non_all_stars | hist_pts_all_rookies
combined_pts_histograms <- combined_pts_histograms +
  plot_annotation(title = "Points Per Game Distribution Comparison (1980-2020)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

# Save Points histograms
ggsave("images/pts_comparison_histograms.png", plot = combined_pts_histograms, width = 15, height = 5)

# Total Rebounds Per Game (TRBpg) Histograms
hist_trb_all_stars <- ggplot(all_stars_df, aes(x = TRBpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "olivedrab3", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("All-Stars") +
  xlab("Total Rebounds Per Game") +
  ylab("Density") +
  theme_minimal() +
  xlim(0, max(df$TRBpg, na.rm = TRUE))

hist_trb_non_all_stars <- ggplot(non_all_stars_df, aes(x = TRBpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightcoral", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Non-All-Stars") +
  xlab("Total Rebounds Per Game") +
  ylab("Density") +
  theme_minimal() +
  xlim(0, max(df$TRBpg, na.rm = TRUE))

hist_trb_all_rookies <- ggplot(all_rookies_df, aes(x = TRBpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightgray", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("All Rookies") +
  xlab("Total Rebounds Per Game") +
  ylab("Density") +
  theme_minimal() +
  xlim(0, max(df$TRBpg, na.rm = TRUE))

# Combine Rebounds histograms
combined_trb_histograms <- hist_trb_all_stars | hist_trb_non_all_stars | hist_trb_all_rookies
combined_trb_histograms <- combined_trb_histograms +
  plot_annotation(title = "Total Rebounds Per Game Distribution Comparison (1980-2020)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

# Save Rebounds histograms
ggsave("images/trb_comparison_histograms.png", plot = combined_trb_histograms, width = 15, height = 5)

# Assists Per Game (ASTpg) Histograms
hist_ast_all_stars <- ggplot(all_stars_df, aes(x = ASTpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "gold", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("All-Stars") +
  xlab("Assists Per Game") +
  ylab("Density") +
  theme_minimal() +
  xlim(0, max(df$ASTpg, na.rm = TRUE))

hist_ast_non_all_stars <- ggplot(non_all_stars_df, aes(x = ASTpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "lightcoral", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Non-All-Stars") +
  xlab("Assists Per Game") +
  ylab("Density") +
  theme_minimal() +
  xlim(0, max(df$ASTpg, na.rm = TRUE))

hist_ast_all_rookies <- ggplot(all_rookies_df, aes(x = ASTpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "lightgray", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("All Rookies") +
  xlab("Assists Per Game") +
  ylab("Density") +
  theme_minimal() +
  xlim(0, max(df$ASTpg, na.rm = TRUE))

# Combine Assists histograms
combined_ast_histograms <- hist_ast_all_stars | hist_ast_non_all_stars | hist_ast_all_rookies
combined_ast_histograms <- combined_ast_histograms +
  plot_annotation(title = "Assists Per Game Distribution Comparison (1980-2020)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

# Save Assists histograms
ggsave("images/ast_comparison_histograms.png", plot = combined_ast_histograms, width = 15, height = 5)

# Minutes Per Game (MPpg) Histograms
hist_mpg_all_stars <- ggplot(all_stars_df, aes(x = MPpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "turquoise4", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("All-Stars") +
  xlab("Minutes Per Game") +
  ylab("Density") +
  theme_minimal() +
  xlim(0, max(df$MPpg, na.rm = TRUE))

hist_mpg_non_all_stars <- ggplot(non_all_stars_df, aes(x = MPpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "lightcoral", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Non-All-Stars") +
  xlab("Minutes Per Game") +
  ylab("Density") +
  theme_minimal() +
  xlim(0, max(df$MPpg, na.rm = TRUE))

hist_mpg_all_rookies <- ggplot(all_rookies_df, aes(x = MPpg)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "lightgray", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("All Rookies") +
  xlab("Minutes Per Game") +
  ylab("Density") +
  theme_minimal() +
  xlim(0, max(df$MPpg, na.rm = TRUE))

# Combine Minutes histograms
combined_mpg_histograms <- hist_mpg_all_stars | hist_mpg_non_all_stars | hist_mpg_all_rookies
combined_mpg_histograms <- combined_mpg_histograms +
  plot_annotation(title = "Minutes Per Game Distribution Comparison (1980-2020)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

# Save Minutes histograms
ggsave("images/mpg_comparison_histograms.png", plot = combined_mpg_histograms, width = 15, height = 5)

# Age Histograms
hist_age_all_stars <- ggplot(filter(all_stars_df, !is.na(Age)), aes(x = Age)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "purple", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("All-Stars") +
  xlab("Age") +
  ylab("Density") +
  theme_minimal() +
  xlim(min(df$Age, na.rm = TRUE), max(df$Age, na.rm = TRUE))

hist_age_non_all_stars <- ggplot(filter(non_all_stars_df, !is.na(Age)), aes(x = Age)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightcoral", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("Non-All-Stars") +
  xlab("Age") +
  ylab("Density") +
  theme_minimal() +
  xlim(min(df$Age, na.rm = TRUE), max(df$Age, na.rm = TRUE))

hist_age_all_rookies <- ggplot(filter(all_rookies_df, !is.na(Age)), aes(x = Age)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightgray", color = "black", alpha = 0.7) +
  geom_density(alpha = .2, fill = "#FF6666") +
  ggtitle("All Rookies") +
  xlab("Age") +
  ylab("Density") +
  theme_minimal() +
  xlim(min(df$Age, na.rm = TRUE), max(df$Age, na.rm = TRUE))

# Combine Age histograms
combined_age_histograms <- hist_age_all_stars | hist_age_non_all_stars | hist_age_all_rookies
combined_age_histograms <- combined_age_histograms +
  plot_annotation(title = "Age Distribution Comparison (1980-2020)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

# Save Age histograms
ggsave("images/age_comparison_histograms.png", plot = combined_age_histograms, width = 15, height = 5)

# ========================
# SCATTER PLOT COMPARISONS
# ========================

# Points vs Minutes Scatter Plots
scatter_pts_mpg_all_stars <- ggplot(all_stars_df, aes(x = MPpg, y = PTSpg)) +
  geom_point(color = "coral", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") + 
  ggtitle("All-Stars") +
  xlab("Minutes Per Game") +
  ylab("Points Per Game") +
  theme_minimal() +
  xlim(0, max(df$MPpg, na.rm = TRUE)) +
  ylim(0, max(df$PTSpg, na.rm = TRUE))

scatter_pts_mpg_non_all_stars <- ggplot(non_all_stars_df, aes(x = MPpg, y = PTSpg)) +
  geom_point(color = "lightblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") + 
  ggtitle("Non-All-Stars") +
  xlab("Minutes Per Game") +
  ylab("Points Per Game") +
  theme_minimal() +
  xlim(0, max(df$MPpg, na.rm = TRUE)) +
  ylim(0, max(df$PTSpg, na.rm = TRUE))

scatter_pts_mpg_all_rookies <- ggplot(all_rookies_df, aes(x = MPpg, y = PTSpg)) +
  geom_point(color = "gray", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  ggtitle("All Rookies") +
  xlab("Minutes Per Game") +
  ylab("Points Per Game") +
  theme_minimal() +
  xlim(0, max(df$MPpg, na.rm = TRUE)) +
  ylim(0, max(df$PTSpg, na.rm = TRUE))

# Combine Points vs Minutes scatter plots
combined_pts_mpg_scatter <- scatter_pts_mpg_all_stars | scatter_pts_mpg_non_all_stars | scatter_pts_mpg_all_rookies
combined_pts_mpg_scatter <- combined_pts_mpg_scatter +
  plot_annotation(title = "Points vs Minutes Per Game Relationship Comparison (1980-2020)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

# Save Points vs Minutes scatter plots
ggsave("images/pts_mpg_scatter_comparison.png", plot = combined_pts_mpg_scatter, width = 15, height = 5)

# Age vs Points Scatter Plots
all_stars_age_pts <- all_stars_df %>% filter(!is.na(Age) & !is.na(PTSpg))
non_all_stars_age_pts <- non_all_stars_df %>% filter(!is.na(Age) & !is.na(PTSpg))
all_rookies_age_pts <- all_rookies_df %>% filter(!is.na(Age) & !is.na(PTSpg))

scatter_age_pts_all_stars <- ggplot(all_stars_age_pts, aes(x = Age, y = PTSpg)) +
  geom_point(color = "lightgreen", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen") + 
  ggtitle("All-Stars") +
  xlab("Age") +
  ylab("Points Per Game") +
  theme_minimal() +
  xlim(min(df$Age, na.rm = TRUE), max(df$Age, na.rm = TRUE)) +
  ylim(0, max(df$PTSpg, na.rm = TRUE))

scatter_age_pts_non_all_stars <- ggplot(non_all_stars_age_pts, aes(x = Age, y = PTSpg)) +
  geom_point(color = "orange", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") + 
  ggtitle("Non-All-Stars") +
  xlab("Age") +
  ylab("Points Per Game") +
  theme_minimal() +
  xlim(min(df$Age, na.rm = TRUE), max(df$Age, na.rm = TRUE)) +
  ylim(0, max(df$PTSpg, na.rm = TRUE))

scatter_age_pts_all_rookies <- ggplot(all_rookies_age_pts, aes(x = Age, y = PTSpg)) +
  geom_point(color = "gray", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  ggtitle("All Rookies") +
  xlab("Age") +
  ylab("Points Per Game") +
  theme_minimal() +
  xlim(min(df$Age, na.rm = TRUE), max(df$Age, na.rm = TRUE)) +
  ylim(0, max(df$PTSpg, na.rm = TRUE))

# Combine Age vs Points scatter plots
combined_age_pts_scatter <- scatter_age_pts_all_stars | scatter_age_pts_non_all_stars | scatter_age_pts_all_rookies
combined_age_pts_scatter <- combined_age_pts_scatter +
  plot_annotation(title = "Age vs Points Per Game Relationship Comparison (1980-2020)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

# Save Age vs Points scatter plots
ggsave("images/age_pts_scatter_comparison.png", plot = combined_age_pts_scatter, width = 15, height = 5)

# Prepare data with factor conversion for consistent plotting
df_factor <- df %>% 
  mutate(
    all_star_status = factor(was_all_star, levels = c(0, 1), labels = c("Non-All-Stars", "All-Stars"))
  )

all_stars_factor <- df_factor %>% filter(was_all_star == 1)
non_all_stars_factor <- df_factor %>% filter(was_all_star == 0)

# General Statistics Boxplots (excluding per-game stats)
df_long_general <- df_factor %>% 
  pivot_longer(
    cols = where(is.numeric) & !was_all_star & !matches("pg$"),
    names_to = "Statistic",
    values_to = "Value"
  )

boxplot_general_all_stars <- df_long_general %>%
  filter(was_all_star == 1) %>%
  ggplot(aes(x = Statistic, y = Value)) +
  geom_boxplot(fill = "skyblue", color = "black", alpha = 0.7) +
  ggtitle("All-Stars") +
  xlab("Statistic") +
  ylab("Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

boxplot_general_non_all_stars <- df_long_general %>%
  filter(was_all_star == 0) %>%
  ggplot(aes(x = Statistic, y = Value)) +
  geom_boxplot(fill = "lightcoral", color = "black", alpha = 0.7) +
  ggtitle("Non-All-Stars") +
  xlab("Statistic") +
  ylab("Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

boxplot_general_all_rookies <- df_long_general %>%
  ggplot(aes(x = Statistic, y = Value)) +
  geom_boxplot(fill = "lightgray", color = "black", alpha = 0.7) +
  ggtitle("All Rookies") +
  xlab("Statistic") +
  ylab("Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine General Statistics boxplots
combined_general_boxplots <- boxplot_general_all_stars / boxplot_general_non_all_stars / boxplot_general_all_rookies
combined_general_boxplots <- combined_general_boxplots +
  plot_annotation(title = "General Statistics Distribution Comparison (1980-2020)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

# Save General Statistics boxplots
ggsave("images/general_stats_boxplot_comparison.png", plot = combined_general_boxplots, width = 15, height = 12)

# Per-Game Statistics Boxplots
df_long_pg <- df_factor %>% 
  pivot_longer(
    cols = where(is.numeric) & !was_all_star & matches("pg$"),
    names_to = "Statistic",
    values_to = "Value"
  )

boxplot_pg_all_stars <- df_long_pg %>%
  filter(was_all_star == 1) %>%
  ggplot(aes(x = Statistic, y = Value)) +
  geom_boxplot(fill = "olivedrab3", color = "black", alpha = 0.7) +
  ggtitle("All-Stars") +
  xlab("Statistic") +
  ylab("Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

boxplot_pg_non_all_stars <- df_long_pg %>%
  filter(was_all_star == 0) %>%
  ggplot(aes(x = Statistic, y = Value)) +
  geom_boxplot(fill = "lightcoral", color = "black", alpha = 0.7) +
  ggtitle("Non-All-Stars") +
  xlab("Statistic") +
  ylab("Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

boxplot_pg_all_rookies <- df_long_pg %>%
  ggplot(aes(x = Statistic, y = Value)) +
  geom_boxplot(fill = "lightgray", color = "black", alpha = 0.7) +
  ggtitle("All Rookies") +
  xlab("Statistic") +
  ylab("Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine Per-Game Statistics boxplots
combined_pg_boxplots <- boxplot_pg_all_stars / boxplot_pg_non_all_stars / boxplot_pg_all_rookies
combined_pg_boxplots <- combined_pg_boxplots +
  plot_annotation(title = "Per-Game Statistics Distribution Comparison (1980-2020)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

# Save Per-Game Statistics boxplots
ggsave("images/pergame_stats_boxplot_comparison.png", plot = combined_pg_boxplots, width = 15, height = 12)

# Total Season Statistics (calculate totals by multiplying per-game by games played)
pg_cols <- grep("pg$", names(df_factor), value = TRUE)
df_total <- df_factor

# Create total statistics
for (col in pg_cols) {
  total_col <- paste0("Total_", sub("pg$", "", col))
  df_total[[total_col]] <- df_total[[col]] * df_total$GP
}

df_long_total <- df_total %>% 
  pivot_longer(
    cols = where(is.numeric) & !was_all_star & matches("Total_"),
    names_to = "Statistic",
    values_to = "Value"
  )

boxplot_total_all_stars <- df_long_total %>%
  filter(was_all_star == 1) %>%
  ggplot(aes(x = Statistic, y = Value)) +
  geom_boxplot(fill = "gold", color = "black", alpha = 0.7) +
  ggtitle("All-Stars") +
  xlab("Statistic") +
  ylab("Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

boxplot_total_non_all_stars <- df_long_total %>%
  filter(was_all_star == 0) %>%
  ggplot(aes(x = Statistic, y = Value)) +
  geom_boxplot(fill = "lightcoral", color = "black", alpha = 0.7) +
  ggtitle("Non-All-Stars") +
  xlab("Statistic") +
  ylab("Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

boxplot_total_all_rookies <- df_long_total %>%
  ggplot(aes(x = Statistic, y = Value)) +
  geom_boxplot(fill = "lightgray", color = "black", alpha = 0.7) +
  ggtitle("All Rookies") +
  xlab("Statistic") +
  ylab("Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine Total Statistics boxplots
combined_total_boxplots <- boxplot_total_all_stars / boxplot_total_non_all_stars / boxplot_total_all_rookies
combined_total_boxplots <- combined_total_boxplots +
  plot_annotation(title = "Total Season Statistics Distribution Comparison (1980-2020)",
                  theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))

# Save Total Statistics boxplots
ggsave("images/total_stats_boxplot_comparison.png", plot = combined_total_boxplots, width = 15, height = 12)

print("All comparative plots have been saved to the images/ directory!")