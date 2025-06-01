library(tidyverse)
library(patchwork)

nba_data <- read_csv("nba_rookie_data_cleaned.csv")

nba_data <- nba_data %>%
  mutate(AllStarStatus = ifelse(was_all_star == 1, "All-Star", "Non All-Star"))

numeric_cols <- c(
  "MPpg", "PTSpg", "FGpg", "FG%", "3P%", "FT%",
  "ORBpg", "DRBpg", "TRBpg", "ASTpg", "STLpg", "BLKpg", "TOVpg"
)

summary_stats <- nba_data %>%
  group_by(AllStarStatus) %>%
  summarise(
    across(
      all_of(numeric_cols),
      list(mean = mean, median = median, min = min, max = max, sd = sd),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

print("Summary Statistics by All-Star Status:")
print(summary_stats)

plot_age_dist <- ggplot(nba_data, aes(x = Age, fill = AllStarStatus)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.6, bins = 30, position = "identity") +
  geom_density(aes(color = AllStarStatus), fill = NA, linewidth = 0.7) +
  labs(
    title = "Age Distribution by All-Star Status",
    x = "Age",
    y = "Density"
  ) +
  theme_minimal()
print(plot_age_dist)

plot_trb_dist <- ggplot(nba_data, aes(x = TRBpg, fill = AllStarStatus)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.6, bins = 30, position = "identity") +
  geom_density(aes(color = AllStarStatus), fill = NA, linewidth = 0.7) +
  labs(
    title = "Total Rebounds per Game Distribution",
    x = "Rebounds Per Game (TRBpg)",
    y = "Density"
  ) +
  theme_minimal()
print(plot_trb_dist)

plot_gp_dist <- ggplot(nba_data, aes(x = GP, fill = AllStarStatus)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.6, bins = 30, position = "identity") +
  geom_density(aes(color = AllStarStatus), fill = NA, linewidth = 0.7) +
  labs(
    title = "Games Played Distribution by All-Star Status",
    x = "Games Played (GP)",
    y = "Density"
  ) +
  theme_minimal()
print(plot_gp_dist)


top_years <- nba_data %>%
  count(`Year Drafted`, name = "PlayerCount") %>%
  slice_max(order_by = PlayerCount, n = 10) %>% 
  pull(`Year Drafted`)

plot_pts_by_year <- nba_data %>%
  filter(`Year Drafted` %in% top_years) %>%
  ggplot(aes(x = factor(`Year Drafted`), y = PTSpg, fill = AllStarStatus)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Points per Game by Year Drafted (Top 10 Years)",
    x = "Draft Year",
    y = "Points Per Game (PTSpg)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_pts_by_year)


plot_ast_vs_trb <- ggplot(nba_data, aes(x = ASTpg, y = TRBpg, color = AllStarStatus)) +
  geom_point(alpha = 0.5, size = 1.5) +
  labs(
    title = "Assists vs. Rebounds per Game",
    x = "Assists Per Game (ASTpg)",
    y = "Rebounds Per Game (TRBpg)"
  ) +
  theme_minimal()
print(plot_ast_vs_trb)


conf_stats <- nba_data %>%
  group_by(Conf, AllStarStatus) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = AllStarStatus,
    values_from = Count,
    values_fill = 0
  ) %>%
  mutate(
    TotalPlayers = `All-Star` + `Non All-Star`,
    `All-Star Rate (%)` = (`All-Star` / TotalPlayers) * 100
  ) %>%
  filter(TotalPlayers > 0)

plot_allstar_rate_conf <- ggplot(conf_stats, aes(x = reorder(Conf, -`All-Star Rate (%)`), y = `All-Star Rate (%)`, fill = Conf)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", `All-Star Rate (%)`)), vjust = -0.5, size = 3.5) +
  labs(
    title = "All-Star Selection Rate by Conference",
    x = "Conference",
    y = "All-Star Rate (%)"
  ) +
  theme_minimal()
print(plot_allstar_rate_conf)