library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(ggrepel)
library(forcats)  # Added for as_factor() function

df <- read_csv("nba_rookie_data_cleaned.csv")
glimpse(df)

# Code to create boxplot for ONE specific stat/column, change the y= to desired column in the aes
df |> ggplot(aes(x = factor(was_all_star), 
                y = PTSpg, 
                fill = factor(was_all_star))) +
          geom_boxplot() +
          geom_jitter(color = "red", size = 0.4, alpha = 0.15) +
          ggtitle("Points per Game") +
          scale_x_discrete(labels = c("0" = "Non All-Star", "1" = "All-Star")) +
          scale_fill_discrete(name = "All-Star Status", labels = c("0" = "Non All-Star", "1" = "All-Star")) +
          labs(x = "Player Type", y = "Points per Game")
  
# Reshape to general statistics such as field goal and free throw percentages
df_long <- df |> 
  pivot_longer(
    cols = where(is.numeric) & !was_all_star & !matches("pg$"),
    names_to = "Statistic",
    values_to = "Value"
  )

# Boxplots for general stats - IMPROVED: Added ncol=4 for better layout
player_stats_plot <- ggplot(df_long, aes(x = factor(was_all_star), y = Value, fill = factor(was_all_star))) +
  geom_boxplot() +
  # geom_jitter(color = "gray", size = 0.4, alpha = 0.1) +
  facet_wrap(~Statistic, scales = "free_y", ncol = 4) +
  scale_x_discrete(labels = c("0" = "Non All-Star", "1" = "All-Star")) +
  scale_fill_discrete(name = "All-Star Status", labels = c("0" = "Non All-Star", "1" = "All-Star")) +
  labs(
    title = "Boxplots of Rookie Season Player Stats",
    x = "Player Type",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    legend.position = "bottom"
  )

# Reshape selected stat columns for PER GAME stats
df_long_pg <- df |> 
  pivot_longer(
    cols = where(is.numeric) & !was_all_star & matches("pg$"),
    names_to = "Statistic",
    values_to = "Value"
  )

# Boxplots for all PER GAME stats - IMPROVED: Added ncol=5 for better layout
stats_pergame_plot <- ggplot(df_long_pg, aes(x = factor(was_all_star), y = Value, fill = factor(was_all_star))) +
  geom_boxplot() +
  # geom_jitter(color = "gray", size = 0.4, alpha = 0.1) +
  facet_wrap(~Statistic, scales = "free_y", ncol = 5) +
  scale_x_discrete(labels = c("0" = "Non All-Star", "1" = "All-Star")) +
  scale_fill_discrete(name = "All-Star Status", labels = c("0" = "Non All-Star", "1" = "All-Star")) +
  labs(
    title = "Rookie Season Stats PER GAME",
    x = "Player Type",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    legend.position = "bottom"
  )

# Get total stats for rookie season by multiplying per game stats by games played
# Find all column names that contain "pg" 
pg_cols <- grep("pg$", names(df), value = TRUE)

df_total <- df
# Multiply them by GP to get total stats for rookie season across all games 
for (col in pg_cols) {
  total_col <- paste0("Total_", sub("pg$", "", col))  # rename STATpg columns with total_stat
  df_total[[total_col]] <- df[[col]] * df$GP
}

# View the total stats columns
df_total[, c("Name", "GP", grep("Total_", names(df_total), value = TRUE))]

df_long_total <- df_total |> 
  pivot_longer(
    cols = where(is.numeric) & !was_all_star & matches("Total"),
    names_to = "Statistic",
    values_to = "Value"
  )

# IMPROVED: Added ncol=5 and better styling for total stats plot
stats_total_plot <- ggplot(df_long_total, aes(x = factor(was_all_star), y = Value, fill = factor(was_all_star))) +
  geom_boxplot() +
  # geom_jitter(color = "gray", size = 0.4, alpha = 0.1) +
  facet_wrap(~Statistic, scales = "free_y", ncol = 5) +
  scale_x_discrete(labels = c("0" = "Non All-Star", "1" = "All-Star")) +
  scale_fill_discrete(name = "All-Star Status", labels = c("0" = "Non All-Star", "1" = "All-Star")) +
  labs(
    title = "Rookie Season Player Stats TOTALS",
    x = "Player Type",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    legend.position = "bottom"
  )

# Check if df_long_total was created properly
print(paste("Number of rows in df_long_total:", nrow(df_long_total)))
print(paste("Unique statistics:", paste(unique(df_long_total$Statistic), collapse = ", ")))

ggsave("images/player_stats_plot.png", player_stats_plot, width = 12, height = 8, dpi = 300)
ggsave("images/stats_pergame_plot.png", stats_pergame_plot, width = 15, height = 10, dpi = 300)
ggsave("images/stats_total_plot.png", stats_total_plot, width = 15, height = 10, dpi = 300)