library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(ggrepel)
setwd("~/schoo/info322/final proj")
df=read_csv("~/schoo/info322/final proj/nba_rookie_data_cleaned.csv")
glimpse(df)
#didnt rly use this
dfNon=nbadf |> filter(was_all_star==0)
#code to create boxplot for ONE specific stat/column, change the y= to desired column in the aes
df |> ggplot (aes(x= as_factor(was_all_star), 
                y=PTSpg, #####
                fill=as_factor(was_all_star)) ) +
          geom_boxplot() +
          geom_jitter(color="red", size=0.4, alpha=0.15)+
          ggtitle("Points per Game")+
          scale_x_discrete(labels = c("0" = "Non All-Star", "1" = "All-Star")) +
          scale_fill_discrete(name = "All-Star Status", labels = c("0" = "Non All-Star", "1" = "All-Star"))+
          labs(x = "Player Type", y = "Points per Game")
  
#reshape to general statistics such as field goal and free throw percentages
df_long <- df |> 
  pivot_longer(
    cols = where(is.numeric) & !was_all_star & !matches("pg$"),
    names_to = "Statistic",
    values_to = "Value"
  )
#boxplots 
player_stats_plot=ggplot(df_long, aes(x = as_factor(was_all_star), y = Value, fill = as_factor(was_all_star))) +
  geom_boxplot() +
  #geom_jitter(color = "gray", size = 0.4, alpha = 0.1) +
  facet_wrap(~Statistic, scales = "free_y") +
  scale_x_discrete(labels = c("0" = "Non All-Star", "1" = "All-Star")) +
  scale_fill_discrete(name = "All-Star Status", labels = c("0" = "Non All-Star", "1" = "All-Star")) +
  labs(
    title = "Boxplots of Rookie Szn Player Stats",
    x = "Player Type",
    y = "Value"
  ) +
  theme_minimal()


# reshape selected stat columns for PER GAME stats
df_long_pg <- df |> 
  pivot_longer(
    cols = where(is.numeric) & !was_all_star & matches("pg$"),
    names_to = "Statistic",
    values_to = "Value"
  )
#boxplots for all PER GAME stats
stats_pergame_plot = ggplot(df_long_pg, aes(x = as_factor(was_all_star), y = Value, fill = as_factor(was_all_star))) +
  geom_boxplot() +
  #geom_jitter(color = "gray", size = 0.4, alpha = 0.1) +
  facet_wrap(~Statistic, scales = "free_y") +
  scale_x_discrete(labels = c("0" = "Non All-Star", "1" = "All-Star")) +
  scale_fill_discrete(name = "All-Star Status", labels = c("0" = "Non All-Star", "1" = "All-Star")) +
  labs(
    title = "Rookie Season Stats PER GAME",
    x = "Player Type",
    y = "Value"
  ) +
  theme_minimal()


#get total stats for rookie szn by multiplying and stats that are PER GAME by the number of games played

#find all column names that contain "pg" 
pg_cols <- grep("pg$", names(df), value = TRUE)

df_total=df
# multiply them by GP to get total stats for rookie szn across all games 
for (col in pg_cols) {
  total_col = paste0("Total_", sub("pg$", "", col))  # rename STATpg columsn with total_stat
  df_total[[total_col]] = df[[col]] * df$GP
}

df_total[, c("Name", "GP", grep("Total_", names(df_total), value = TRUE))]
df_long_total <- df_total |> 
  pivot_longer(
    cols = where(is.numeric) & !was_all_star & matches("Total"),
    names_to = "Statistic",
    values_to = "Value"
  )

stats_total_plot = ggplot(df_long_total, aes(x = as_factor(was_all_star), y = Value, fill = as_factor(was_all_star))) +
  geom_boxplot() +
  #geom_jitter(color = "gray", size = 0.4, alpha = 0.1) +
  facet_wrap(~Statistic, scales = "free_y") +
  scale_x_discrete(labels = c("0" = "Non All-Star", "1" = "All-Star")) +
  scale_fill_discrete(name = "All-Star Status", labels = c("0" = "Non All-Star", "1" = "All-Star")) +
  labs(
    title = "Rookie Season Player Stats TOTALS",
    x = "Player Type",
    y = "Value"
  ) 
  theme_minimal()



#PLOTS
player_stats_plot
stats_pergame_plot
stats_total_plot
