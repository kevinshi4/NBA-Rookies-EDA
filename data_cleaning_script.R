# clear environment
rm(list = ls())

# load needed libraries
#install.packages("readr")
library(readr)
library(dplyr)
library(tidyr)

# first set your working directory
#setwd()

# load and inspect data
df_rookie_data_main <- read_csv("nba_rookie_data_1980-2016.csv")
glimpse(df_rookie_data_main)
df_rookie_hof_data <- read_csv("nba_rookie_hof_data_1980-2016.csv")
glimpse(df_rookie_hof_data)
df_rookie_data_extra <- read_csv("nba_rookie_data_1979-2020.csv")
glimpse(df_rookie_data_extra)
df_allstar_data <- read_csv("nba_all_star_data_1980-2022.csv")
glimpse(df_allstar_data)

# Joins required:
# 1. Join 'Hall of Fame Class' from df_rookie_hof_data with df_rookie_data_main
# 2. Add the rows with 'Year' 2017-2020 from df_rookie_data_extra to df_rookie_data_main
#    Join 'Team', 'Conf', 'Age', 'Target' from df_rookie_data_extra with df_rookie_data_main
# 3. Join players from df_allstar_data with df_rookie_data_main
#    Add the column: the teams the player played for during all star selection
#    Add the column: the years the player got selected as an all star


# Perform FIRST JOIN and data cleaning
# 1. address null values in df_rookie_data_main and df_rookie_hof_data
na_rows_rookie_main <- which(rowSums(is.na(df_rookie_data_main)) > 0)
print(na_rows_rookie_main) # these rows are empty and can be seen right away in the csv
df_rookie_data_main <- df_rookie_data_main[-na_rows_rookie_main, ]
glimpse(df_rookie_data_main)
which(rowSums(is.na(df_rookie_data_main)) > 0) # now there are no empty rows

na_rows_rookie_hof <- which(rowSums(is.na(df_rookie_hof_data)) == ncol(df_rookie_hof_data)-1)
print(na_rows_rookie_hof) # these rows are empty and can be seen right away in the csv
df_rookie_hof_data <- df_rookie_hof_data[-na_rows_rookie_hof, ]
glimpse(df_rookie_hof_data)
which(rowSums(is.na(df_rookie_hof_data)) == ncol(df_rookie_hof_data)-1) # now there are no empty rows

# 2. order the rows in df_rookie_data_main and df_rookie_hof_data by 'Year Drafted' descending
df_rookie_data_main <- df_rookie_data_main %>% arrange(desc(`Year Drafted`))
df_rookie_data_main
df_rookie_hof_data <- df_rookie_hof_data %>% arrange(desc(`Year Drafted`))
df_rookie_hof_data
tail(df_rookie_data_main, 10)
tail(df_rookie_hof_data, 10)
dim(df_rookie_data_main) # 1538   23
dim(df_rookie_hof_data)  # 1537   24

# 3. check if there are any duplicate rookie names (dimension mismatch above)
setdiff(df_rookie_data_main$Name, df_rookie_hof_data$Name)
df_rookie_data_main$Name[duplicated(df_rookie_data_main$Name)]
df_rookie_hof_data$Name[duplicated(df_rookie_hof_data$Name)]
# it seems that "Jamie Feick" is a duplicate in df_rookie_data_main, one drafted in 1996 and another in 1998
# however, "Jamie Feick" was drafted in 1996, so we need to delete the "Jamie Feick" in 1998
df_rookie_data_main <- df_rookie_data_main %>% filter(!(Name == "Jamie Feick" & `Year Drafted` == 1998))
dim(df_rookie_data_main) # 1537   23
dim(df_rookie_hof_data)  # 1537   24
# now the number of rows match

# 4. JOIN 'Hall of Fame Class' from df_rookie_hof_data with df_rookie_data_main
df_rookie_data_main <- df_rookie_data_main %>%
  left_join(df_rookie_hof_data %>% select(Name, `Year Drafted`, `Hall of Fame Class`), by = c("Name", "Year Drafted"))
glimpse(df_rookie_data_main)
dim(df_rookie_data_main) # 1537   24
# Check if any NAs exist outside of 'Hall of Fame Class'
any(rowSums(is.na(df_rookie_data_main[ , setdiff(names(df_rookie_data_main), "Hall of Fame Class")])) > 0) # no empty rows


# Perform SECOND JOIN and data cleaning
glimpse(df_rookie_data_main)
glimpse(df_rookie_data_extra)
# 1. convert '3P%' column from chr to dbl
df_rookie_data_main$`3P%` <- as.numeric(df_rookie_data_main$`3P%`)
sum(is.na(df_rookie_data_main$`3P%`))
which(is.na(df_rookie_data_main$`3P%`))
df_rookie_data_main[is.na(df_rookie_data_main$`3P%`), ]
df_rookie_data_main <- df_rookie_data_main[!is.na(df_rookie_data_main$`3P%`), ]

# 2. address any null values in df_rookie_data_extra
na_rows_rookie_extra <- which(rowSums(is.na(df_rookie_data_extra)) > 0)
print(na_rows_rookie_extra) # no null values

# 3. order the rows in df_rookie_data_extra by 'Year' descending
df_rookie_data_extra <- df_rookie_data_extra %>% arrange(desc(`Year`))
df_rookie_data_extra

# 4. filter df_rookie_data_extra to 1980-2020
df_rookie_data_extra <- df_rookie_data_extra %>% filter(Year >= 1980)
glimpse(df_rookie_data_extra)
tail(df_rookie_data_extra)

# 5. check duplicates
# there is 1 duplicated row: 278 Charles Jones    PHI    1984 and 338 Charles Jo… PHO    1984
df_rookie_data_extra %>% filter(duplicated(select(., Player, Year))) 
df_rookie_data_extra$Player[duplicated(df_rookie_data_extra$Player)]
df_rookie_data_extra %>% filter(Player %in% Player[duplicated(Player)]) %>%
  select(...1, Player, Team, Year, Games) %>% print(n = Inf)
# when doing some research, we find some erroneous entries, so we delete those rows:
#     278 Charles Jones    PHI    1984
#     1944 Marcus Williams  SAS    2007   
#     2363 Chris Smith      NYK    2013     
#     2067 Reggie Williams  GSW    2010
#     1282 Charles Jones    CHI    1999
#     453 Cedric Henderson ATL    1986
#     1550 Ken Johnson    MIA    2002
#     2191 Walker Russell DET    2012
df_rookie_data_extra <- df_rookie_data_extra %>% filter(!(Player == "Charles Jones" & Team == "PHI"))
df_rookie_data_extra <- df_rookie_data_extra %>% filter(!(Player == "Marcus Williams" & Team == "SAS"))
df_rookie_data_extra <- df_rookie_data_extra %>% filter(!(Player == "Chris Smith" & Team == "NYK"))
df_rookie_data_extra <- df_rookie_data_extra %>% filter(!(Player == "Reggie Williams" & Team == "GSW"))
df_rookie_data_extra <- df_rookie_data_extra %>% filter(!(Player == "Charles Jones" & Team == "CHI"))
df_rookie_data_extra <- df_rookie_data_extra %>% filter(!(Player == "Cedric Henderson" & Team == "ATL"))
df_rookie_data_extra <- df_rookie_data_extra %>% filter(!(Player == "Ken Johnson" & Team == "MIA"))
df_rookie_data_extra <- df_rookie_data_extra %>% filter(!(Player == "Walker Russell" & Team == "DET"))
df_rookie_data_extra$Player[duplicated(df_rookie_data_extra$Player)]
df_rookie_data_extra %>% filter(Player %in% Player[duplicated(Player)]) %>%
  select(...1, Player, Team, Year, Games) %>% print(n = Inf)

# 6. fix player names (deleting astericks)
glimpse(df_rookie_data_extra)
df_rookie_data_extra$Player <- gsub("\\*$", "", df_rookie_data_extra$Player)

# 7. Add the rows with 'Year' 2017-2020 from df_rookie_data_extra to df_rookie_data_main
df_rookie_data_extra_20172020 <- df_rookie_data_extra %>% filter(Year >= 2017)
df_rookie_data_extra_20172020
names(df_rookie_data_main)
names(df_rookie_data_extra_20172020)
df_rookie_data_extra_20172020_renamed <- df_rookie_data_extra_20172020 %>%
  select(
    `...1`, Player, Year, Games, MPpg, PTSpg, FGpg,
    `FG%`, `3P%`, `FTpg`,`FT%`, ORBpg, DRBpg, TRBpg,
    ASTpg, STLpg, BLKpg, TOVpg
  ) %>%
  rename(
    index = `...1`,
    Name = Player,
    `Year Drafted` = Year,
    GP = Games,
    MIN = MPpg,
    PTS = PTSpg,
    FGM = FGpg,
    FTM = FTpg,
    `OREB` = ORBpg,
    `DREB` = DRBpg,
    `REB` = TRBpg,
    `AST` = ASTpg,
    `STL` = STLpg,
    `BLK` = BLKpg,
    `TOV` = TOVpg
  )
head(df_rookie_data_extra_20172020_renamed, 2)
#View(df_rookie_data_extra_20172020)
common_cols <- intersect(names(df_rookie_data_main), names(df_rookie_data_extra_20172020_renamed))
df_rookie_data_main_filtered <- df_rookie_data_main %>% select(all_of(common_cols))
df_rookie_data_extra_20172020_renamed_filtered <- df_rookie_data_extra_20172020_renamed %>% select(all_of(common_cols))
df_rookie_data_main <- bind_rows(df_rookie_data_main_filtered, df_rookie_data_extra_20172020_renamed_filtered)

dim(df_rookie_data_main_filtered)
dim(df_rookie_data_extra_20172020_renamed_filtered)
glimpse(df_rookie_data_main)
df_rookie_data_main <- df_rookie_data_main %>% arrange(desc(`Year Drafted`))
glimpse(df_rookie_data_main)
check_na <- which(rowSums(is.na(df_rookie_data_main)) > 0)
print(check_na)
head(df_rookie_data_main)
tail(df_rookie_data_main)

# 8. JOIN 'Team', 'Conf', 'Age', 'Target' from df_rookie_data_extra with df_rookie_data_main
df_rookie_data_main
df_rookie_data_extra <- rename(df_rookie_data_extra, `Debut Team` = Team)
df_rookie_data_extra

df_rookie_data_main_19802020 <- df_rookie_data_main %>%
  left_join(
    df_rookie_data_extra %>% select(Player, Year, `Debut Team`, Conf, Age, Target),
    by = c("Name" = "Player", "Year Drafted" = "Year")
  )

df_rookie_data_main_19802020
dim(df_rookie_data_main_19802020)
head(df_rookie_data_main_19802020)
tail(df_rookie_data_main_19802020)
df_rookie_data_main_19802020 %>% filter(duplicated(select(., Name, `Year Drafted`)))
glimpse(df_rookie_data_main_19802020)


# Perform THIRD JOIN and data cleaning
glimpse(df_rookie_data_main_19802020)
glimpse(df_allstar_data)
# 1. address null values in df_allstar_data
sum(is.na(df_allstar_data)) # there are null values in df_allstar_data
na_rows_allstar <- which(rowSums(is.na(df_allstar_data)) > 0)
print(na_rows_allstar) 
df_allstar_data[na_rows_allstar, ]
df_allstar_data <- df_allstar_data[-na_rows_allstar, ]
glimpse(df_allstar_data)
# now there are no empty rows (the one row deleted was Magic Johnson, 
# but he is not in the rookie data, so it is acceptable to delete this row)
which(rowSums(is.na(df_allstar_data)) > 0)

# 2. order the rows in df_rookie_data_extra by 'Year' descending
df_allstar_data <- df_allstar_data %>% arrange(desc(`year`))
df_allstar_data

# 3. merge first and last name columns in df_allstar_data to get the player's full name
df_allstar_data <- df_allstar_data %>%
  mutate(name = paste(first, last, sep = " "))
glimpse(df_allstar_data)

# 4. check duplicate entries
df_allstar_data %>% filter(duplicated(select(., name, year))) # no duplicates

# 5. for each player, create a column that contains a list of the years they were all stars
years_by_name <- df_allstar_data %>%
  group_by(name) %>%
  summarise(seasons_selected = paste(sort(unique(year)), collapse = ", "), .groups = "drop")
df_allstar_data <- df_allstar_data %>%
  left_join(years_by_name, by = "name")
glimpse(df_allstar_data)
head(df_allstar_data)

# 6. for each player, create a column that contains a list of the teams they have played for when selected
teams_by_name <- df_allstar_data %>%
  group_by(name) %>%
  summarise(teams_selected_on = paste(sort(unique(team)), collapse = ", "), .groups = "drop")
df_allstar_data <- df_allstar_data %>%
  left_join(teams_by_name, by = "name")
glimpse(df_allstar_data)
head(df_allstar_data[, c("name", "team", "year", "seasons_selected", "teams_selected_on")], 10)

# 7. JOIN 'seasons_selected' and 'teams_selected_on' from df_allstar_data with df_rookie_data_main_19802020
glimpse(df_allstar_data)
df_allstar_data_subset <- df_allstar_data[, c("name", "seasons_selected", "teams_selected_on")]
df_allstar_data_subset <- df_allstar_data_subset %>% distinct(name, .keep_all = TRUE)
df_allstar_data_subset
df_allstar_data_subset %>% filter(duplicated(name)) # no duplicates
tail(df_allstar_data_subset)

library(stringi)
df_rookie_data_main_19802020 <- df_rookie_data_main_19802020 %>%
  mutate(Name_clean = stri_trans_general(Name, "Latin-ASCII"))
df_allstar_data_subset <- df_allstar_data_subset %>%
  mutate(name_clean = stri_trans_general(name, "Latin-ASCII"))
glimpse(df_rookie_data_main_19802020)
glimpse(df_allstar_data_subset)

df_rookie_data_main_19802020 <- df_rookie_data_main_19802020 %>%
  left_join(df_allstar_data_subset %>% select(name_clean, seasons_selected, teams_selected_on), by=c("Name_clean" = "name_clean"))
df_rookie_data_main_19802020
glimpse(df_rookie_data_main_19802020)

df_rookie_data_main_19802020[df_rookie_data_main_19802020$Name_clean == "Jayson Tatum", -c(1,22)]


# Save df_rookie_data_main_19802020 as the final cleaned csv
write.csv(df_rookie_data_main_19802020, "nba_cleaned2.csv", row.names = FALSE)
