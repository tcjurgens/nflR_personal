# practicing filtering the dataframe in different ways

library(tidyverse)
library(glue)
library(nflreadr)

#### Use this start of code when filtering for specific team data

load_sharpe_data <- function(file_name) {
  url <- glue("https://raw.githubusercontent.com/nflverse/nfldata/master/data/{file_name}.csv")
  suppressWarnings({ df <- read_csv(url, col_types = cols()) })
  return(df)
}
gambling_dataf <-load_sharpe_data("games") %>% 
  select(season, week, game_type, home_coach, away_coach, home_team, away_team, home_qb_name, away_qb_name,
         home_score, away_score, spread_line, result, home_moneyline, away_moneyline,
         total_line, total, overtime, home_rest, away_rest, div_game,
         weekday, gametime, stadium, roof, surface)

# change gametime to character type column so its easier to filter 
gambling_dataf$gametime <- as.character(gambling_dataf$gametime)

gambling_dataf <- gambling_dataf %>%
  mutate(
    over = case_when(total - total_line > 0 ~ 1),
    under = case_when(total - total_line < 0 ~ 1),
    push = case_when(total - total_line == 0 ~ 2)
  )

# filtering practice

# filter for all road favorites in 2021 and if the results
road_favorites <- gambling_dataf$spread_line < 0 & gambling_dataf$season %in% c(2021) 
road_favorites_2021 <- gambling_dataf[road_favorites,] %>%
  filter(!is.na(home_score))


overs <- (sum(road_favorites_2021$over , na.rm = TRUE))
unders <- (sum(road_favorites_2021$under, na.rm = TRUE))
# 23 overs and 25 unders when the road team has been favored. No pushes on the total

# spread results // add ATS results columns
road_favorites_2021 <- road_favorites_2021 %>%
  mutate(
    ATS_w = case_when(result - spread_line < 0 ~ 1),
    ATS_l = case_when(result - spread_line > 0 ~ 1),
    ATS_push = case_when(result - spread_line == 0 ~ 2)
  )


ATS_wins <- (sum(road_favorites_2021$ATS_w , na.rm = TRUE))
ATS_losses <- (sum(road_favorites_2021$ATS_l, na.rm = TRUE))
# there have been 24 ATS wins and 24 ATS losses when the road team has been favored in 2021

# filter for outright losses
home_team_wins <- road_favorites_2021 %>%
  filter(result > 0)

# heading into wk9,, 16/48 times the road team has been favored-- the home team has won outright. 