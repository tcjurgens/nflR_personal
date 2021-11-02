library(tidyverse)
library(glue)
library(nflfastR)

load_sharpe_data <- function(file_name) {
  url <- glue("https://raw.githubusercontent.com/nflverse/nfldata/master/data/{file_name}.csv")
  suppressWarnings({ df <- read_csv(url, col_types = cols()) })
  return(df)
}
gambling_dataf <-load_sharpe_data("games") %>% 
  select(season, week, game_type, home_coach, away_coach, home_team, away_team, home_qb_name, away_qb_name,
         home_score, away_score, spread_line, result, home_moneyline, away_moneyline,
         total_line, total, overtime, home_rest, away_rest, div_game,
         weekday, gametime, referee, stadium, roof, surface, temp, wind)

gambling_dataf$gametime <- as.character(gambling_dataf$gametime)

div_games <- gambling_dataf %>%
  filter(div_game == 1, !is.na(home_score)) %>%
  filter(game_type == 'REG') %>%
  filter(season %in% c(2005:2019,2021)) %>%
  mutate(
    ATS_win = case_when(
      result - spread_line > 0 ~ 1   # HOME team covered
    ),
    ATS_loss = case_when(
      result - spread_line < 0 ~ -1  # HOME team did not cover
    ),
    ATS_push = case_when(
      result - spread_line == 0 ~ 2
    )
  ) %>%
  mutate(
    over = case_when(
      total - total_line > 0 ~ 1   # HOME team covered
    ),
    under = case_when(
      total - total_line < 0 ~ -1  # HOME team did not cover
    ),
    push = case_when(
      total - total_line == 0 ~ 2
    )
  )


home_ats_wins <- (sum(div_games$ATS_win, na.rm = TRUE))
road_ats_wins <- (-sum(div_games$ATS_loss, na.rm = TRUE))
ats_push <- (sum(div_games$ATS_push, na.rm = TRUE))/2 

overs <- (sum(div_games$over, na.rm = TRUE))
unders <- (-sum(div_games$under, na.rm = TRUE))
pushes <- (sum(div_games$push, na.rm = TRUE))/2 