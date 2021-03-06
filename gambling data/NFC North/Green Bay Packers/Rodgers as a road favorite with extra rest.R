library(tidyverse)
library(glue)
library(nflfastR)

load_sharpe_data <- function(file_name) {
  url <- glue("https://raw.githubusercontent.com/nflverse/nfldata/master/data/{file_name}.csv")
  suppressWarnings({ df <- read_csv(url, col_types = cols()) })
  return(df)
}
gambling_dataf<-load_sharpe_data("games") %>%
  select(season, week, game_type, home_coach, away_coach, home_team, away_team, home_qb_name, away_qb_name,
         home_score, away_score, spread_line, result, home_moneyline, away_moneyline,
         total_line, total, overtime, home_rest, away_rest,
         weekday, gametime)

gambling_dataf$gametime <- as.character(gambling_dataf$gametime)

rodgers <- gambling_dataf %>%
  filter(away_qb_name == 'Aaron Rodgers', !is.na(home_score)) %>%
  filter(spread_line < 0) %>%
  filter(away_rest > home_rest) %>%
  mutate(
    ATS_win = case_when(
      result - spread_line > 0 ~ 1   # HOME team covered
    ),
    ATS_loss = case_when(
      result - spread_line < 0 ~ 1  # HOME team did not cover
    ),
    ATS_push = case_when(
      result - spread_line == 0 ~ 1
    )
  ) %>%
  mutate(
    over = case_when(
      total - total_line > 0 ~ 1
    ),
    under = case_when(
      total - total_line < 0 ~ 1
    ),
    push = case_when(
      total - total_line == 0 ~ 1
      )
  )

home_ats_wins <- (sum(home_bears_games$ATS_win, na.rm = TRUE))
road_ats_wins <- (sum(home_bears_games$ATS_loss, na.rm = TRUE))
ats_push <- (sum(home_bears_games$ATS_push, na.rm = TRUE)) 

# BAD BET
chi_home_cover_pct = round((home_ats_wins/sum(home_ats_wins,road_ats_wins,ats_push))*100,3)