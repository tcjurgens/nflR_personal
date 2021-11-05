library(tidyverse)
library(glue)
library(nflfastR)
library(tidyverse)

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

# converts gametimes to objects to make life easier
gambling_dataf$gametime <- as.character(gambling_dataf$gametime)

# filter for only primetime Kirk Cousins games
prime_kirk <- gambling_dataf %>%
  filter(home_qb_name == 'Kirk Cousins' | away_qb_name == 'Kirk Cousins') %>%
  filter(gametime %in% c('20:30:00',	
                         '20:25:00',
                         '20:20:00',
                         '20:15:00',
                         '19:10:00')) 
home_pt_kirk <- prime_kirk %>%
  filter(home_qb_name == 'Kirk Cousins', !is.na(home_score)) %>%
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
  )

home_ats_wins <- (sum(home_pt_kirk$ATS_win, na.rm = TRUE))
road_ats_wins <- (sum(home_pt_kirk$ATS_loss, na.rm = TRUE))
ats_push <- (sum(home_pt_kirk$ATS_push, na.rm = TRUE))

# BAD BET

