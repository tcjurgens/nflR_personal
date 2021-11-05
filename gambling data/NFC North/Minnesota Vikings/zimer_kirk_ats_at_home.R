library(tidyverse)
library(glue)

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

viks_games <- gambling_dataf %>%
  filter(home_team == 'MIN',
         home_coach == 'Mike Zimmer',
         home_qb_name == 'Kirk Cousins',
         game_type == 'REG')

viks_games <- viks_games %>%
  select(season, week, home_team, away_team, home_coach, home_qb_name, spread_line, result) %>%
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

home_ats_wins <- (sum(viks_games$ATS_win, na.rm = TRUE))
road_ats_wins <- (sum(viks_games$ATS_loss, na.rm = TRUE))
ats_push <- (sum(viks_games$ATS_push, na.rm = TRUE))

## BAD BET
zimmer_kirk_ats_home_win_pct <- round((home_ats_wins / (home_ats_wins + road_ats_wins + ats_push))*100 ,3) 
