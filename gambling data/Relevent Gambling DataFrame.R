library(tidyverse)
library(glue)

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
         weekday, gametime, referee, stadium, roof, surface, temp, wind)

# change gametime to character type column so its easier to filter 
gambling_dataf$gametime <- as.character(gambling_dataf$gametime)

#filter so favored home teams spread_line column will now be negative
for(row in 1:nrow(gambling_dataf)) {
  gambling_dataf[row,'spread_line'] <- (gambling_dataf[row,'spread_line'])*-1
}
#result column will be negative if the home team wins outright
for(row in 1:nrow(gambling_dataf)) {
  gambling_dataf[row,'result'] <- (gambling_dataf[row,'result'])*-1
}

#add spread and total results
# ATS_win signifies that the home team has covered
gambling_dataf <- gambling_dataf %>%
  mutate(
    ATS_win = case_when(result - spread_line < 0 ~ 1),
    ATS_loss = case_when(result - spread_line > 0 ~ 1),
    ATS_push = case_when(result - spread_line == 0 ~ 1)
  ) %>%
  mutate(
    over = case_when(total - total_line > 0 ~ 1),
    under = case_when(total - total_line < 0 ~ 1),
    push = case_when(total - total_line == 0 ~ 1)
  )

  




