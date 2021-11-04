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

# filters for all thursday night games when teams have equal rest
thursday_games <- gambling_dataf %>%
  filter(weekday == 'Thursday',
         game_type == 'REG',
         home_rest == away_rest,
         !is.na(home_score)
         )

#filter so favored home teams spread_line column will now be negative
for(row in 1:nrow(thursday_games)) {
  thursday_games[row,'spread_line'] <- (thursday_games[row,'spread_line'])*-1
}
#result column will be negative if the home team wins outright
for(row in 1:nrow(thursday_games)) {
  thursday_games[row,'result'] <- (thursday_games[row,'result'])*-1
}

#add spread and total results
# ATS_win signifies that the home team has covered
thursday_games <- thursday_games %>%
  mutate(
    ATS_win = case_when(result - spread_line < 0 ~ 1),
    ATS_loss = case_when(result - spread_line > 0 ~ -1),
    ATS_push = case_when(result - spread_line == 0 ~ 2)
  ) %>%
  mutate(
    over = case_when(total - total_line > 0 ~ 1),
    under = case_when(total - total_line < 0 ~ -1),
    push = case_when(total - total_line == 0 ~ 2)
  )



home_ats_thursday <- (sum(thursday_games$ATS_win, na.rm = TRUE))
road_ats_thursday <- (-sum(thursday_games$ATS_loss, na.rm = TRUE))
ats_push_thursday <- (sum(thursday_games$ATS_push, na.rm = TRUE))/2 

thursday_overs <- (sum(thursday_games$over, na.rm = TRUE))
thursday_unders <- (-sum(thursday_games$under, na.rm = TRUE))
thursday_pushes <- (sum(thursday_games$push, na.rm = TRUE))/2 
