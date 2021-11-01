### Code adds ATS win/ loss/ and push columns

# adding columns to the datafram which fit the selected filters
           <- gambling_dataf %>%
  filter(home_team == '', !is.na(home_score)) %>%  # !is.na(home_score) used to remove upcoming games from the data
  #filter(home_coach == '' 
  #filter(home_qb_name == ''
  #filter(spread_line == ''   , etc.
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
  )
           
  #mutate(
    #over = case_when(
    #total - total_line > 0 ~ 1   
    #),
    #under = case_when(
      #total - total_line < 0 ~ -1  
    #),
    #push = case_when(
      #total - total_line == 0 ~ 2
    #)
  #)

home_ats_wins <- (sum(home_bears_games$ATS_win, na.rm = TRUE))
road_ats_wins <- (-sum(home_bears_games$ATS_loss, na.rm = TRUE))
ats_push <- (sum(home_bears_games$ATS_push, na.rm = TRUE))/2 

home_cover_pct = round((home_ats_wins/sum(home_ats_wins,road_ats_wins,ats_push))*100,3)