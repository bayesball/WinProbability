compute.win.probs <- function(d, S){
  # adds variables P.OLD, P.NEW, and WPA
  # to retrosheet data with run expectancies
  invlogit <- function(x) exp(x) / (1 + exp(x))
  
  d %>% 
    mutate(half.inning.row = 2 * INN_CT + BAT_HOME_ID,
          runs0 = ifelse(BAT_HOME_ID == 1,
      HOME_SCORE_CT - AWAY_SCORE_CT + RUNS.STATE,
      HOME_SCORE_CT - AWAY_SCORE_CT - RUNS.STATE)) -> d
  
d %>% 
 mutate(P.OLD = invlogit(S[half.inning.row, "Beta0"] +
          S[half.inning.row, "Beta1"] * runs0),
        runs1 = ifelse(BAT_HOME_ID == 1,
          HOME_SCORE_CT - AWAY_SCORE_CT + 
              RUNS.NEW.STATE + RUNS.SCORED,
          HOME_SCORE_CT - AWAY_SCORE_CT - 
              RUNS.NEW.STATE - RUNS.SCORED),
        P.NEW = invlogit(S[half.inning.row, "Beta0"] +
          S[half.inning.row, "Beta1"] * runs1),
       WPA = P.NEW - P.OLD) %>% 
      select(-c("half.inning.row", "runs0", "runs1"))
}
