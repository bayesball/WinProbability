compute.win.probs <- function(d){
  prob.win.game4 <- function(d){

    # create Runs.game data frame

    library(dplyr)
    d %>% filter(BAT_HOME_ID == 1) %>%
      group_by(GAME_ID) %>%
      summarize(Home.Runs = sum(RUN1_DEST_ID >= 4) +
                  sum(RUN2_DEST_ID >= 4) +
                  sum(RUN3_DEST_ID >= 4) +
                  sum(BAT_DEST_ID >= 4)) -> Runs.game.home

    d %>% filter(BAT_HOME_ID == 0) %>%
      group_by(GAME_ID) %>%
      summarize(Home.Runs = sum(RUN1_DEST_ID >= 4) +
                  sum(RUN2_DEST_ID >= 4) +
                  sum(RUN3_DEST_ID >= 4) +
                  sum(BAT_DEST_ID >= 4)) -> Runs.game.visitor

    Runs.game <- inner_join(Runs.game.home, Runs.game.visitor,
                            by="GAME_ID")
    names(Runs.game) <- c("GAME_ID", "Home.Runs", "Visitor.Runs")

    # compute runs scored for each inning for home and away teams

    d %>% filter(BAT_HOME_ID == 1) %>%
      group_by(GAME_ID, INN_CT) %>%
      summarize(Runs = sum(RUN1_DEST_ID >= 4) +
                  sum(RUN2_DEST_ID >= 4) +
                  sum(RUN3_DEST_ID >= 4) +
                  sum(BAT_DEST_ID >= 4)) -> Runs.inning.home

    # new to incorporate half-innings
    Runs.inning.home <- rbind(data.frame(Runs.inning.home[, 1:2],
                                         Runs=0, Half=0),
                              data.frame(Runs.inning.home,
                                         Half=1)) %>%
      mutate(Half.Inning = 2 * INN_CT + Half - 1) %>%
      arrange(GAME_ID, INN_CT, Half.Inning)

    d %>% filter(BAT_HOME_ID == 0) %>%
      group_by(GAME_ID, INN_CT) %>%
      summarize(Runs = sum(RUN1_DEST_ID >= 4) +
                  sum(RUN2_DEST_ID >= 4) +
                  sum(RUN3_DEST_ID >= 4) +
                  sum(BAT_DEST_ID >= 4)) -> Runs.inning.visitor

    # new to incorporate half-innings
    Runs.inning.visitor <- rbind(data.frame(Runs.inning.visitor,
                                            Half=0),
                                 data.frame(Runs.inning.visitor[, 1:2],
                                            Runs=0, Half=1)) %>%
      mutate(Half.Inning = 2 * INN_CT + Half - 1) %>%
      arrange(GAME_ID, INN_CT, Half.Inning)

    # find running scores for each half-inning for home and away

    CRuns.inning.home <-   Runs.inning.home %>%
      group_by(GAME_ID) %>%
      mutate(CRuns = cumsum(Runs)) %>%
      select(GAME_ID, Half.Inning, CRuns)
    CRuns.inning.visitor <- Runs.inning.visitor %>%
      group_by(GAME_ID) %>%
      mutate(CRuns = cumsum(Runs))  %>%
      select(GAME_ID, Half.Inning, CRuns)

    CRuns.inning <- inner_join(CRuns.inning.home,
                               CRuns.inning.visitor,
                               by=c("GAME_ID", "Half.Inning"))

    names(CRuns.inning) <- c("GAME_ID", "Half.Inning",
                             "Home.Runs", "Visitor.Runs")
    CRuns.inning %>%
      arrange(GAME_ID, Half.Inning) -> CRuns.inning

    # merge the inning and game data frames

    Final <- inner_join(Runs.game, CRuns.inning,
                        by="GAME_ID")
    names(Final) <- c("GAME_ID", "Final.Home", "Final.Visitor",
                      "Half.Inning",
                      "Home", "Visitor")

    # add difference and game result variables

    Final %>%
      mutate(Difference = Home - Visitor,
             Result = ifelse(Final.Home >
                               Final.Visitor, 1, 0)) -> Final

    fit.model <- function(inning){
      coef(glm(Result ~ Difference,
               data = filter(Final, Half.Inning==inning),
               family = binomial))
    }

    Final %>%
      summarize(p.home.win = mean(Result)) %>%
      pull() -> p.home.win

    S0 <- data.frame(Half.Inning = 0,
                     Beta0 = log(p.home.win /
                                   (1 - p.home.win)),
                     Beta1 = NA)
    S <- sapply(1:16, fit.model)
    S1 <- rbind(S0,
                data.frame(Half.Inning=1:16,
                           Beta0=S[1, ], Beta1=S[2, ])) %>%
      mutate(Inning = c(0, rep(1:8, each = 2)),
             Half = ifelse(Half.Inning / 2 ==
                             floor(Half.Inning / 2),
                           "Bottom", "Top"))

    # fill in additional rows for innings up to 25

    S2 <- slice(S1, 16:17) %>%
      mutate(Inning = 9,
             Half.Inning = 17:18)

    for (inning in 10:25){
      S3 <- slice(S1, 16:17) %>%
        mutate(Inning = inning,
               Half.Inning =
                 c(2 * inning - 1, 2 * inning))
      S2 <- rbind(S2, S3)
    }
    rbind(S1, S2)
  }
  ##################

  S <- prob.win.game4(d)

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
