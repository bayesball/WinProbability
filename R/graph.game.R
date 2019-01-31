graph.game <- function(d, game.id){
  require(ggplot2)
  d1 <- subset(d, substr(HALF.INNING, 1, 12) == game.id)
  d1$Play <- 1:dim(d1)[1]
  yr <- substr(d1[1, "HALF.INNING"], 4, 7)
  mo <- substr(d1[1, "HALF.INNING"], 8, 9)
  day <- substr(d1[1, "HALF.INNING"], 10, 11)
  TH <- theme(
    plot.title = element_text(colour = "blue",
      size = 18, hjust = 0.5
    ))
  print(ggplot(d1, aes(Play, P.NEW)) + 
          geom_line() + 
          ylim(0, 1) + TH +
          geom_hline(yintercept=0.5, color="red") +
          ylab("Probability Home Team Wins") +
          labs(title = paste(d1[1, "AWAY_TEAM_ID"], 
                            "AT",
                          substr(d1[1, "HALF.INNING"], 1, 3),
                        "--", mo, "/", day, "/", yr )))
  d1
}