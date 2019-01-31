\name{graph.game}
\alias{graph.game}

\title{
  Computes Graph of Win Probabilities for a Game
}
\description{
  Given a Retrosheet play-by-play dataset with win probability values added, function will display the win probabilities for a specific game.
}
\usage{
  graph.game(d, game.id)
}
\arguments{
  \item{d}{
    Retrosheet dataset with win probabilities computed
  }
  \item{game.id}{
    Game id in hometeam, year, month, day format
  }
}

\value{
  Retrosheet rows for the specific game.
}
\author{
  Jim Albert
}

