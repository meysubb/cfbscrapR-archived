% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfb_sp_ranking.R
\name{cfb_sp_ranking}
\alias{cfb_sp_ranking}
\title{Get S&P+ historical rating data}
\usage{
cfb_sp_ranking(year = NULL, team = NULL, conference = NULL)
}
\arguments{
\item{year}{Year (optional)}

\item{team}{Team information (optional)}

\item{confefence}{Conference Abbreivation - S&P+ information by confderence}
}
\description{
If conference is provided Get average S&P+ historical rating data by conference
}
\examples{

cfb_sp_ranking(year=2019)

cfb_sp_ranking(team='Texas A&M')

cfb_sp_ranking(2019,team="Texas")

cfb_sp_ranking(conference='SEC')

cfb_sp_ranking(year=2018,conference='SEC')
}
\keyword{SP+}
